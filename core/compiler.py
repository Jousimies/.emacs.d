"""Incremental byte compilation into the generated build mirror."""

from __future__ import annotations

import concurrent.futures
import os
import subprocess
import tempfile
from pathlib import Path

from . import context
from .build_tree import (
    build_source_path,
    is_elisp_source_file,
    prepare_build_tree,
    should_skip_directory,
)
from .context import (
    CONFIG_LISP_DIR,
    LOAD_PATH_CACHE,
    PACKAGE_AUTOLOADS,
    PACKAGE_DIR,
    ROOT,
    elisp_string,
    find_emacs,
    log,
)

# Compile every normal package by default.  Keep this denylist limited to
# packages with their own build process or known unsafe compile-time effects.
PACKAGE_BYTE_COMPILE_EXCLUDES = {
    "auctex": "built by its Makefile",
    "auctex-latexmk": "requires AUCTeX's generated tex-buf library",
    "benchmark-init-el": "built by its Makefile",
    "emacs-reader": "configured explicitly because its autoloads have side effects",
    "pdf-tools": "uses a platform-specific native server build",
    "rimel": "its Makefile supplies a liberime compile-time stub",
}

PACKAGE_DEVELOPMENT_DIRS = {"etc", "targets"}
PACKAGE_DEVELOPMENT_SUFFIXES = (
    "-bench.el",
    "-subtest.el",
    "-test.el",
    "-tests.el",
)


def is_package_development_file(filename: str) -> bool:
    """Return whether FILENAME is a package test or benchmark source."""
    return (
        filename.startswith(("test-", "tests-"))
        or "-test-" in filename
        or filename.endswith(PACKAGE_DEVELOPMENT_SUFFIXES)
    )


def is_compilable_elisp_file(path: Path) -> bool:
    """Return whether PATH is an Elisp source file worth byte compiling."""
    if not is_elisp_source_file(path):
        return False
    try:
        text = path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return False
    # Emacs accepts this setting in either the first-line cookie or the
    # trailing Local Variables block.
    return "no-byte-compile: t" not in text


def collect_config_compile_files() -> list[Path]:
    """Return config source files for byte compilation."""
    if not CONFIG_LISP_DIR.is_dir():
        return []
    return [
        path for path in sorted(CONFIG_LISP_DIR.glob("*.el"))
        if is_compilable_elisp_file(path) and path.name != PACKAGE_AUTOLOADS.name
    ]


def collect_package_compile_files() -> list[Path]:
    """Return all normal package sources except documented exclusions."""
    result: list[Path] = []
    if not PACKAGE_DIR.is_dir():
        return result

    for package in sorted(PACKAGE_DIR.iterdir()):
        if (
            not package.is_dir()
            or should_skip_directory(package)
            or package.name in PACKAGE_BYTE_COMPILE_EXCLUDES
        ):
            continue
        for current, dirs, files in os.walk(package):
            current_path = Path(current)
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue
            dirs[:] = [
                directory for directory in dirs
                if (
                    directory not in PACKAGE_DEVELOPMENT_DIRS
                    and not should_skip_directory(current_path / directory)
                )
            ]
            for filename in files:
                path = current_path / filename
                if (
                    not is_package_development_file(filename)
                    and is_compilable_elisp_file(path)
                ):
                    result.append(path)
    return list(dict.fromkeys(result))


def split_chunks(items: list[Path], chunks: int) -> list[list[Path]]:
    """Split ITEMS into at most CHUNKS balanced chunks."""
    if chunks <= 1 or len(items) <= 1:
        return [items]
    return [items[index::chunks] for index in range(chunks) if items[index::chunks]]


def run_emacs_byte_compile_chunk(
    emacs: str,
    files: list[Path],
    index: int,
) -> tuple[int, str]:
    """Byte compile one chunk of build-tree files in a separate Emacs."""
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", delete=False) as handle:
        file_list = Path(handle.name)
        for path in files:
            handle.write(path.as_posix() + "\n")

    file_list_elisp = elisp_string(file_list.as_posix())
    load_cache = elisp_string(LOAD_PATH_CACHE.as_posix())
    package_autoloads = elisp_string(PACKAGE_AUTOLOADS.as_posix())
    emacs_directory = elisp_string(ROOT.as_posix() + "/")
    verbose_elisp = "t" if context.VERBOSE else "nil"
    script = f'''
(progn
(require 'cl-lib)
(require 'bytecomp)
(setq user-emacs-directory "{emacs_directory}")
(setq byte-compile-warnings nil)
(when (file-exists-p "{load_cache}")
  (load "{load_cache}" nil t))
;; Compilation creates/removes .elc files while other workers load libraries.
;; Use normal lookup rather than the runtime feature/file-name cache here.
(setq load-path-filter-function nil)
(when (file-exists-p "{package_autoloads}")
  (load "{package_autoloads}" nil t))
(let ((files (with-temp-buffer
               (insert-file-contents "{file_list_elisp}")
               (split-string (buffer-string) "\n" t)))
      (verbose {verbose_elisp})
      (ok 0)
      (errors 0))
  (dolist (file files)
    (condition-case err
        (progn
          (when verbose (message "INFO     Byte compiling [%d] %s" {index} file))
          (if (byte-compile-file file)
              (setq ok (1+ ok))
            (setq errors (1+ errors))
            (message "ERROR    Byte compile failed %s" file)))
      (error
       (setq errors (1+ errors))
       (message "ERROR    Byte compile failed %s: %S" file err))))
  (message "INFO     byte compile chunk {index} done: %d ok, %d failed" ok errors)
  (kill-emacs (if (> errors 0) 1 0))))
'''
    try:
        result = subprocess.run(
            [emacs, "--batch", "-Q", "--eval", script],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=False,
        )
    finally:
        file_list.unlink(missing_ok=True)

    output = "\n".join(
        value for value in [result.stdout.strip(), result.stderr.strip()] if value
    )
    return result.returncode, output


def print_compile_output(output: str, failed: bool = False) -> None:
    """Print compile output according to verbosity."""
    if not output:
        return
    if context.VERBOSE or failed:
        print(output)
        return
    for line in output.splitlines():
        if line.startswith(("ERROR", "WARN")):
            print(line)


def stale_byte_compile_files(files: list[Path]) -> list[Path]:
    """Return source files whose build .elc is missing or older."""
    if context.FORCE:
        return files
    stale: list[Path] = []
    for source in files:
        build_source = build_source_path(source)
        build_elc = build_source.with_suffix(".elc")
        try:
            if (
                not build_elc.exists()
                or build_source.stat().st_mtime_ns > build_elc.stat().st_mtime_ns
            ):
                stale.append(source)
        except OSError:
            stale.append(source)
    return stale


def run_emacs_byte_compile(files: list[Path]) -> bool:
    """Byte compile FILES in the build tree, keeping .el and .elc together."""
    emacs = find_emacs()
    if not emacs:
        log("未找到 emacs，跳过 byte compile", "WARN")
        return True
    if not files:
        log("没有需要 byte compile 的文件")
        return True

    prepare_build_tree()
    stale_files = stale_byte_compile_files(files)
    if not stale_files:
        log(f"byte compile 未变化: {len(files)} 个文件已是最新")
        return True

    build_files = [build_source_path(path) for path in stale_files]
    workers = min(max(1, os.cpu_count() or 1), len(build_files), 4)
    chunks = split_chunks(build_files, workers)
    log(f"开始 byte compile: {len(build_files)} 个文件，workers={workers}")

    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
        futures = [
            executor.submit(run_emacs_byte_compile_chunk, emacs, chunk, index)
            for index, chunk in enumerate(chunks, 1)
        ]
        for future in concurrent.futures.as_completed(futures):
            returncode, output = future.result()
            failed = returncode != 0
            print_compile_output(output, failed=failed)
            if failed:
                ok = False
    if ok:
        log(f"byte compile 完成: {len(build_files)} / {len(files)} 个文件")
        return True
    log("byte compile 失败", "ERROR")
    return False


def byte_compile_files() -> bool:
    """Byte compile config files and selected packages."""
    files = collect_config_compile_files() + collect_package_compile_files()
    return run_emacs_byte_compile(files)
