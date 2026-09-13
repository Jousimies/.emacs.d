"""Load-path feature index and package autoload generation."""

from __future__ import annotations

import os
import re
import subprocess
from pathlib import Path

from . import context
from .build_tree import (
    build_elc_path,
    build_source_path,
    contains_elisp,
    is_elisp_source_file,
    should_skip_directory,
)
from .context import (
    BUILD_CACHE_DIR,
    CONFIG_LISP_DIR,
    LOAD_PATH_CACHE,
    PACKAGE_AUTOLOADS,
    PACKAGE_DIR,
    ROOT,
    any_newer_than,
    elisp_string,
    find_emacs,
    log,
    write_text_if_changed,
)

FEATURE_RE = re.compile(r"^[a-zA-Z0-9][a-zA-Z0-9_+-]*$")
AUTOLOAD_DEF_RE = re.compile(r"^\s*\((?:cl-)?defun\s+([^\s()]+)", re.M)

# These packages have side-effectful autoload cookies and must be configured
# explicitly instead of entering the global package-autoloads.el.
PACKAGE_AUTOLOAD_EXCLUDES = {"emacs-reader", "pdf-tools"}


def collect_package_cache_data() -> tuple[list[str], dict[str, list[str]]]:
    """Collect package load-path directories and feature map in one walk."""
    load_paths: list[str] = []
    feature_map: dict[str, list[str]] = {}
    if not PACKAGE_DIR.is_dir():
        print(f"⚠ packages 目录不存在: {PACKAGE_DIR}")
        return load_paths, feature_map

    for package in sorted(PACKAGE_DIR.iterdir()):
        if not package.is_dir() or should_skip_directory(package):
            continue
        load_paths.append(package.relative_to(ROOT).as_posix())
        for current, dirs, files in os.walk(package):
            current_path = Path(current)
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue
            dirs[:] = [
                directory for directory in dirs
                if not should_skip_directory(current_path / directory)
            ]
            if current_path != package and contains_elisp(files):
                load_paths.append(current_path.relative_to(ROOT).as_posix())

            for filename in files:
                if not filename.endswith(".el"):
                    continue
                if filename.endswith(("-autoloads.el", "-pkg.el")):
                    continue
                stem = Path(filename).stem
                if not FEATURE_RE.match(stem):
                    continue
                source = current_path / filename
                paths = feature_map.setdefault(stem, [])
                paths.append(build_elc_path(source).relative_to(ROOT).as_posix())
                paths.append(build_source_path(source).relative_to(ROOT).as_posix())

    for feature, paths in feature_map.items():
        elc_files = [path for path in paths if path.endswith(".elc")]
        el_files = [path for path in paths if path.endswith(".el")]
        feature_map[feature] = list(dict.fromkeys(elc_files + el_files))
    return list(dict.fromkeys(load_paths)), feature_map


def collect_package_autoload_dirs() -> list[Path]:
    """Return package directories that directly contain autoloadable files."""
    result: list[Path] = []
    if not PACKAGE_DIR.is_dir():
        return result
    for package in sorted(PACKAGE_DIR.iterdir()):
        if (
            not package.is_dir()
            or should_skip_directory(package)
            or package.name in PACKAGE_AUTOLOAD_EXCLUDES
        ):
            continue
        for current, dirs, files in os.walk(package):
            current_path = Path(current)
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue
            dirs[:] = [
                directory for directory in dirs
                if not should_skip_directory(current_path / directory)
            ]
            if any(
                filename.endswith(".el")
                and not filename.startswith(".")
                and not filename.endswith(("-autoloads.el", "-pkg.el"))
                for filename in files
            ):
                result.append(BUILD_CACHE_DIR / current_path.relative_to(ROOT))
    return list(dict.fromkeys(result))


def collect_config_autoloads() -> list[tuple[str, str]]:
    """Collect ;;;###autoload defuns from lisp/*.el."""
    autoloads: list[tuple[str, str]] = []
    if not CONFIG_LISP_DIR.is_dir():
        return autoloads
    for file in sorted(CONFIG_LISP_DIR.glob("*.el")):
        if file.name in {LOAD_PATH_CACHE.name, PACKAGE_AUTOLOADS.name}:
            continue
        text = file.read_text(encoding="utf-8", errors="ignore")
        for chunk in text.split(";;;###autoload")[1:]:
            match = AUTOLOAD_DEF_RE.search(chunk)
            if match:
                autoloads.append((match.group(1), file.stem))
    return list(dict.fromkeys(autoloads))


def generate_load_path_cache() -> None:
    package_paths, feature_map = collect_package_cache_data()
    config_autoloads = collect_config_autoloads()
    LOAD_PATH_CACHE.parent.mkdir(parents=True, exist_ok=True)
    build_paths = [
        (BUILD_CACHE_DIR / path).relative_to(ROOT).as_posix()
        for path in ["lisp", *package_paths]
    ]
    lines: list[str] = [
        ";;; load-path-cache.el --- generated load-path + feature cache -*- lexical-binding: t; -*-",
        ";; Auto-generated by update_emacs.py. DO NOT EDIT.",
        "",
        "(require 'cl-lib)",
        "",
        "(let ((build-load-path",
        "       (mapcar (lambda (path) (expand-file-name path user-emacs-directory))",
        "               '(",
    ]
    lines.extend(f'          "{elisp_string(path)}"' for path in build_paths)
    lines.extend([
        "          ))))",
        "  (setq load-path (append build-load-path load-path)))",
        "",
        ";;; Config autoloads generated from ;;;###autoload cookies in lisp/*.el.",
    ])
    lines.extend(
        f"(autoload '{elisp_string(function)} \"{elisp_string(library)}\" nil t)"
        for function, library in config_autoloads
    )
    lines.extend([
        "",
        "(defvar my/feature-path-cache",
        "  (let ((tbl (make-hash-table :test #'equal :size 512)))",
    ])
    for feature, paths in sorted(feature_map.items()):
        path_list = " ".join(f'"{elisp_string(path)}"' for path in paths)
        lines.append(f'    (puthash "{elisp_string(feature)}" \'({path_list}) tbl)')
    lines.extend([
        "    tbl)",
        '  "feature-name → list of relative paths.")',
        "",
        "(defun my/load-path-filter (path file suffixes)",
        '  "Filter PATH using precomputed feature cache when possible."',
        "  (if (or (file-name-directory file) (not (boundp 'my/feature-path-cache)))",
        "      path",
        "    (let* ((candidates (gethash file my/feature-path-cache))",
        "           (abs-dirs (when candidates",
        "                       (delete-dups",
        "                        (mapcar (lambda (rel)",
        "                                  (file-name-directory",
        "                                   (expand-file-name rel user-emacs-directory)))",
        "                                candidates))))",
        "           ;; Both lists are expanded from `user-emacs-directory', so",
        "           ;; string comparison is sufficient.  `file-equal-p' performs",
        "           ;; filesystem work and is especially expensive on Windows.",
        "           (matched (and abs-dirs",
        "                         (cl-remove-if-not",
        "                          (lambda (dir) (member dir abs-dirs))",
        "                          path))))",
        "      (cond",
        "       (matched matched)",
        "       ((fboundp 'load-path-filter-cache-directory-files)",
        "        (load-path-filter-cache-directory-files path file suffixes))",
        "       (t path)))))",
        "",
        "(when (boundp 'load-path-filter-function)",
        "  (setq load-path-filter-function #'my/load-path-filter))",
        "",
        "(provide 'load-path-cache)",
        ";;; load-path-cache.el ends here",
        "",
    ])
    changed = write_text_if_changed(LOAD_PATH_CACHE, "\n".join(lines))
    status = "已生成" if changed else "未变化"
    log(f"{status} {LOAD_PATH_CACHE.relative_to(ROOT)}")
    log(
        f"load-path 目录: {len(package_paths)}  "
        f"feature 条目: {len(feature_map)}  "
        f"autoload 条目: {len(config_autoloads)}",
        verbose_only=not changed,
    )


def generate_package_autoloads() -> bool:
    """Generate package autoloads with Emacs' own autoload scanner."""
    emacs = find_emacs()
    if not emacs:
        print("⚠ 未找到 emacs，跳过 package-autoloads.el 生成")
        return True
    if not PACKAGE_DIR.is_dir():
        print(f"⚠ packages 目录不存在，跳过 autoload: {PACKAGE_DIR}")
        return True

    autoload_file = elisp_string(PACKAGE_AUTOLOADS.as_posix())
    autoload_dirs = collect_package_autoload_dirs()
    sources = [
        path
        for directory in autoload_dirs
        for path in directory.glob("*.el")
        if is_elisp_source_file(path)
    ]
    sources.extend(sorted((ROOT / "core").glob("*.py")))
    sources.append(ROOT / "update_emacs.py")
    if not any_newer_than(sources, PACKAGE_AUTOLOADS):
        log(f"未变化 {PACKAGE_AUTOLOADS.relative_to(ROOT)}")
        log(f"autoload 扫描目录: {len(autoload_dirs)}", verbose_only=True)
        return True

    PACKAGE_AUTOLOADS.unlink(missing_ok=True)
    build_packages_dir = BUILD_CACHE_DIR / "packages"
    dirs_elisp = "\n        ".join(
        f'(\"{elisp_string(path.relative_to(build_packages_dir).as_posix())}\" . '
        f'\"{elisp_string(path.as_posix())}\")'
        for path in autoload_dirs
    )
    verbose_elisp = "t" if context.VERBOSE else "nil"
    script = f'''
(progn
(require 'autoload)
(setq generated-autoload-file "{autoload_file}")
(let ((backup-inhibited t) (make-backup-files nil)
      (version-control 'never) (autoload-timestamps nil)
      (verbose {verbose_elisp}))
  (dolist (entry '({dirs_elisp}))
    (let ((display-name (car entry)) (dir (cdr entry)))
      (when verbose (message "INFO     Scraping %s..." display-name))
      (condition-case err
          (let ((inhibit-message t)) (update-directory-autoloads dir))
        (error (message "ERROR    Skip autoloads for %s: %S" display-name err))))))
(with-temp-buffer
  (when (file-exists-p generated-autoload-file)
    (insert-file-contents generated-autoload-file))
  (goto-char (point-min))
  (insert ";;; package-autoloads.el --- generated package autoloads -*- lexical-binding: t; -*-\n")
  (insert ";; Auto-generated by update_emacs.py. DO NOT EDIT.\n\n")
  (goto-char (point-max))
  (insert "\n(provide 'package-autoloads)\n;;; package-autoloads.el ends here\n")
  (write-region (point-min) (point-max) generated-autoload-file nil 'silent)))
'''
    result = subprocess.run(
        [emacs, "--batch", "-Q", "--eval", script],
        cwd=ROOT,
        text=True,
        capture_output=True,
        check=False,
    )
    if result.returncode != 0:
        print("❌ package-autoloads.el 生成失败")
        if result.stdout.strip():
            print(result.stdout.strip())
        if result.stderr.strip():
            print(result.stderr.strip())
        return False

    if PACKAGE_AUTOLOADS.exists():
        text = PACKAGE_AUTOLOADS.read_text(encoding="utf-8")
        build_relative = os.path.relpath(
            BUILD_CACHE_DIR, PACKAGE_AUTOLOADS.parent
        ).replace("\\", "/")
        text = text.replace(
            f'"{build_relative}/', f'"{elisp_string(BUILD_CACHE_DIR.as_posix())}/'
        )
        text = text.replace(
            '"../.cache/elisp/build/',
            f'"{elisp_string(BUILD_CACHE_DIR.as_posix())}/',
        )
        text = text.replace(
            '"../packages/', f'"{elisp_string(PACKAGE_DIR.as_posix())}/'
        )
        PACKAGE_AUTOLOADS.write_text(text, encoding="utf-8")
    log(f"已生成 {PACKAGE_AUTOLOADS.relative_to(ROOT)}")
    log(f"autoload 扫描目录: {len(autoload_dirs)}")
    if result.stderr.strip():
        for line in result.stderr.splitlines():
            if not context.VERBOSE and re.search(r"\bScraping\b", line):
                continue
            print(line)
    return True
