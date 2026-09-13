"""Python, Elisp syntax, and batch startup smoke tests."""

from __future__ import annotations

import ast
import subprocess
from pathlib import Path

from .context import (
    CONFIG_LISP_DIR,
    IDLE_FEATURE_CACHE,
    LOAD_PATH_CACHE,
    PACKAGE_AUTOLOADS,
    ROOT,
    elisp_string,
    find_emacs,
    log,
)
from .doctor import capture_output


def python_source_files() -> list[Path]:
    return [ROOT / "update_emacs.py", *sorted((ROOT / "core").glob("*.py"))]


def run_python_syntax_test() -> bool:
    for path in python_source_files():
        try:
            ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        except (OSError, SyntaxError) as error:
            log(f"Python 语法错误: {error}", "ERROR")
            return False
    log(f"Python 语法检查通过（{len(python_source_files())} 个文件）")
    return True


def elisp_test_files() -> list[Path]:
    files = [ROOT / "early-init.el", ROOT / "init.el"]
    files.extend(sorted(CONFIG_LISP_DIR.glob("*.el")))
    files.extend(sorted((ROOT / "scripts").glob("*.el")))
    return [path for path in dict.fromkeys(files) if path.is_file()]


def run_elisp_syntax_test(emacs: str) -> bool:
    """Read every local Elisp form without evaluating configuration code."""
    file_forms = " ".join(
        f'"{elisp_string(path.as_posix())}"' for path in elisp_test_files()
    )
    script = f'''
(let ((files '({file_forms})) (errors 0))
  (dolist (file files)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (emacs-lisp-mode)
          (check-parens)
          (goto-char (point-min))
          (condition-case nil
              (while t (read (current-buffer)))
            (end-of-file nil)))
      (error
       (setq errors (1+ errors))
       (princ (format "ERROR %s: %S\\n" file err)))))
  (princ (format "Checked %d Elisp files; %d syntax errors\\n"
                 (length files) errors))
  (kill-emacs (if (> errors 0) 1 0)))
'''
    result = capture_output([emacs, "--batch", "-Q", "--eval", script])
    if result is None:
        log("无法启动 Emacs 语法测试", "ERROR")
        return False
    if result.stdout.strip():
        print(result.stdout.strip())
    if result.returncode != 0 and result.stderr.strip():
        print(result.stderr.strip())
    return result.returncode == 0


def run_startup_smoke_test(emacs: str) -> bool:
    """Load early-init/init and all deferred modules in batch Emacs."""
    emacs_directory = elisp_string(ROOT.as_posix() + "/")
    early_init = elisp_string((ROOT / "early-init.el").as_posix())
    init_file = elisp_string((ROOT / "init.el").as_posix())
    script = f'''
(progn
  (setq user-emacs-directory "{emacs_directory}")
  (load "{early_init}" nil nil)
  (load "{init_file}" nil nil)
  (unless (fboundp 'my/load-config-modules)
    (error "my/load-config-modules is unavailable"))
  (my/load-config-modules)
  (let ((source-signature (my/idle-loader-source-signature))
        (raw-count (length my/idle-loader-forms)))
    (my/idle-loader--prepare)
    (unless (equal source-signature
                   my/idle-loader-generated-source-signature)
      (error "Generated idle plan does not match the raw idle queue"))
    (princ (format "Prepared idle plan: %d raw tasks -> %d generated tasks\\n"
                   raw-count (length my/idle-loader-forms))))
  (let ((failed
         (seq-filter
          (lambda (feature)
            (eq (plist-get (alist-get feature my/config-module-status) :state)
                'failed))
          my/config-modules)))
    (princ (format "Loaded %d modules; %d failed\\n"
                   (length my/config-modules) (length failed)))
    (when failed (princ (format "Failed modules: %S\\n" failed)))
    (kill-emacs (if failed 1 0))))
'''
    try:
        result = subprocess.run(
            [emacs, "--batch", "-Q", "--debug-init", "--eval", script],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=False,
            timeout=120,
        )
    except (OSError, subprocess.TimeoutExpired) as error:
        log(f"batch startup test 无法完成: {error}", "ERROR")
        return False
    if result.stdout.strip():
        print(result.stdout.strip())
    if result.returncode != 0 and result.stderr.strip():
        print(result.stderr.strip())
    return result.returncode == 0


def run_tests(skip_startup: bool = False) -> bool:
    """Run Python, Elisp syntax, and optional startup checks."""
    if not run_python_syntax_test():
        return False
    emacs = find_emacs()
    if not emacs:
        log("找不到 Emacs，无法运行 Elisp 测试", "ERROR")
        return False
    if not run_elisp_syntax_test(emacs):
        return False
    log("Elisp 语法检查通过")
    if skip_startup:
        return True
    if not all(
        path.is_file()
        for path in (LOAD_PATH_CACHE, PACKAGE_AUTOLOADS, IDLE_FEATURE_CACHE)
    ):
        log("缺少生成缓存；请先运行 sync", "ERROR")
        return False
    ok = run_startup_smoke_test(emacs)
    log(
        "batch startup smoke test 通过" if ok else "batch startup smoke test 失败",
        "INFO" if ok else "ERROR",
    )
    return ok
