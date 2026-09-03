#!/usr/bin/env python3

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent
PACKAGE_DIR = ROOT / "packages"
LOAD_PATH_CACHE = ROOT / "lisp" / "load-path-cache.el"


def run_command(
    command: list[str],
    cwd: Path | None = None,
) -> bool:
    """Run COMMAND and return True on success."""
    try:
        subprocess.run(
            command,
            cwd=cwd or ROOT,
            check=True,
        )
        return True
    except subprocess.CalledProcessError as err:
        print(f"❌ 命令执行失败: {' '.join(command)}")
        print(f"   exit code: {err.returncode}")
        return False


def update_git_repository() -> bool:
    """Update the main Git repository."""
    print("📦 更新主仓库...")

    return run_command(
        [
            "git",
            "pull",
            "--ff-only",
        ]
    )


def init_submodules() -> bool:
    """Initialize missing submodules."""
    print("\n📦 初始化 Git submodule...")

    return run_command(
        [
            "git",
            "submodule",
            "update",
            "--init",
            "--recursive",
        ]
    )


def update_submodules() -> bool:
    """Update all Git submodules to their remote branches."""
    print("\n📦 更新 Git submodule...")

    return run_command(
        [
            "git",
            "submodule",
            "update",
            "--remote",
            "--recursive",
        ]
    )


def should_skip_directory(path: Path) -> bool:
    """Return True when PATH must not participate in load-path.

    Important rules:

    - Respect Emacs `.nosearch`.
    - Skip Git metadata.
    - Skip CVS/RCS.
    - Skip hidden directories.
    """

    name = path.name

    if name in {
        ".git",
        "CVS",
        "RCS",
        "__pycache__",
    }:
        return True

    if name.startswith("."):
        return True

    if (path / ".nosearch").exists():
        return True

    return False


def contains_elisp(files: list[str]) -> bool:
    """Return True if FILES contains an Emacs Lisp library."""
    return any(
        filename.endswith((".el", ".elc"))
        for filename in files
    )


def collect_package_load_paths() -> list[str]:
    """Collect third-party package directories for Emacs load-path.

    Package roots are included first.

    Subdirectories are recursively included only when:

    - they directly contain .el or .elc files;
    - neither they nor their parent search path is blocked by `.nosearch`.

    All returned paths are relative to ROOT, so one cache works on both
    Windows and macOS.
    """

    result: list[str] = []

    if not PACKAGE_DIR.is_dir():
        print(f"⚠ packages 目录不存在: {PACKAGE_DIR}")
        return result

    for package in sorted(PACKAGE_DIR.iterdir()):
        if not package.is_dir():
            continue

        if should_skip_directory(package):
            continue

        #
        # Top-level package directory.
        #
        # This preserves the semantics of the old:
        #
        #   (add-to-list 'load-path dir t)
        #
        result.append(
            package.relative_to(ROOT).as_posix()
        )

        for current, dirs, files in os.walk(package):
            current_path = Path(current)

            #
            # If the current directory itself has .nosearch,
            # neither it nor anything below it should be searched.
            #
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue

            #
            # Prune directories before os.walk descends into them.
            #
            dirs[:] = [
                dirname
                for dirname in dirs
                if not should_skip_directory(
                    current_path / dirname
                )
            ]

            #
            # Root package directory has already been added.
            #
            if current_path == package:
                continue

            #
            # Only add useful Lisp directories.
            #
            if contains_elisp(files):
                result.append(
                    current_path.relative_to(ROOT).as_posix()
                )

    #
    # Remove duplicates while preserving order.
    #
    return list(dict.fromkeys(result))


def elisp_string(value: str) -> str:
    """Escape VALUE for use as an Emacs Lisp string."""
    return (
        value
        .replace("\\", "\\\\")
        .replace('"', '\\"')
    )


def generate_load_path_cache() -> None:
    """Generate lisp/load-path-cache.el."""

    package_paths = collect_package_load_paths()

    LOAD_PATH_CACHE.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    lines = [
        ";;; load-path-cache.el --- generated load-path cache -*- lexical-binding: t; -*-",
        "",
        ";; This file is generated automatically by update_emacs.py.",
        ";; DO NOT EDIT MANUALLY.",
        "",
        ";; Third-party packages are intentionally appended to `load-path'.",
        ";; This keeps Emacs built-in libraries ahead of third-party packages.",
        "",
        "(let ((package-load-path",
        "       (mapcar",
        "        (lambda (path)",
        "          (expand-file-name path user-emacs-directory))",
        "        '(",
    ]

    for path in package_paths:
        lines.append(
            f'          "{elisp_string(path)}"'
        )

    lines.extend(
        [
            "          ))))",
            "",
            "  ;; Keep the original load-path first.",
            "  ;; Third-party packages come last.",
            "  (setq load-path",
            "        (append load-path package-load-path)))",
            "",
            "(provide 'load-path-cache)",
            "",
            ";;; load-path-cache.el ends here",
            "",
        ]
    )

    LOAD_PATH_CACHE.write_text(
        "\n".join(lines),
        encoding="utf-8",
    )

    print()
    print(
        f"⚡ 已生成 load-path cache:"
        f" {LOAD_PATH_CACHE.relative_to(ROOT)}"
    )
    print(
        f"   第三方 load-path 目录数量: {len(package_paths)}"
    )


def verify_load_path_cache() -> bool:
    """Perform a few inexpensive sanity checks."""

    if not LOAD_PATH_CACHE.exists():
        print("❌ load-path-cache.el 未生成")
        return False

    text = LOAD_PATH_CACHE.read_text(
        encoding="utf-8"
    )

    #
    # AUCTeX's style directory is the exact type of path that caused
    # feature shadowing such as style/url.elc shadowing builtin url.el.
    #
    suspicious = (
        "packages/auctex/style" in text
        or "packages\\auctex\\style" in text
    )

    if suspicious:
        print(
            "❌ 检测到 AUCTeX style 目录进入 load-path cache"
        )
        print(
            "   请检查 packages/auctex/style/.nosearch"
        )
        return False

    print("✅ load-path cache 基本检查通过")
    return True


def main() -> int:
    print("=" * 60)
    print("Emacs configuration updater")
    print("=" * 60)

    if not (ROOT / ".git").exists():
        print(f"❌ 不是 Git 仓库: {ROOT}")
        return 1

    if not update_git_repository():
        return 1

    if not init_submodules():
        return 1

    if not update_submodules():
        return 1

    print("\n⚡ 生成 load-path cache...")

    generate_load_path_cache()

    if not verify_load_path_cache():
        return 1

    print()
    print("=" * 60)
    print("✅ 更新完成")
    print("=" * 60)

    return 0


if __name__ == "__main__":
    sys.exit(main())
