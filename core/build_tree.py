"""Mirrored Elisp source tree used for loading and byte compilation."""

from __future__ import annotations

import os
import shutil
from pathlib import Path

from .context import (
    BUILD_CACHE_DIR,
    CONFIG_LISP_DIR,
    PACKAGE_AUTOLOADS,
    PACKAGE_DIR,
    ROOT,
    is_windows,
    log,
)


def should_skip_directory(path: Path) -> bool:
    name = path.name
    if name in {".git", "CVS", "RCS", "__pycache__", "test", "tests", "doc", "docs"}:
        return True
    return name.startswith(".") or (path / ".nosearch").exists()


def contains_elisp(files: list[str]) -> bool:
    return any(filename.endswith((".el", ".elc")) for filename in files)


def is_elisp_source_file(path: Path) -> bool:
    """Return whether PATH is a normal Elisp source file."""
    name = path.name
    return (
        path.suffix == ".el"
        and not name.startswith(".")
        and not name.endswith(("-autoloads.el", "-pkg.el"))
    )


def is_runtime_data_file(path: Path) -> bool:
    """Return whether PATH is runtime data needed beside package Elisp."""
    return path.suffix == ".eld" and not path.name.startswith(".")


def build_source_path(source: Path) -> Path:
    """Return straight/elpaca-style build path for SOURCE."""
    return BUILD_CACHE_DIR / source.relative_to(ROOT)


def build_elc_path(source: Path) -> Path:
    """Return .elc path next to SOURCE's build symlink."""
    return build_source_path(source).with_suffix(".elc")


def collect_build_source_files() -> list[Path]:
    """Return Elisp and runtime data files mirrored into the build directory."""
    files = [
        path for path in sorted(CONFIG_LISP_DIR.glob("*.el"))
        if is_elisp_source_file(path) and path.name != PACKAGE_AUTOLOADS.name
    ]
    if not PACKAGE_DIR.is_dir():
        return files

    for package in sorted(PACKAGE_DIR.iterdir()):
        if not package.is_dir() or should_skip_directory(package):
            continue
        for current, dirs, names in os.walk(package):
            current_path = Path(current)
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue
            dirs[:] = [
                directory for directory in dirs
                if not should_skip_directory(current_path / directory)
            ]
            for name in names:
                path = current_path / name
                if is_elisp_source_file(path) or is_runtime_data_file(path):
                    files.append(path)
    return list(dict.fromkeys(files))


def prepare_build_tree() -> None:
    """Mirror Elisp and adjacent runtime data into the build directory."""
    BUILD_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    sources = collect_build_source_files()
    desired = {build_source_path(source) for source in sources}

    removed = 0
    for built_file in BUILD_CACHE_DIR.rglob("*"):
        if built_file.is_dir() or built_file.suffix not in {".el", ".eld"}:
            continue
        if built_file not in desired:
            built_file.unlink(missing_ok=True)
            if built_file.suffix == ".el":
                built_file.with_suffix(".elc").unlink(missing_ok=True)
            removed += 1

    linked = copied = unchanged = 0
    use_symlink = not is_windows()
    for source in sources:
        target = build_source_path(source)
        target.parent.mkdir(parents=True, exist_ok=True)
        if use_symlink:
            if target.is_symlink():
                try:
                    if target.resolve() == source.resolve():
                        continue
                except OSError:
                    pass
            if target.exists() or target.is_symlink():
                target.unlink()
            target.symlink_to(source)
            linked += 1
            continue

        if target.is_symlink():
            target.unlink()
        if (
            not target.exists()
            or source.stat().st_mtime_ns > target.stat().st_mtime_ns
            or source.stat().st_size != target.stat().st_size
        ):
            shutil.copy2(source, target)
            copied += 1
        else:
            unchanged += 1
    log(
        f"已同步 build 目录: symlink {linked}, copy {copied}, "
        f"unchanged {unchanged}, removed {removed}",
        verbose_only=True,
    )
