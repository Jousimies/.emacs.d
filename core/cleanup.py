"""Removal of artifacts owned by the maintenance command."""

from __future__ import annotations

import shutil

from .context import (
    BUILD_CACHE_DIR,
    CACHE_STAMP,
    ENVIRONMENT_CACHE,
    IDLE_FEATURE_CACHE,
    LOAD_PATH_CACHE,
    PACKAGE_AUTOLOADS,
    ROOT,
    log,
)


def clean_generated_artifacts(dry_run: bool = False) -> bool:
    """Remove only files and directories owned by this script."""
    targets = (
        BUILD_CACHE_DIR,
        LOAD_PATH_CACHE,
        PACKAGE_AUTOLOADS,
        IDLE_FEATURE_CACHE,
        ENVIRONMENT_CACHE,
        CACHE_STAMP,
    )
    for target in targets:
        if not (target.exists() or target.is_symlink()):
            continue
        relative = target.relative_to(ROOT)
        if dry_run:
            log(f"would remove {relative}")
        elif target.is_dir() and not target.is_symlink():
            shutil.rmtree(target)
            log(f"已删除 {relative}")
        else:
            target.unlink()
            log(f"已删除 {relative}")
    if not dry_run:
        log("清理完成；运行 sync 可重新生成全部缓存")
    return True
