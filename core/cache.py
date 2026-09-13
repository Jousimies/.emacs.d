"""Coordination and validation for all generated Elisp caches."""

from __future__ import annotations

import concurrent.futures

from .autoload import generate_load_path_cache, generate_package_autoloads
from .build_tree import prepare_build_tree
from .context import (
    CACHE_STAMP,
    IDLE_FEATURE_CACHE,
    LOAD_PATH_CACHE,
    ROOT,
    log,
)
from .idle import generate_idle_feature_cache


def generate_caches_parallel() -> bool:
    """Generate load-path, autoload, and idle-feature caches."""
    prepare_build_tree()
    log("并行生成 load-path 缓存与 package autoloads")
    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        futures = {
            executor.submit(generate_load_path_cache): "load-path-cache",
            executor.submit(generate_package_autoloads): "package-autoloads",
        }
        for future in concurrent.futures.as_completed(futures):
            name = futures[future]
            try:
                result = future.result()
            except Exception as error:  # noqa: BLE001 - workflow boundary
                ok = False
                log(f"{name} 生成失败: {error!r}", "ERROR")
            else:
                if result is False:
                    ok = False
    if ok:
        ok = generate_idle_feature_cache()
    if ok:
        CACHE_STAMP.parent.mkdir(parents=True, exist_ok=True)
        CACHE_STAMP.touch()
    return ok


def verify_cache() -> bool:
    if not LOAD_PATH_CACHE.exists():
        log("load-path-cache.el 未生成", "ERROR")
        return False
    text = LOAD_PATH_CACHE.read_text(encoding="utf-8")
    if "packages/auctex/style" in text:
        log("AUCTeX style 进入了 cache，请检查 .nosearch", "ERROR")
        return False
    if not IDLE_FEATURE_CACHE.exists():
        log("idle-features.el 未生成", "ERROR")
        return False
    idle_text = IDLE_FEATURE_CACHE.read_text(encoding="utf-8")
    if (
        "my/idle-loader-generated-roots" not in idle_text
        or "my/idle-loader-generated-features" not in idle_text
    ):
        log("idle-features.el 内容无效", "ERROR")
        return False
    log("cache 检查通过")
    return True
