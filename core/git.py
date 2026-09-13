"""Safe Git and submodule metadata updates."""

from __future__ import annotations

import concurrent.futures
import os
import subprocess
from pathlib import Path

from .context import ROOT, run_command


def get_uninitialized_submodule_paths() -> list[str]:
    """Return submodule paths that are not initialized yet."""
    try:
        result = subprocess.run(
            ["git", "submodule", "status", "--recursive"],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=False,
        )
    except FileNotFoundError:
        print("❌ 找不到命令: git")
        return []

    if result.returncode != 0:
        print("❌ 获取 submodule 状态失败")
        if result.stderr.strip():
            print(result.stderr.strip())
        return []

    paths: list[str] = []
    for line in result.stdout.splitlines():
        if not line.startswith("-"):
            continue
        parts = line[1:].strip().split()
        if len(parts) >= 2:
            paths.append(parts[1])
    return paths


def find_submodule_owner(path: str) -> tuple[Path, str] | None:
    """Find the repository that owns submodule PATH and its relative path."""
    absolute_path = (ROOT / path).resolve()
    for owner in [absolute_path.parent, *absolute_path.parents]:
        if owner == ROOT.parent:
            break
        if not ((owner / ".git").exists() or owner == ROOT):
            continue
        gitmodules = owner / ".gitmodules"
        if not gitmodules.exists():
            continue
        try:
            result = subprocess.run(
                [
                    "git", "config", "-f", str(gitmodules),
                    "--get-regexp", r"^submodule\..*\.path$",
                ],
                cwd=owner,
                text=True,
                capture_output=True,
                check=False,
            )
        except FileNotFoundError:
            return None
        if result.returncode != 0:
            continue
        for line in result.stdout.splitlines():
            parts = line.split(maxsplit=1)
            if len(parts) != 2:
                continue
            relative_path = parts[1].strip()
            if (owner / relative_path).resolve() == absolute_path:
                return owner, relative_path
    return None


def initialize_missing_submodules() -> bool:
    """Initialize missing submodules without touching existing checkouts."""
    paths = get_uninitialized_submodule_paths()
    if not paths:
        print("✅ submodule 均已初始化，跳过 checkout/update，避免打断已有分支")
        return True

    ok = True
    seen: set[str] = set()
    while paths:
        progressed = False
        print(f"📦 初始化 {len(paths)} 个缺失的 submodule（仅缺失项会检出记录版本）...")
        for path in sorted(paths, key=lambda item: item.count("/")):
            if path in seen:
                continue
            owner_info = find_submodule_owner(path)
            if owner_info is None:
                continue
            owner, relative_path = owner_info
            display_owner = owner.relative_to(ROOT) if owner != ROOT else Path(".")
            command = ["git", "submodule", "update", "--init", "--", relative_path]
            if run_command(command, cwd=owner):
                print(f"  ✅ {display_owner}/{relative_path}")
                progressed = True
            else:
                ok = False
                print(f"  ❌ {display_owner}/{relative_path}")
            seen.add(path)

        paths = [path for path in get_uninitialized_submodule_paths() if path not in seen]
        if paths and not progressed:
            ok = False
            print("❌ 仍有 submodule 未初始化，但找不到所属父仓库：")
            for path in paths:
                print(f"  - {path}")
            break
    return ok


def get_submodule_paths() -> list[Path]:
    """Return initialized submodule paths, including nested submodules."""
    try:
        result = subprocess.run(
            [
                "git", "submodule", "foreach", "--recursive", "--quiet",
                "printf '%s\\0' \"$displaypath\"",
            ],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=False,
        )
    except FileNotFoundError:
        print("❌ 找不到命令: git")
        return []

    if result.returncode != 0:
        print("❌ 获取 submodule 列表失败")
        if result.stderr.strip():
            print(result.stderr.strip())
        return []
    paths = [ROOT / path for path in result.stdout.split("\0") if path]
    return list(dict.fromkeys(paths))


def fetch_submodule(path: Path) -> tuple[Path, bool, str]:
    """Fetch one submodule; designed for parallel execution."""
    try:
        result = subprocess.run(
            ["git", "fetch", "--all", "--prune"],
            cwd=path,
            text=True,
            capture_output=True,
            check=False,
        )
    except OSError as error:
        return path, False, f"无法启动 git fetch (cwd: {path}): {error}"
    output = (result.stdout or "") + (result.stderr or "")
    return path, result.returncode == 0, output.strip()


def fetch_submodules_parallel() -> bool:
    """Fetch all submodules concurrently without changing their revisions."""
    paths = get_submodule_paths()
    if not paths:
        print("ℹ 未发现已初始化的 submodule")
        return True

    max_workers = min(8, len(paths), os.cpu_count() or 4)
    print(f"📦 并发 fetch {len(paths)} 个 submodule（workers={max_workers}）...")
    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = [executor.submit(fetch_submodule, path) for path in paths]
        for future in concurrent.futures.as_completed(futures):
            path, success, output = future.result()
            relative_path = path.relative_to(ROOT) if path.is_relative_to(ROOT) else path
            if success:
                print(f"  ✅ {relative_path}")
            else:
                ok = False
                print(f"  ❌ {relative_path}")
                if output:
                    print(output)
    return ok


def fetch_git_metadata() -> bool:
    """Fetch latest commits without changing checked-out revisions."""
    print("📦 拉取主仓库远程信息（fetch only，不更新工作区）...")
    if not run_command(["git", "fetch", "--all", "--prune"]):
        return False

    print("\n📦 初始化缺失的 submodule（不会触碰已存在的 submodule 分支）...")
    if not run_command(["git", "submodule", "sync", "--recursive"]):
        return False
    if not initialize_missing_submodules():
        return False

    print("\n📦 拉取 submodule 远程信息（fetch only，不切换到远程最新）...")
    if not fetch_submodules_parallel():
        return False

    print("\nℹ 已 fetch 最新提交，但未更新当前仓库或 submodule。")
    print("   如需更新主仓库: git pull --ff-only")
    print("   如需把 submodule 切到本仓库记录版本: git submodule update --init --recursive")
    print("   注意：上面这条 git submodule update 会让 submodule 进入 detached HEAD")
    print("   如需手动追踪 submodule 远程最新: git submodule update --remote --recursive")
    return True
