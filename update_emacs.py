#!/usr/bin/env python3
"""
并行更新 .emacs.d 的所有 git 子模块
自动识别每个子模块的默认分支（main / master）
并确保 HEAD 指向分支而不是 detached commit
"""

import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

ROOT = Path(__file__).resolve().parent
MAX_WORKERS = 8          # 可根据网络和机器调整，建议 6~12


def run(cmd, cwd=None, check=True, capture=False):
    """执行命令"""
    result = subprocess.run(
        cmd,
        cwd=cwd or ROOT,
        check=check,
        text=True,
        capture_output=True,
    )
    if capture:
        return result.stdout.strip()
    if result.returncode != 0 and check:
        raise subprocess.CalledProcessError(
            result.returncode, cmd, result.stdout, result.stderr
        )
    return result


def get_default_branch(submodule_path: Path) -> str:
    """获取子模块的默认分支名"""
    # 1. origin/HEAD（最准确）
    try:
        ref = run(
            ["git", "symbolic-ref", "refs/remotes/origin/HEAD"],
            cwd=submodule_path,
            capture=True,
            check=False,
        )
        if ref and "refs/remotes/origin/" in ref:
            return ref.split("/")[-1]
    except Exception:
        pass

    # 2. remote show
    try:
        out = run(
            ["git", "remote", "show", "origin"],
            cwd=submodule_path,
            capture=True,
            check=False,
        )
        for line in out.splitlines():
            if "HEAD branch" in line:
                return line.split(":")[-1].strip()
    except Exception:
        pass

    # 3. 本地已有分支
    for candidate in ("main", "master"):
        try:
            run(
                ["git", "show-ref", "--verify", f"refs/heads/{candidate}"],
                cwd=submodule_path,
                check=True,
                capture=True,
            )
            return candidate
        except subprocess.CalledProcessError:
            continue

    return "master"


def update_submodule(path: Path) -> str:
    """更新单个子模块，返回状态信息"""
    rel = path.relative_to(ROOT)

    try:
        run(["git", "fetch", "origin", "--prune"], cwd=path, check=False)

        branch = get_default_branch(path)

        try:
            run(["git", "checkout", branch], cwd=path)
        except subprocess.CalledProcessError:
            run(["git", "checkout", "-B", branch, f"origin/{branch}"], cwd=path)

        run(["git", "reset", "--hard", f"origin/{branch}"], cwd=path)

        return f"✅ {rel} → {branch}"
    except Exception as e:
        return f"❌ {rel} → {e}"


def main():
    print("=" * 60)
    print("并行更新 .emacs.d 子模块")
    print("=" * 60)

    os.chdir(ROOT)

    print("\n🔧 初始化并更新子模块（串行）...")
    run(["git", "submodule", "update", "--init", "--recursive"])

    result = run(["git", "submodule", "status", "--recursive"], capture=True)
    submodules = []
    for line in result.splitlines():
        parts = line.strip().split()
        if len(parts) >= 2:
            p = ROOT / parts[1]
            if p.exists():
                submodules.append(p)

    if not submodules:
        print("没有找到任何子模块")
        return

    print(f"\n找到 {len(submodules)} 个子模块，使用 {MAX_WORKERS} 个线程并行更新...\n")

    success, failed = 0, 0
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = {executor.submit(update_submodule, sm): sm for sm in submodules}

        for future in as_completed(futures):
            msg = future.result()
            print(msg)
            if msg.startswith("✅"):
                success += 1
            else:
                failed += 1

    print("\n" + "=" * 60)
    print(f"完成: {success} 成功, {failed} 失败")
    print("=" * 60)

    if failed:
        sys.exit(1)


if __name__ == "__main__":
    main()
