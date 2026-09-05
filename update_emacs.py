#!/usr/bin/env python3
"""Emacs 配置首次安装 / 更新脚本。

功能：
  1. git pull + submodule init/update
  2. 生成 load-path + feature 缓存
  3. 自动 make 需要编译的包（auctex、benchmark-init-el）
  4. 尝试编译 pdf-tools（Linux/macOS 较顺；Windows 给出明确指引）

用法：
  python update_emacs.py              # 完整更新 + 编译 + 缓存
  python update_emacs.py --cache      # 只刷新缓存
  python update_emacs.py --build      # 只做 make / pdf-tools 编译
  python update_emacs.py --skip-git   # 跳过 git，只做编译 + 缓存
"""

from __future__ import annotations

import argparse
import os
import platform
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
PACKAGE_DIR = ROOT / "packages"
LOAD_PATH_CACHE = ROOT / "lisp" / "load-path-cache.el"

FEATURE_RE = re.compile(r"^[a-zA-Z0-9][a-zA-Z0-9_+-]*$")

# 需要 make 的包（相对 packages/ 的路径）
MAKE_TARGETS = [
    "auctex",
    "benchmark-init-el",
]


def run_command(
    command: list[str],
    cwd: Path | None = None,
    env: dict | None = None,
    check: bool = True,
) -> bool:
    try:
        subprocess.run(
            command,
            cwd=cwd or ROOT,
            env=env,
            check=check,
        )
        return True
    except subprocess.CalledProcessError as err:
        print(f"❌ 命令失败: {' '.join(command)}")
        print(f"   exit code: {err.returncode}")
        return False
    except FileNotFoundError:
        print(f"❌ 找不到命令: {command[0]}")
        return False


def which(cmd: str) -> str | None:
    return shutil.which(cmd)


def is_windows() -> bool:
    return platform.system() == "Windows"


def configure_mail_account_env() -> None:
    """首次使用时为 Windows 配置 MAIL_ACCOUNT 环境变量。"""
    if not is_windows():
        return

    if os.environ.get("MAIL_ACCOUNT"):
        print(f"✅ 已检测到 MAIL_ACCOUNT: {os.environ['MAIL_ACCOUNT']}")
        return

    if not sys.stdin.isatty():
        print("ℹ 未检测到 MAIL_ACCOUNT，且当前不是交互终端，跳过设置")
        return

    print("\n📧 未检测到 MAIL_ACCOUNT 环境变量。")
    print("   该变量会被 Emacs 中的 org2calendar-account 读取。")
    account = input("请输入默认 Microsoft 账号邮箱（直接回车跳过）: ").strip()
    if not account:
        print("ℹ 已跳过 MAIL_ACCOUNT 设置")
        return

    os.environ["MAIL_ACCOUNT"] = account

    # setx 写入当前用户环境变量；新启动的 Emacs / 终端才会读取到。
    try:
        result = subprocess.run(["setx", "MAIL_ACCOUNT", account], check=False)
    except FileNotFoundError:
        result = None
    if result is not None and result.returncode == 0:
        print("✅ 已写入用户环境变量 MAIL_ACCOUNT")
        print("   请重启 Emacs，或重新登录后生效。")
        return

    # 某些精简环境可能没有 setx，尝试 PowerShell API。
    escaped_account = account.replace("'", "''")
    ps_command = (
        "[Environment]::SetEnvironmentVariable"
        f"('MAIL_ACCOUNT', '{escaped_account}', 'User')"
    )
    try:
        result = subprocess.run(
            ["powershell", "-NoProfile", "-Command", ps_command],
            check=False,
        )
    except FileNotFoundError:
        result = None
    if result is not None and result.returncode == 0:
        print("✅ 已通过 PowerShell 写入用户环境变量 MAIL_ACCOUNT")
        print("   请重启 Emacs，或重新登录后生效。")
    else:
        print("❌ 自动写入 MAIL_ACCOUNT 失败，请手动设置系统环境变量。")


def update_git() -> bool:
    print("📦 更新主仓库...")
    if not run_command(["git", "pull", "--ff-only"]):
        return False

    print("\n📦 初始化 / 更新 submodule...")
    if not run_command(["git", "submodule", "update", "--init", "--recursive"]):
        return False

    # 可选：跟踪远程最新（首次安装建议开，日常更新也可开）
    print("\n📦 更新 submodule 到远程最新...")
    return run_command(
        ["git", "submodule", "update", "--remote", "--recursive"]
    )


def should_skip_directory(path: Path) -> bool:
    name = path.name
    if name in {".git", "CVS", "RCS", "__pycache__", "test", "tests", "doc", "docs"}:
        return True
    if name.startswith("."):
        return True
    if (path / ".nosearch").exists():
        return True
    return False


def contains_elisp(files: list[str]) -> bool:
    return any(f.endswith((".el", ".elc")) for f in files)


def collect_package_load_paths() -> list[str]:
    result: list[str] = []
    if not PACKAGE_DIR.is_dir():
        print(f"⚠ packages 目录不存在: {PACKAGE_DIR}")
        return result

    for package in sorted(PACKAGE_DIR.iterdir()):
        if not package.is_dir() or should_skip_directory(package):
            continue
        result.append(package.relative_to(ROOT).as_posix())

        for current, dirs, files in os.walk(package):
            current_path = Path(current)
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue
            dirs[:] = [
                d for d in dirs
                if not should_skip_directory(current_path / d)
            ]
            if current_path == package:
                continue
            if contains_elisp(files):
                result.append(current_path.relative_to(ROOT).as_posix())

    return list(dict.fromkeys(result))


def collect_feature_map() -> dict[str, list[str]]:
    feature_map: dict[str, list[str]] = {}
    if not PACKAGE_DIR.is_dir():
        return feature_map

    for package in sorted(PACKAGE_DIR.iterdir()):
        if not package.is_dir() or should_skip_directory(package):
            continue
        for current, dirs, files in os.walk(package):
            current_path = Path(current)
            if (current_path / ".nosearch").exists():
                dirs[:] = []
                continue
            dirs[:] = [
                d for d in dirs
                if not should_skip_directory(current_path / d)
            ]
            for fname in files:
                if not (fname.endswith(".el") or fname.endswith(".elc")):
                    continue
                if fname.endswith(("-autoloads.el", "-pkg.el",
                                   "-autoloads.elc", "-pkg.elc")):
                    continue
                stem = Path(fname).stem
                if not FEATURE_RE.match(stem):
                    continue
                rel = (current_path / fname).relative_to(ROOT).as_posix()
                feature_map.setdefault(stem, []).append(rel)

    for feat, paths in feature_map.items():
        elc = [p for p in paths if p.endswith(".elc")]
        el = [p for p in paths if p.endswith(".el")]
        feature_map[feat] = list(dict.fromkeys(elc + el))
    return feature_map


def elisp_string(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"')


def generate_load_path_cache() -> None:
    package_paths = collect_package_load_paths()
    feature_map = collect_feature_map()
    LOAD_PATH_CACHE.parent.mkdir(parents=True, exist_ok=True)

    lines: list[str] = [
        ";;; load-path-cache.el --- generated load-path + feature cache -*- lexical-binding: t; -*-",
        ";; Auto-generated by update_emacs.py. DO NOT EDIT.",
        "",
        "(let ((package-load-path",
        "       (mapcar (lambda (path) (expand-file-name path user-emacs-directory))",
        "               '(",
    ]
    for path in package_paths:
        lines.append(f'          "{elisp_string(path)}"')
    lines.extend([
        "          ))))",
        "  (setq load-path (append load-path package-load-path)))",
        "",
        "(defvar my/feature-path-cache",
        "  (let ((tbl (make-hash-table :test #'equal :size 512)))",
    ])
    for feat, paths in sorted(feature_map.items()):
        path_list = " ".join(f'"{elisp_string(p)}"' for p in paths)
        lines.append(f'    (puthash "{elisp_string(feat)}" \'({path_list}) tbl)')
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
        "                                candidates)))))",
        "      (cond",
        "       ((and abs-dirs (cl-intersection abs-dirs path :test #'file-equal-p))",
        "        (cl-intersection abs-dirs path :test #'file-equal-p))",
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
    LOAD_PATH_CACHE.write_text("\n".join(lines), encoding="utf-8")
    print(f"\n⚡ 已生成 {LOAD_PATH_CACHE.relative_to(ROOT)}")
    print(f"   load-path 目录: {len(package_paths)}  feature 条目: {len(feature_map)}")


def verify_cache() -> bool:
    if not LOAD_PATH_CACHE.exists():
        print("❌ load-path-cache.el 未生成")
        return False
    text = LOAD_PATH_CACHE.read_text(encoding="utf-8")
    if "packages/auctex/style" in text:
        print("❌ AUCTeX style 进入了 cache，请检查 .nosearch")
        return False
    print("✅ cache 检查通过")
    return True


def build_make_packages() -> bool:
    """对需要 make 的包执行 make。"""
    ok = True
    make = which("make")
    if not make:
        print("⚠ 未找到 make，跳过 auctex / benchmark-init 编译")
        print("   Windows 请在 MSYS2 UCRT64/MINGW64 里安装: pacman -S make")
        return False

    for name in MAKE_TARGETS:
        pkg = PACKAGE_DIR / name
        if not pkg.is_dir():
            print(f"⚠ 跳过 {name}（目录不存在）")
            continue
        print(f"\n🔨 make {name} ...")
        if run_command([make], cwd=pkg):
            print(f"✅ {name} 编译完成")
        else:
            print(f"❌ {name} 编译失败")
            ok = False
    return ok


def build_pdf_tools() -> bool:
    """尝试编译 pdf-tools（epdfinfo）。"""
    pdf_dir = PACKAGE_DIR / "pdf-tools"
    if not pdf_dir.is_dir():
        print("⚠ packages/pdf-tools 不存在，跳过")
        return True

    print("\n📄 处理 pdf-tools ...")

    if is_windows():
        print("""
⚠️  Windows 上 pdf-tools 需要 MSYS2 环境，脚本不会强行编译。
推荐做法（二选一）：

【推荐】安装预编译 server（最省事）
  1. 安装 MSYS2：https://www.msys2.org/
  2. 打开 MINGW64 终端，执行：
       pacman -Syu
       pacman -S mingw-w64-x86_64-emacs-pdf-tools-server
  3. 把 C:\\msys64\\mingw64\\bin 加入系统 PATH，或在 Emacs 里：
       (setenv "PATH" (concat "C:\\\\msys64\\\\mingw64\\\\bin;" (getenv "PATH")))
  4. 重启 Emacs，打开任意 PDF 即可。

【自己编译】
  1. MINGW64 终端：
       pacman -S base-devel mingw-w64-x86_64-toolchain \\
                 mingw-w64-x86_64-zlib mingw-w64-x86_64-libpng \\
                 mingw-w64-x86_64-poppler
  2. cd 到 packages/pdf-tools
  3. make -s
  4. 把 server/epdfinfo.exe 放到 pdf-tools 目录下，并确保 PATH 含 mingw64/bin
""")
        # 检测是否已经有 epdfinfo
        for candidate in [
            pdf_dir / "epdfinfo.exe",
            pdf_dir / "server" / "epdfinfo.exe",
        ]:
            if candidate.exists():
                print(f"✅ 已发现 {candidate.relative_to(ROOT)}")
                return True
        print("ℹ 当前未找到 epdfinfo.exe，请按上面步骤处理")
        return True

    # Linux / macOS
    make = which("make")
    if not make:
        print("⚠ 未找到 make，跳过 pdf-tools 编译")
        return False

    print("🔨 编译 pdf-tools (make -s) ...")
    if run_command([make, "-s"], cwd=pdf_dir):
        print("✅ pdf-tools 编译完成")
        return True
    print("❌ pdf-tools 编译失败（可稍后在 Emacs 里 M-x pdf-tools-install）")
    return False


def print_first_time_tips() -> None:
    print("""
============================================================
首次配置完成！接下来：

1. 启动 Emacs（建议先用 --debug-init 看一次有无错误）
2. 打开任意 PDF 文件，确认 pdf-tools 正常
3. 打开 .tex 文件，确认 AUCTeX 已加载（应能看到 auctex-autoloads）

Windows 特别注意：
- auctex 若 make 失败，可在 MSYS2 UCRT64 里：
    pacman -S base-devel git make texinfo
    并把 Emacs bin 与 MiKTeX bin 加入 PATH 后再 make
- pdf-tools 强烈建议用 MSYS2 的预编译包（见上方说明）

日常更新只需：
  python update_emacs.py
============================================================
""")


def main() -> int:
    parser = argparse.ArgumentParser(description="Emacs config updater / first-time setup")
    parser.add_argument("--cache", action="store_true", help="只生成 load-path 缓存")
    parser.add_argument("--build", action="store_true", help="只编译需要 make 的包")
    parser.add_argument("--skip-git", action="store_true", help="跳过 git 操作")
    args = parser.parse_args()

    print("=" * 60)
    print("Emacs configuration updater / first-time setup")
    print("=" * 60)

    configure_mail_account_env()

    if args.cache:
        generate_load_path_cache()
        return 0 if verify_cache() else 1

    if args.build:
        build_make_packages()
        build_pdf_tools()
        generate_load_path_cache()
        return 0 if verify_cache() else 1

    if not args.skip_git:
        if not (ROOT / ".git").exists():
            print(f"❌ 不是 Git 仓库: {ROOT}")
            return 1
        if not update_git():
            return 1

    print("\n🔨 编译需要 make 的包...")
    build_make_packages()

    build_pdf_tools()

    print("\n⚡ 生成 load-path 缓存...")
    generate_load_path_cache()
    if not verify_cache():
        return 1

    print_first_time_tips()
    return 0


if __name__ == "__main__":
    sys.exit(main())
