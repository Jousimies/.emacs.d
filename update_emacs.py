#!/usr/bin/env python3
"""Emacs 配置首次安装 / 更新脚本。

功能：
  1. git fetch 主仓库与 submodule 的最新提交，但默认不切换/更新工作区
  2. 生成 straight/elpaca 风格的 .cache/elisp/build 源文件镜像
  3. 生成 load-path + feature 缓存与 package autoloads
  4. 自动 make 需要编译的包（auctex、benchmark-init-el）
  5. 默认 byte compile 配置与常用包，.elc 放在 build 镜像目录
  6. 尝试处理 pdf-tools server（Linux 编译；Windows/macOS 给出指引）

用法：
  python update_emacs.py              # 完整更新 + make + 缓存 + byte compile
  python update_emacs.py --cache      # 只刷新缓存/autoload
  python update_emacs.py --compile    # 只刷新缓存/autoload 并 byte compile
  python update_emacs.py --build      # 只做 make / pdf-tools + 缓存 + byte compile
  python update_emacs.py --no-compile # 默认/--build 流程中跳过 byte compile
  python update_emacs.py --skip-git   # 跳过 git，只做编译 + 缓存
  python update_emacs.py --mail       # 只设置 MAIL_ACCOUNT 邮件地址
"""

from __future__ import annotations

import argparse
import concurrent.futures
import os
import platform
import plistlib
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent
PACKAGE_DIR = ROOT / "packages"
LOAD_PATH_CACHE = ROOT / "lisp" / "load-path-cache.el"
PACKAGE_AUTOLOADS = ROOT / "lisp" / "package-autoloads.el"
CONFIG_LISP_DIR = ROOT / "lisp"
ELISP_CACHE_DIR = ROOT / ".cache"
BUILD_CACHE_DIR = ELISP_CACHE_DIR / "packages-build"

FEATURE_RE = re.compile(r"^[a-zA-Z0-9][a-zA-Z0-9_+-]*$")
AUTOLOAD_DEF_RE = re.compile(
    r"^\s*\((?:cl-)?defun\s+([^\s()]+)",
    re.M,
)

# 需要 make 的包（相对 packages/ 的路径）
MAKE_TARGETS = [
    "auctex",
    "benchmark-init-el",
]

# Some packages put side-effect forms behind ;;;###autoload cookies, e.g. adding
# hooks/advice that can load unfinished native modules during startup.  These
# packages should be configured explicitly with use-package instead of being
# included in the global package-autoloads.el.
PACKAGE_AUTOLOAD_EXCLUDES = {
    "emacs-reader",
    "pdf-tools",
}

PACKAGE_COMPILE_EXCLUDES = PACKAGE_AUTOLOAD_EXCLUDES | set(MAKE_TARGETS)

BYTE_COMPILE_PACKAGES = {
    "cape",
    "compat",
    "consult",
    "dash.el",
    "denote",
    "embark",
    "gptel",
    "llama",
    "magit",
    "marginalia",
    "meow",
    "orderless",
    "org-gtd.el",
    "parsebib",
    "pdf-tools",
    "s.el",
    "with-editor",
}

COMPILE_FILE_EXCLUDES = {
    # These require optional dag-draw, which is not vendored in packages/.
    "org-gtd-dag-draw.el",
    "org-gtd-graph-mode.el",
    "org-gtd-graph-navigation.el",
    "org-gtd-graph-transient.el",
    "org-gtd-graph-view.el",
    "org-gtd-project-operations.el",
}

VERBOSE = False
FORCE = False


def log(message: str, level: str = "INFO", verbose_only: bool = False) -> None:
    """Print a small, consistent log line."""
    if verbose_only and not VERBOSE:
        return
    print(f"{level:<8} {message}")


def run_command(
    command: list[str],
    cwd: Path | None = None,
    env: dict | None = None,
) -> bool:
    """Run COMMAND and return whether it exited successfully.

    stdout/stderr are captured so failures can show complete diagnostics.
    The return value is always based on the process exit status; callers no
    longer have to rely on subprocess exceptions to detect failures.
    """
    workdir = cwd or ROOT
    try:
        result = subprocess.run(
            command,
            cwd=workdir,
            env=env,
            text=True,
            capture_output=True,
            check=False,
        )
    except FileNotFoundError:
        print(f"❌ 找不到命令: {command[0]}")
        return False

    if result.returncode == 0:
        return True

    print(f"❌ 命令失败: {shlex.join(command)}")
    print(f"   cwd: {workdir}")
    print(f"   exit code: {result.returncode}")
    if result.stdout:
        print("   stdout:")
        print(result.stdout.rstrip())
    if result.stderr:
        print("   stderr:")
        print(result.stderr.rstrip())
    return False


def which(cmd: str) -> str | None:
    return shutil.which(cmd)


def is_windows() -> bool:
    return platform.system() == "Windows"


def is_macos() -> bool:
    return platform.system() == "Darwin"


def detect_mail_account() -> str | None:
    """Detect MAIL_ACCOUNT from current env or the user's zsh startup files."""
    account = os.environ.get("MAIL_ACCOUNT")
    if account:
        return account

    zsh = which("zsh")
    if zsh:
        try:
            result = subprocess.run(
                [zsh, "-lc", "print -r -- ${MAIL_ACCOUNT-}"],
                text=True,
                capture_output=True,
                check=False,
            )
        except OSError:
            result = None
        if result is not None and result.returncode == 0:
            account = result.stdout.strip()
            if account:
                os.environ["MAIL_ACCOUNT"] = account
                return account

    # Fallback: parse simple lines in ~/.zshenv, including non-exported assignments.
    zshenv = Path.home() / ".zshenv"
    if zshenv.exists():
        pattern = re.compile(r"^\s*(?:export\s+)?MAIL_ACCOUNT=(.*)\s*$")
        for line in zshenv.read_text(encoding="utf-8", errors="ignore").splitlines():
            match = pattern.match(line)
            if not match:
                continue
            value = match.group(1).split("#", 1)[0].strip()
            if value:
                account = shlex.split(value)[0] if value else ""
                if account:
                    os.environ["MAIL_ACCOUNT"] = account
                    return account
    return None


def configure_mail_account_env(force: bool = False) -> None:
    """首次使用时为 Windows/macOS 配置 MAIL_ACCOUNT 环境变量。"""
    if not (is_windows() or is_macos()):
        return

    account = detect_mail_account()
    if account and not force:
        print(f"✅ 已检测到 MAIL_ACCOUNT: {account}")
        return

    if not sys.stdin.isatty():
        print("ℹ 未检测到 MAIL_ACCOUNT，且当前不是交互终端，跳过设置")
        return

    print("\n📧 未检测到 MAIL_ACCOUNT 环境变量。" if not account else "\n📧 重新设置 MAIL_ACCOUNT 环境变量。")
    print("   该变量会被 Emacs 中的 org2calendar-account 读取。")
    account = input("请输入默认 Microsoft 账号邮箱（直接回车跳过）: ").strip()
    if not account:
        print("ℹ 已跳过 MAIL_ACCOUNT 设置")
        return

    os.environ["MAIL_ACCOUNT"] = account

    if is_windows():
        set_windows_mail_account_env(account)
    elif is_macos():
        set_macos_mail_account_env(account)


def set_windows_mail_account_env(account: str) -> None:
    """写入 Windows 当前用户环境变量。"""
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


def set_macos_mail_account_env(account: str) -> None:
    """写入 macOS GUI 会话与默认 shell 配置。"""
    try:
        result = subprocess.run(
            ["launchctl", "setenv", "MAIL_ACCOUNT", account],
            check=False,
        )
    except FileNotFoundError:
        result = None
    if result is not None and result.returncode == 0:
        print("✅ 已写入当前 launchctl 环境变量（供 Dock/Finder 启动的 Emacs 使用）")
    else:
        print("⚠ launchctl setenv 失败，请手动设置 GUI 会话环境变量。")

    agent_dir = Path.home() / "Library" / "LaunchAgents"
    agent_file = agent_dir / "local.update-emacs.mail-account.plist"
    agent = {
        "Label": "local.update-emacs.mail-account",
        "ProgramArguments": ["/bin/launchctl", "setenv", "MAIL_ACCOUNT", account],
        "RunAtLoad": True,
    }
    try:
        agent_dir.mkdir(parents=True, exist_ok=True)
        with agent_file.open("wb") as fp:
            plistlib.dump(agent, fp)
        gui_domain = f"gui/{os.getuid()}"
        subprocess.run(
            ["launchctl", "bootout", gui_domain, str(agent_file)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
        subprocess.run(
            ["launchctl", "bootstrap", gui_domain, str(agent_file)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
        print(f"✅ 已写入登录自启动环境变量: {agent_file}")
    except OSError as err:
        print(f"⚠ 写入 LaunchAgent 失败: {err}")

    shell_name = Path(os.environ.get("SHELL", "")).name
    if shell_name == "fish":
        rc_file = Path.home() / ".config" / "fish" / "config.fish"
        line = f"set -gx MAIL_ACCOUNT {account};"
    elif shell_name == "bash":
        rc_file = Path.home() / ".bash_profile"
        line = f"export MAIL_ACCOUNT={shlex.quote(account)}"
    else:
        rc_file = Path.home() / ".zshenv"
        line = f"export MAIL_ACCOUNT={shlex.quote(account)}"

    marker_begin = "# >>> update_emacs.py MAIL_ACCOUNT >>>"
    marker_end = "# <<< update_emacs.py MAIL_ACCOUNT <<<"
    block = f"{marker_begin}\n{line}\n{marker_end}\n"

    try:
        rc_file.parent.mkdir(parents=True, exist_ok=True)
        old = rc_file.read_text(encoding="utf-8") if rc_file.exists() else ""
        if marker_begin in old and marker_end in old:
            new = re.sub(
                rf"{re.escape(marker_begin)}.*?{re.escape(marker_end)}\n?",
                block,
                old,
                flags=re.S,
            )
        else:
            new = old.rstrip() + ("\n\n" if old.strip() else "") + block
        rc_file.write_text(new, encoding="utf-8")
        print(f"✅ 已写入 shell 配置: {rc_file}")
        print("   请重启 Emacs/终端，或重新登录后生效。")
    except OSError as err:
        print(f"❌ 写入 shell 配置失败: {err}")
        print(f"   可手动加入: {line}")


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
        # `git submodule status` prefixes uninitialized submodules with '-'.
        if not line.startswith("-"):
            continue
        parts = line[1:].strip().split()
        if len(parts) >= 2:
            paths.append(parts[1])
    return paths


def find_submodule_owner(path: str) -> tuple[Path, str] | None:
    """Find the repository that owns submodule PATH and its relative path there."""
    abs_path = (ROOT / path).resolve()
    for owner in [abs_path.parent, *abs_path.parents]:
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
            rel = parts[1].strip()
            if (owner / rel).resolve() == abs_path:
                return owner, rel
    return None


def initialize_missing_submodules() -> bool:
    """Initialize missing submodules only; do not touch already checked-out ones."""
    paths = get_uninitialized_submodule_paths()
    if not paths:
        print("✅ submodule 均已初始化，跳过 checkout/update，避免打断已有分支")
        return True

    ok = True
    seen: set[str] = set()
    while paths:
        progressed = False
        print(f"📦 初始化 {len(paths)} 个缺失的 submodule（仅缺失项会检出记录版本）...")
        for path in sorted(paths, key=lambda p: p.count("/")):
            if path in seen:
                continue
            owner_info = find_submodule_owner(path)
            if owner_info is None:
                # This can happen for nested submodules before their parent is initialized.
                continue
            owner, rel = owner_info
            display_owner = owner.relative_to(ROOT) if owner != ROOT else Path(".")
            if run_command(["git", "submodule", "update", "--init", "--", rel], cwd=owner):
                print(f"  ✅ {display_owner}/{rel}")
                seen.add(path)
                progressed = True
            else:
                ok = False
                print(f"  ❌ {display_owner}/{rel}")
                seen.add(path)

        paths = [p for p in get_uninitialized_submodule_paths() if p not in seen]
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
                "printf '%s\\n' \"$toplevel/$sm_path\"",
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

    paths = [Path(line.strip()) for line in result.stdout.splitlines() if line.strip()]
    return list(dict.fromkeys(paths))


def fetch_submodule(path: Path) -> tuple[Path, bool, str]:
    """Fetch one submodule. Designed for parallel execution."""
    result = subprocess.run(
        ["git", "fetch", "--all", "--prune"],
        cwd=path,
        text=True,
        capture_output=True,
        check=False,
    )
    output = (result.stdout or "") + (result.stderr or "")
    return path, result.returncode == 0, output.strip()


def fetch_submodules_parallel() -> bool:
    """Fetch all submodules concurrently without changing checked-out revisions."""
    paths = get_submodule_paths()
    if not paths:
        print("ℹ 未发现已初始化的 submodule")
        return True

    max_workers = min(8, len(paths), (os.cpu_count() or 4))
    print(f"📦 并发 fetch {len(paths)} 个 submodule（workers={max_workers}）...")

    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = [executor.submit(fetch_submodule, path) for path in paths]
        for future in concurrent.futures.as_completed(futures):
            path, success, output = future.result()
            rel = path.relative_to(ROOT) if path.is_relative_to(ROOT) else path
            if success:
                print(f"  ✅ {rel}")
            else:
                ok = False
                print(f"  ❌ {rel}")
                if output:
                    print(output)
    return ok


def update_git() -> bool:
    """Fetch latest commits without changing the checked-out revisions."""
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


def build_source_path(source: Path) -> Path:
    """Return straight/elpaca-style build path for SOURCE."""
    return BUILD_CACHE_DIR / source.relative_to(ROOT)


def build_elc_path(source: Path) -> Path:
    """Return .elc path next to SOURCE's build symlink."""
    return build_source_path(source).with_suffix(".elc")


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
                d for d in dirs
                if not should_skip_directory(current_path / d)
            ]

            has_elisp = contains_elisp(files)
            if current_path != package and has_elisp:
                load_paths.append(current_path.relative_to(ROOT).as_posix())

            for fname in files:
                if not fname.endswith(".el"):
                    continue
                if fname.endswith(("-autoloads.el", "-pkg.el")):
                    continue
                stem = Path(fname).stem
                if not FEATURE_RE.match(stem):
                    continue
                source = current_path / fname
                feature_map.setdefault(stem, []).append(build_elc_path(source).relative_to(ROOT).as_posix())
                feature_map.setdefault(stem, []).append(build_source_path(source).relative_to(ROOT).as_posix())

    for feat, paths in feature_map.items():
        elc = [p for p in paths if p.endswith(".elc")]
        el = [p for p in paths if p.endswith(".el")]
        feature_map[feat] = list(dict.fromkeys(elc + el))

    return list(dict.fromkeys(load_paths)), feature_map


def collect_package_autoload_dirs() -> list[Path]:
    """Return package directories that directly contain autoloadable .el files."""
    result: list[Path] = []
    if not PACKAGE_DIR.is_dir():
        return result

    for package in sorted(PACKAGE_DIR.iterdir()):
        if (not package.is_dir()
                or should_skip_directory(package)
                or package.name in PACKAGE_AUTOLOAD_EXCLUDES):
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
            if any(
                f.endswith(".el")
                and not f.startswith(".")
                and not f.endswith(("-autoloads.el", "-pkg.el"))
                for f in files
            ):
                result.append(BUILD_CACHE_DIR / current_path.relative_to(ROOT))

    return list(dict.fromkeys(result))


def elisp_string(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"')


def write_text_if_changed(path: Path, content: str) -> bool:
    """Write CONTENT to PATH only when it changed."""
    if path.exists() and path.read_text(encoding="utf-8", errors="ignore") == content:
        return False
    path.write_text(content, encoding="utf-8")
    return True


def any_newer_than(paths: list[Path], target: Path) -> bool:
    """Return whether any existing path is newer than TARGET."""
    if FORCE or not target.exists():
        return True
    target_mtime = target.stat().st_mtime_ns
    for path in paths:
        try:
            if path.stat().st_mtime_ns > target_mtime:
                return True
        except OSError:
            return True
    return False


def collect_config_autoloads() -> list[tuple[str, str]]:
    """Collect ;;;###autoload defuns from lisp/*.el."""
    autoloads: list[tuple[str, str]] = []
    if not CONFIG_LISP_DIR.is_dir():
        return autoloads

    for file in sorted(CONFIG_LISP_DIR.glob("*.el")):
        if file.name in {LOAD_PATH_CACHE.name, PACKAGE_AUTOLOADS.name}:
            continue
        text = file.read_text(encoding="utf-8", errors="ignore")
        parts = text.split(";;;###autoload")
        for chunk in parts[1:]:
            match = AUTOLOAD_DEF_RE.search(chunk)
            if not match:
                continue
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
    for path in build_paths:
        lines.append(f'          "{elisp_string(path)}"')
    lines.extend([
        "          ))))",
        "  (setq load-path (append build-load-path load-path)))",
        "",
        ";;; Config autoloads generated from ;;;###autoload cookies in lisp/*.el.",
    ])
    for fn, library in config_autoloads:
        lines.append(
            f"(autoload '{elisp_string(fn)} \"{elisp_string(library)}\" nil t)"
        )
    lines.extend([
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
    emacs = which("emacs")
    if not emacs:
        print("⚠ 未找到 emacs，跳过 package-autoloads.el 生成")
        return True
    if not PACKAGE_DIR.is_dir():
        print(f"⚠ packages 目录不存在，跳过 autoload: {PACKAGE_DIR}")
        return True

    autoload_file = elisp_string(PACKAGE_AUTOLOADS.as_posix())
    autoload_dirs = collect_package_autoload_dirs()
    autoload_sources = [
        path
        for directory in autoload_dirs
        for path in directory.glob("*.el")
        if is_elisp_source_file(path)
    ] + [Path(__file__)]
    if not any_newer_than(autoload_sources, PACKAGE_AUTOLOADS):
        log(f"未变化 {PACKAGE_AUTOLOADS.relative_to(ROOT)}")
        log(f"autoload 扫描目录: {len(autoload_dirs)}", verbose_only=True)
        return True

    PACKAGE_AUTOLOADS.unlink(missing_ok=True)
    build_packages_dir = BUILD_CACHE_DIR / "packages"
    dirs_elisp = "\n        ".join(
        f'("{elisp_string(path.relative_to(build_packages_dir).as_posix())}" . "{elisp_string(path.as_posix())}")'
        for path in autoload_dirs
    )
    verbose_elisp = "t" if VERBOSE else "nil"
    script = f'''
(progn
(require 'autoload)
(setq generated-autoload-file "{autoload_file}")
(let ((backup-inhibited t)
      (make-backup-files nil)
      (version-control 'never)
      (autoload-timestamps nil)
      (verbose {verbose_elisp}))
  (dolist (entry '({dirs_elisp}))
    (let ((display-name (car entry))
          (dir (cdr entry)))
      (when verbose
        (message "INFO     Scraping %s for package-autoloads.el..." display-name))
      (condition-case err
          (let ((inhibit-message t))
            (update-directory-autoloads dir))
        (error
         (message "ERROR    Skip autoloads for %s: %S" display-name err)))
      (when verbose
        (message "INFO     Scraping %s for package-autoloads.el...done" display-name)))))
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
    if result.returncode == 0:
        if PACKAGE_AUTOLOADS.exists():
            text = PACKAGE_AUTOLOADS.read_text(encoding="utf-8")
            text = text.replace('"../.cache/elisp/build/', f'"{elisp_string(BUILD_CACHE_DIR.as_posix())}/')
            text = text.replace('"../packages/', f'"{elisp_string((ROOT / "packages").as_posix())}/')
            PACKAGE_AUTOLOADS.write_text(text, encoding="utf-8")
        log(f"已生成 {PACKAGE_AUTOLOADS.relative_to(ROOT)}")
        log(f"autoload 扫描目录: {len(autoload_dirs)}")
        if result.stderr.strip():
            for line in result.stderr.splitlines():
                if not VERBOSE and re.search(r"\bScraping\b", line):
                    continue
                print(line)
        return True
    print("❌ package-autoloads.el 生成失败")
    if result.stdout.strip():
        print(result.stdout.strip())
    if result.stderr.strip():
        print(result.stderr.strip())
    return False


def collect_build_source_files() -> list[Path]:
    """Return source files mirrored into the build directory."""
    skipped = {PACKAGE_AUTOLOADS.name}
    files = [
        path for path in sorted(CONFIG_LISP_DIR.glob("*.el"))
        if is_elisp_source_file(path) and path.name not in skipped
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
                d for d in dirs
                if not should_skip_directory(current_path / d)
            ]
            for name in names:
                path = current_path / name
                if is_elisp_source_file(path):
                    files.append(path)
    return list(dict.fromkeys(files))


def prepare_build_tree() -> None:
    """Mirror .el files into .cache/elisp/build like straight/elpaca builds.

    macOS/Unix use symlinks by default.  Windows copies files by default because
    creating symlinks often requires extra privileges.  The build directory is
    the only package load-path root; original package directories are kept as
    source repositories, not load paths.
    """
    BUILD_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    sources = collect_build_source_files()
    desired = {build_source_path(source) for source in sources}

    removed = 0
    for built_el in BUILD_CACHE_DIR.rglob("*.el"):
        if built_el.is_dir():
            continue
        if built_el not in desired:
            built_el.unlink(missing_ok=True)
            built_el.with_suffix(".elc").unlink(missing_ok=True)
            removed += 1

    linked = 0
    copied = 0
    updated = 0
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
        if (not target.exists()
                or source.stat().st_mtime_ns > target.stat().st_mtime_ns
                or source.stat().st_size != target.stat().st_size):
            shutil.copy2(source, target)
            copied += 1
        else:
            updated += 1

    log(
        f"已同步 build 目录: symlink {linked}, copy {copied}, unchanged {updated}, removed {removed}",
        verbose_only=True,
    )


def generate_caches_parallel() -> bool:
    """Generate load-path cache and package autoloads concurrently."""
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
            except Exception as err:  # noqa: BLE001 - top-level task boundary
                ok = False
                log(f"{name} 生成失败: {err!r}", "ERROR")
            else:
                if result is False:
                    ok = False
    return ok


def is_elisp_source_file(path: Path) -> bool:
    """Return whether PATH is a normal Elisp source file."""
    name = path.name
    return (
        path.suffix == ".el"
        and not name.startswith(".")
        and not name.endswith(("-autoloads.el", "-pkg.el"))
    )


def is_compilable_elisp_file(path: Path) -> bool:
    """Return whether PATH is an Elisp source file worth byte compiling."""
    if not is_elisp_source_file(path):
        return False
    try:
        first_line = path.read_text(encoding="utf-8", errors="ignore").splitlines()[0]
    except (OSError, IndexError):
        return False
    return "no-byte-compile: t" not in first_line


def collect_config_compile_files() -> list[Path]:
    """Return config source files for byte compilation."""
    if not CONFIG_LISP_DIR.is_dir():
        return []
    skipped = {PACKAGE_AUTOLOADS.name}
    return [
        path for path in sorted(CONFIG_LISP_DIR.glob("*.el"))
        if is_compilable_elisp_file(path) and path.name not in skipped
    ]


def collect_package_compile_files() -> list[Path]:
    """Return package source files selected for byte compilation."""
    result: list[Path] = []
    if not PACKAGE_DIR.is_dir():
        return result

    for package in sorted(PACKAGE_DIR.iterdir()):
        if (not package.is_dir()
                or should_skip_directory(package)
                or package.name not in BYTE_COMPILE_PACKAGES
                or package.name in PACKAGE_COMPILE_EXCLUDES):
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
                path = current_path / fname
                if is_compilable_elisp_file(path) and path.name not in COMPILE_FILE_EXCLUDES:
                    result.append(path)

    return list(dict.fromkeys(result))


def remove_source_elc_files(files: list[Path]) -> None:
    """Remove .elc files next to original sources."""
    removed = 0
    for source in files:
        elc = source.with_suffix(".elc")
        if elc.exists() and not elc.is_relative_to(BUILD_CACHE_DIR):
            elc.unlink()
            removed += 1
    if removed:
        log(f"已删除源目录旁的 .elc: {removed} 个")


def split_chunks(items: list[Path], chunks: int) -> list[list[Path]]:
    """Split ITEMS into at most CHUNKS balanced chunks."""
    if chunks <= 1 or len(items) <= 1:
        return [items]
    return [items[i::chunks] for i in range(chunks) if items[i::chunks]]


def run_emacs_byte_compile_chunk(emacs: str, files: list[Path], index: int) -> tuple[int, str]:
    """Byte compile one chunk of build-tree FILES in a separate Emacs process."""
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", delete=False) as fp:
        file_list = Path(fp.name)
        for path in files:
            fp.write(path.as_posix() + "\n")

    file_list_elisp = elisp_string(file_list.as_posix())
    load_cache = elisp_string(LOAD_PATH_CACHE.as_posix())
    package_autoloads = elisp_string(PACKAGE_AUTOLOADS.as_posix())
    verbose_elisp = "t" if VERBOSE else "nil"
    script = f'''
(progn
(require 'cl-lib)
(require 'bytecomp)
(setq byte-compile-warnings nil)
(when (file-exists-p "{load_cache}")
  (load "{load_cache}" nil t))
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

    output = "\n".join(x for x in [result.stdout.strip(), result.stderr.strip()] if x)
    return result.returncode, output


def print_compile_output(output: str, failed: bool = False) -> None:
    """Print compile output according to verbosity."""
    if not output:
        return
    if VERBOSE or failed:
        print(output)
        return
    for line in output.splitlines():
        if line.startswith(("ERROR", "WARN")):
            print(line)


def stale_byte_compile_files(files: list[Path]) -> list[Path]:
    """Return source files whose build .elc is missing or older."""
    if FORCE:
        return files
    stale: list[Path] = []
    for source in files:
        build_source = build_source_path(source)
        build_elc = build_source.with_suffix(".elc")
        try:
            if (not build_elc.exists()
                    or build_source.stat().st_mtime_ns > build_elc.stat().st_mtime_ns):
                stale.append(source)
        except OSError:
            stale.append(source)
    return stale


def run_emacs_byte_compile(files: list[Path]) -> bool:
    """Byte compile FILES in the build tree, keeping .el and .elc together."""
    emacs = which("emacs")
    if not emacs:
        log("未找到 emacs，跳过 byte compile", "WARN")
        return True
    if not files:
        log("没有需要 byte compile 的文件")
        return True

    prepare_build_tree()
    stale_files = stale_byte_compile_files(files)
    remove_source_elc_files(files)
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
        remove_source_elc_files(files)
        log(f"byte compile 完成: {len(build_files)} / {len(files)} 个文件")
        return True
    log("byte compile 失败", "ERROR")
    return False


def byte_compile_files() -> bool:
    """Byte compile config files and selected packages."""
    files = collect_config_compile_files() + collect_package_compile_files()
    return run_emacs_byte_compile(files)


def verify_cache() -> bool:
    if not LOAD_PATH_CACHE.exists():
        log("load-path-cache.el 未生成", "ERROR")
        return False
    text = LOAD_PATH_CACHE.read_text(encoding="utf-8")
    if "packages/auctex/style" in text:
        log("AUCTeX style 进入了 cache，请检查 .nosearch", "ERROR")
        return False
    log("cache 检查通过")
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
    """Prepare pdf-tools without local compilation on Windows/macOS."""
    pdf_dir = PACKAGE_DIR / "pdf-tools"
    if not pdf_dir.is_dir():
        print("⚠ packages/pdf-tools 不存在，跳过")
        return True

    print("\n📄 处理 pdf-tools ...")

    if is_windows():
        # On Windows, building epdfinfo locally from packages/pdf-tools is fragile.
        # Prefer the MSYS2 prebuilt server package instead.
        msystem = os.environ.get("MSYSTEM", "MINGW64").upper()
        package_by_msystem = {
            "MINGW64": "mingw-w64-x86_64-emacs-pdf-tools-server",
            "UCRT64": "mingw-w64-ucrt-x86_64-emacs-pdf-tools-server",
            "CLANG64": "mingw-w64-clang-x86_64-emacs-pdf-tools-server",
        }
        package = package_by_msystem.get(
            msystem,
            "mingw-w64-x86_64-emacs-pdf-tools-server",
        )

        if which("epdfinfo"):
            print(f"✅ 已在 PATH 中发现 epdfinfo: {which('epdfinfo')}")
            return True

        pacman = which("pacman")
        if pacman:
            print(f"🔧 Windows 上使用 MSYS2 pacman 安装 pdf-tools server: {package}")
            if run_command([pacman, "-S", "--needed", package]):
                print("✅ pdf-tools server 安装完成")
                print("   请确保对应 MSYS2 bin 目录在 PATH 中，然后重启 Emacs。")
                return True
            print("❌ pacman 安装 pdf-tools server 失败")
            return False

        print(f"""
⚠️  Windows 上不再尝试在 packages/pdf-tools 里 make 编译。
请安装 MSYS2，并在对应终端里执行：

  pacman -Syu
  pacman -S --needed {package}

然后把对应 bin 目录加入系统 PATH，例如：
  C:\\msys64\\mingw64\\bin
  C:\\msys64\\ucrt64\\bin

重启 Emacs 后打开 PDF 即可。
""")
        return True

    if is_macos():
        print("✅ macOS 跳过本地 make 编译 pdf-tools")
        print("   如首次打开 PDF 未自动可用，在 Emacs 中执行: M-x pdf-tools-install")
        return True

    # Linux keeps the previous local build path.
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
- pdf-tools 会通过 MSYS2 pacman 安装预编译 server，不在本地 make

macOS 特别注意：
- pdf-tools 不在脚本里 make；如首次打开 PDF 未自动可用，执行 M-x pdf-tools-install

日常更新只需：
  python update_emacs.py

只设置邮件账号：
  python update_emacs.py --mail
============================================================
""")


def main() -> int:
    parser = argparse.ArgumentParser(description="Emacs config updater / first-time setup")
    parser.add_argument("--cache", action="store_true", help="只生成 load-path 缓存")
    parser.add_argument("--build", action="store_true", help="只编译需要 make 的包")
    parser.add_argument("--skip-git", action="store_true", help="跳过 git 操作")
    parser.add_argument("--mail", action="store_true", help="只设置 MAIL_ACCOUNT 邮件地址")
    parser.add_argument("--compile", action="store_true", help="只生成缓存并执行 byte compile")
    parser.add_argument("--byte-compile", action="store_true", help="执行 byte compile")
    parser.add_argument("--no-compile", action="store_true", help="默认/--build 流程中跳过 byte compile")
    parser.add_argument("--force", action="store_true", help="强制重新生成 autoload/cache 并重新 byte compile")
    parser.add_argument("-v", "--verbose", action="store_true", help="显示详细日志，包括 autoload 扫描目录和编译文件")
    args = parser.parse_args()

    global VERBOSE, FORCE
    VERBOSE = args.verbose
    FORCE = args.force

    print("=" * 60)
    print("Emacs configuration updater / first-time setup")
    print("=" * 60)

    if args.mail:
        configure_mail_account_env(force=True)
        return 0

    configure_mail_account_env()

    compile_requested = args.compile or args.byte_compile

    if args.cache:
        ok = generate_caches_parallel() and verify_cache()
        if ok and compile_requested:
            ok = byte_compile_files()
        return 0 if ok else 1

    if args.compile:
        ok = generate_caches_parallel() and verify_cache()
        if ok:
            ok = byte_compile_files()
        return 0 if ok else 1

    if args.build:
        build_make_packages()
        build_pdf_tools()
        ok = generate_caches_parallel() and verify_cache()
        if ok and not args.no_compile:
            ok = byte_compile_files()
        return 0 if ok else 1

    if not args.skip_git:
        if not (ROOT / ".git").exists():
            print(f"❌ 不是 Git 仓库: {ROOT}")
            return 1
        if not update_git():
            return 1

    print("\n🔨 编译需要 make 的包...")
    build_make_packages()

    build_pdf_tools()

    if not generate_caches_parallel():
        return 1
    if not verify_cache():
        return 1
    if not args.no_compile:
        if not byte_compile_files():
            return 1

    print_first_time_tips()
    return 0


if __name__ == "__main__":
    sys.exit(main())
