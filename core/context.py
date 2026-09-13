"""Shared paths, runtime options, and subprocess helpers."""

from __future__ import annotations

import os
import platform
import shlex
import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PACKAGE_DIR = ROOT / "packages"
CONFIG_LISP_DIR = ROOT / "lisp"
LOAD_PATH_CACHE = CONFIG_LISP_DIR / "load-path-cache.el"
PACKAGE_AUTOLOADS = CONFIG_LISP_DIR / "package-autoloads.el"
ELISP_CACHE_DIR = ROOT / ".cache"
BUILD_CACHE_DIR = ELISP_CACHE_DIR / "packages-build"
IDLE_FEATURE_CACHE = ELISP_CACHE_DIR / "idle-features.el"
ENVIRONMENT_CACHE = ELISP_CACHE_DIR / "environment.el"
CACHE_STAMP = ELISP_CACHE_DIR / "elisp-cache.stamp"

VERBOSE = False
FORCE = False


def configure_runtime(*, verbose: bool, force: bool) -> None:
    """Set process-wide CLI options used by maintenance operations."""
    global VERBOSE, FORCE
    VERBOSE = verbose
    FORCE = force


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
    """Run COMMAND and return whether it exited successfully."""
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


def which(command: str) -> str | None:
    return shutil.which(command)


def is_windows() -> bool:
    return platform.system() == "Windows"


def is_macos() -> bool:
    return platform.system() == "Darwin"


def find_emacs() -> str | None:
    """Find Emacs, including native Windows installs outside MSYS PATH."""
    configured = os.environ.get("EMACS")
    if configured:
        executable = which(configured)
        if executable:
            return executable
        print(f"⚠ EMACS 指定的程序不可用: {configured}")
        return None
    executable = which("emacs")
    if executable:
        return executable
    if is_windows():
        candidate = Path("C:/opt/emacs/bin/emacs.exe")
        if candidate.is_file():
            return str(candidate)
    return None


def elisp_string(value: str) -> str:
    """Escape VALUE for an Emacs Lisp string literal."""
    return (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\r", "\\r")
        .replace("\n", "\\n")
        .replace("\t", "\\t")
    )


def write_text_if_changed(path: Path, content: str) -> bool:
    """Write CONTENT only when it differs; return whether the file changed."""
    if path.exists() and path.read_text(encoding="utf-8") == content:
        return False
    path.parent.mkdir(parents=True, exist_ok=True)
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
