"""Read-only health checks for the Emacs configuration."""

from __future__ import annotations

import datetime as dt
import platform
import re
import subprocess
import sys
from pathlib import Path

from .context import (
    BUILD_CACHE_DIR,
    CACHE_STAMP,
    CONFIG_LISP_DIR,
    ENVIRONMENT_CACHE,
    IDLE_FEATURE_CACHE,
    LOAD_PATH_CACHE,
    PACKAGE_AUTOLOADS,
    PACKAGE_DIR,
    ROOT,
    find_emacs,
    which,
)

REQUIRED_FILES = (
    ROOT / "early-init.el",
    ROOT / "init.el",
    CONFIG_LISP_DIR / "init-util.el",
    ROOT / "scripts" / "generate-idle-features.el",
)
REQUIRED_EXECUTABLES = ("git",)
OPTIONAL_EXECUTABLES = ("rg", "bsdtar", "make")


class DoctorReport:
    """Collect and display configuration health checks."""

    def __init__(self) -> None:
        self.ok_count = 0
        self.warning_count = 0
        self.error_count = 0

    def ok(self, message: str) -> None:
        self.ok_count += 1
        print(f"✅ {message}")

    def warn(self, message: str) -> None:
        self.warning_count += 1
        print(f"⚠  {message}")

    def error(self, message: str) -> None:
        self.error_count += 1
        print(f"❌ {message}")

    def finish(self, strict: bool = False) -> bool:
        print("\n" + "-" * 60)
        print(
            f"Doctor: {self.ok_count} passed, "
            f"{self.warning_count} warnings, {self.error_count} errors"
        )
        return self.error_count == 0 and (not strict or self.warning_count == 0)


def capture_output(
    command: list[str],
    cwd: Path | None = None,
) -> subprocess.CompletedProcess[str] | None:
    """Run a read-only command and return its captured result."""
    try:
        return subprocess.run(
            command,
            cwd=cwd or ROOT,
            text=True,
            capture_output=True,
            check=False,
        )
    except OSError:
        return None


def emacs_version(emacs: str) -> str | None:
    result = capture_output(
        [emacs, "--batch", "-Q", "--eval", "(princ emacs-version)"]
    )
    if result is None or result.returncode != 0:
        return None
    return result.stdout.strip() or None


def configured_modules() -> list[str]:
    """Read the deferred module list from init.el without evaluating it."""
    init_file = ROOT / "init.el"
    if not init_file.is_file():
        return []
    text = init_file.read_text(encoding="utf-8", errors="ignore")
    match = re.search(
        r"\(setq\s+my/config-modules\s+'?\((.*?)\)\s*\)",
        text,
        flags=re.S,
    )
    if not match:
        return []
    return re.findall(r"\binit-[a-zA-Z0-9-]+\b", match.group(1))


def newest_mtime(paths: list[Path]) -> int:
    mtimes: list[int] = []
    for path in paths:
        try:
            mtimes.append(path.stat().st_mtime_ns)
        except OSError:
            continue
    return max(mtimes, default=0)


def doctor_generated_caches(report: DoctorReport) -> None:
    config_sources = [
        ROOT / "early-init.el",
        ROOT / "init.el",
        ROOT / "update_emacs.py",
        ROOT / "scripts" / "generate-idle-features.el",
        *sorted((ROOT / "core").glob("*.py")),
        *CONFIG_LISP_DIR.glob("*.el"),
        *PACKAGE_DIR.rglob("*.el"),
    ]
    generated = {LOAD_PATH_CACHE, PACKAGE_AUTOLOADS, IDLE_FEATURE_CACHE}
    source_mtime = newest_mtime([
        path for path in config_sources if path not in generated
    ])

    caches_ready = True
    for cache in (LOAD_PATH_CACHE, PACKAGE_AUTOLOADS, IDLE_FEATURE_CACHE):
        relative = cache.relative_to(ROOT)
        if not cache.is_file():
            caches_ready = False
            report.error(f"缺少生成缓存 {relative}；请运行 sync")
        else:
            report.ok(f"{relative} 已生成")

    if caches_ready:
        if not CACHE_STAMP.is_file():
            report.warn("缓存没有同步时间戳；建议运行 sync")
        elif CACHE_STAMP.stat().st_mtime_ns < source_mtime:
            report.warn("Elisp 缓存旧于配置源文件；建议运行 sync")
        else:
            report.ok("Elisp 缓存同步时间戳有效")

    if not BUILD_CACHE_DIR.is_dir():
        report.error("缺少 .cache/packages-build；请运行 sync")
        return
    broken = [
        path
        for path in BUILD_CACHE_DIR.rglob("*.el")
        if path.is_symlink() and not path.exists()
    ]
    if broken:
        report.error(f"build 镜像有 {len(broken)} 个失效符号链接；请运行 clean 后再 sync")
    else:
        report.ok("build 镜像中没有失效符号链接")

    if not ENVIRONMENT_CACHE.is_file():
        report.warn("尚未生成 shell 环境快照；建议运行 env")
    else:
        age = dt.datetime.now().timestamp() - ENVIRONMENT_CACHE.stat().st_mtime
        if age > 30 * 24 * 60 * 60:
            report.warn("shell 环境快照已超过 30 天；建议运行 env")
        else:
            report.ok("shell 环境快照存在且未过期")


def doctor_submodules(report: DoctorReport) -> None:
    if not (ROOT / ".git").exists():
        report.error(f"不是 Git 仓库: {ROOT}")
        return
    result = capture_output(["git", "submodule", "status", "--recursive"])
    if result is None or result.returncode != 0:
        report.error("无法读取 submodule 状态")
        return
    lines = [line for line in result.stdout.splitlines() if line]
    uninitialized = [line for line in lines if line.startswith("-")]
    conflicts = [line for line in lines if line.startswith("U")]
    drifted = [line for line in lines if line.startswith("+")]
    if uninitialized:
        report.error(f"有 {len(uninitialized)} 个 submodule 未初始化")
    if conflicts:
        report.error(f"有 {len(conflicts)} 个 submodule 存在合并冲突")
    if drifted:
        report.warn(f"有 {len(drifted)} 个 submodule 偏离仓库记录提交")
    if not (uninitialized or conflicts or drifted):
        report.ok(f"{len(lines)} 个 submodule 均位于仓库记录提交")

    status = capture_output(
        ["git", "status", "--short", "--ignore-submodules=none", "--", "packages"]
    )
    if status is not None and status.returncode == 0 and status.stdout.strip():
        report.warn(
            f"packages 下有 {len(status.stdout.splitlines())} 个工作区状态变化（不会自动覆盖）"
        )


def doctor_modules(report: DoctorReport) -> None:
    modules = configured_modules()
    if not modules:
        report.error("无法从 init.el 解析 my/config-modules")
        return
    duplicates = sorted({name for name in modules if modules.count(name) > 1})
    if duplicates:
        report.error(f"模块列表包含重复项: {', '.join(duplicates)}")

    missing: list[str] = []
    bad_provides: list[str] = []
    for module in dict.fromkeys(modules):
        source = CONFIG_LISP_DIR / f"{module}.el"
        if not source.is_file():
            missing.append(module)
            continue
        content = source.read_text(encoding="utf-8", errors="ignore")
        if not re.search(rf"\(provide\s+'{re.escape(module)}\)", content):
            bad_provides.append(module)
    if missing:
        report.error(f"缺少模块文件: {', '.join(missing)}")
    if bad_provides:
        report.error(f"模块未 provide 自身 feature: {', '.join(bad_provides)}")
    if not (duplicates or missing or bad_provides):
        report.ok(f"{len(modules)} 个延迟模块的文件与 provide 声明一致")


def doctor_tree_sitter(report: DoctorReport, emacs: str) -> None:
    script = """
(if (not (fboundp 'treesit-language-available-p))
    (princ "unsupported\n")
  (dolist (lang '(python lua))
    (princ (format "%s=%s\n" lang
                   (if (treesit-language-available-p lang) "yes" "no")))))
"""
    result = capture_output([emacs, "--batch", "-Q", "--eval", script])
    if result is None or result.returncode != 0:
        report.warn("无法检查 Tree-sitter grammar")
        return
    if "unsupported" in result.stdout:
        report.warn("当前 Emacs 不支持内建 Tree-sitter")
        return
    unavailable = [
        line.split("=", 1)[0]
        for line in result.stdout.splitlines()
        if line.endswith("=no")
    ]
    if unavailable:
        report.warn(f"缺少 Tree-sitter grammar: {', '.join(unavailable)}")
    else:
        report.ok("已启用语言的 Tree-sitter grammar 均可用")


def run_doctor(strict: bool = False) -> bool:
    """Run read-only health checks for this configuration."""
    report = DoctorReport()
    print("\nConfiguration files")
    for path in REQUIRED_FILES:
        if path.is_file():
            report.ok(str(path.relative_to(ROOT)))
        else:
            report.error(f"缺少 {path.relative_to(ROOT)}")

    print("\nRuntime")
    if sys.version_info >= (3, 10):
        report.ok(f"Python {platform.python_version()}")
    else:
        report.error("需要 Python 3.10 或更高版本")

    emacs = find_emacs()
    if not emacs:
        report.error("找不到 Emacs；可通过 EMACS 指定可执行文件")
    else:
        version = emacs_version(emacs)
        if version:
            report.ok(f"Emacs {version}: {emacs}")
            if re.search(r"\.0\.50(?:\D|$)", version):
                report.warn("当前是开发版 Emacs；升级后建议重新运行 clean、sync 和 test")
        else:
            report.error(f"Emacs 无法以 batch 模式运行: {emacs}")

    for executable in REQUIRED_EXECUTABLES:
        path = which(executable)
        if path:
            report.ok(f"{executable}: {path}")
        else:
            report.error(f"缺少必要命令: {executable}")
    for executable in OPTIONAL_EXECUTABLES:
        path = which(executable)
        if path:
            report.ok(f"{executable}: {path}")
        else:
            report.warn(f"缺少可选命令: {executable}")

    print("\nModules and packages")
    doctor_modules(report)
    doctor_submodules(report)

    print("\nGenerated state")
    doctor_generated_caches(report)
    if emacs:
        doctor_tree_sitter(report, emacs)
    return report.finish(strict=strict)
