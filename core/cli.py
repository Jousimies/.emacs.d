"""Command-line parsing and high-level maintenance workflows."""

from __future__ import annotations

import argparse
import sys

from .build import run_build
from .cache import generate_caches_parallel, verify_cache
from .cleanup import clean_generated_artifacts
from .compiler import byte_compile_files
from .context import ROOT, configure_runtime, log
from .doctor import run_doctor
from .environment import generate_environment_cache
from .git import fetch_git_metadata
from .mail import configure_mail_account_env
from .testing import run_tests


def add_output_options(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("-v", "--verbose", action="store_true", help="显示详细日志")


def create_parser() -> argparse.ArgumentParser:
    """Build the CLI with options scoped to the command that uses them."""
    parser = argparse.ArgumentParser(
        description="维护本地 Emacs 配置（不带命令时执行 sync）",
    )
    subparsers = parser.add_subparsers(dest="command", metavar="COMMAND")

    sync = subparsers.add_parser(
        "sync",
        help="生成缓存并 byte compile（默认）",
        description="同步 build 镜像、autoload 和缓存，然后 byte compile。",
    )
    add_output_options(sync)
    sync.add_argument(
        "--no-compile",
        action="store_true",
        help="只生成缓存，不执行 byte compile",
    )
    sync.add_argument(
        "--force",
        action="store_true",
        help="忽略时间戳，重新生成并编译",
    )

    fetch = subparsers.add_parser(
        "fetch",
        help="fetch 主仓库和 submodule，不切换版本",
    )
    add_output_options(fetch)

    build = subparsers.add_parser(
        "build",
        help="构建 AUCTeX、benchmark-init 和 pdf-tools",
    )
    add_output_options(build)

    setup = subparsers.add_parser(
        "setup",
        help="首次安装：mail + fetch + build + sync",
    )
    add_output_options(setup)
    setup.add_argument("--no-mail", action="store_true", help="不询问 MAIL_ACCOUNT")
    setup.add_argument(
        "--no-compile",
        action="store_true",
        help="生成缓存但不执行 byte compile",
    )
    setup.add_argument(
        "--force",
        action="store_true",
        help="忽略时间戳，重新生成并编译",
    )

    doctor = subparsers.add_parser("doctor", help="只读配置体检")
    add_output_options(doctor)
    doctor.add_argument(
        "--strict",
        action="store_true",
        help="存在警告时也返回失败",
    )

    environment = subparsers.add_parser(
        "env",
        help="刷新白名单 shell 环境快照",
    )
    add_output_options(environment)

    test = subparsers.add_parser("test", help="运行语法和启动测试")
    add_output_options(test)
    test.add_argument(
        "--skip-startup",
        action="store_true",
        help="只检查语法，不运行启动测试",
    )

    clean = subparsers.add_parser("clean", help="删除本脚本的生成物")
    add_output_options(clean)
    clean.add_argument(
        "--dry-run",
        action="store_true",
        help="只显示将删除的内容",
    )

    mail = subparsers.add_parser("mail", help="设置 MAIL_ACCOUNT")
    add_output_options(mail)
    return parser


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse ARGV, treating no command (or sync options alone) as sync."""
    arguments = list(sys.argv[1:] if argv is None else argv)
    if not arguments:
        arguments = ["sync"]
    elif arguments[0] not in {"-h", "--help"} and arguments[0].startswith("-"):
        arguments.insert(0, "sync")
    return create_parser().parse_args(arguments)


def run_sync(*, compile_files: bool) -> bool:
    """Refresh all generated Elisp state and optionally byte compile it."""
    if not generate_caches_parallel() or not verify_cache():
        return False
    return not compile_files or byte_compile_files()


def require_git_repository() -> bool:
    if (ROOT / ".git").exists():
        return True
    log(f"不是 Git 仓库: {ROOT}", "ERROR")
    return False


def run_mail_setup() -> bool:
    configure_mail_account_env(force=True)
    return True


def print_first_time_tips() -> None:
    print("""
============================================================
首次配置完成！接下来：

1. 启动 Emacs（建议先用 --debug-init 看一次有无错误）
2. 打开任意 PDF 文件，确认 pdf-tools 正常
3. 打开 .tex 文件，确认 AUCTeX 已加载

日常同步缓存与编译产物：
  python update_emacs.py

获取远程仓库信息：
  python update_emacs.py fetch
============================================================
""")


def run_setup(args: argparse.Namespace) -> bool:
    """Run the explicit first-time setup workflow."""
    if not args.no_mail:
        configure_mail_account_env()
    if not require_git_repository() or not fetch_git_metadata():
        return False
    if not run_build():
        return False
    if not run_sync(compile_files=not args.no_compile):
        return False
    print_first_time_tips()
    return True


def dispatch(args: argparse.Namespace) -> bool:
    """Run the already-parsed command."""
    handlers = {
        "sync": lambda: run_sync(compile_files=not args.no_compile),
        "fetch": lambda: require_git_repository() and fetch_git_metadata(),
        "build": run_build,
        "setup": lambda: run_setup(args),
        "doctor": lambda: run_doctor(strict=args.strict),
        "env": generate_environment_cache,
        "test": lambda: run_tests(skip_startup=args.skip_startup),
        "clean": lambda: clean_generated_artifacts(dry_run=args.dry_run),
        "mail": run_mail_setup,
    }
    return handlers[args.command]()


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    configure_runtime(
        verbose=getattr(args, "verbose", False),
        force=getattr(args, "force", False),
    )
    return 0 if dispatch(args) else 1
