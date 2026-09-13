"""Explicit MAIL_ACCOUNT configuration for desktop Emacs sessions."""

from __future__ import annotations

import os
import plistlib
import re
import shlex
import subprocess
import sys
from pathlib import Path

from .context import is_macos, is_windows, which


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

    zshenv = Path.home() / ".zshenv"
    if zshenv.exists():
        pattern = re.compile(r"^\s*(?:export\s+)?MAIL_ACCOUNT=(.*)\s*$")
        for line in zshenv.read_text(encoding="utf-8", errors="ignore").splitlines():
            match = pattern.match(line)
            if not match:
                continue
            value = match.group(1).split("#", 1)[0].strip()
            if value:
                account = shlex.split(value)[0]
                if account:
                    os.environ["MAIL_ACCOUNT"] = account
                    return account
    return None


def configure_mail_account_env(force: bool = False) -> None:
    """Configure MAIL_ACCOUNT on Windows/macOS when explicitly requested."""
    if not (is_windows() or is_macos()):
        return

    account = detect_mail_account()
    if account and not force:
        print(f"✅ 已检测到 MAIL_ACCOUNT: {account}")
        return

    if not sys.stdin.isatty():
        print("ℹ 未检测到 MAIL_ACCOUNT，且当前不是交互终端，跳过设置")
        return

    message = "\n📧 未检测到 MAIL_ACCOUNT 环境变量。"
    if account:
        message = "\n📧 重新设置 MAIL_ACCOUNT 环境变量。"
    print(message)
    print("   该变量会被 Emacs 中的 org2calendar-account 读取。")
    account = input("请输入默认 Microsoft 账号邮箱（直接回车跳过）: ").strip()
    if not account:
        print("ℹ 已跳过 MAIL_ACCOUNT 设置")
        return

    os.environ["MAIL_ACCOUNT"] = account
    if is_windows():
        set_windows_mail_account_env(account)
    else:
        set_macos_mail_account_env(account)


def set_windows_mail_account_env(account: str) -> None:
    """Persist MAIL_ACCOUNT in the Windows user environment."""
    try:
        result = subprocess.run(["setx", "MAIL_ACCOUNT", account], check=False)
    except FileNotFoundError:
        result = None
    if result is not None and result.returncode == 0:
        print("✅ 已写入用户环境变量 MAIL_ACCOUNT")
        print("   请重启 Emacs，或重新登录后生效。")
        return

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
    """Persist MAIL_ACCOUNT for macOS GUI sessions and the login shell."""
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
    except OSError as error:
        print(f"⚠ 写入 LaunchAgent 失败: {error}")

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
    except OSError as error:
        print(f"❌ 写入 shell 配置失败: {error}")
        print(f"   可手动加入: {line}")
