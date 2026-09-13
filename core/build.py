"""Platform-specific native package build steps."""

from __future__ import annotations

import os
import shlex
from pathlib import Path

from .context import PACKAGE_DIR, find_emacs, is_macos, is_windows, run_command, which

MAKE_TARGETS = ("auctex", "benchmark-init-el")


def build_make_packages() -> bool:
    """Run make for packages that own a supported Makefile build."""
    make = which("make")
    if not make:
        print("⚠ 未找到 make，跳过 auctex / benchmark-init 编译")
        print("   Windows 请在 MSYS2 UCRT64/MINGW64 里安装: pacman -S make")
        return False

    emacs = find_emacs()
    if not emacs:
        print("⚠ 未找到 emacs，跳过 make 编译；请设置 EMACS 为 emacs.exe 的完整路径")
        return False
    emacs_command = shlex.quote(Path(emacs).as_posix())

    ok = True
    for name in MAKE_TARGETS:
        package = PACKAGE_DIR / name
        if not package.is_dir():
            print(f"⚠ 跳过 {name}（目录不存在）")
            continue
        print(f"\n🔨 make {name} ...")
        variable = "EMACSBIN" if name == "auctex" else "EMACS"
        if run_command([make, f"{variable}={emacs_command}"], cwd=package):
            print(f"✅ {name} 编译完成")
        else:
            print(f"❌ {name} 编译失败")
            ok = False
    return ok


def build_pdf_tools() -> bool:
    """Prepare the platform-appropriate pdf-tools server."""
    pdf_dir = PACKAGE_DIR / "pdf-tools"
    if not pdf_dir.is_dir():
        print("⚠ packages/pdf-tools 不存在，跳过")
        return True

    print("\n📄 处理 pdf-tools ...")
    if is_windows():
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


def run_build() -> bool:
    """Build every package with a platform-specific native build step."""
    make_ok = build_make_packages()
    pdf_ok = build_pdf_tools()
    return make_ok and pdf_ok
