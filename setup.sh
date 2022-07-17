brew_install() {
    echo "\nInstalling $1"
    if brew list $1 &>/dev/null; then
        echo "${1} is already installed"
    else
        brew install $1 && echo "$1 is installed"
    fi
}

brew_install "homebrew/cask-fonts/font-iosevka"

brew_install "pngpaste"

npm install -g git+https://gitlab.com/matsievskiysv/math-preview

brew_install "mu"
brew_install "isync"
brew_install "msmtp"
