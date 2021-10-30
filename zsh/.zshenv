ZDOTDIR=$HOME
XDG_CACHE_HOME="$HOME/.cache"
XDG_CONFIG_HOME="$HOME/.config"
XDG_DATA_HOME="$HOME/.local/share"
XDG_BIN_HOME="$HOME/.local/bin"
PS1='%B%(?.%F{39}.%F{190})░%f%b '
HISTFILE="$ZDOTDIR/.zhistory"
EDITOR="emacs"

# Load Rust ecosystem
. "$HOME/.cargo/env"
