# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH="$HOME/.cargo/bin:$PATH"
export PAGER="/usr/bin/most -s" # require 'most'

test -r /home/lud/.opam/opam-init/init.sh && . /home/lud/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

rustup completions bash > ~/.local/share/bash-completion/completions/rustup

source /home/lud/.config/broot/launcher/bash/br

if [ -f ~/.bashrc.min ]; then
    source ~/.bashrc.min
fi
