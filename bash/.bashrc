# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH="$HOME/.cargo/bin:$PATH"

if [ -f ~/.bashrc.min ]; then
    source ~/.bashrc.min
fi
