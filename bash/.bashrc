# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export PATH="$HOME/.cargo/bin:$PATH"
export PAGER="/usr/bin/most -s" # require 'most'

# opam configuration
test -r /home/lud/.opam/opam-init/init.sh && . /home/lud/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

if [ -f ~/.bashrc.min ]; then
    source ~/.bashrc.min
fi
