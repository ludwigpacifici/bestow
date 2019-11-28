# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export PATH="$HOME/.cargo/bin:$PATH"

if [ -f ~/.bashrc.min ]; then
    source ~/.bashrc.min
fi
