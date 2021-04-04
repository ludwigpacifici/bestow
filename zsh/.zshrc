[ -d "$ZGEN_DIR" ] || git clone https://github.com/tarjoilija/zgen "$ZGEN_DIR"
source $ZGEN_SOURCE
# if the init script doesn't exist
if ! zgen saved; then
    zgen load junegunn/fzf shell
    zgen save
fi

source $ZDOTDIR/.zkeybinds.zsh
source $ZDOTDIR/.zcompletions.zsh

fpath+=$ZDOTDIR/.zcompletions/

# History
HISTSIZE=2048
SAVEHIST=2048

setopt INC_APPEND_HISTORY # Write to the history file immediately, not when the shell exits.
setopt EXTENDED_HISTORY # Write the history file in the ':start:elapsed;command' format.
setopt HIST_FIND_NO_DUPS # Do not display a previously found event.
setopt HIST_IGNORE_SPACE # Do not record an event starting with a space.
setopt HIST_IGNORE_DUPS # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS # Delete an old recorded event if a new event is a duplicate.
setopt HIST_VERIFY # Show command with history expansion to user before running it
setopt HIST_SAVE_NO_DUPS # Do not write a duplicate event to the history file.
setopt HIST_EXPIRE_DUPS_FIRST # Expire a duplicate event first when trimming history.
setopt APPEND_HISTORY # Appends history to history file on exit
setopt SHARE_HISTORY # Share history between all sessions.

# Command completion
autoload -Uz compinit
compinit

# Treat these characters as part of a word.
WORDCHARS=''

setopt INTERACTIVECOMMENTS # Activate the bash-style comments
unsetopt BEEP # Quiet

# opam configuration
test -r /home/lud/.opam/opam-init/init.zsh && . /home/lud/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

alias n='$EDITOR'
