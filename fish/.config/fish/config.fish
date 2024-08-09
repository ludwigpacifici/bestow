if status is-interactive
    set -g fish_greeting
    # Commands to run in interactive sessions can go here
    set -U fish_user_paths $HOME/.cargo/bin $fish_user_paths
    set -Ux EDITOR emacs -Q -nw
end
