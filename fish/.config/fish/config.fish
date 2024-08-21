# Fish settings
set fish_greeting

# Set OS (Linux, Darwin)
set os (uname)

# Environment variables
export EDITOR=/usr/bin/vim
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
export INFRA_HOME=$HOME/src/infra
set -gx PATH $HOME/src/infra/bin $PATH
set -gx PATH $JAVA_HOME/bin $PATH
set -gx PATH $HOME/.cargo/bin $PATH
set -gx PATH $HOME/.config/intellij/bin $PATH
set -gx PATH $HOME/miniconda3/bin $PATH
set -gx PATH /usr/local/go/bin $PATH
set -gx PATH /usr/local/sbin $PATH
set -gx PATH /opt/homebrew/bin $PATH
set -gx PATH $HOME/.ghcup/bin/ $PATH
set -gx PATH $HOME/src/infra/media-sorter/bin $PATH
set -gx PATH /Applications/SuperCollider.app/Contents/MacOS/ $PATH
set -gx PATH /Applications/Gnucash.app/Contents/MacOS/ $PATH
set -gx PATH /Applications/Gnucash.app/Contents/Resources/bin/ $PATH
set -gx PATH /Users/bmiller/.emacs.d/bin $PATH
set -gx PATH /Users/bmiller/.local/bin $PATH

# Direnv
direnv hook fish | source

# Aliases
alias e='emacsclient -c'
alias g=git
alias rg='rg --hidden'
alias rm='rm -i'
alias rmm=trash
alias sv='git save'
alias vi=/opt/homebrew/bin/nvim
alias pn=pnpm
alias px=pnpx

switch (uname)
case Linux
  setxkbmap -layout us -option ctrl:nocaps
  export JAVA_HOME=/usr/lib/jvm/jdk-16.0.1/
case Darwin
  alias ls='eza'
  alias ll='eza -la'
  alias tree='eza -Ta -I .git'
end

function git_clean_branches
  g br -d (g br | string trim)
end

eval (/opt/homebrew/bin/brew shellenv)

rbenv init - fish | source

function tmux-a
    if test (count $argv) -eq 0
        echo "Usage: tmux-a <session-name>"
        return 1
    end

    set session_name $argv[1]
    tmux attach-session -t $session_name 2>/dev/null; or tmux new-session -s $session_name
end

# pnpm
set -gx PNPM_HOME "/Users/bmiller/Library/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
