# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/spenserw/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -Uz colors && colors

autoload -U promptinit && promptinit
prompt walters
autoload -Uz compinit
compinit

export TERMINAL=xterm

# Path
export PATH="$PATH:$HOME/documents/scripts"

# Aliases
alias ls='ls --color=auto -lah'
alias vi='vim'
alias c='clear'
alias tmux="TERM=screen-256color tmux"
alias pretty='python -m json.tool'
alias asm='gcc -S -fno-asynchronous-unwind-tables -fno-dwarf2-cfi-asm'
alias exa='exa -la'

bindkey '\e[1;5C' forward-word
bindkey '\e[1;5D' backward-word

# Python
export PATH="$PATH:$HOME/.local/bin"

source $HOME/.rvm/scripts/rvm

# Temporary, let's remove this once annoying errors are fixed in Rails 6.x
export RUBYOPT='-W:no-deprecated'
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
