# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/josiah/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="avit"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

export EDITOR=vim

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git git-flow git-extras fasd mvn nvm scala postgres python pylint pep8 autopep8 history-substring-search docker docker-compose emacs colored-man-pages colorize command-not-found vi-mode globalias common-aliases compleat vi-mode zsh-navigation-tools zsh-autosuggestions zsh-syntax-highlighting nix-zsh-completions)

source $ZSH/oh-my-zsh.sh

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#

export PATH=/home/josiah/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/bin/git-tf-2.0.3:/usr/local/bin/git-tf-2.0.3:/home/josiah/.local/bin:/usr/local/android-sdk/bin:/usr/local/android-sdk/tools:/usr/local/android-sdk/tools/bin:/home/josiah/.nix-profile/bin:/home/josiah/.nix-profile/sbin:/home/josiah/.cabal/bin:/home/josiah/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/bin/git-tf-2.0.3:/usr/local/bin/git-tf-2.0.3:/home/josiah/.local/bin:/usr/local/android-sdk/bin:/usr/local/android-sdk/tools:/usr/local/android-sdk/tools/bin:/home/josiah/.local/bin:/home/josiah/.nix-profile/bin:/home/josiah/.nix-profile/sbin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin:/home/josiah/programs/intellij/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/android-sdk/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/local/android-sdk/bin:/usr/lib/jvm/java-8-openjdk-amd64/bin:/usr/share/maven/bin:/home/josiah/.conscript/bin
#
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
alias screeps-dir=/home/josiah/.config/Screeps/scripts/screeps.com/tutorial-1
export PATH="$HOME/.cargo/bin:$PATH"

set -o vi

alias j='z'
alias jj='zz'
alias zshrc='v ~/.zshrc'
export d=/media/josiah/extra-drive-1
alias lpgp="lpass show --password -c"

export HISTCONTROL=ignoreboth

# added by Miniconda3 installer
export PATH="/home/josiah/miniconda3/bin:$PATH"
. /home/josiah/miniconda3/etc/profile.d/conda.sh
alias ipyvi="ipython --TerminalInteractiveShell.editing_mode=vi"

export PATH="/home/josiah/.nix-profile/bin:/home/josiah/.nix-profile/sbin:/home/josiah/.cabal/bin:$PATH"
eval $(thefuck --alias)
