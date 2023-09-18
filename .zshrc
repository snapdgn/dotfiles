# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#(cat ~/.cache/wal/sequences &)
#
#
# Vertical Bar Cursor
#echo '\e[5 q'
#
# aliases
alias ls="exa --icons"
#alias ls="colorls"
alias gc="git clone"
alias v="nvim"
#alias cls="clear"
alias cd="z"
alias cat="bat"
alias bp="bpython"
alias df="duf"
alias xxd="hexyl"
alias lg="lazygit"
alias diff="diff-so-fancy"
alias k="kubectl"
alias ipy="ipython"
alias vt="vagrant"
alias cclip='xclip -selection clipboard'
#alias man="tldr"
alias grep="rg"
alias db="distrobox"

export EDITOR='nvim'
export TERM='xterm-256color'

# colored outputs
alias ip='ip -color=auto'
export MANPAGER="less -R --use-color -Dd+r -Du+b"
alias diff='diff --color=auto'
# set paths
export PATH="/home/snapdgn/.local/bin/:$PATH"
export PATH="/home/snapdgn/.cargo/bin/:$PATH"
export PATH="/home/snapdgn/.local/share/gem/ruby/3.0.0/bin:$PATH"
export PATH=$PATH:/home/snapdgn/.ghcup/ghc/9.2.8/bin/

#ENV Variables
#export BITIA_SERVER="https://public.bitia.link/api/v1"
#export BITIA_SERVER="http://localhost:3141"
#export BITIA_SERVER="http://192.168.0.101:3141"
#
# use zoxide
# eval "$(starship init zsh)"
eval "$(zoxide init zsh)"
eval "$(direnv hook zsh)"

## var colors
#source $(dirname $(gem which colorls))/tab_complete.sh

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk
#

# zinit plugins
zinit ice depth=1; zinit light romkatv/powerlevel10k
zinit load zsh-users/zsh-autosuggestions
zinit load "zsh-users/zsh-syntax-highlighting"

#plugins end
#
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
