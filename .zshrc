# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

SAVEHIST=100000
export EDITOR="nvim"
export VISUAL="$EDITOR"

export ZSH="$HOME/.oh-my-zsh"

source $ZSH/oh-my-zsh.sh
# pretty sure not needed
# ZSH_DISABLE_COMPFIX=true
# plugins=(git)

alias ,.z='. ~/.zshrc'

alias ,beep='osascript -e "beep 2"'
alias ,b=',beep'
alias ,bb=',beep; ,beep'
alias ,bbb=',beep; ,beep; ,beep'
alias ,bx=',beep; exit"'

alias ,cl='clear'
alias ,clear='clear' # because I keep typing it

alias ,d='docker'
alias ,dcu='docker-compose up'
alias ,dcud='docker-compose up -d'
alias ,dcd='docker-compose down'
alias ,dcr='docker-compose restart'
alias ,dps='docker ps'
alias ,dpsa='docker ps -a'
alias ,dspa='docker system prune -a'
alias ,dspv='docker system prune --volumes'
alias ,dvp='docker volume prune'
alias ,di='docker images'
alias ,dockernuke=',dsa; ,drma; yes | ,dspa'
alias ,dn='dsa; drma; yes | dsp'
drm() {
  docker rm "$@"
}
alias ,drma='docker rm $(docker ps -a -q)'
alias ,dvrma='docker volume rm $(docker volume ls -q)'
,drmi() {
  docker rmi "$@"
}
alias ,drmai='docker rmi $(docker images -a -q)'
function ,dl() {
  docker logs --timestamps "$@"
}
function ,dlf() {
  docker logs -f --timestamps "$@"
}
function ,ds() {
  docker stop "$@"
}
# exec into the first container listed by docker ps
function ,de() {
  docker exec -it $(docker ps -q | head -n 1) sh
}

alias ,dsa='docker stop $(docker ps -a -q)'

alias python='python3';

# For emacs vterm, from here: https://github.com/akermu/emacs-libvterm#shell-side-configuration
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# also for vterm; track current directory so dired opens the working dir of the terminal
# direct paste from vterm README
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'


# Created by `pipx` on 2025-02-24 14:09:13
export PATH="$PATH:/Users/robertkirby/.local/bin"
