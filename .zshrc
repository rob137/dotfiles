# Amazon Q pre block. Keep at the top of this file.
[[ -f "${HOME}/Library/Application Support/amazon-q/shell/zshrc.pre.zsh" ]] && builtin source "${HOME}/Library/Application Support/amazon-q/shell/zshrc.pre.zsh"
# Enablen Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
export PS1="▶ %F{blue}%~%f ❯ "

SAVEHIST=100000
export EDITOR="vim"
export VISUAL="$EDITOR"

export ZSH="$HOME/.oh-my-zsh"

source $ZSH/oh-my-zsh.sh
# pretty sure not needed
# ZSH_DISABLE_COMPFIX=true
# plugins=(git)

alias ,.z='. ~/.zshrc'

alias cd..='cd ..'
alias cd-='cd -'

alias ,g='cd ~/g'

alias ,sv='source venv/bin/activate'

alias ,tp='tree | pbcopy'
alias ,hp='history | pbcopy'
alias ,ht='history | tail'

alias ,pt='pytest'

alias ,nu='nvm use'
alias ,nrd='npm run dev'
alias ,nci='npm clean-install'
alias ,nuci='nvm use && npm clean-install'
alias ,nuni='nvm use && npm install'
alias ,ni='npm install'

alias ,beep='osascript -e "beep 2"'
alias ,b=',beep'
alias ,bb=',beep; ,beep'
alias ,bbb=',beep; ,beep; ,beep'
alias ,bx=',beep; exit"'

# Function to feed a file from ./prompts/ to ,c (claude code)
pc() {
  local prompt_file="./prompts/$1"
  echo "$prompt_file"
  if [ -f "$prompt_file" ]; then
    cat "$prompt_file" | ,c
  else
    echo "Error: File $prompt_file does not exist."
  fi
}
# Custom completion function for pc
_pc_complete() {
  # Use the current working directory's ./prompts/ folder
  local prompt_dir="$PWD/prompts"
  # Reply with files in ./prompts/ matching *.org
  _files -W "$prompt_dir" -g "*.org"
}
# Bind the completion function to pc
compdef _pc_complete pc

alias ,pc='pc'

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
  vterm_printf "51;A$(whoami)@$(hostname):${PWD}"
}

precmd_functions+=(vterm_prompt_end)   # <-- send before each prompt
PROMPT='%F{cyan}%~%f ❯ '


# Created by `pipx` on 2025-02-24 14:09:13
export PATH="$PATH:/Users/robertkirby/.local/bin"

# nvm initialization moved to bottom of file
# Lazy load PyEnv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
pyenv() {
    unset -f pyenv python python3
    eval "$(pyenv init - zsh)"
    pyenv "$@"
}
# Lazy load Python
python() { pyenv >/dev/null 2>&1; unset -f python 2>/dev/null; python "$@"; }
python3() { pyenv >/dev/null 2>&1; unset -f python3 2>/dev/null; python3 "$@"; }


# Copy all file contents recursively, prefixing each with its path.
treecopy() {
    find . -type f \
        ! -path "*/node_modules/*" \
        ! -name "package-lock.json" \
        -print | while read -r file; do
            echo "===== $file ====="
            cat "$file"
        done | pbcopy
    echo "Copied all file contents with paths to clipboard."
}
alias ,treecopy=treecopy
# Search for a pattern, exclude certain files, and copy results to clipboard.
rgcopy() {
    if [ -z "$1" ]; then
        echo "Usage: rgcopy '<pattern>'"
        return 1
    fi
    rg -l "$1" \
        --glob '!package.json' \
        --glob '!package-lock.json' \
        --glob '!*.md' | \
    xargs -I {} sh -c 'echo "===== {} ====="; cat "{}"' | pbcopy
    echo "Copied matching file contents to clipboard."
}
alias ,rgcopy='rgcopy'

kill-port() { lsof -ti tcp:$1 | xargs -r kill -9; }
alias ,kp='kill-port'


# not committed to source control
source ~/.zshrc-local
source ~/.aider-env

# Added by Windsurf
export PATH="/Users/robertkirby/.codeium/windsurf/bin:$PATH"

alias ,cfa='codex --full-auto'

alias ,claude='claude --dangerously-skip-permissions'
# alias codex='command codex --yolo --search --model=gpt-5-codex -c model_reasoning_effort="high"'
alias codex='command codex --yolo --enable web_search_request'
alias ,c='codex'
alias ,cc=',c'
alias ,cdsp='claude --dangerously-skip-permissions'

alias ,q='q' # butter fingers

codex_full_auto_with_notes() {
  if [[ -z "$1" ]]; then
    echo "Usage: cfaplan <path_to_plan_file>"
    echo "Example: cfaplan /Users/robertkirby/g/Legal-Document-Analysis/agent-notes/multi-upload-pipeline-plan.org"
    return 1
  fi

  local plan_file="$1"

  if [[ ! -f "$plan_file" ]]; then
    echo "Error: Plan file not found at '$plan_file'"
    return 1
  fi

  # The actual command
  ,cfa "see $plan_file - implement, checking off completed items as you go"
}
alias ,cfan='codex_full_auto_with_notes'



# ─────────────────────────────────────────────────────────────────
#   - Usage: ,afp <path/to/your/plan.org>
#   - Builds a prompt that tells amp to finish the next items in that file.
function amp_feed_plan {
  if [[ -z "$1" ]]; then
    echo "Usage: ,afp <path/to/plan-file.org>"
    return 1
  fi

  local filepath="$1"
  local prompt="Please complete the next set of items in the plan at '$filepath'. Check them off in the plan file as you complete them: '- [X]'"

  echo "$prompt" | amp;
}
alias ,afp='amp_feed_plan'
# ─────────────────────────────────────────────────────────────────

alias ,amp='amp'
alias ,atc='amp threads continue'


# Amp compact and continue thread (latest by default, or specified thread ID)
amp_compact_continue() {
    local thread_id="$1"

    if [[ -z "$thread_id" ]]; then
        # No argument provided, use latest thread
        thread_id=$(amp threads list | head -3 | tail -1 | awk '{print $NF}')
    fi

    if [[ -n "$thread_id" ]]; then
        echo "Compacting thread: $thread_id"
        amp threads compact "$thread_id"
        echo "Continuing thread: $thread_id"
        amp threads continue "$thread_id"
    else
        echo "No threads found"
    fi
}
alias ,acc=amp_compact_continue

# Lazy load SDKMAN
export SDKMAN_DIR="$HOME/.sdkman"
sdk() {
    unset -f sdk
    [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
    sdk "$@"
}

# YouTube Transcript Tool
alias gt='/Users/robertkirby/sandpit/youtube-transcript-tool/get_transcript.py'
alias get-youtube='/Users/robertkirby/sandpit/youtube-transcript-tool/get-youtube'

alias ,qctat='q chat --trust-all-tools'

# Toggle Azure MCP prompt helpers
alias ,toggleazuremcp='codex exec "$(cat ~/prompts/toggle-azure-mcp.md)"'
alias ,tam=',toggleazuremcp'

# Always initialize nvm (instead of lazy loading)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"


# opencode
export PATH=/Users/robertkirby/.opencode/bin:$PATH
export COLORTERM=truecolor

# Amazon Q post block. Keep at the bottom of this file.
[[ -f "${HOME}/Library/Application Support/amazon-q/shell/zshrc.post.zsh" ]] && builtin source "${HOME}/Library/Application Support/amazon-q/shell/zshrc.post.zsh"
