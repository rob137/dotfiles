# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source ~/.zshrc.rob-universal

alias am='cd ~/g/affiliate-mono'
alias asm='cd ~/g/aff-services-mono'
alias auc='cd ~/g/api-ui-core'
alias es='cd ~/g/events-service'
alias esu='cd ~/g/events-service-ui'
alias mc='cd ~/g/microservices-cms'
alias pf='cd ~/g/pickswise-frontend'
alias pa='cd ~/g/pickswise-app'
alias rc='cd ~/g/react-components'

alias python='python3'
alias pip='pip3'

alias ui='v ~/notes/ui.txt'
alias backend='v ~/notes/backend.txt'
alias be='backend'
alias other='v ~/notes/other.txt'
alias branch='v ~/notes/branches.txt'
alias br='branch'
alias branches='branch'
alias bra='branch'
alias opennotes='cpb && vim ~/notes/$(pbpaste | sed -E "s/feature\/|bugfix\///").txt'
alias on='opennotes'

function personal() {
  eval "$1 --author=\"Rob <robertaxelkirby@gmail.com>\""
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
