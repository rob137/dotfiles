# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export EDITOR="nvim"
export VISUAL="$EDITOR"
source ~/.zshrc.rob-universal

alias vi='vim'
alias am='cd ~/g/affiliate-mono'
alias asm='cd ~/g/aff-services-mono'
alias asmabs='cd ~/g/aff-services-mono/apps/bookmakers-service'
alias asmbs='cd ~/g/aff-services-mono/apps/bookmakers-service'
alias auc='cd ~/g/api-ui-core'
alias es='cd ~/g/events-service'
alias esu='cd ~/g/events-service-ui'
alias esui='esu'
alias mc='cd ~/g/microservices-cms'
alias pf='cd ~/g/pickswise-frontend'
alias pa='cd ~/g/pickswise-app'
alias rc='cd ~/g/react-components'
alias bas='cd ~/g/aff-services-mono/apps/bookmaker-aliases-service'
alias of='cd ~/g/aff-services-mono/apps/offers-frontend'
alias basb='cd /Users/robert.kirby/g/aff-services-mono-COPY-💥💥💥💥💥/apps/bookmaker-aliases-frontend'
alias baf='cd ~/g/aff-services-mono/apps/bookmaker-aliases-frontend'
alias is='cd ~/g/aff-services-mono/apps/image-service'
alias bs='cd ~/g/aff-services-mono/apps/bookmakers-service'
alias pb='cd ~/g/aff-services-mono/apps/pickswise-beffe'
alias pbs='cd ~/g/aff-services-mono/apps/pickswise-beffe/src'
alias pbscp='cd ~/g/aff-services-mono/apps/pickswise-beffe/src/cmd/pickswise-beffe/'

alias nrsd='npm run start:dev'
alias nrd='npm run dev'
alias nrt='npm run test'
alias nrtw='npm run test:watch'
alias nrte='npm run test:e2e'
alias nrtew='npm run test:e2e:watch'
alias nrtc='npm run test:cov'
alias nrtec='npm run test:e2e:cov'
alias nuke-and-start='npm run typeorm schema:drop; mcd; rm -rf dist/; docker system prune --volumes --force; mcu && nrsd'
alias nas='nuke-and-start'

alias mcu='make compose-up'
alias mcd='make compose-down'
alias ml='make logs'
alias mcul='mcu && ml'

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

alias refresh-api-ui-core='cd ~/g/aff-services-mono/apps/offers-frontend &&
                          rm ssgat-api-ui-core-*.tgz;
                          cd ~/g/api-ui-core &&
                          rm ssgat-api-ui-core-*.tgz;
                          npm run pack &&
                          mv ~/g/api-ui-core/ssgat-api-ui-core-*.tgz ~/g/aff-services-mono/apps/offers-frontend &&
                          cd - &&
                          npm install ssgat-api-ui-core-*.tgz'
alias rauc='refresh-api-ui-core'


function personal() {
  eval "$1 --author=\"Rob <robertaxelkirby@gmail.com>\""
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Add go stuff to path
export PATH="$PATH:$(go env GOPATH)/bin"
# Add typescript-language-server to path
export PATH="$PATH:/Users/robert.kirby/.nvm/versions/node/v17.9.0/bin"

