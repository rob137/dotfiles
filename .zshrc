# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export EDITOR="nvim"
export VISUAL="$EDITOR"
source ~/.zshrc.rob-universal

alias ,vi='vim'
alias ,am='cd ~/g/affiliate-mono'
alias ,amswpf='cd ~/g/affiliate-mono/src/web/props-frontend/'
alias ,amswp='amswpf'
alias ,jfw='cd ~/g/janus-frontend-web'
alias ,pf='cd ~/g/props-frontend/'
alias ,ps='cd ~/g/platform-services'
alias ,psabs='cd ~/g/platform-services/apps/bookmakers-service'
alias ,psbs='cd ~/g/platform-services/apps/bookmakers-service'
alias ,sm='cd ~/g/sandbox-monorepo'
alias ,smbc='cd ~/g/sandbox-monorepo/apps/bull-concurrency/'
alias ,auc='cd ~/g/api-ui-core'
alias ,es='cd ~/g/platform-services/apps/events-service'
alias ,dlfesc='docker logs -f events_service_container'
alias ,dlfes='dlfesc'
alias ,esu='cd ~/g/platform-services/apps/events-service-ui'
alias ,esui='esu'
alias ,mc='cd ~/g/microservices-cms'
alias ,pa='cd ~/g/pickswise-app'
alias ,rc='cd ~/g/react-components'
alias ,bas='cd ~/g/platform-services/apps/bookmaker-aliases-service'
alias ,psln='cd ~/g/platform-services/libs/nest'
alias ,ne='psln' # note that 'nest' is used by the cli
alias ,sk='cd ~/g/platform-services/libs/nest/src/surrogate-keys/'
alias ,ff='cd ~/g/platform-services/libs/nest/src/feature-flags/'
alias ,pslntu='cd ~/g/platform-services/libs/nest-test-utils/'
alias ,tns='cd ~/g/platform-services/apps/test-nest-service/'
alias ,jv='cd ~/g/sandbox-monorepo/apps/jsonld-validator/'
alias ,snu='cd ~/g/platform-services//libs/ssg-nest-utils/'
alias ,of='cd ~/g/platform-services/apps/offers-frontend'
alias ,dm='cd ~/g/debug-mono/'
alias ,rw='cd ~/g/recent-winners/'
alias ,dh='cd ~/sandpit/dataherald/'
alias ,aice='cd ~/g/aice/'
alias ,ace=',aice'
alias ,le='cd ~/sandpit/laravel-example/'

alias ,baf='cd ~/g/platform-services/apps/bookmaker-aliases-frontend'
alias ,is='cd ~/g/platform-services/apps/image-service'
alias ,bs='cd ~/g/platform-services/apps/bookmakers-service'
alias ,pb='cd ~/g/platform-services/apps/pickswise-beffe'
alias ,pbs='cd ~/g/platform-services/apps/pickswise-beffe/src'
alias ,pbscp='cd ~/g/platform-services/apps/pickswise-beffe/src/cmd/pickswise-beffe/'

alias ,cl='composer lint'
alias ,ct='composer test'

alias ,dn="dcud && nrsd"
alias ,nrb='npm run build'
alias ,nb='nrb'
alias ,nrw='npm run web'
alias ,nrsd='npm run start:dev'
alias ,nrd='npm run dev'
alias ,nrl='npm run lint'
alias ,nrpc='npm run prettier:check'
alias ,nrpf='npm run prettier:format'
alias ,nrt='npm run test'
alias ,nrtw='npm run test:watch'
alias ,nrte='npm run test:e2e'
alias ,nrtew='npm run test:e2e:watch'
alias ,nrtc='npm run test:cov'
alias ,nrtec='npm run test:e2e:cov'
alias ,nuke-and-start='npm run typeorm schema:drop; mcd; rm -rf dist/; docker system prune --volumes --force; mcu && nrsd'
alias ,nas='nuke-and-start'
alias ,rmrfd='rm -rf ./dist/'

alias ,mr='make run'
alias ,mb='make build'
alias ,mcu='make compose-up'
alias ,mcul='make compose-up-local'
alias ,mcuu='make compose-up-uat'
alias ,mcd='make compose-down'
alias ,ml='make logs'
alias ,ml='make logs'
alias ,mcul='mcu && ml'
alias ,ms='make setup'

alias ,pip='pip3'

alias ,sail='[ -f sail ] && sh sail || sh vendor/bin/sail'
alias sail=',sail'
alias ,su='sail up'
alias ,sud='sail up -d'
alias ,sd='sail down'
alias ,sps='sail ps'
alias ,spsa='sail ps -a'
alias ,st='sail test'
alias ,cst='clear; sail test'
alias ,sal='sail artisan larastan'
alias ,sac='sail artisan check'
alias ,snrb='sail npm run build'
alias ,snrd='sail npm run dev'
alias ,snid='sail npm install'
alias ,snci='sail npm clean-install'
alias ,sad='sail artisan db'
alias ,sads='sail artisan db splat'
alias ,sl='sail logs'
alias ,sa='sail artisan'


#### Database Schema Printing / Copying ####
# Generic method
process_tables() {
  local database="$1"
  shift
  for table in "$@"; do
    sail artisan db:table "$table" --database="$database"
    if [ $? -ne 0 ]; then
      echo "Error: Failed to process table '$table'. Stopping."
      return 1
    fi
  done
}
# Specific methods
sadt() {
  process_tables "" "$@"
}
sadts() {
  process_tables "splat" "$@"
}
sadtss() {
  for table in "$@"; do
    process_tables "splat" "splat.$table"
  done
}
sadtsso() {
  for table in "$@"; do
    process_tables "splat" "splat_output.$table"
  done
}
sadtp() {
  sadt "$@" | pbcopy
}
sadtps() {
  sadts "$@" | pbcopy
}
sadtpss() {
  sadtss "$@" | pbcopy
}
sadtpsso() {
  sadtsso "$@" | pbcopy
}
alias ,sadt='sadt'
alias ,sadtp='sadtp'
alias ,sadts='sadts'
alias ,sadtps='sadtps'
alias ,sadtss='sadtss'
alias ,sadtpss='sadtpss'
alias ,sadtsso='sadtsso'
alias ,sadtpsso='sadtpsso'


alias ,other='v ~/notes/other.txt'
alias ,branch='v ~/notes/branches.txt'
alias ,br='branch'
alias ,branches='branch'
alias ,bra='branch'
alias ,opennotes='cpb && vim ~/notes/$(pbpaste | sed -E "s/feature\/|bugfix\///").txt'
alias ,on='opennotes'
alias ,tgpl='cd ~/sandpit/go-playground/the-go-programming-language'
alias ,corsair='cd ~/sandpit/corsair.el/'

# Creates a file on the desktop containing all code from aice (minus dep lock files)
# Originally intended to help with RAG in GPTs
alias save-aice=',aice && while read file; do echo "$file"; echo; cat "$file"; echo -e "\n\n"; done < <(git ls-tree -r HEAD --name-only | grep -v "composer.lock" | grep -v "package-lock.json") > ~/Desktop/aice-code-$(date "+%Y-%m-%d_%H-%M-%S").txt'
alias serve-aice=',aice && ,sud && ,snrd'
alias start-aice='serve-aice'
alias run-aice='start-aice'

alias ,refresh-api-ui-core='cd ~/g/platform-services/apps/offers-frontend &&
                          rm ssgat-api-ui-core-*.tgz;
                          cd ~/g/api-ui-core &&
                          rm ssgat-api-ui-core-*.tgz;
                          npm run pack &&
                          mv ~/g/api-ui-core/ssgat-api-ui-core-*.tgz ~/g/platform-services/apps/offers-frontend &&
                          cd - &&
                          npm install ssgat-api-ui-core-*.tgz'
alias ,rauc='refresh-api-ui-core'


# takes an arg and applies it to sail test command
# e.g. ",stf testName" will run `sail test --filter="testName"`
# If no args are provided, it will run `sail test`
function sail_test_filter() {
  if [ -z "$1" ]; then
    sail test
  else
    sail test --filter="$1"
  fi
}
alias ,stf='sail_test_filter'

function clear_sail_test_filter() {
  clear;
  sail_test_filter "$1"
}
alias ,cstf='clear_sail_test_filter'

function personal() {
  eval "$1 --author=\"Rob <robertaxelkirby@gmail.com>\""
}


function copy_db_tables_to_clipboard() {
    export $(grep -E 'DB_DATABASE=|DB_USERNAME=|DB_PASSWORD=' /Users/robert.kirby/g/aice/.env | xargs)
    docker exec -it aice-pgsql-1 psql -U $DB_USERNAME -d $DB_DATABASE -c "\dt public.*" | pbcopy
}
alias copy-aice-schema='docker exec aice-pgsql-1 /bin/bash -c '\''PAGER="" psql -U postgres -d ai_content_engine -c "\\d public.*"'\'' | pbcopy'
alias copy-schema=copy-aice-schema
alias copy-db=copy-aice-schema
alias copy-database=copy-aice-schema
alias copy-tables=copy-aice-schema


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Add go stuff to path
export PATH="$PATH:$(go env GOPATH)/bin"
# Add typescript-language-server to path
export PATH="$PATH:/Users/robert.kirby/.nvm/versions/node/v17.9.0/bin"

