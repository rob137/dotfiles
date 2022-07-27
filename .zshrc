source ~/.zshrc.rob-universal

export JAVA_HOME=/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home;
export PATH="$JAVA_HOME/bin:$PATH";
export PATH="~/.bin/apache-maven-3.6.3/bin:$PATH"
export PATH="~/g/CESG/bossa/bossa:$PATH";
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin";
export PATH="/usr/local/Cellar/vim/8.2.2525/bin/vim:$PATH"

function feature() {
	git checkout -b feature/CESG-"$1"
}
function bugfix() {
	git checkout -b bugfix/CESG-"$1"
}
function exp() {
	git checkout -b exp/CESG-"$1"
}

alias updatedb='/usr/libexec/locate.updatedb'

alias python='python3'
alias pip='pip3'

alias general='v ~/notes/general.txt'
alias gen='general'
alias ui='v ~/notes/ui.txt'
alias backend='v ~/notes/backend.txt'
alias be='backend'
alias other='v ~/notes/other.txt'
alias mo='v ~/notes/mongo.txt'
alias branch='v ~/notes/branches.txt'
alias br='branch'
alias branches='branch'
alias bra='branch'
alias ea='cd ~/g/cesg/endpoint-actions'
alias eat='cd ~/g/cesg/endpoint-actions/terraform'
alias api='cd ~/g/platform/api-store'
alias ci='cd ~/g/cloud/cloud-infrastructure'
alias hui='cat ~/notes/ui.txt | head'
alias hb='v ~/notes/branches.txt '
alias reviewers='v ~/notes/pr/reviewers.txt'
alias opennotes='cpb && vim ~/notes/$(pbpaste | sed -E "s/feature\/|bugfix\///").txt'
alias on='opennotes'
alias rev='reviewers'
alias li='cd ~/g/tests/lightning'
alias lili='cd ~/g/tests/lightning/build/install/lightning'
alias oni='cd ~/g/cesg/openresty-nchan-image'
alias mps='cd ~/g/cesg/mcs-push-server'
alias mdi='cd ~/g/cesg/mcs-data-ingest'
alias ete='cd ~/g/cesg/esg-ti-enrichment'
alias enrich='cd ~/g/cesg/esg-ti-enrichment'
alias avro='cd ~/sandpit/avro'
alias hadoop='cd ~/sandpit/hadoop'
alias en='cd ~/g/cesg/endpoint-state-query'
alias ti='cd ~/g/cesg/esg-ti-enrichment'
alias pas='cd ~/g/platform/api-store/'
alias pa='pas'
alias sc='cd ~/g/cloud/sophos-cloud'
alias cl='sc'
alias clui='cd ~/g/cloud/ui'
alias uiso='cd ~/g/ui/sophos'
alias cdb='cd ~/g/ui/sophos && npm run cdb'
alias ocus='cd ~/g/cloud/ui/src/customer'
alias ncus='cd ~/g/ui/sophos/libs/customer'
alias ocor='cd ~/g/cloud/ui/src/core'
alias ncor='cd ~/g/ui/sophos/libs/core'
alias nsha='cd ~/g/ui/sophos/libs/shared'
alias osha='cd ~/g/cloud/ui/src/shared'
alias nov='cd ~/g/nova'
alias bo='cd ~/g/CESG/bossa'
alias hmr='cd ~/g/tools/saas-tools/python/hmr-dep'
alias accounts='v ~/g/accounts.txt'
alias acc='accounts'
# Copy last account without newline (final line of accounts file)
alias lacc='tail -1 ~/g/accounts.txt | tr -d "\n" | pbcopy'
alias lace='lacc && exit;'
alias bossa='~/g/CESG/bossa/bossa'
alias b='bossa'
alias ui-build-affected='cd ~/g/ui/sophos && npm run build-affected; beep'
alias ui-serve='docker-compose -f ~/g/cloud/ui/runlocal_docker/docker-compose.yml up -d'
alias pink='~/g/cloud/ui/runlocal_docker/run_chrome.sh'
alias ui-go='ui-serve && pink'
alias uigo='ui-go'
alias ui-update-old='cd ~/g/cloud/ui && npm run update-sophos-packages; beep'
alias cq='cd ~/g/cloud/ui && npm run customer:quick; beep'
alias qu='cd ~/g/cloud/ui && npm run quick; beep'

alias cca="echo 'cloud clean assemble...'; ~/g/cloud/sophos-cloud/gradlew clean assemble; beep"
alias scca="cca"
alias ca="cca"
alias lca="~/g/tests/lightning/gradlew clean assemble; beep"
alias eca="~/g/endpoint-state-query/gradlew clean assemble; beep"

ui-reload-shared() {
  cd ~/g/ui/sophos &&
  nx run shared:package &&
  cd ~/g/cloud/ui &&
  tgz_path=$(find ~/g/ui/sophos/dist/libs/shared -type f -name 'sophos-shared-*.tgz') &&
  npm install @sophos/shared@file:///$tgz_path &&
  npm run ssp:quick;
  beep;
}
# Example: 'ui-reload customer' reloads the customer package in the cloud UI
ui-reload() {
  cd ~/g/ui/sophos &&
  nx run $1:package &&
  cd ~/g/cloud/ui &&
  tgz_path=$(find ~/g/ui/sophos/dist/libs/$1 -type f -name "sophos-$1-*.tgz") &&
  npm install @sophos/"$1"@file:///$tgz_path &&
  npm run "$1"\:quick;
  beep;
}
alias clui-match-dev='cd ~/g/cloud/ui && gchd && gpl && npm install && npm run quick'
alias clui-match-develop='clui-match-dev'
alias uiso-match-dev='cd ~/g/ui/sophos && gchd && gpl && npm install && npm run install-global-deps && npm run build-affected'
alias uiso-match-develop='uiso-match-dev'
function personal() {
  eval "$1 --author=\"Rob <robertaxelkirby@gmail.com>\""
}

