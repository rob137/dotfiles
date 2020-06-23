#! /bin/bash 
set -euo pipefail

# Additional steps to finish setup of zsh / oh-my-zsh

chsh -s $(which zsh)
# Install ohmyzsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# Use preferred ohmyzsh theme (note powerline fonts required, installed above)
sed -i 's/robbyrussell/agnoster/g' ~/.zshrc

sed -i 's/HISTSIZE=1000/HISTSIZE=100000/g' ~/.bashrc
sed -i 's/HISTFILESIZE=2000/HISTFILESIZE=100000/g' ~/.bashrc
echo "EDITOR='vim'" >> ~/.bashrc
echo "HISTSIZE=100000
SAVEHIST=100000
EDITOR='vim'" >> ~/.zshrc

printf "\nNow restart and then go into Gnome terminal and set the color scheme to 'Solarized'\n"
