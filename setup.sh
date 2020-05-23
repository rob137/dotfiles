#! /bin/bash 
set -euo pipefail

# git clone https://github.com/rob137/dotfiles.git

sudo apt-get update -y
sudo apt-get install -y git curl wget tmux vim zsh fonts-powerline tree htop gnome-terminal firefox xclip

# Make zsh default shell
chsh -s $(which zsh)
Install ohmyzsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# Use preferred ohmyzsh theme (note powerline fonts required, installed above)
sed -i 's/robbyrussell/agnoster/g' ~/.zshrc

sed -i 's/HISTSIZE=1000/HISTSIZE=100000/g' ~/.bashrc
sed -i 's/HISTFILESIZE=2000/HISTFILESIZE=100000/g' ~/.bashrc
echo "HISTSIZE=100000
SAVEHIST=100000" >> ~/.zshrc

# Setup vim using dotfiles repository
mkdir -p ~/.vim/swapfiles ~/.vim/colors
ln -s ~/dotfiles/monokai.vim ~/.vim/colors/monokai.vim
rm ~/.vimrc && ln -s ~/dotfiles/.vimrc ~/.vimrc

# Install vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
 			    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# Use vim-plug to install vim dependencies
vim +'PlugInstall --sync' +qa

printf "\nOn restarting, go into Gnome terminal and set the color scheme to 'Solarized'\n"

shutdown --reboot
