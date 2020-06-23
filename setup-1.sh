#! /bin/bash 
set -euo pipefail

# Setup stuff for fresh install of Ubuntu on Chromebook

# Beforehand, run:
# sudo apt-get install git && git clone https://github.com/rob137/dotfiles.git

sudo apt-get update -y

# Install tools other than zsh
sudo apt-get install -y git curl wget tmux vim ack fonts-powerline tree htop gnome-terminal firefox xclip locate 
# Populate paths db for locate command
sudo updatedb

# Set up vimdiff
git config --global diff.tool vimdiff
git config --global merge.tool vimdiff
git config --global alias.vimdiff difftool


# Setup gitlola
ln -s ~/dotfiles/.gitconfig ~/.gitconfig

# Tmux conf - currently just sets 256 colors
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf

# Setup vim using dotfiles repository
mkdir -p ~/.vim/swapfiles ~/.vim/colors
ln -s ~/dotfiles/monokai.vim ~/.vim/colors/monokai.vim
rm ~/.vimrc && ln -s ~/dotfiles/.vimrc ~/.vimrc

# Install vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
 			    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# Use vim-plug to install vim dependencies
vim +'PlugInstall --sync' +qa

# Change default keyboard (requires input - accept all default options, but change to UK on second screen)
sudo dpkg-reconfigure keyboard-configuration

# Make zsh default shell
# Will cause terminal session to end and open zsh, so run setup-2.sh once this is done
sudo apt-get install -y zsh
