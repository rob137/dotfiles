#! /bin/bash 
set -euo pipefail

# Setup stuff for fresh install of Ubuntu on Chromebook

# Beforehand, run:
# sudo apt-get install git && git clone https://github.com/rob137/dotfiles.git

path=$(dirname "$(readlink -f "$0")")

sudo apt-get update -y

# Install tools other than zsh
sudo apt-get install -y git curl wget tmux vim ack-grep fonts-powerline tree htop gnome-terminal firefox xclip locate 
pip install grip
# Populate paths db for locate command
sudo updatedb

# Setup gitlola
ln -s $path/.gitconfig ~/.gitconfig

# Makes nodemon clear on refresh
ln -s $path/nodemon.json ~/nodemon.json

ln -s %path/.tmux.conf ~/.tmux.conf

ln -s %path/.emacs ~/.emacs

# Setup vim using dotfiles repository
mkdir -p ~/.vim/colors
ln -s $path/monokai.vim ~/.vim/colors/monokai.vim
rm ~/.vimrc
ln -s $path/.vimrc ~/.vimrc
ln -s $path/.vimrc.coc ~/.vimrc.coc
ln -s $path/coc-settings.json ~/.vim/coc-settings.json
mkdir ~/.vim/undodir

# Install vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
 			    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# Use vim-plug to install vim dependencies, then install CoC language servers
vim +'PlugInstall --sync | CocInstall coc-html coc-css coc-docker coc-tsserver coc-json coc-xml coc-yaml' +qa

# Change default keyboard (requires input - accept all default options, but change to UK on second screen)
sudo dpkg-reconfigure keyboard-configuration

# Make zsh default shell
# Will cause terminal session to end and open zsh, so run setup-2.sh once this is done
sudo apt-get install -y zsh
