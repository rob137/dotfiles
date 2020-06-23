#! /bin/bash 
set -euo pipefail

# Beforehand, run:
# sudo apt-get install git && git clone https://github.com/rob137/dotfiles.git

sudo apt-get update -y

# Install tools other than zsh
sudo apt-get install -y git curl wget tmux vim ack fonts-powerline tree htop gnome-terminal firefox xclip locate 
# Populate paths db for locate command
sudo updatedb

# Will cause terminal session to end and open zsh, so run setup-2.sh once this is done
sudo apt-get install -y zsh
