#! /bin/bash 
set -euo pipefail

# Change default keyboard (requires input - accept all default options, but change to UK on second screen)
sudo dpkg-reconfigure keyboard-configuration
# Make zsh default shell
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

# Setup vim using dotfiles repository
mkdir -p ~/.vim/swapfiles ~/.vim/colors
ln -s ~/dotfiles/monokai.vim ~/.vim/colors/monokai.vim
rm ~/.vimrc && ln -s ~/dotfiles/.vimrc ~/.vimrc

# Setup gitlola
ln -s ~/dotfiles/.gitconfig ~/.gitconfig

# Install vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
 			    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# Use vim-plug to install vim dependencies
vim +'PlugInstall --sync' +qa

printf "\nNow restart and then go into Gnome terminal and set the color scheme to 'Solarized'\n"
