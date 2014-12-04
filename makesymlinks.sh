#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in desired directory
############################

########## Variables

dir=$GIT_REPOS/dotfiles           # dotfiles directory
files="vimrc gvimrc ctags i3blocks.conf i3/config i3/bin/display.sh config zshrc tmux.conf"

##########

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

# move any existing dotfiles to dotfiles directory,
# then create symlinks from the homedir to any files in the dotfiles directory
# specified in $files
for file in $files; do
    echo "Moving .$file to $dir"
    mv ~/.$file ./$file
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done
