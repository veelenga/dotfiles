#!/bin/bash
############################
# restore.sh
# This script creates symlinks from the home directory to any desired dotfiles in desired directory
############################

########## Variables

dir=$GIT_REPOS/dotfiles           # dotfiles directory
temp=$GIT_REPOS/dotfiles/temp     # backup directory
files="vimrc gvimrc ctags i3blocks.conf i3/ zshrc tmux.conf"

##########

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done\n"

if [ -e $temp ]; then
  rm -r $temp/
fi

mkdir $temp

# move any existing dotfiles to backup directory,
# then create symlinks from the homedir to any files in the dotfiles directory
# specified in $files
for file in $files; do
    if [ -e ~/.$file ]; then
      mv ~/.$file $temp/$file
    fi
    if [ -e $dir/$file ]; then
      cp -rs $dir/$file ~/.$file
      ls -al ~/ | grep "\.$file"
    fi
done

echo "\nYour old dotfiles were saved in $temp directory if any"
