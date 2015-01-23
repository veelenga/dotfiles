#!/bin/bash
############################
# restore.sh
# This script creates symlinks from the home directory to any desired dotfiles in desired directory
############################

# Beautify list of current symlinks recursively
#
# Prints symlink in next format:
#    path_to_file -> path_to_symlink
#
# If $1 is directory iterate recursively all child 
#  files/folders.
#
# $1 - path to file/directory to print it's symlink
pretty_symlink() {
      if [ -d "$1" ]; then
        for e in "$1"/*
        do
          pretty_symlink $(dirname $e)/$(basename $e) # avoid multiple separators
        done
      else
        echo "$1 -> $(readlink $1)"
      fi
}

########## Variables

dir=$GIT_REPOS/dotfiles           # dotfiles directory
temp=$GIT_REPOS/dotfiles/temp     # backup directory
files="vimrc gvimrc ctags zshrc i3blocks.conf i3/ notify-osd compton.conf Xdefaults moc/"

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
      pretty_symlink ~/.$file
    fi
done

echo "\nYour old dotfiles were saved in $temp directory if any"
