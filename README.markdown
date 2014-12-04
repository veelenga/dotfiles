Dotfiles
========
This repository includes all of my custom dotfiles. It contains a script (bin/restore.sh) to hep with managing and updating your dotfiles.

Using this repo
========

First, fork this repo.

Then, add your dotfiles:

```sh
$ git clone git@github.com:username/dotfiles.git $GIT_REPOS/dotfiles
$ cd dotfiles
$  # edit files
$  # edit files
$ git push origin master
```

Finally, to install your dotfiles onto a new system:

```sh
$ git clone git@github.com:username/dotfiles.git $GIT_REPOS/dotfiles
$ cd $GIT_REPOS/dotfiles
$ sh bin/restore.sh # creates symlinks to install files
```

Sample output (depends on your config):

```sh
Changing to the /home/user/Dev/git_repos/dotfiles directory ...done

lrwxrwxrwx  1 user user      51 Dec  4 23:43 .vimrc -> /home/user/Dev/git_repos/dotfiles/vimrc
lrwxrwxrwx  1 user user      52 Dec  4 23:43 .gvimrc -> /home/user/Dev/git_repos/dotfiles/gvimrc
lrwxrwxrwx  1 user user      51 Dec  4 23:43 .ctags -> /home/user/Dev/git_repos/dotfiles/ctags
lrwxrwxrwx  1 user user      59 Dec  4 23:43 .i3blocks.conf -> /home/user/Dev/git_repos/dotfiles/i3blocks.conf
lrwxrwxrwx  1 user user      51 Dec  4 23:43 .zshrc -> /home/user/Dev/git_repos/dotfiles/zshrc
lrwxrwxrwx  1 user user      55 Dec  4 23:43 .tmux.conf -> /home/user/Dev/git_repos/dotfiles/tmux.conf

Your old dotfiles were saved in /home/user/Dev/projects/git_repos/dotfiles/temp directory if any
```
