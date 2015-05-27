# Dotfiles
This repository includes all of my custom dotfiles. 

[![Foo](https://sc-cdn.scaleengine.net/i/f1150c1698a3e48b480ec9c368b0f6cd.png)](https://sc-cdn.scaleengine.net/i/f1150c1698a3e48b480ec9c368b0f6cd.png)

# Using this repo

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

/home/user/.vimrc -> /home/user/git_repos/dotfiles/vimrc
/home/user/.gvimrc -> /home/user/git_repos/dotfiles/gvimrc
/home/user/.ctags -> /home/user/git_repos/dotfiles/ctags
/home/user/.zshrc -> /home/user/git_repos/dotfiles/zshrc
/home/user/.tmux.conf -> /home/user/git_repos/dotfiles/tmux.conf
/home/user/.i3blocks.conf -> /home/user/git_repos/dotfiles/i3blocks.conf
/home/user/.i3/bin/currency.sh -> /home/user/git_repos/dotfiles/i3/bin/currency.sh
/home/user/.i3/bin/display.sh -> /home/user/git_repos/dotfiles/i3/bin/display.sh
/home/user/.i3/bin/vpnc.sh -> /home/user/git_repos/dotfiles/i3/bin/vpnc.sh
/home/user/.i3/config -> /home/user/git_repos/dotfiles/i3/config

Your old dotfiles were saved in /home/user/git_repos/dotfiles/temp directory if any
```
