#!/usr/bin/env bash

case "$EMACS" in
  emacs24)
    sudo add-apt-repository -y ppa:cassou/emacs
    sudo apt-get update -qq
    sudo apt-get -qq install emacs24 emacs24-el
    ;;
  emacs-snapshot)
    sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
    sudo apt-get update -qq
    sudo apt-get install -qq  emacs-snapshot emacs-snapshot-el
    ;;
  *)
    echo "Unsupported EMACS='$EMACS'"
    exit 1
    ;;
esac

wget -O - https://github.com/cask/cask/archive/master.tar.gz | tar -xvz
make EMACS="$EMACS" CASK="cask-master/bin/cask"
