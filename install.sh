#/bin/sh

set -xe

cp    ./.emacs $HOME
cp -r ./.emacs.d/snippets/ $HOME/.emacs.d
cp -r ./.emacs.d/early-init.el/ $HOME
cp -r ./.config/ $HOME
