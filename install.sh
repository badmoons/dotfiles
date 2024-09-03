#/bin/sh

set -xe

cp    ./.emacs $HOME
cp -r ./.emacs.d/snippets/ $HOME
cp -r ./.emacs.d/early-init.el/ $HOME
cp -r ./.config/ $HOME
