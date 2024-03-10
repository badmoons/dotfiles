#/bin/sh

set -xe

cp $HOME/.emacs ./
cp -r $HOME/.emacs.d/snippets/ ./.emacs.d/
cp -r $HOME/.emacs.d/early-init.el ./.emacs.d/
cp -r $HOME/.config/foot/ ./.config/
cp -r $HOME/.config/sway/ ./.config/
