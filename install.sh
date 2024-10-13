#/bin/sh

set -xe

cp    ./.tmux.conf 		$HOME
cp    ./.emacs			$HOME
cp -r ./.emacs.d/snippets/ 	$HOME/.emacs.d/
cp -r ./.emacs.d/early-init.el 	$HOME/.emacs.d/
cp -r ./.config/ 		$HOME
