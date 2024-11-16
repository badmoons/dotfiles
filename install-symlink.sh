#/bin/bash

set -xe

ln -si	 $(pwd)/.tmux.conf 		$HOME
ln -si	 $(pwd)/.emacs			$HOME
ln -si	 $(pwd)/.emacs.d/snippets/ 	$HOME/.emacs.d/
ln -si	 $(pwd)/.emacs.d/early-init.el 	$HOME/.emacs.d/
ln -si	 $(pwd)/.config/ 		$HOME
