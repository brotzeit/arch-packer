arch-packer
============

[![Melpa](https://melpa.org/packages/arch-packer-badge.svg)](http://melpa.milkbox.net/#/arch-packer)

Arch-packer provides a package menu and allows you to install, upgrade and delete packages. It can be 
used with `pacman` or the AUR helper `pacaur`.

![](https://raw.githubusercontent.com/brotzeitmacher/arch-packer.el/master/arch-packer.png)

## Usage

To use it call `M-x arch-packer-list-packages`.

Shortcuts for `arch-packer-package-menu-mode` buffers:

 * `m`     `arch-packer-menu-mark-unmark` remove mark
 * `d`     `arch-packer-menu-mark-delete` mark for deletion
 * `U`     `arch-packer-menu-mark-all-upgrades` mark all upgradable
 * `u`     `arch-packer-menu-mark-upgrade` mark for upgrade
 * `r`     `arch-packer-list-packages` refresh package list
 * `i`     `arch-packer-install-package` prompt user for packages
 * `x`     `arch-packer-menu-execute` perform marked package menu actions
 * `b`     `arch-packer-menu-visit-homepage` follow link
 * `RET`   `arch-packer-pkg-info` display info in seperate buffer

Change default to pacaur:

`(setq arch-packer-default-command "pacaur")`


## Installation

The package can be installed from MELPA.

    (require 'arch-packer)
