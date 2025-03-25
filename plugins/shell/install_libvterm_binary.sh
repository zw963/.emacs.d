#!/usr/bin/env bash

folder=~/Git/emacs-libvterm

(
    cd $folder
    git clean -fdx
    mkdir -p build
    cd build
    cmake -DUSE_SYSTEM_LIBVTERM=no ..
    make
)

cp $folder/vterm-module.so .
