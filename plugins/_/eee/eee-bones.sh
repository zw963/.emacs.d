#!/usr/bin/env bash

bones --align 50 $1 | fzf --ansi --exact --cycle \
               --delimiter : --nth '3..' \
               --reverse \
               --no-sort \
               --border \
               --color "border:#A15ABD" \
               --bind 'ctrl-f:page-down,ctrl-b:page-up' \
               --preview 'bat --color=always {1} --highlight-line {2}' \
               --preview-window 'up,70%,border-bottom,+{2}+3/3,~3' |
    xargs -0 -I{} bash -c 'echo "{}" | cut -d : -f1-3' |
    xargs -0 -I{} bash -c 'if [[ ! "{}" = /* ]]; then realpath "{}"; else echo "{}"; fi'
