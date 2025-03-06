#!/usr/bin/env bash

export TEMP=$(mktemp -u)
trap 'rm -f "$TEMP"' EXIT


CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh
EE_REGEX=${CURR_DIR}/eee-rich-regex.sh

# Switch between Ripgrep mode and fzf filtering mode (CTRL-T)
rm -f /tmp/rg-fzf-{r,f}

INITIAL_QUERY="$1"

export QUERY_PATH="${2:-.}"

TRANSFORMER='
  rg_pat={q:1}      # The first word is passed to ripgrep
  fzf_pat={q:2..}   # The rest are passed to fzf

  if ! [[ -r "$TEMP" ]] || [[ $rg_pat != $(cat "$TEMP") ]]; then
    echo "$rg_pat" > "$TEMP"
    printf "reload:sleep 0.05; rg --column --line-number --no-heading --color=always --smart-case %q %q || true" "$rg_pat" ${QUERY_PATH} 
  fi
  echo "+search:$fzf_pat"
'

fzf --ansi --disabled --query "$INITIAL_QUERY" \
    --delimiter : --nth 3.. \
    --reverse \
    --exact \
    --cycle \
    --border \
    --with-shell 'bash -c' \
    --bind "start,change:transform:$TRANSFORMER" \
    --color "hl:-1:underline,hl+:-1:underline:reverse,border:#A15ABD" \
    --delimiter : \
    --preview 'bat --color=always {1} --highlight-line {2}' \
    --preview-window 'up,70%,border-line,+{2}+3/3,~3' \
    --bind 'ctrl-f:page-down,ctrl-b:page-up' |
    xargs -0 -I{} echo $(pwd)/{}

