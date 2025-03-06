#!/usr/bin/env bash
set -e

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh
EE_REGEX=${CURR_DIR}/eee-rich-regex.sh

# Switch between Ripgrep mode and fzf filtering mode (CTRL-T)
rm -f /tmp/rg-fzf-{r,f}
RG_PREFIX="rg --column --line-number --hidden --no-heading --color=always --smart-case -g '!.git' --follow "
# INITIAL_QUERY="${*:-}"
INITIAL_QUERY="$1"
QUERY_PATH="${2:-.}"
fzf --ansi --disabled --query "$INITIAL_QUERY" \
	--exact \
	--cycle \
	--border \
	--bind "start:reload:$RG_PREFIX {q} ${QUERY_PATH}" \
	--bind "change:reload:sleep 0.01;( eval ${EE_REGEX} {q} | xargs -IXX $RG_PREFIX XX ${QUERY_PATH}) || true " \
    --bind 'ctrl-f:page-down,ctrl-b:page-up' \
	--bind "${FZF_BINDS}" \
	--bind 'ctrl-t:transform: [[ ! $FZF_PROMPT =~ ripgrep ]] &&
      echo "rebind(change)+change-prompt(1. ripgrep> )+disable-search+transform-query:echo \{q} > /tmp/rg-fzf-f; cat /tmp/rg-fzf-r" ||
      echo "unbind(change)+change-prompt(2. fzf> )+enable-search+transform-query:echo \{q} > /tmp/rg-fzf-r; cat /tmp/rg-fzf-f"' \
	--color "hl:-1:underline,hl+:-1:underline:reverse,border:#A15ABD" \
	--prompt '1. ripgrep> ' \
  --delimiter : --nth '3..' \
	--header-first \
	--header " CWD:$(pwd)
[Ctrl-t]: Switch between ripgrep/fzf
${HEADER_KEYBIND_HELP}
" \
	--layout=reverse-list \
	--preview 'bat --color=always {1} --highlight-line {2}' \
	--preview-window 'up,70%,border-bottom,+{2}+3/3,~3' |
	xargs -0 -I{} echo $(pwd)/{}
#    --bind 'enter:become(vim {1} +{2})'
