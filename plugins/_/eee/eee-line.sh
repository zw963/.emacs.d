#!/usr/bin/env bash

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh

QUERY_FILE="$1"

bat -n --color always --decorations always "${QUERY_FILE}" | fzf --ansi \
	--exact \
	--cycle \
  --layout reverse \
  --no-sort \
	--border \
  --bind 'ctrl-f:page-down,ctrl-b:page-up' \
  | awk '{print $1}' | xargs -0 -I{} echo "${QUERY_FILE}":{}
