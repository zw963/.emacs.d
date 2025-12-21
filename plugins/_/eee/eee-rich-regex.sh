#!/usr/bin/env bash

echo eee-rich-regex: $@ >> /tmp/a.txt
echo end >> /tmp/a.txt

# Generate permutations for regexps joining
function generate_permutations {
    local items=("$@")
    local len=${#items[@]}
    if (( len == 0 )); then
        echo ""
        return
    elif (( len == 1 )); then
        echo "(${items[0]})"
        return
    fi

    local i j first remaining
    for ((i=0; i<len; i++)); do
        first="${items[i]}"
        remaining=()
        for ((j=0; j<len; j++)); do
            if (( j != i )); then
                remaining+=("${items[j]}")
            fi
        done
        generate_permutations "${remaining[@]}" | while read -r rest; do
            echo "(${first}).*${rest}"
        done
    done
}

# Main script
type="emacs"
regexps=()

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --type)
            type="$2"
            shift 2
            ;;
        *)
            regexps+=("$1")
            shift
            ;;
    esac
done

joined=""

case "$type" in
    "pcre")
        if (( ${#regexps[@]} > 1 )); then
            printf -v joined "^%s" "$(printf "(?=.*%s)" "${regexps[@]}")"
        else
            joined="${regexps[0]}"
        fi
        ;;
    "basic")
        if (( ${#regexps[@]} > 0 )); then
            IFS=.\* joined="${regexps[*]}"
        else
            joined=""
        fi
        ;;
    *)
        if (( ${#regexps[@]} > 3 )); then
            ignored="${regexps[@]:3}"
            echo "Too many regexps, ${ignored} ignored. Use post-filtering!" >&2
            regexps=("${regexps[@]:0:3}")
        fi
        if (( ${#regexps[@]} > 0 )); then
            permutations=$(generate_permutations "${regexps[@]}")
            joined=$(echo "$permutations" | paste -sd '|' -)
            # if [[ $type == "emacs" ]]; then
                # joined=$(sed 's/(/\\(/g; s/)/\\)/g; s/|/\\|/g' <<< "$joined")
            # fi
        else
            joined=""
        fi
        ;;
esac

echo "$joined"
