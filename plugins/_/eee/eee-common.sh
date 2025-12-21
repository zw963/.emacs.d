CURR_DIR=$(dirname $(readlink -f $0))


# [M-1]:file;   [M-2]:rg;    [M-3]: imenu;
# [M-4]:symbol; [M-5]:ee-rg; [M-6]: git;
# HEADER_KEYBIND_HELP="[F1]: ee-rg, [F2]: ee-find, [F3]: ee-symbols(TODO)"
HEADER_KEYBIND_HELP=""

EEE_RG_SCRIPT=${CURR_DIR}/eee-rg.sh
EEE_FIND_SCRIPT=${CURR_DIR}/eee-find.sh

FZF_BINDS="\
f1:become(${EEE_RG_SCRIPT}),\
f2:become(${EEE_FIND_SCRIPT} {})\
"

error() {
    echo "error: $*"
    exit 1
} >&2

check_tools() {
    local tool
    for tool; do
        case "$tool" in
            bat)
                __lookup_tool BAT bat batcat
                ;;

            devicon-lookup)
                __lookup_tool DEVICON_LOOKUP devicon-lookup
                ;;

            fd)
                __lookup_tool FD fd fdfind
                ;;

            fzf)
                __lookup_tool FZF fzf
                ;;

            rg)
                __lookup_tool RG rg
                ;;

            *)
                error "unknown tool '$tool'"
        esac
    done
}

__lookup_tool() {
    local var knownas t p
    var=$1
    shift
    knownas=$1
    for t; do
        p="$(command -v $t)" && {
            eval "$var='$p'"
            return 0
        }
    done

    error "tool '$knownas' not installed"
}
