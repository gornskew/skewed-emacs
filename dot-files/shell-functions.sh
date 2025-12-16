#!/bin/bash

function eskew() {
    docker exec -it --detach-keys "ctrl-^" skewed-emacs emacsclient -t "$@"
}

function egskew() {
    docker exec -it --detach-keys "ctrl-^" skewed-emacs emacsclient -c "$@" &
}
    
# Show welcome message
echo ""
echo "Shell Commands for the 🦖 Skewed Emacs Stack 🦖"
echo ""
printf "\n%b\n" "${BLUE}𝚃𝚘 𝙻𝚊𝚞𝚗𝚌𝚑 𝙴𝚖𝚊𝚌𝚜:${NC}"
printf "%b\n" "\`eskew\` ${BLUE}for a terminal emacsclient, or${NC}"
printf "%b\n\n" "\`egskew\` ${BLUE}for a graphical one.${NC}"
echo ""
