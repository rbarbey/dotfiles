#!/bin/bash

# Load shell dotfiles previously "installed" via Makefile
for file in ~/.{path,bash_prompt,aliases,functions,extra,exports}; do
    if [[ -r "$file" ]] && [[ -f "$file" ]]; then
      source "$file"
    fi
done
unset file

shopt -s histappend

start_services() {
    # start colima sync
    if ! colima status > /dev/null 2>&1; then
        colima start default > /dev/null 2>&1
    fi

    # set DOCKER_HOST if respective colima container is there
    if [ -S $HOME/.colima/default/docker.sock ]; then
        export DOCKER_HOST="unix://$HOME/.colima/default/docker.sock"
    fi

    # start minikube
    if ! minikube status > /dev/null 2>&1 ; then
        minikube start > /dev/null 2>&1
    fi
}

# start Emacs
if ! pgrep Emacs > /dev/null; then
    emacs --bg-daemon > /dev/null 2>&1 &
fi

# start slow services in the background
start_services &

# auto-complete
if [ -f /usr/local/etc/bash_completion ]; then
  source /usr/local/etc/bash_completion
fi

# new bash-completion v2.0
if [ -f /usr/local/share/bash-completion/bash_completion ]; then
    source /usr/local/share/bash-completion/bash_completion
fi

# bash completion for homebrew on Apple M1
if [[ -r /opt/homebrew/etc/profile.d/bash_completion.sh ]]; then
    source /opt/homebrew/etc/profile.d/bash_completion.sh
fi
