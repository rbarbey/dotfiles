#!/bin/bash

# Load shell dotfiles previously "installed" via Makefile
for file in ~/.{bash_prompt,aliases,functions,path,extra,exports}; do
    if [[ -r "$file" ]] && [[ -f "$file" ]]; then
      source "$file"
    fi
done
unset file

shopt -s histappend

# auto-complete
if [ -f /usr/local/etc/bash_completion ]; then
  source /usr/local/etc/bash_completion
fi

# new bash-completion v2.0
if [ -f /usr/local/share/bash-completion/bash_completion ]; then
    source /usr/local/share/bash-completion/bash_completion
fi

# bash completions for Google Cloud SDK
if [ -f /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc ]; then
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc
fi

if minikube status > /dev/null ; then
  eval $(minikube docker-env)
else
  echo 'Minikube is not running so in case you need Docker, start Minikube and run eval $(minikube docker-env)'
fi
