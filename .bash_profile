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
