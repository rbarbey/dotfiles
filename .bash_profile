#!/bin/bash

# Load shell dotfiles previously "installed" via Makefile
for file in ~/.{bash_prompt,aliases,functions,path}; do
    if [[ -r "$file" ]] && [[ -f "$file" ]]; then
	source "$file"
    fi
done
unset file

shopt -s histappend
