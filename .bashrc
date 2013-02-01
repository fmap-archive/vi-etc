#!/usr/bin/env bash

# First, make sure we're running interactively:

if [ -n "$PS1" ] ; then
  
  # Add my personal scripts to the path..

  BIN="$HOME/bin"
  [ -d $BIN                 ] && PATH="$BIN:$PATH"      || echo "Error sourcing user scripts."

  # ..and source my preferred environment, aliases and functions.

  [ -r ~/.bash/environment  ] && . ~/.bash/environment  || echo "Error reading environment file."
  [ -r ~/.bash/aliases      ] && . ~/.bash/aliases      || echo "Error reading aliases file."
  [ -r ~/.bash/functions    ] && . ~/.bash/functions    || echo "Error reading functions file."

  # Then set my preferred prompts..

  PS1="\W$(PS1)\$ " # Command Prompt
  PS2="$(PS2)\$ "   # Secondary Prompt
  PS3="? "          # Select Prompt
  PS4="+ "          # Debugging Prompt

  # ..and the title, in being allowed to exist, should be informative.

  PROMPT_COMMAND="set_window_title"

  # Source host-specific settings, if any.

  [ -r ~/.bash/local        ] && . ~/.bash/local

fi
