#!/bin/bash

PATH_LINE="export PATH=\$PATH:$(pwd)/bin"

function link(){
  if [ ! -f $1 ]
  then
    echo "$2 doesn't exists skipping"
  else
    grep -qxF "$PATH_LINE" $1
    if [ $? -ne 0 ]
    then
      echo "$PATH_LINE" >> $1
      echo "linked in $2"
    else
      echo "already linked in $2"
    fi
  fi
}



link ~/.profile "~/.profile"
link ~/.zshrc "~/.zshrc"
link ~/.bash_profile "~/.bash_profile"

# update $PATH in current shell window
eval $PATH_LINE