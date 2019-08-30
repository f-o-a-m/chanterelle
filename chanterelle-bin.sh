#!/bin/bash

# Borrowed with love from https://stackoverflow.com/a/246128/1763937
get_script_dir() {
  SOURCE="${BASH_SOURCE[0]}"
  while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
  done
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  echo "$DIR"
}

CHNTRL_DIR=$(get_script_dir)
CHNTRL_OUTPUT_DIR="$CHNTRL_DIR/output";

if [ ! -f './output/ChanterelleMain/index.js' ] 
then 
    echo "Did not detect a project-level Chanterelle version, will fall back to the system-wide installation." >&2
    echo "Make sure you have purescript-chantrelle in your PureScript dependencies and it is compiled." >&2
    node -e "require('$CHNTRL_OUTPUT_DIR/ChanterelleMain/index.js').main();" -- "chanterelle" $*
else
    node -e "require('./output/ChanterelleMain/index.js').main();" -- "chanterelle" $*
fi

