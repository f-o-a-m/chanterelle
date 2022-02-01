#!/usr/bin/env bash

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

CHNTRL_DIR=$(get_script_dir);
CHNTRL_OUTPUT_DIR="$CHNTRL_DIR/output";
CHNTRL_MAIN_MODULE="ChanterelleMain/index.js"
CHNTRL_GLOBAL_MAIN="$CHNTRL_OUTPUT_DIR/$CHNTRL_MAIN_MODULE";
PURS_OUTPUT=${PURS_OUTPUT:-"./output"}
CHNTRL_LOCAL_MAIN="$PURS_OUTPUT/$CHNTRL_MAIN_MODULE";

run_chanterelle() {
    CHANTERELLEMAIN_INDEX_JS="$1"; shift
    exec node -e "require('$CHANTERELLEMAIN_INDEX_JS').main();" -- "chanterelle" $@
}

global_install_available() {
    [ -f "$CHNTRL_GLOBAL_MAIN" ]
}

global_postinstall() {
    cd "$CHNTRL_DIR"
    if [ "$EUID" == "0" ]
    then
        npm install && npm run global-postinstall-root && npm run build
    else
        npm install && npm run global-postinstall-non-root && npm run build
    fi

    if ! global_install_available
    then
      echo '`chanterelle global-postinstall` did not complete successfully, see output above, correct the issue, and try again'
      exit 1
    fi
}

if [ "$1" == "global-postinstall" ]
then
    if global_install_available && [ "$2" != "--force" ]
    then
      echo 'chanterelle global-postinstall appears to have already completed successfully. Rerun with --force if you want to run it again anyway'
    else
      global_postinstall
    fi
else
    if [ -f "$CHNTRL_LOCAL_MAIN" ]
    then
      run_chanterelle "$CHNTRL_LOCAL_MAIN" $@
    else
        echo "Did not detect a project-level Chanterelle version, will attempt to fall back to the system-wide installation." >&2
        echo "Make sure you have purescript-chantrelle in your PureScript dependencies and it is compiled." >&2
        if ! global_install_available
        then
          echo 'A global installation of chanterelle is not available, likely because `chanterelle global-postinstall` was never run or did not complete successfully' >&2
          echo 'Please run `chanterelle global-postinstall` and try again' >&2
          exit 1
        else
          export CHNTRL_IS_GLOBAL=yes
          run_chanterelle "$CHNTRL_GLOBAL_MAIN" $@
        fi
    fi
fi

