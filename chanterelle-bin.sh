#!/bin/bash


if [ ! -d './output' ] 
then 
    echo "No './output' directory, make sure to compile purescript project."
    exit 1
fi


if [ ! -f './output/ChanterelleMain/index.js' ] 
then 
    echo "Make sure you have purescript-chantrelle in your purescript dependencies and it is compiled."
    exit 1
fi

node -e "console.log(process.argv);require('./output/ChanterelleMain/index.js').main();" -- "chanterelle" $*
