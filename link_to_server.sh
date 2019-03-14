#!/usr/bin/env bash

if [ $# -eq 0 ]
    then
        echo "You need to supply the basepath to the backend server"
        exit 1
fi

STATIC_PATH=$1/static
MY_PATH="$( cd "$(dirname "$0")" ; pwd -P )"

ln -s $MY_PATH/index.html $STATIC_PATH/index.html
ln -s $MY_PATH/elm.js $STATIC_PATH/elm.js
ln -s $MY_PATH/assets $STATIC_PATH
ln -s $MY_PATH/translations $STATIC_PATH