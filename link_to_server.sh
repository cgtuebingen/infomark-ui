#!/usr/bin/env bash

if [ $# -eq 0 ]
    then
        echo "You need to supply the basepath to the backend server"
fi

STATIC_PATH=$1/static
MY_PATH="`dirname \"$0\"`"

echo $MY_PATH

ln -s $MY_PATH/index.html $STATIC_PATH/index.html
ln -s $MY_PATH/elm.js $STATIC_PATH/elm.js
ln -s $MY_PATH/assets $STATIC_PATH
ln -s $MY_PATH/translations $STATIC_PATH