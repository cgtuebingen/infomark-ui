#!/usr/bin/env bash

if [ $# -eq 0 ]
    then
        echo "You need to supply the basepath to the backend server"
        exit 1
fi

STATIC_PATH=$1/static
MY_PATH="$( cd "$(dirname "$0")" ; pwd -P )"

ln -s $MY_PATH/build/index.html $STATIC_PATH/index.html
ln -s $MY_PATH/build/asset-manifest.json $STATIC_PATH/asset-manifest.json
ln -s $MY_PATH/build/service-worker.js $STATIC_PATH/service-worker.js
ln -s $MY_PATH/build/manifest.json $STATIC_PATH/manifest.json
ln -s $MY_PATH/build/icons $STATIC_PATH
ln -s $MY_PATH/build/images $STATIC_PATH
ln -s $MY_PATH/build/static $STATIC_PATH
ln -s $MY_PATH/build/styles $STATIC_PATH
ln -s $MY_PATH/build/translations $STATIC_PATH