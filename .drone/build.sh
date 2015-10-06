#!/bin/bash
set -e
cd $DRONE_BUILD_DIR
printenv
rm RELEASE_NOTES.md ; echo "### 1.0.$DRONE_BUILD_NUMBER" > RELEASE_NOTES.md
#/build.sh BuildPackage
./build.sh PublishNuget nugetkey=${APIKEY}
