#!/usr/bin/env bash

./build.sh
cp src/ldbnf/bin/Release/* ../BNF-vNext/packages/NICE.ldbnf/tools/
cp splitter/bin/Release/* ../BNF-vNext/packages/NICE.ldbnf/tools/
