#!/bin/bash

lein codox
#cp ./target/docs

TITLE=$( awk -vRS="</title>" '/<title>/{gsub(/.*<title>|\n+/,"");print;exit}' ./target/doc/index.html )

DOCSDEST="../think.image.docs/"

MEH=$DOCSDEST$TITLE

mkdir "$MEH"

mv ./target/doc "$MEH"/ 
