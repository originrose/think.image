#!/bin/bash
env
lein with-profile -user localrepo install /opencv-3.3.0/build/bin/opencv-330.jar thinktopic/opencv-java 3.3.0
echo "don't worry, there's no error, lein's just being dumb"
lein with-profile -user localrepo install /opencv-3.3.0/opencv-native-330.jar thinktopic/opencv-java-native 3.3.0
echo "don't worry, there's no error, lein's just being dumb"
lein repl
