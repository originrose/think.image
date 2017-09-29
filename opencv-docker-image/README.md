# ThinkTopic's OpenCV image

## What is this for?

If you want to quickly do useful things involving Clojure, Cortex and OpenCV, this is your best bet, especially if you have plans to have an easy transition from your dev workflow to production.

This directory contains everything needed to build `thinktopic/opencv-cortex`, an image based on [`thinktopic/cortex-base`](https://github.com/thinktopic/cortex/blob/master/scripts/Dockerfile.cortex-base).

## Features

*  OpenCV 3.3.0, compiled with ffmpeg, Java bindings and most optional dependencies.
*  The same Ubuntu / CUDA / Cortex setup as a lot of our machine learning projects
*  Can run it in any lein project, and it'll spin up a REPL (using *your* `project.clj`) with access to your repo's dir, with OpenCV support that works without needing to change any of your dependencies, etc.
*  Runs as your current user, not root, so you don't end up with bizarre issues with files in your repo that you can't modify
*  Keeps a cache of its own maven dependencies in the app dir, so it doesn't interfere with your own ~/.m2
*  By default it starts a REPL for your project, bound locally on a fixed port

## How to use

Run this from within the root dir of your leiningen app:

```clj
docker pull docker.thinktopic.com/opencv-cortex
docker run --rm -u (id -u):(id -g) -i -t -v (pwd):/usr/src/app -p 6666:6666 --name cv-repl thinktopic/opencv-cortex
```

You should be greeted with a REPL. You can connect more sessions to the REPL like this:

```
lein repl :connect 0.0.0.0:6666
```

Or however you so choose. You can also just use the REPL you attach to by default when you do Docker run.

If something goes wrong, file an issue here or just ping Alistair.

### Trying OpenCV out

Try running this at the REPL, after adding "sample-video.mp4" to the root of your repo:

```clj
(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)
(import '(org.opencv.videoio VideoCapture))
(import '(org.opencv.core Point Mat))
(import '(org.opencv.imgcodecs Imgcodecs))

(def vc
  (VideoCapture. "sample-video.mp4"))

(def output
  (Mat.))

(assert (.isOpened vc))

(.read vc output)

(assert (not (.empty output)))

(Imgcodecs/imwrite "first_frame_of_video.jpg" output)
```

## Rough edges

*  The local maven repo for the container is stored in a directory called "?". Fixing this is a matter of including a settings.xml file (for maven) in the Dockerfile, in the user's home directory.

## Building

```
docker build . -t thinktopic/opencv-cortex
```

