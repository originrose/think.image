# think.image
[![Clojars Project](https://clojars.org/thinktopic/think.image/latest-version.svg)](https://clojars.org/thinktopic/think.image)

Various algorithms and utilities for dealing with images.

## Image Abstraction

Think.image contains an abstract protocol for operating on rects of pixels; that is pulling a rect of an arbitrary format
out of an image into an array of data and writing an array of data into an image.  The library will convert formats appropriately
and transparently so for instance if you have an alpha-masked RGBA image and you ask for a rect of single-byte data it will
perform the alpha multiply *and* grayscale conversion for you.  A similar conversion will apply when writing pixel data
back into the image.  So as a user you simply need to think in rects and formats and let the library perform whatever
conversions you need automatically.

This protocol is defined in [protocols.clj](src/think/image/protocols.clj) while user-level access to it is defined
in [image.clj](src/think/image/image.clj).  In order to use this protocol with buffered images
```clojure
(:require [think.image.core]
          [think.image.image :as img]) ;;access to user level functionality.
```

Examples of using this protocol are in some various other pieces of code, namely [patch.clj](src/think/image/patch.clj).

There is a generic java implementation for the data conversion.

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
