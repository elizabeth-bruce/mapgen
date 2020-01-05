# mapgen

![A map generated via the mapgen utility](https://raw.githubusercontent.com/elizabeth-bruce/mapgen/master/assets/mapgen_example_github.png)

A Haskell service to render randomly-generated world maps for fantasy and role-playing settings.

This is a proof-of-concept intended for the author to practice developing Haskell. No claims of functionality or reliability are currently made :)

## Requirements:

Besides the Haskell environment, you will need the following dependencies installed in your local environment before being able to build:
* [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)
* [fftw](http://www.fftw.org/)

## Steps to Run:

* `stack build`
* `stack run <width> <height>`
