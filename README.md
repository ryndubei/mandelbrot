mandelbrot
==========

Basic Mandelbrot set generator in Gloss.

Build
-----

First, install [GHCup](https://www.haskell.org/ghcup/), then

```
sudo apt install freeglut3-dev
cd mandelbrot
stack build
```

Use `stack run` to run the program.

Usage
-----

Use left click to zoom in and right click to zoom out.

- 'z' : increase iterations by 1
- 'x' : decrease iterations by 1