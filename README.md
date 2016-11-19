# amby: statistical data visualization

[![Build Status](https://travis-ci.org/jsermeno/amby.svg?branch=master)](https://travis-ci.org/jsermeno/amby)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](Haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](tl;dr Legal: BSD3)

<img src="https://cloud.githubusercontent.com/assets/197051/19674286/a8a8e3f6-9a4c-11e6-9bdf-2a67b6d46660.png" alt="normal distribution plot" width="200" height="150">
<img src="https://cloud.githubusercontent.com/assets/197051/19674456/eaff0d42-9a4d-11e6-9560-e41f64514fb9.png" alt="clean theme equation plot" width="200" height="150">
<img src="https://cloud.githubusercontent.com/assets/197051/19674436/cfa79db6-9a4d-11e6-84b3-ba5a6000a41b.png" alt="multiple beta distributions" width="200" height="150">

A statistics visualization library built on top of [Chart](https://github.com/timbod7/haskell-chart) inspired by [Seaborn](https://github.com/mwaskom/seaborn). Amby provides a high level interface to quickly display attractive visualizations. Amby also provides tools to display Charts from both Amby and the Chart package within GHCi.

## Basics

```haskell
λ> import qualified Amby as Am
```

Here's how you might plot the standard normal distribution.

```haskell
λ> import qualified Statistics.Distribution.Normal as Stats
λ> let d = Stats.standard
λ> let x = Am.contDistrDomain d 10000
λ> let y = Am.contDistrRange d x
λ> Am.save $ Am.plot x y
```

<img src="https://cloud.githubusercontent.com/assets/197051/19674286/a8a8e3f6-9a4c-11e6-9bdf-2a67b6d46660.png" alt="normal distribution plot" width="400" height="300">

## Plot distributions

```haskell
λ> let sampleData = concat $ zipWith replicate [5, 4, 3, 7] [0..3]
λ> Am.save $ Am.distPlot sampleData
```

<img src="https://cloud.githubusercontent.com/assets/197051/20458700/b0f45c84-ae79-11e6-8995-dc93cf41bac3.png" alt="distplot" width="400" height="300">

## Rendering

There are several ways to render plots.

First, Amby provides the helper functions `save` and `saveSvg` that will save a graph to the file `.__amby.png` and `.__amby.svg` respectively.

Second, you can use any rendering methods that the underlying [Chart](https://github.com/timbod7/haskell-chart) library provides by converting an `AmbyChart ()` to a `EC (Layout Double Double) ()` with the `getEC` function.

Third—if you have a terminal that supports images such as iTerm2—you can display charts directly inside the GHCi repl. Just install the [`imgcat`](https://github.com/eddieantonio/imgcat#Build) executable, and the [`pretty-display`](https://github.com/jsermeno/pretty-display) library. See [below](https://github.com/jsermeno/amby#dependencies) for further installation instructions.

<img src="https://cloud.githubusercontent.com/assets/197051/20401530/36e64424-acc7-11e6-889a-a664b4de9f82.png" alt="terminal example" width="637" height="524">

## Plot graph using equations

You can also specify graphs using a domain and an equation.

```haskell
λ> Am.save $ Am.plotEq [0,0.001..4] sqrt
```

<img src="https://cloud.githubusercontent.com/assets/197051/19674456/eaff0d42-9a4d-11e6-9560-e41f64514fb9.png" alt="clean theme equation plot" width="400" height="300">

## Multiple container types

Plotting functions work on both lists and generic vectors of doubles.

```haskell
λ> Am.save $ Am.plotEq [0,0.001..4] sqrt
λ> Am.save $ Am.plotEq (Am.linspace 0 4 4000) sqrt
```

## Combine graphs using do notation

```haskell
λ> import Statistics.Distribution.Beta as Stats
λ> let plotBeta a b =
λ|       let d = Stats.betaDistr a b
λ|           x = Am.contDistrDomain d 10000
λ|           y = Am.contDistrRange d x
λ|       Am.plot x y
λ> Am.save $ do
λ|   Am.theme Am.cleanTheme
λ|   plotBeta 0.5 0.5
λ|   plotBeta 5 1
λ|   plotBeta 1 3
λ|   plotBeta 2 2
λ|   plotBeta 2 5
λ|   ylim (0.0, 2.5)
```

<img src="https://cloud.githubusercontent.com/assets/197051/19674436/cfa79db6-9a4d-11e6-84b3-ba5a6000a41b.png" alt="multiple beta distributions" width="400" height="300">

## Dependencies

To use amby you'll first need to install Chart and gtk2hs if you don't already have them.

### Chart and Gtk2Hs

***Mac OS X***

Here are the instructions I used to install Chart and gtk2hs on OS X El Capitan with stack.

```sh
stack install Chart-diagrams
brew cask install xquartz
brew install glib cairo gtk gettext fontconfig freetype
```

Add the following environment variable `export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig` to `.bashrc` or similar file.

```sh
stack install alex happy
stack install gtk2hs-buildtools
stack install glib
stack install -- gtk --flag gtk:have-quartz-gtk
stack install Chart-cairo
```

***Linux and Windows***

Instructions for installing gtk2hs on Linux and Windows can be found [here](https://wiki.haskell.org/Gtk2Hs/Installation).

Likewise, run

```sh
stack install Chart-diagrams
stack install Chart-cairo
```

### Imgcat

To be able to display charts in ghci with a terminal such as iTerm2 you'll need `imgcat` and `pretty-display`.

**Mac OS X**

```sh
brew tap eddieantonio/eddieantonio
brew install imgcat
```

**Linux and Windows**

For more information visit [imgcat's repository](https://github.com/eddieantonio/imgcat#Build)

### pretty-display

1. Add `pretty-display` to your cabal file.
2. `stack build`
3. Place the following in your `.ghci` file. If you're using stack you can put this file at the root of your project.

```haskell
import Text.Display

:set -interactive-print=Text.Display.dPrint
:def pp (\_ -> return ":set -interactive-print=Text.Display.dPrint")
:def npp (\_ -> return ":set -interactive-print=print")
```

4. Restart ghci.

## Other tips

### Auto-reload files

If using the 'save' or 'saveSvg' functions because your terminal is unable to display images within GHCi you can use a tool such as [entr](https://github.com/clibs/entr) to run a command like `open` whenever the file is saved.

```sh
ls -d __amby.png | entr -r open /_
```
