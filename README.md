# amby

A statistics visualization library built on top of [Chart](https://github.com/timbod7/haskell-chart) heavily inspired by [Seaborn](https://github.com/mwaskom/seaborn). Amby provides a high level interface to quickly display attractive visualizations.

## Dependencies

To use amby you'll first need to install Chart and gtk2hs if you don't already have them.

### Mac OS X

Here are the instructions I used to install Chart and gtk2hs on OS X El Capitan with stack.

```
stack install Chart-diagrams
brew cask install xquartz
brew install glib cairo gtk gettext fontconfig freetype
```

Add the following environment variable `export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig` to `.bashrc` or similar file.

```
stack install alex happy
stack install gtk2hs-buildtools
stack install glib
stack install -- gtk --flag gtk:have-quartz-gtk
stack install Chart-cairo
```

### Linux and Windows

Instructions for installing gtk2hs on Linux and Windows can be found [here](https://wiki.haskell.org/Gtk2Hs/Installation).

Likewise, run

```
stack install Chart-diagrams
stack install Chart-cairo
```

## Usage

```
λ> import qualified Amby as Am
```

Here's how you might plot the normal distribution.

```
λ> import qualified Statistics.Distribution.Normal as Stats
λ> let d = Stats.standard
λ> let x = Am.contDistrDomain d 10000
λ> let y = Am.contDistrRange d x
λ> Am.save $ Am.plot x y
```

<img src="https://cloud.githubusercontent.com/assets/197051/19674286/a8a8e3f6-9a4c-11e6-9bdf-2a67b6d46660.png" alt="normal distribution plot" width="400" height="300">

### Interactivity

Amby provides shortcuts to speedup iteration. `Am.save` is a shortcut that saves the graph as a png file using the Cairo backend to a file named `__amby.png`. This allows you to run a command in a terminal window such as:

```
ls -d __amby.png | entr -r imgcat /_
```

This will allow you to create graphs in ghci, and have them display in another window instanteously.

<img src="https://cloud.githubusercontent.com/assets/197051/19673860/5045b520-9a49-11e6-8c04-04e96dcab4fb.png" alt="terminal example" width="512" height="320">

### Plot graph using equations

You can also specify graphs using a domain and an equation.

```
λ> Am.save $ Am.plotEq [0,0.001..4] sqrt
```

<img src="https://cloud.githubusercontent.com/assets/197051/19674456/eaff0d42-9a4d-11e6-9560-e41f64514fb9.png" alt="clean theme equation plot" width="400" height="300">

### Multiple container types

Plotting functions work on both lists and generic vectors of doubles.

```
λ> Am.save $ Am.plotEq [0,0.001..4] sqrt
λ> Am.save $ Am.plotEq (Am.linspace 0 4 4000) sqrt
```

### Combine graphs using do notation

```
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
