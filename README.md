# amby: statistical data visualization

[![Build Status](https://travis-ci.org/jsermeno/amby.svg?branch=master)](https://travis-ci.org/jsermeno/amby)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](Haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](tl;dr Legal: BSD3)

<img src="https://cloud.githubusercontent.com/assets/197051/20642847/34a870ec-b3e8-11e6-90c9-ed7885be476d.png" alt="boxplot with empty bins" width="200" height="150">
<img src="https://cloud.githubusercontent.com/assets/197051/20501027/02c44a82-b006-11e6-9589-91dd9fe141c0.png" alt="normal distribution plot" width="200" height="150">
<img src="https://cloud.githubusercontent.com/assets/197051/20501025/0182f9de-b006-11e6-8a38-24b081b9892c.png" alt="clean theme equation plot" width="200" height="150">
<img src="https://cloud.githubusercontent.com/assets/197051/20501066/37c7f0ee-b006-11e6-91b8-b951cafa4100.png" alt="multiple beta distributions" width="200" height="150">

A statistics visualization library built on top of [Chart](https://github.com/timbod7/haskell-chart) inspired by [Seaborn](https://github.com/mwaskom/seaborn). Amby provides a high level interface to quickly display attractive visualizations. Amby also provides tools to display Charts from both Amby and the Chart package within GHCi.

## Plotting basics

The simplest plotting function is `plot'`. Here's how you might plot the standard normal distribution.

```haskell
λ> import Amby
λ> import qualified Statistics.Distribution.Normal as Stats

λ> let x = contDistrDomain Stats.standard 10000
λ> let y = contDistrRange Stats.standard x
λ> plot' x y
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501102/6000a114-b006-11e6-91d7-e4c5f4ffaf47.png" alt="normal distribution plot" width="400" height="300">

Notice the tick mark `'` after `plot'`. This indicates a function that accepts no optional arguments.

## Plotting univariate distributions

This tutorial mirrors the first section of [Seaborn](http://seaborn.pydata.org/tutorial/distributions.html)'s python tutorial.

Use `distplot` to view univariate distributions. By default this will create a histogram and fit a kernel density estimate.

```haskell
λ> z <- random Stats.standard 100
λ> distPlot' z
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501027/02c44a82-b006-11e6-9589-91dd9fe141c0.png" alt="distplot" width="400" height="300">

### Histograms

The `distPlot` histogram automatically chooses a reasonable number of bins and counts the data points in each bin. To view the position of each data point you can add a rugplot.

```haskell
λ> distPlot z $ kde .= False >> rug .= True
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501031/05564e1c-b006-11e6-9e9f-d85782268979.png" alt="histogram with rugplot" width="400" height="300">

Choosing a different number of bins for the histogram can reveal different patterns in the data.

```haskell
λ> distPlot z $ bins .= 20 >> kde .= False >> rug .= True
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501028/0409113e-b006-11e6-84b4-c97a6a993656.png" alt="histogram with more bins" width="400" height="300">

### Kernel density estimation

Kernel density estimation can be a useful too for plotting the shape of the distribution.

```haskell
λ> distplot z $ hist .= False >> rug .= True
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501100/5db3604a-b006-11e6-964b-4a118bacd9cb.png" alt="Kernel density estimation" width="400" height="300">

A kernel density estimation is a summation of several normal distributions, each centered on each of the data points.

```haskell
λ> import qualified Statistics.Sample as Stats
λ> import qualified Data.Vector.Unboxed as U
λ> let bandwidth = 1.059 * Stats.stdDev z * fromIntegral (U.length z) ** ((-1) / 5)
λ> let xs = linspace (-6) 6 200
λ> let a = U.take 30 z

λ> let foldFn _ b = plot xs (contDistrRange (Stats.normalDistr b bandwidth) xs)
λ> U.foldM foldFn () a >> rugPlot a (color .= K >> linewidth .= 3) >> xlim (-4, 4)
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501580/7c76344c-b008-11e6-8666-28b6e5b18cca.png" alt="Kernel density estimation explanation" width="400" height="300">

The resulting curve is normalized so the area under it is equal to 1. This is what is provided with the `kdePlot` function.

```haskell
λ> kdePlot z $ shade .= True
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501070/3aca17c2-b006-11e6-8b7e-3258d20e6c84.png" alt="Kernel density estimation" width="400" height="300">

The bandwith (`bw`) parameter of the KDE controls how tightly the estimation is fit to the data, much like the bin size in a histogram. The default behaviour tries to guess a good value, but it may be helpful to try larger or smaller values.

```haskell
λ> kdePlot' z >> kdePlot z (bw .= BwScalar 0.2) >> kdePlot z (bw .= BwScalar 2)
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501065/361df4aa-b006-11e6-9f6d-94fb6ebd2fef.png" alt="Kernel density estimation bandwidth" width="400" height="300">

You can also control how far past the range of your dataset the curve is drawn. However this only influences how the curve is drawn, not how it is fit.

```haskell
λ> kdePlot z (cut .= 0 >> shade .= True) >> rugPlot' z
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501066/37c7f0ee-b006-11e6-91b8-b951cafa4100.png" alt="Kernel density estimation cut" width="400" height="300">

## Plotting categorical data

In this section we'll see how to visualize the relationship between a numeric variable and one or more categorical variables.

### Plotting distributions of observations within categories

Boxplots can facilitate easy comparisons across category levels. This kind of plot shows the three quartile values of the distribution along with extreme values. The "whiskers" extend to points that lie within 1.5 IQRs (interquartile range) of the lower and upper quartile, and then observations that fall outside this range are displayed independently. Importantly, this means that each value in the boxplot corresponds to an actual observation in the data:

For convenience we'll use the `loadDataset` method from `Amby.Utils` to load datasets.

The simplest way to draw a boxplot is to use the `boxPlot` function.

```haskell
λ> ds <- loadDataset tips
λ> head ds
Tip
  { totalBill = 16.99
  , tip = 1.01
  , sex = "Female"
  , smoker = "No"
  , day = "Sun"
  , time = "Dinner"
  , tipSize = 2
  }
λ> (b, p, s, k, d, t, _) <- getTipColumns ds
````

Draw a single horizontal boxplot.

```haskell
λ> boxPlot' b
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642841/346fb11c-b3e8-11e6-9a02-92b192ce17b0.png" alt="single horizontal boxplot" width="400" height="300">

Draw a vertical boxplot grouped by a categorical variable.

```haskell
λ> boxPlot b $ fac .= d >> axis .= YAxis
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642842/34890522-b3e8-11e6-9765-0dce804a3773.png" alt="boxplot with one factor" width="400" height="300">

Draw a vertical boxplot with nested grouping by two categorical variables.

```haskell
λ> boxPlot b $ fac .= s >> hue .= d >> axis .= YAxis >> color .= G
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642843/3499815e-b3e8-11e6-8d34-b05983453bd2.png" alt="boxplot with two factors" width="400" height="300">

Draw a boxplot when some bins are empty.

```haskell
λ> theme springTheme >> boxPlot b (fac .= d >> hue .= t)
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642847/34a870ec-b3e8-11e6-90c9-ed7885be476d.png" alt="boxplot with empty bins" width="400" height="300">

Control box order.

```haskell
λ> boxPlot p $ fac .= changeOrder t ["Dinner", "Lunch"]
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642848/34abcd8c-b3e8-11e6-92e5-e39985ed05a4.png" alt="boxplot with manual order" width="400" height="300">

If you want to compare more than two categorical variables you can use `factorPlot`.

```haskell
λ> gridTheme cleanTheme >> factorPlot b (fac .= s >> hue .= d >> col .= k)
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642846/34a6210c-b3e8-11e6-82eb-91badd31c1d3.png" alt="boxplot with three factors" width="800" height="300">

We can add labels.

```haskell
λ> factorPlot b $ fac .= s >> hue .= d >> col .= k >> colLabel .= "smoker"
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642845/34a0c4c8-b3e8-11e6-8308-20aef22361f7.png" alt="labeled boxplot with three factors" width="800" height="300">

You can compare up to four categorical variables using `factorPlot`.

```haskell
λ> factorPlot b $ fac .= s >> hue .= d >> col .= k >> row .= t
```

<img src="https://cloud.githubusercontent.com/assets/197051/20642844/349dc46c-b3e8-11e6-97d8-028e39308020.png" alt="boxplot with four factors" width="800" height="600">

## Rendering

There are several ways to render plots.

First, Amby provides the helper functions `save` and `saveSvg` that will save a graph to the file `.__amby.png` and `.__amby.svg` respectively. `save` uses the Cairo backend, while `saveSvg` uses the Diagrams backend. The Diagrams backend produces better looking charts, but is slower.

```haskell
λ> save $ distPlot' z
λ> saveSvg $ distPlot' z
```

Second, you can use any rendering methods that the underlying [Chart](https://github.com/timbod7/haskell-chart) library provides by converting an `AmbyChart ()` or `AmbyGrid ()` to a `Renderable (LayoutPick Double Double Double)` with the `getRenderable` function.

```haskell
λ> import Graphics.Rendering.Chart.Easy (def)
λ> import Graphics.Rendering.Chart.Backend.Cairo as Cairo
λ> import Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
λ> Cairo.renderableToFile def "myFile.png" $ getRenderable $ distPlot' z
λ> Diagrams.renderableToFile def "myFile.svg" $ getRenderable $ distPlot' z
```

Third—if you have a terminal that supports images such as iTerm2—you can display charts directly inside the GHCi repl. Just install the [`imgcat`](https://github.com/eddieantonio/imgcat#Build) executable, and the [`pretty-display`](https://github.com/jsermeno/pretty-display) library. See [here](https://github.com/jsermeno/amby#dependencies) for further installation instructions.

```haskell
λ> distPlot' z
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501861/9933bc5c-b009-11e6-91b8-3c7ddbf72353.png" alt="terminal example" width="400" height="300">

## Plotting equations

You can also specify graphs using a domain and an equation.

```haskell
λ> plotEq' [0,0.001..4] sqrt
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501101/5fe7abfa-b006-11e6-989d-f7c18524e02f.png" alt="clean theme equation plot" width="400" height="300">

## Multiple container types

Plotting functions work on both lists and generic vectors of doubles.

```haskell
λ> plotEq' [0,0.001..4] sqrt
λ> plotEq' (linspace 0 4 4000) sqrt
```

## Combine graphs using do notation

```haskell
λ> import Statistics.Distribution.Beta as Stats
λ> :set +m
λ> let plotBeta a b =
λ|       let d = Stats.betaDistr a b
λ|           x = contDistrDomain d 10000
λ|           y = contDistrRange d x
λ|       in plot' x y
λ> do
λ|   theme cleanTheme
λ|   plotBeta 0.5 0.5
λ|   plotBeta 5 1
λ|   plotBeta 1 3
λ|   plotBeta 2 2
λ|   plotBeta 2 5
λ|   ylim (0.0, 2.5)
```

<img src="https://cloud.githubusercontent.com/assets/197051/20501025/0182f9de-b006-11e6-8a38-24b081b9892c.png" alt="multiple beta distributions" width="400" height="300">

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
