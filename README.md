# amby

A statistics visualization library built on top of [Charts](https://github.com/timbod7/haskell-chart). Amby provides a high level interface to quickly use visualization to explore data.

## Dependencies

To use amby you'll first need to install Charts and gtk2hs if you don't already have them.

### Mac OS X

Here are the instructions I used to install Charts and gtk2hs on OS X El Capitan with stack.

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
