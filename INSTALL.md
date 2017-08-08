# Install Guide

If you just want to play Polished Crystal, use one of the .ips patches as described in [README.md](README.md#download-and-play). To build a ROM yourself from the source code, follow these instructions.

> You know it's sitting back and looking at it that you realize that Gamefreak is actually really good devs.
> Like you have Pokemon and it's a simple formula "How can you fuck it up?" and then all of these Romhacks basically go "Like this."

– Anonymous, [/vp/](http://boards.4chan.org/vp/)


## Windows

To build on Windows, install [**Cygwin**](http://cygwin.com/install.html) with the default settings.

In the installer, select the following packages: `make`, `git`, `gettext`, `python`, `gcc-core`.

Then download [**rgbds 0.3.1**](https://github.com/bentley/rgbds/releases/). Other versions are not compatible. Extract the archive and put all the .exe and .dll files in C:\Cygwin\usr\local\bin.

In the **Cygwin terminal**:

```bash
git clone --recursive https://github.com/roukaour/polishedcrystal
cd polishedcrystal
```

To build **polishedcrystal.gbc**:

```bash
make
```

To build other versions:

```bash
make [faithful] [nortc] [monochrome] [debug]
```

For example, to build a mononchrome faithful version:

```bash
make faithful monochrome
```


## Linux

Python 2.7 is required.

```bash
sudo apt-get install make gcc bison git python

git clone https://github.com/rednex/rgbds
cd rgbds
git checkout v0.3.1
sudo make install
cd ..

git clone --recursive https://github.com/roukaour/polishedcrystal
cd polishedcrystal
```

To build **polishedcrystal.gbc**:

```bash
make
```

To build other versions:

```bash
make [faithful] [nortc] [monochrome] [debug]
```


## Mac OS X

In **Terminal**, run:

```bash
xcode-select --install

git clone https://github.com/rednex/rgbds
cd rgbds
git checkout v0.3.1
sudo make install
cd ..

git clone --recursive https://github.com/roukaour/polishedcrystal
cd polishedcrystal
```

To build **polishedcrystal.gbc**:

```bash
make
```

To build other versions:

```bash
make [faithful] [nortc] [monochrome] [debug]
```


## Notes

* If `gettext` no longer exists, get `libsasl2-3` and `ca-certificates`.
