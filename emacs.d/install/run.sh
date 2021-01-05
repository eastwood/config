#!/usr/bin/env bash

export CFLAGS="-O2 -I/usr/local/opt/libgccjit/include"
export LDFLAGS="-L/usr/local/opt/libgccjit/lib/gcc/10"
export LD_LIBRARY_PATH="/usr/local/opt/libgccjit/lib/gcc/10"

brew install \
 pkg-config \
 autoconf \
 gnutls \
 texinfo \
 librsvg \
 jansson \
 libgccjit

brew --prefix libgccjit
git clone https://github.com/emacs-mirror/emacs.git --depth 100 --branch "master"

cd emacs 

./autogen.sh
./configure \
  --with-ns \
  --enable-ns-self-contained \
  --with-xwidgets \
  --without-all \
  --without-pop \
  --with-xml2 \
  --with-json \
  --with-rsvg \
  --with-jpeg \
  --with-png \
  --with-gif \
  --with-tiff \
  --with-gnutls \
  --with-threads \
  --with-modules \
  --with-zlib
  # --with-nativecomp

