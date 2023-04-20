#!/bin/sh
set -e

# This script downloads the binaries for the most recent version of TabNine.

version="$(curl -sS https://update.tabnine.com/bundles/version)"
# targets='i686-pc-windows-gnu
#     i686-unknown-linux-musl
#     x86_64-apple-darwin
#     aarch64-apple-darwin
#     x86_64-pc-windows-gnu
#     x86_64-unknown-linux-musl'

targets='x86_64-unknown-linux-gnu'
folder=~/.TabNine

echo "$targets" | while read target
do
    path=$version/$target
    url=https://update.tabnine.com/bundles/$path/TabNine.zip
    mkdir -p $folder/$path
    echo "downloading $path"
    echo "$url"
    curl -sS $url > $folder/$path/TabNine.zip
    unzip -o $folder/$path/TabNine.zip -d $folder/$path
    rm $folder/$path/TabNine.zip
    chmod +x $folder/$path/*
done
