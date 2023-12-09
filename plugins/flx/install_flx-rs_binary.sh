#!/usr/bin/env bash

folder=~/Git/flx-rs/core
mkdir -p bin

(
    cd $folder
    cargo build --release
)

cp $folder/target/release/libflx_rs_core.so bin/flx-rs.x86_64-unknown-linux-gnu.so
