#!/bin/bash
cargo fmt
cargo clippy -- -Wclippy::pedantic
