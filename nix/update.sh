#! /usr/bin/env bash

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/reflex-frp/reflex-dom-contrib > reflex-dom-contrib.json
nix-prefetch-git https://github.com/3noch/reflex-dom-nested-routing > reflex-dom-nested-routing.json
