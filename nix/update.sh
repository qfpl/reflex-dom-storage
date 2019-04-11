#! /usr/bin/env nix-shell
#! nix-shell -p nix-prefetch-git -i bash

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > $(dirname $0)/reflex-platform.json
