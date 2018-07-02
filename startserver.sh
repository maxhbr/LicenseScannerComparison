#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

have() { type "$1" &> /dev/null; }

have nix-shell && {
    have php || {
        echo "start via nix-shell"
        exec nix-shell -p php --command "$0 $@"
    }
}

cd "$(dirname $0)/_result"
php -S 127.0.0.1:8080
