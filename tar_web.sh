#!/usr/bin/env bash
find ./result/web/ \( -type f -o -type l -o -type d \) -printf "%P\n" | tar -czf vesmor_web.tar.gz --no-recursion -C ./result/web -T -
