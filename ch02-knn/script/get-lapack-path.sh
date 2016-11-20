#!/bin/bash
cygpath -w $(dirname $(find /mingw64 -name 'liblapack*' | head -1))
