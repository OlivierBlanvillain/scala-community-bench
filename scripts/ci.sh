#!/bin/sh

sh scripts/dotty-jmh.sh

sh scripts/scalac-jmh.sh

python scripts/scalac-standalone.py
