mkdir -p target
find * -name '*.scala' | xargs | xargs scalac -d target -Yno-predef -Yno-imports
