#!/bin/sh

set -eux

cat <<EOF > build.sbt
enablePlugins(JmhPlugin)

scalaVersion := "2.11.12"

scalacOptions ++= Seq("-Yno-predef", "-Yno-imports")

excludeFilter in unmanagedSources := "jmh-mokes.scala"
EOF

mkdir -p project
cat <<EOF > project/plugins.sbt
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.3")
EOF

sbt "jmh:run -f1 -i1 -wi 1"

rm build.sbt
rm project/plugins.sbt
