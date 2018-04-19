#!/usr/bin/env python

import sys
import os
import errno
import subprocess as subp

def mkdir(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise

def where(cmd):
    if os.path.isfile(cmd):
        return cmd
    else:
        paths = os.environ['PATH'].split(os.pathsep)
        for p in paths:
            f = os.path.join(p, cmd)
            if os.path.isfile(f):
                return f
        else:
            return None

def run(cmd):
    print(">>> " + str(cmd))
    return subp.check_output(cmd)

def compile():
    return run(['sbt', 'compile'])

sbt = where('sbt')
java = where('java')

java_opts = ['-Xms1024M', '-Xmx1024M']

scalaVersion = '2.11.12'
scalaBinaryVersion = '.'.join(scalaVersion.split('.')[:2])

classpath = [
        os.path.abspath('target/scala-{}/classes'.format(scalaBinaryVersion)),
        os.path.expanduser('~/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-{}.jar'.format(scalaVersion)),
]

benchmarks = [
        ('bounce.BounceBenchmark', '100', '1331')
]

runs = 10
iterations = 10000

if __name__ == "__main__":
    with open('build.sbt', 'w') as build:
        build.write('scalaVersion := "{}"'.format(scalaVersion))
    compile()
    for (bench, input, output) in benchmarks:
        for n in xrange(runs):
            mkdir('results/{}/'.format(bench))
            cmd = [java]
            cmd.extend(java_opts)
            cmd.extend(['-classpath', ':'.join(classpath)])
            cmd.extend([bench, str(iterations), input, output])
            result = run(cmd)
            with open('results/{}/{}'.format(bench, n), 'w+') as outfile:
                outfile.write(result)


