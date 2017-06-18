#!/bin/bash 

set -ex

sbt ++2.12.2 coreJVM/argsFile

git clone https://github.com/scala/compiler-benchmark.git
cd compiler-benchmark

mkdir -p ${HOME}/.sbt/0.13

cat << EOS > ${HOME}/.sbt/0.13/resolver.sbt
resolvers ++= (
  if (scalaVersion.value.contains("-bin"))
     List("scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/")
  else Nil
)
EOS

for i in 2.12.2 2.12.3-bin-d1ec01a
do
  sbt "set scalaVersion in compilation := \"$i\"" "hot -p source=@${TRAVIS_BUILD_DIR}/core/jvm/target/compile.args"
done
