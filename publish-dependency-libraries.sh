set -e

git clone https://github.com/xuwei-k/kind-projector.git
cd kind-projector
git checkout 2.12.0-M5
sbt publishLocal
cd ..
git clone https://github.com/xuwei-k/scalacheck.git
cd scalacheck
git checkout 1.12.5-Scala-2.12.0-M5
sbt '++ 2.12.0-M5' js/publishLocal jvm/publishLocal
cd ..
