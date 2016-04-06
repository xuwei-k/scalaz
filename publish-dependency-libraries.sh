git clone https://github.com/scala-js/scala-js.git &&
git clone https://github.com/non/kind-projector.git &&
git clone https://github.com/xuwei-k/scalacheck.git &&
cd scala-js &&
git checkout v0.6.8 &&
sbt '++ 2.12.0-M4' ";compiler/publishLocal;library/publishLocal;javalibEx/publishLocal;testInterface/publishLocal;stubs/publishLocal;jasmineTestFramework/publishLocal;jUnitRuntime/publishLocal;jUnitPlugin/publishLocal" && # https://groups.google.com/d/msg/scala-internals/NZ4pVWB4HWY/CWtJJCqbHAAJ
cd ../kind-projector &&
git checkout v0.7.1 &&
sbt '++ 2.12.0-M4' publishLocal &&
cd ../scalacheck &&
git checkout 1.12.5-update-scalajs &&
sbt '++ 2.12.0-M4' js/publishLocal jvm/publishLocal &&
cd ..
