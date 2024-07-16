## Getting started

Go to <https://github.com/pfeodrippe/vybe-games> and start the REPL for this project using

``` shell
# This will put the dynamic libs in the right place and start raylib in the main thread

# LINUX
clj -M:dev -m vybe.native.loader && clj -M:dev -m vybe.raylib

# MAC (OSX)
clj -M:dev -m vybe.native.loader && clj -M:osx -m vybe.raylib
```
