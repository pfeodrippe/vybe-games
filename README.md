JDK 22+ is required.

A sample game for Vybe (https://github.com/pfeodrippe/vybe).

## Getting started

``` shell
# This will put the dynamic libs in the right place and start raylib in the main thread,
# open the REPL and call call the `init` function inside `leo.clj`.

# Linux (x64)
clj -M:linux -m vybe.native.loader && clj -M:linux -m vybe.raylib

# Mac (Universal)
clj -M:osx -m vybe.native.loader && clj -M:osx -m vybe.raylib

# Windows (x64)
clj -M:win -m vybe.native.loader && clj -M:win -m vybe.raylib
```

## Build/Package

``` shell
clj -T:build uber

# -- OSX
rm -rf target/classes && jpackage --java-options '-XstartOnFirstThread' --java-options '--enable-native-access=ALL-UNNAMED' -i target -n leo --main-class leo --main-jar leo-0.1.0-standalone.jar

# -- Linux or Windows
rm -rf target/classes && jpackage --java-options '--enable-native-access=ALL-UNNAMED' -i target -n leo --main-class leo --main-jar leo-0.1.0-standalone.jar
```
