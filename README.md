JDK 22+ is required.

Sample games for Vybe (https://github.com/pfeodrippe/vybe).

## Getting started

``` shell
# This will put the dynamic libs in the right place and start raylib in the main thread,
# open the REPL and call call the `init` function inside `leo.clj`.
# To start audio, install SuperCollider (https://supercollider.github.io/downloads.html) you may also need to add a configuration file if it was not added automatically https://doc.sccode.org/Reference/StartupFile.html and start the audio server (start SuperCollider app and run Boot Server)
# After this is done, in leo.clj evaluate (va/audio-enable!) and you can now play music (e.g. (va/sound (demo 0.2 (sin-osc 400))))

# Linux (x64)
clj -M:linux -m vybe.native.loader && clj -M:linux -m vybe.raylib

# Linux (x64) Basic, for example, if you don't have access to AVX2 instructions
clj -M:linux-basic -m vybe.native.loader && clj -M:linux-basic -m vybe.raylib

# Mac (Universal)
clj -M:osx -m vybe.native.loader && clj -M:osx -m vybe.raylib

# Windows (x64)
clj -M:win -m vybe.native.loader ; clj -M:win -m vybe.raylib
```

If you want to test it using the local vybe project, append `:dev` to the
aliases (e.g. for Mac, we would have `clj -M:osx:dev -m vybe.native.loader && clj -M:osx:dev -m vybe.raylib`).

## Build/Package

``` shell
clj -T:build uber

# -- OSX
rm -rf target/classes && jpackage --java-options '-XstartOnFirstThread' --java-options '--enable-native-access=ALL-UNNAMED' -i target -n leo --main-class leo --main-jar leo-0.1.0-standalone.jar

# -- Linux or Windows (using WSL)
rm -rf target/classes && jpackage --java-options '--enable-native-access=ALL-UNNAMED' -i target -n leo --main-class leo --main-jar leo-0.1.0-standalone.jar
```
