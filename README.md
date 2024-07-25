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
