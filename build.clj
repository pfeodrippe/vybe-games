(ns build
  (:require
   [clojure.tools.build.api :as b]))

(defn lib [n]
  (symbol "io.github.pfeodrippe" n))

(def version
  (format "0.1.%s%s"
          (b/git-count-revs nil)
          (if-let [suffix (System/getenv "VYBE_VERSION_SUFFIX")]
            (str "-" suffix)
            "")))

(def class-dir "target/classes")

(def basis
  (b/create-basis
   {:project "deps.edn"
    :aliases [:osx]}))

(defn uber-file
  [n]
  (format "target/%s-%s-standalone.jar" (name (lib n)) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :ns-compile '[leo]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file (uber-file "leo")
           :basis basis
           :main 'leo}))

;; clj -T:build uber
