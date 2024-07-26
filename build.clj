(ns build
  (:require
   [clojure.string :as str]
   [clojure.tools.build.api :as b]))

(def os
  (let [os? #(str/includes? (str/lower-case (System/getProperty "os.name")) %)]
    (cond
      (os? "win") :win
      (os? "mac") :osx
      :else :linux)))

(println :OS_while_building os)

(def basis
  (b/create-basis
   {:project "deps.edn"
    :aliases [os]}))

(defn clj
  [main-args]
  (let [cmd (b/java-command {:basis basis
                             :main 'clojure.main
                             :main-args (mapv str main-args)})]
    (b/process cmd)))

(defn lib [n]
  (symbol "io.github.pfeodrippe" n))

(def version
  (format "0.1.0"))

(def class-dir "target/classes")

(defn uber-file
  [n]
  (format "target/%s-%s-standalone.jar" (name (lib n)) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  #_(clj ["-m" 'vybe.native.loader])
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :ns-compile '[leo.main]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file (uber-file "leo")
           :basis basis
           :main 'leo.main}))
