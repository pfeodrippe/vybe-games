(ns leo.main
  "Namespace that should be used by the published application only."
  (:gen-class)
  (:require
   [leo]))

(defn -main
  [& _args]
  (leo/-main []))
