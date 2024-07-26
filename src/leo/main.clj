(ns leo.main
  "Namespace that should be used by the published application only."
  (:gen-class))

(defn -main
  [& _args]
  ((requiring-resolve 'leo/-main)))
