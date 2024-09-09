(ns hooks.vybe
  (:require [clj-kondo.hooks-api :as api]))

(defn with-query [{:keys [node]}]
  (let [[w binding-vec & body] (rest (:children node))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   binding-vec
                   w
                   body))]
    {:node new-node}))

(defn defquery [{:keys [node]}]
  (let [[name w binding-vec & body] (rest (:children node))
        new-node (api/list-node
                  [(api/token-node 'let)
                   binding-vec
                   (api/list-node
                    (list*
                     (api/token-node 'let)
                     (api/vector-node [w (api/map-node {})
                                       (api/token-node '_) (api/list-node [(api/token-node 'def) name name])])
                     body))])]
    {:node new-node}))
