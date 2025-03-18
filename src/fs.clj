(ns fs
  (:require [vybe.flecs]
            [vybe.panama])
  (:import [vybe.flecs VybeFlecsWorldMap]
           [vybe.panama VybePMap]))


(extend-protocol flow-storm.runtime.values/SnapshotP
  VybeFlecsWorldMap
  (snapshot-value [^VybeFlecsWorldMap wm]
    (persistent!
     (reduce-kv (fn [acc eid e]
                  (if (seq e)
                    (assoc! acc eid e )
                    acc))
                (transient {})
                wm))))

(extend-protocol flow-storm.runtime.values/SnapshotP
  VybePMap
  (snapshot-value [^VybePMap vpm]
    (into {} vpm)))
