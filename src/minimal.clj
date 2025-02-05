(ns minimal
  (:require
   [vybe.flecs :as vf]
   [vybe.game :as vg]
   [vybe.raylib.c :as vr.c]
   [vybe.raylib :as vr]
   [vybe.type :as vt]
   [vybe.panama :as vp])
  (:import
   (org.vybe.raylib raylib)))

#_ (init)

(def unit-z
  (vt/Vector3 [0 0 -1]))

(def unit-y
  (vt/Vector3 [0 1 0]))

(def unit-x
  (vt/Vector3 [1 0 0]))

(defn draw
  [w delta-time]
  ;; For debugging
  (def w w)

  ;; TODO Move it into game.
  (vf/with-query w [_ :vg/camera-active
                    translation [:mut vt/Translation]
                    rotation [:mut vt/Rotation]
                    transform vt/Transform]
    (let [key-down? #(vr.c/is-key-down %1)
          move-forward (delay
                         (fn [pos v]
                           (vr.c/vector-3-add pos
                                              (-> (vr.c/vector-3-transform unit-z transform)
                                                  (vr.c/vector-3-subtract translation)
                                                  (vr.c/vector-3-scale v)))))
          move-right (delay
                       (fn [pos v]
                         (vr.c/vector-3-add pos
                                            (-> (vr.c/vector-3-transform unit-z transform)
                                                (vr.c/vector-3-subtract translation)
                                                (vr.c/vector-3-scale v)
                                                (vr.c/vector-3-cross-product unit-y)))))
          new-translation (cond-> (vt/Translation [0 0 0])
                            (key-down? (raylib/KEY_W)) (@move-forward 0.1)
                            (key-down? (raylib/KEY_S)) (@move-forward -0.1)
                            (key-down? (raylib/KEY_D)) (@move-right 0.1)
                            (key-down? (raylib/KEY_A)) (@move-right -0.1))]
      (when (or (realized? move-forward)
                (realized? move-right))
        (merge translation (-> (vr.c/vector-3-normalize new-translation)
                               (vr.c/vector-3-scale (* delta-time 8.0))
                               (vr.c/vector-3-add translation))))

      (when (and (< 0 (vr.c/get-mouse-x) 600)
                 (< 0 (vr.c/get-mouse-y) 600))
        (merge rotation (-> rotation
                            (vr.c/quaternion-multiply
                             (vr.c/quaternion-from-axis-angle unit-y (* (:x (vr.c/get-mouse-delta))
                                                                        -0.7
                                                                        delta-time)))
                            vr.c/quaternion-normalize)))))

  ;; Progress the systems (using Flecs).
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)

  ;; Add some lights (from the blender model).
  (vg/draw-lights w)
  ;; You can also reset it to the default shader (no lights!) or use any othe
  ;; shader you want.
  #_(vg/draw-lights w (get (::vg/shader-default w) vt/Shader))

  ;; Render stuff into the screen (using Raylib) using a built-in effect.
  (vg/with-drawing-fx w (vg/fx-painting w {:dither-radius 0.2})
    (vr.c/clear-background (vr/Color [20 20 20 255]))

    ;; Here we do a query for the active camera (it's setup when loading the model).
    (vf/with-query w [_ :vg/camera-active
                      camera vt/Camera]
      (vg/with-camera camera
        (vg/draw-scene w)))

    #_(vr.c/draw-text (-> (w (vf/path [:my/model :vg.gltf/Scene :vg.gltf/Cube :vg.gltf/lamp]))
                          vybe.blender/entity-trs
                          :rotation
                          ((juxt :x :y :z :w))
                          str)
                      20 190 27 (vr/Color [228 128 228 255]))

    #_(vybe.blender/entity-sync!
       (w (vf/path [:my/model :vg.gltf/Scene :vg.gltf/Cube])))
    #_(vybe.blender/entity-sync!
       (w (vf/path [:my/model :vg.gltf/Scene :vg.gltf/Cube :vg.gltf/lamp])))

    (vr.c/draw-fps 510 570)))

#_ (init)

(defn init
  []
  (let [w (vf/make-world)]
    ;; If you want to enable debugging (debug messages + clerk + flecs explorer),
    ;; uncomment line below.
    #_(vg/debug-init! w)

    (vg/start! w 600 600 #'draw
               (fn [w]
                 (-> w
                     ;; Load model (as a resource).
                     ;; You should have `minimal.glb` (a GLTF file) available.
                     (vg/model :my/model (vg/resource "com/pfeodrippe/vybe/model/minimal.glb")))))))

#_(init)

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))

  ;; Start main thread.
  (vr/-main))
