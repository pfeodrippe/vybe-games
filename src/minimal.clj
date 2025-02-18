 (ns minimal
  (:require
   [vybe.flecs :as vf]
   [vybe.game :as vg]
   [vybe.raylib.c :as vr.c]
   [vybe.raylib :as vr]
   [vybe.type :as vt]))

(defn draw
  [w delta-time]
  ;; For debugging
  (def w w)

  ;; Progress the systems (using Flecs).
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)

  ;; Add some lights (from the blender model).
  (vg/draw-lights w)

  ;; Render stuff into the screen (using Raylib) using a built-in effect.
  (vg/with-drawing-fx w (vg/fx-painting w)
    (vr.c/clear-background (vr/Color [255 20 100 255]))

    ;; Here we do a query for the active camera (it's setup when loading the model).
    (vf/with-query w [_ :vg/camera-active
                      camera vt/Camera]
      (vg/with-camera camera
        (vg/draw-scene w)))

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
                     (vg/model :my/model (vg/resource "minimal.glb")))))))

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))

  ;; Start main thread.
  (vr/-main))
