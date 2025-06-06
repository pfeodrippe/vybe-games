(ns minimal
  "Example with minimal setup, it will load a builtin GLTF (.glb) model with
  which contains a cube."
  (:require
   [vybe.flecs :as vf]
   [vybe.game :as vg]
   [vybe.raylib.c :as vr.c]
   [vybe.raylib :as vr]
   [vybe.type :as vt]))

(defn draw
  [w delta-time]
  ;; For debugging
  #_(def w w)

  ;; --8<-- [start:flecs_physics]
  ;; Progress the systems (using Flecs).
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)
  ;; --8<-- [end:flecs_physics]

  ;; --8<-- [start:rendering]
  ;; Add some lights (from the blender model).
  (vg/draw-lights w)

  ;; Render stuff into the screen (using Raylib) using a built-in effect.
  (vg/with-drawing
    (vg/with-fx w {:drawing true
                   :shaders (vg/fx-painting w)}
      (vr.c/clear-background (vr/Color [255 20 100 255]))

      ;; Here we do a query for the active camera (it's setup when loading the model).
      (vf/with-query w [_ :vg/camera-active
                        camera vt/Camera]
        (vg/with-camera camera
          (vg/draw-scene w)))

      (vr.c/draw-fps 510 570))))
;; --8<-- [end:rendering]

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
                     ;; We are going to load a bultin model, but you can use any .glb
                     ;; resource you have.
                     (vg/model :my/model (vg/resource "com/pfeodrippe/vybe/model/minimal.glb")))))))

#_ (init)

(defn -main
  "This is used for testing, don't bother."
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))

  ;; Exit app after some time (for testing).
  #_(future
      (try
        (Thread/sleep 5000)
        (System/exit 0)
        (catch Exception e
          (println e))))

  ;; Start main thread.
  (vr/-main))
