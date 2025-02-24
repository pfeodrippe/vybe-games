(ns noel
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

(defn draw
  [{:keys [::vg/shader-shadowmap] :as w}
   delta-time]
  ;; For debugging
  (def w w)

  #_(vybe.blender/entities-ignore! w [:pilot_a :pilot_b :pilot_c :pilot_d])
  #_(vybe.blender/entity-sync! w)

  ;; Accept inputs (mouse + WASD) to move the camera.
  (vg/camera-move! w)

  ;; Progress the systems (using Flecs).
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)

  ;; Add some lights (from the blender model).
  #_(vr.c/rl-set-clip-planes -1000 1000)
  (vg/draw-lights w)
  #_(vr.c/rl-set-clip-planes 0.01 1000)
  ;; You can also reset it to the default shader (no lights!) or use any othe
  ;; shader you want.
  #_(vg/draw-lights w (get (::vg/shader-default w) vt/Shader))

  (vg/draw-lights w (get shader-shadowmap vt/Shader) vg/draw-scene {:scene :vg.gltf.scene/track_scene})
  (vg/with-target (w (vf/path [:my/model :vg.gltf/office :vg.gltf/tv :vg.gltf/screen]))
    (vr.c/clear-background (vr/Color [50 100 100 255]))
    (vg/with-camera (get (w (vf/path [:my/model :vg.gltf/track_camera])) vt/Camera)
      (vg/draw-scene w {:scene :vg.gltf.scene/track_scene})))

  ;; Trigger animations continuously.
  (vf/with-query w [_ [:or
                       :vg.gltf.anim/pilot_axisAction.001
                       :vg.gltf.anim/pilot_axisAction.002
                       :vg.gltf.anim/pilot_axisAction.003
                       :vg.gltf.anim/pilot_axisAction.004]
                    _ :vg/animation
                    e :vf/entity]
    (conj e :vg.anim/active))

  ;; Render stuff into the screen (using Raylib) using a built-in effect.
  (vg/with-drawing
    (vg/with-drawing-fx w (vg/fx-painting w {:dither-radius 0.2})
      (vr.c/clear-background (vr/Color [20 20 20 255]))

      ;; Here we do a query for the active camera (it's setup when loading the model).
      (vf/with-query w [_ :vg/camera-active
                        camera vt/Camera]
        (vg/with-camera camera
          (vg/draw-scene w {:scene :vg.gltf.scene/Scene})
          #_(vg/draw-debug w)))

      (vr.c/draw-fps 510 570))

    (vg/with-drawing-fx w (vg/fx-painting w {:dither-radius 0.1})
      (vr.c/draw-rectangle-pro (vr/Rectangle [320 325 220 120]) (vt/Vector2 [0 0]) 0
                               (vr/Color [90 20 90 200]))
      (vr.c/draw-rectangle-pro (vr/Rectangle [320 320 215 115]) (vt/Vector2 [0 0]) 0
                               (vr/Color [205 234 235 255]))
      (vr.c/gui-group-box (vr/Rectangle [330 330 200 100]) "Monster")
      (vr.c/gui-dummy-rec (vr/Rectangle [340 340 180 80]) "Que tu quer???????????\n???")) ))

#_ (init)

(def screen-width 600)
(def screen-height 600)

(defn init
  []
  (let [w (vf/make-world)
        model-path (vg/resource "noel.glb" {:throw-exception false
                                            :target-folder "resources"})]
    #_ (vg/debug-init! w)
    #_(vybe.blender/gltf-model-path! model-path)

    (vg/start! w screen-width screen-height #'draw
               (fn [w]
                 (vr.c/gui-load-style-sunny)
                 (-> w
                     (vg/model :my/model model-path))))))

#_(init)

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))

  ;; Start main thread.
  (vr/-main))
