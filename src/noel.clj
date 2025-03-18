(ns noel
  (:require
   [vybe.flecs :as vf]
   [vybe.game :as vg]
   [vybe.raylib.c :as vr.c]
   [vybe.raylib :as vr]
   [vybe.type :as vt]
   [vybe.panama :as vp]
   [vybe.c :as vc]
   [vybe.math :as vm]
   [vybe.jolt :as vj]
   [vybe.jolt.c :as vj.c]
   [fs]
   [flow-storm.runtime.debuggers-api :refer [set-recording]])
  (:import
   (org.vybe.raylib raylib)))

#_ (init)

#_(vf/defobserver on-contact-added _w
  [{:keys [contact-manifold]} [:event vj/OnContactAdded]
   ee [:src '?e :vg/camera-active]
   ttt [:src '?e [:out vt/Translation]]
   vel [:src '?e vt/Velocity]
   it :vf/world]
  #_(merge ttt (mapv identity ttt))
  #_(let [{:keys [x y z]} vel
        penetration #_(* (:penetration_depth contact-manifold) 1)
          (* (vr.c/get-frame-time) 4)]
    (-> ttt
        (update :x - (* x penetration))
        #_(update :y * -1 y penetration)
        (update :z - (* z penetration))))

  #_(let [[x y z] (:normal contact-manifold)
          penetration (* (:penetration_depth contact-manifold) (vr.c/get-frame-time) 100)]
    (-> ttt
        (update :x - (* x penetration))
        #_(update :y * -1 y penetration)
        (update :z - (* z penetration))))

  (println :AAAA
           (vr.c/get-frame-time)
           ttt
           vel
           (:penetration_depth contact-manifold)
           (:normal contact-manifold))
    #_(va/sound (synth/ks1 (+ (rand-int 20) 50))))

(def screen-width 600)
(def screen-height 600)

(def screen-width-half (/ screen-width 2.0))
(def screen-height-half (/ screen-height 2.0))

(vf/defobserver on-contact-added _w
  [{:keys [body-1 body-2 contact-manifold]} [:event vj/OnContactAdded]
   ee [:src '?e :vg/camera-active]
   #_ #_ttt [:src '?e [:out vt/Translation]]
   camera [:src '?e vt/Camera]
   vel [:out [:src '?e vt/Velocity]]
   phys [:src (vg/root) vj/PhysicsSystem]
   w :vf/world]
  (let [collider (w (vf/path [:my/model #_:vg.gltf/Cube :vg.gltf/player__collider]))
        [body n] (cond
                   (= (vg/body->entity w body-1) collider)
                   [body-1 :a]

                   (= (vg/body->entity w body-2) collider)
                   [body-2 :b])]

    (when body
      (let [vel (vr.c/vector-3-multiply (vt/Velocity (:normal contact-manifold)) vel)]
        (merge (get (w (vf/path [:my/model :vg.gltf/player__collider])) vt/Velocity)
               {:x (* (if (neg? (nth (:normal contact-manifold) 0))
                        (:x vel)
                        (- (:x vel)))
                      5)
                :z (* (if (neg? (nth (:normal contact-manifold) 2))
                        (:z vel)
                        (- (:z vel)))
                      5)})))))

(defn raycasted-entity
  [w]
  (let [{:keys [position direction]} (-> (vt/Vector2 [screen-width-half screen-height-half])
                                         (vr.c/vy-get-screen-to-world-ray (-> (w (vf/path [:my/model :vg.gltf/Camera]))
                                                                              (get vt/Camera))))
        pos (vr.c/vector-3-add position (-> (vt/Vector3 direction)
                                            (vr.c/vector-3-scale 0.2)))
        direction (mapv #(* % 10000) (vals direction))
        phys (get (w (vg/root)) vj/PhysicsSystem)
        body (vj/cast-ray phys pos direction)]
    (vg/body->entity w body)))

(defn- draw-cursor
  ([]
   (draw-cursor {}))
  ([{:keys [color-fg radius-inner radius-outer size]
     :or {color-fg (vr/Color [210 190 200 255])
          radius-inner 3
          radius-outer 8
          size 0.8}}]
   (let [radius-inner (* radius-inner size)
         radius-outer (* radius-outer size)]
     (doseq [i (range 2)]
       (vr.c/draw-circle-lines 301 301 (+ radius-outer (* i 3 size)) (vr/Color [20 20 20 255]))
       (vr.c/draw-circle-lines 300 300 (+ radius-outer (* i 3 size)) (vr/Color color-fg)))

     (vr.c/draw-circle 301 301 radius-inner (vr/Color [20 20 20 255]))
     (vr.c/draw-circle 300 300 radius-inner color-fg))))

(defn- draw-text
  ([text x y]
   (draw-text text x y {}))
  ([text x y {:keys [size]
              :or {size 20}}]
   (vr.c/draw-text text (+ x 3) (+ y 3) size (vr/Color [20 20 20 255]))
   (vr.c/draw-text text x y size (vr/Color [210 220 120 255]))))

(defn draw
  [w delta-time]
  ;; For debugging
  (def w w)

  (on-contact-added w)

  #_(do (vybe.blender/entities-ignore! w [:pilot_a :pilot_b :pilot_c :pilot_d :Cube #_:player__collider])
        (vybe.blender/entity-sync! w))

  ;; Trigger animations continuously.
  (vf/with-query w [_ [:or
                       :vg.gltf.anim/pilot_axisAction.001
                       :vg.gltf.anim/pilot_axisAction.002
                       :vg.gltf.anim/pilot_axisAction.003
                       :vg.gltf.anim/pilot_axisAction.004]
                    _ :vg/animation
                    e :vf/entity]
    (conj e [(vt/Scalar 0.5) :vg.anim/speed])
    (conj e :vg.anim/active))

  ;; Physics.
  (conj (w (vf/path [:my/model :vg.gltf/Cube])) :vg/dynamic)
  (conj (w (vf/path [:my/model #_:vg.gltf/Camera :vg.gltf/player__collider])) :vg/collide-with-static :lala)
  (conj (w (vf/path [:my/model :vg.gltf/office :vg.gltf/tv])) :vg/kinematic)

  ;; Accept inputs (mouse + WASD) to move the camera (or any other character via
  ;; a tag).
  (vg/camera-move! w {:sensitivity 0.5
                      :rot-sensitivity 1.5
                      :entity-tag #_:vg/camera-active :lala})

  ;; Replicate rotation + translation from collider to camera.
  (merge (get (w (vf/path [:my/model :vg.gltf/Camera])) vt/Translation)
         (get (w (vf/path [:my/model :vg.gltf/player__collider])) vt/Translation))
  (merge (get (w (vf/path [:my/model :vg.gltf/Camera])) vt/Rotation)
         (get (w (vf/path [:my/model :vg.gltf/player__collider])) vt/Rotation))

  ;; Switch cameras.
  (when (vg/key-pressed? :space)
    (vf/with-query w [_ :vg/camera-active
                      e :vf/entity]
      (if (= e (w (vf/path [:my/model :vg.gltf/Camera])))
        (assoc w (vf/path [:my/model :vg.gltf/Camera.001]) [:vg/camera-active])
        (assoc w (vf/path [:my/model :vg.gltf/Camera]) [:vg/camera-active]))))

  (when (vg/key-pressed? :r) (set-recording true))
  (when (vg/key-pressed? :g) (set-recording false))

  (vg/default-systems w)
  ;; Progress the systems (using Flecs).
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)

  ;; Add some lights (from the blender model).
  #_(vr.c/rl-set-clip-planes -1000 1000)
  #_(vr.c/rl-set-clip-planes 0.01 1000)

  ;; Message.
  (vg/with-fx-default w {:flip-y true}
    (vr.c/clear-background (vr/Color [20 20 20 0]))
    (vr.c/draw-rectangle-pro (vr/Rectangle [320 325 225 120]) (vt/Vector2 [0 0]) 0
                             (vr/Color [20 30 40 255]))
    (vr.c/draw-rectangle-pro (vr/Rectangle [320 320 215 115]) (vt/Vector2 [0 0]) 0
                             (vr/Color [255 234 235 255]))
    (vr.c/gui-group-box (vr/Rectangle [330 330 200 100]) "MONSTER")
    (vr.c/gui-dummy-rec (vr/Rectangle [340 340 180 80]) "Que tu quer???????????\n??? Por quê?"))

  #_ (init)

  (let [raycasted (= (vf/get-name (raycasted-entity w))
                     (vf/path [:my/model :vg.gltf/office :vg.gltf/tv]))
        switch? (and raycasted (vr.c/is-mouse-button-released (raylib/MOUSE_BUTTON_LEFT)))
        tv (w (vf/path [:my/model :vg.gltf/office :vg.gltf/tv]))
        _ (when switch?
            (debugger)

            (if (::turned-off tv)
              (disj tv ::turned-off)
              (conj tv ::turned-off)))
        turned-off (get tv ::turned-off)]
    ;; Track
    (vg/draw-lights w {:scene :vg.gltf.scene/track_scene})
    (vg/with-target (w (vf/path [:my/model :vg.gltf/office :vg.gltf/tv :vg.gltf/screen]))
      (if turned-off
        (vr.c/clear-background (vr/Color [10 10 10 255]))
        (do
          (vr.c/clear-background (vr/Color [100 100 200 255]))
          (vg/with-camera (get (w (vf/path [:my/model :vg.gltf/track_camera])) vt/Camera)
            (vg/draw-scene w {:scene :vg.gltf.scene/track_scene})
            (vg/draw-billboard (w (vf/path [:my/model :vg.gltf/track_camera]))
                               (get-in (::vg/render-texture w) [vr/RenderTexture2D :texture])
                               (-> (vp/clone (get (w (vf/path [:my/model :vg.gltf/pilot_d])) vt/Translation))
                                   (update :y + 2.2)
                                   (update :x + 0)
                                   (update :z + 0))
                               {:scale 8})))))


    ;; General.
    (vg/draw-lights w {:scene :vg.gltf.scene/Scene})
    #_ (vg/draw-lights w (get (::vg/shader-default w) vt/Shader))

    (vg/with-drawing

      (vg/with-drawing-fx w (vg/fx-painting w {:dither-radius 0.2 #_dither-radius})
        (vr.c/clear-background (vr/Color [15 15 17 255]))

        (vf/with-query w [_ :vg/camera-active
                          camera vt/Camera]
          (vg/with-camera camera
            (vg/draw-scene w (merge {:scene :vg.gltf.scene/Scene}
                                    #_{:colliders true}))

            #_(vr.c/draw-grid 10 0.5)

            #_(vg/draw-debug w {:scene :vg.gltf.scene/Scene})))

        (when raycasted
          #_(vr.c/draw-rectangle-pro (vr/Rectangle [300 314 170 40]) (vt/Vector2 [0 0]) 0 (vr/Color [150 230 190 255]))
          #_(vr.c/draw-rectangle-pro (vr/Rectangle [300 310 170 39]) (vt/Vector2 [0 0]) 0 (vr/Color [20 30 40 255]))
          (draw-text (if turned-off "Turn On" "Turn Off")
                     288 310 {:size 45}))

        #_(draw-cursor #_{#_ #_:radius-inner (+ (Math/sin (* (vr.c/get-time) 2))
                                                8)
                          :size (+ (* (Math/sin (* (vr.c/get-time) 8))
                                      0.1)
                                   0.5)}
                       {:size 0.8}))

      (draw-cursor #_{#_ #_:radius-inner (+ (Math/sin (* (vr.c/get-time) 2))
                                            8)
                      :size (+ (* (Math/sin (* (vr.c/get-time) 8))
                                  0.1)
                               0.5)}
                   (merge {:size 0.8}
                          (when raycasted
                            {:radius-inner (if raycasted 8 3)
                             :color-fg (if raycasted
                                         (vr/Color [210 220 120 255])
                                         (vr/Color [210 190 200 255]))})))

      (vr.c/draw-fps 510 570))))

#_ (init)

(defn init
  []
  (let [w (vf/make-world)
        model-path (vg/resource "noel.glb" {:throw-exception false
                                            :target-folder "resources"})]
    #_ (vg/debug-init! w)
    #_(vybe.blender/gltf-model-path! model-path)

    (vg/start! w screen-width screen-height #'draw
               (fn [w]
                 (vr.c/hide-cursor)
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
