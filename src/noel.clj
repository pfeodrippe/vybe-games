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
   [vybe.jolt.c :as vj.c])
  (:import
   (org.vybe.raylib raylib)))

(vg/try-requiring-flow-storm!)

#_ (init)

(vf/defobserver on-contact-added _w
  [{:keys [body-1 body-2 contact-manifold]} [:event vj/OnContactAdded]
   _ [:src '?e :vg/camera-active]
   vel [:out [:src '?e vt/Velocity]]
   w :vf/world]
  (let [collider (w (vf/path [:my/model :vg.gltf/player__collider]))
        body (cond
               (= (vg/body->entity w body-1) collider)
               body-1

               (= (vg/body->entity w body-2) collider)
               body-2)]

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
  (let [{:keys [position direction]} (-> (vt/Vector2 [(/ (vr.c/get-screen-width) 2.0)
                                                      (/ (vr.c/get-screen-height) 2.0)])
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
         radius-outer (* radius-outer size)
         width (/ (vr.c/get-screen-width) 2.0)
         height (/ (vr.c/get-screen-height) 2.0)]
     (doseq [i (range 2)]
       (vr.c/draw-circle-lines (+ width 1) (+ height 1) (+ radius-outer (* i 3 size)) (vr/Color [20 20 20 255]))
       (vr.c/draw-circle-lines width height (+ radius-outer (* i 3 size)) (vr/Color color-fg)))

     (vr.c/draw-circle (+ width 1) (+ height 1) radius-inner (vr/Color [20 20 20 255]))
     (vr.c/draw-circle  width height radius-inner color-fg))))

(defn- draw-text
  ([text x y]
   (draw-text text x y {}))
  ([text x y {:keys [size]
              :or {size 20}}]
   (vr.c/draw-text text (+ x 3) (+ y 3) size (vr/Color [20 20 20 255]))
   (vr.c/draw-text text x y size (vr/Color [210 220 120 255]))))

(defn- scene->entity-path
  [w camera scene]
  (let [rt (vg/->rt w ::vg/rt-1-by-1)]
    (vg/with-fx w {:rt rt}
      (vg/with-camera camera
        (vg/draw-scene w {:scene scene
                          :use-color-ids true})))

    (->> (-> (vr.c/rl-read-texture-pixels (:id (:texture rt))
                                          1 1 (raylib/RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8))
             (vp/arr (* 1 1) vr/Color)
             first)
         (vg/color-identifier->entity w)
         vf/get-path)))

#_(init)

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
    (conj e [(vt/Scalar 0.4) :vg.anim/speed])
    (conj e :vg.anim/active))

  ;; Physics.
  (conj (w (vf/path [:my/model :vg.gltf/player__collider])) :vg/collide-with-static ::player-collider)
  (conj (w (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001])) :vg/kinematic)

  ;; Accept inputs (mouse + WASD) to move the camera (or any other character via
  ;; a tag).
  (vg/camera-move! w {:sensitivity 0.5
                      :rot-sensitivity 1.5
                      :entity-tag #_:vg/camera-active ::player-collider
                      :rot-pitch-limit (/ Math/PI 3)})

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
        (do (assoc w (vf/path [:my/model :vg.gltf/Camera.001]) [:vg/camera-active])
            (vf/disable (w (vf/path [:my/model :vg.gltf/office :vg.gltf/ceil]))))
        (do (assoc w (vf/path [:my/model :vg.gltf/Camera]) [:vg/camera-active])
            (vf/enable (w (vf/path [:my/model :vg.gltf/office :vg.gltf/ceil])))))))

  (vg/default-systems w)
  ;; Progress the systems (using Flecs).
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)

  ;; Add some lights (from the blender model).
  #_(vr.c/rl-set-clip-planes -1000 1000)
  #_(vr.c/rl-set-clip-planes 0.01 1000)

  #_ (init)

  (let [tv-path (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001])
        raycasted (= (vf/get-name (raycasted-entity w)) tv-path)
        switch? (and raycasted (vr.c/is-mouse-button-released (raylib/MOUSE_BUTTON_LEFT)))
        tv (w tv-path)
        _ (when switch?
            (if (::turned-on tv)
              (disj tv ::turned-on)
              (conj tv ::turned-on)))
        turned-on (get tv ::turned-on)]

    ;; Message.
    (vg/with-fx w {:rt ::screen-rt
                   :flip-y true}
      (let [[x y] [330 330]]
        (vr.c/clear-background (vr/Color [20 20 20 0]))
        (vr.c/draw-rectangle-pro (vr/Rectangle [(- x 10) (- y 5) 215 115]) (vt/Vector2 [0 0]) 0
                                 (vr/Color [20 30 40 255]))
        (vr.c/draw-rectangle-pro (vr/Rectangle [(- x 10) (- y 10) 215 115]) (vt/Vector2 [0 0]) 0
                                 (vr/Color [255 234 235 255]))
        (vr.c/gui-group-box (vr/Rectangle [x y 200 100])
                            (condp #(< (mod %2 6) %1) (vr.c/get-time)
                              1 "MONSTER +++"
                              3 "MONSTER ++"
                              5 "MONSTER +"
                              "MONSTER"))
        (vr.c/gui-dummy-rec (vr/Rectangle [(+ x 10) (+ y 10) 180 80])
                            (condp #(< (mod %2 5) %1) (vr.c/get-time)
                              1 "Que tu quer???????????\n??? Por quê?"
                              "Que tu quer???????????"))))

    ;; Message being identified by a color id so we can use it when shading.
    (vg/with-fx w {:rt (vg/rt-get ::message-id 600 600)
                   :shaders [[::vg/shader-solid {:u_color (vg/color-identifier w ::message-color)}]]}
      (vg/draw-rt w ::screen-rt))

    ;; Track
    (vg/draw-lights w {:scene :vg.gltf.scene/track_scene})
    ;; Draw to RT.
    (vg/with-fx w {:rt ::screen-rt
                   :target (w (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001 :vg.gltf/screen]))
                   :shaders [[::vg/shader-edge-2d {:edge_fill 1.0
                                                   :vg.shader.bypass/entities
                                                   (when turned-on
                                                     [(w (vf/path [:my/model :vg.gltf/track_path.001]))
                                                      (vg/color-identifier w ::message-color)])}]]}
      (if turned-on
        (do (vr.c/clear-background (vr/Color [100 100 200 255]))
            (vg/with-camera (get (w (vf/path [:my/model :vg.gltf/track_camera])) vt/Camera)
              (vg/draw-scene w {:scene :vg.gltf.scene/track_scene})
              (vg/draw-billboard (w (vf/path [:my/model :vg.gltf/track_camera]))
                                 (:texture (if (:use-color-ids vg/*context*)
                                             (vg/rt-get ::message-id 600 600)
                                             (vg/->rt w ::screen-rt)))
                                 (-> (vp/clone (get (w (vf/path [:my/model :vg.gltf/pilot_d])) vt/Translation))
                                     (update :y + 2.2)
                                     (update :x + 0)
                                     (update :z + 0))
                                 {:scale 8})))
        (vr.c/clear-background (vr/Color [10 10 10 255]))))

    ;; General.
    #_(vg/draw-lights w {:scene :vg.gltf.scene/Scene})
    (vg/draw-lights w {:scene :vg.gltf.scene/Scene :shader ::vg/shader-default})

    (vg/with-drawing

      (let [fx (vg/fx-painting w {:dither-radius 0.9})]

        (vr.c/clear-background (vr/Color [15 15 17 255]))
        (vf/with-query w [_ :vg/camera-active
                          camera vt/Camera]

          ;; Copy cursor's entity path to the clipboard.
          (when (vg/key-pressed? :z)
            (when-let [path (scene->entity-path w camera :vg.gltf.scene/Scene)]
              (println (format "Copied path %s to clipboard" path))
              (vr.c/set-clipboard-text (pr-str path))))

          ;; Edge shader (no tv screen).
          (vg/with-fx w {:drawing true
                         :shaders (-> [[::vg/shader-edge-2d {:edge_fill 1.0
                                                             :vg.shader.bypass/entities
                                                             [(w (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001 :vg.gltf/screen]))
                                                              (w (vf/path [:my/model :vg.gltf/office :vg.gltf/floor :vg.gltf/_nomad_unskew
                                                                           :vg.gltf/Boîte.003 :vg.gltf/_nomad_unskew.001 :vg.gltf/button_red]))
                                                              (w (vf/path [:my/model :vg.gltf/office :vg.gltf/floor :vg.gltf/_nomad_unskew
                                                                           :vg.gltf/Boîte.003 :vg.gltf/_nomad_unskew.002 :vg.gltf/button_green]))]}]]
                                      (concat fx))}
            (vg/with-camera camera
              (vg/draw-scene w (merge {:scene :vg.gltf.scene/Scene}
                                      #_{:colliders true}))

              #_(vr.c/draw-grid 10 0.5)
              #_(vg/draw-debug w {:scene :vg.gltf.scene/Scene})))))

      ;; Text
      (vg/with-fx w {:drawing true
                     :shaders [[::vg/shader-dither {:u_radius 1.0
                                                    :u_offsets (vt/Vector3 (mapv #(* % (+ 0.1
                                                                                          (vg/wobble 0.2))
                                                                                     0.5)
                                                                                 [0.02 (+ 0.016 (vg/wobble 0.01))
                                                                                  (+ 0.040 (vg/wobble 0.01))]))}]

                               [::vg/shader-noise-blur {:u_radius (+ 1.0 (rand 1))}]]}
        (when raycasted
          (draw-text (if turned-on "Turn Off" "Turn On")
                     (- (/ (vr.c/get-screen-width) 2.0) 12)
                     (+ (/ (vr.c/get-screen-height) 2.0) 10)
                     {:size 45})))

      (draw-cursor (merge {:size 0.8}
                          (when raycasted
                            {:radius-inner (if raycasted 8 3)
                             :color-fg (if raycasted
                                         (vr/Color [210 220 120 255])
                                         (vr/Color [210 190 200 255]))})))

      #_(vg/draw-rt (vg/rt-get ::ddd 600 600))

      (vr.c/draw-fps (- (vr.c/get-screen-width) 90)
                     (- (vr.c/get-screen-height) 30)))))

#_ (init)

(def game-config
  ;; iPad config.
  #_{:screen-size [1080 700]
     :window-position [0 0]}

  ;; Full-screen config.
  #_{:full-screen true
     :window-position [0 0]}

  ;; 600x600 config.
  {:screen-size [600 600]
   :window-position [1120 200]})

(defn init
  []
  (let [w (vf/make-world)
        model-path (vg/resource "noel.glb" {:throw-exception false
                                            :target-folder "resources"})]
    #_ (vg/debug-init! w)
    #_(vybe.blender/gltf-model-path! model-path)

    (vg/start! w (-> {:draw-var #'draw
                      :init-fn (fn [w]
                                 (vr.c/hide-cursor)
                                 (vr.c/gui-load-style-sunny)
                                 (-> w
                                     (vg/model :my/model model-path)
                                     (vg/render-texture ::screen-rt 600 600)
                                     (vg/render-texture ::render-texture
                                                        (vr.c/get-screen-width)
                                                        (vr.c/get-screen-height))))}
                     (merge game-config)))))

#_ (init)

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))

  ;; Start main thread.
  (vr/-main))
