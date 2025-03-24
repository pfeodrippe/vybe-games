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

#_(def screen-width 600)
#_(def screen-height 600)

#_(def screen-width-half (/ screen-width 2.0))
#_(def screen-height-half (/ screen-height 2.0))

(vf/defobserver on-contact-added _w
  [{:keys [body-1 body-2 contact-manifold]} [:event vj/OnContactAdded]
   ee [:src '?e :vg/camera-active]
   #_ #_ttt [:src '?e [:out vt/Translation]]
   camera [:src '?e vt/Camera]
   vel [:out [:src '?e vt/Velocity]]
   phys [:src (vg/root) vj/PhysicsSystem]
   w :vf/world]
  (let [collider (w (vf/path [:my/model :vg.gltf/player__collider]))
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
  (let [rt (get (::vg/rt-1-by-1 w) vr/RenderTexture2D)]
    (vg/with-render-texture rt
      (vg/with-camera camera
        (vg/draw-scene w {:scene scene
                          :use-color-ids true})))

    (->> (-> (vr.c/rl-read-texture-pixels (:id (:texture rt))
                                          1 1 (raylib/RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8))
             (vp/arr (* 1 1) vr/Color)
             first)
         (vg/color-identifier->entity w)
         vf/get-path)))

(defn- shader-bypass-entities
  [w {:keys [shader entities draw]}]
  ;; Below RT and shader activation/enabling can be moved to `vybe.game` and
  ;; supported from `with-fx`.
  (vg/with-render-texture (get (::vg/render-texture w) vr/RenderTexture2D)
    (draw {}))

  (vr.c/rl-active-texture-slot 15)
  (vr.c/rl-enable-texture (:id (:texture (get (::vg/render-texture w) vr/RenderTexture2D))))
  (vr.c/rl-enable-shader (:id shader))
  (vg/set-uniform shader
                  {:u_color_ids_tex 15
                   :u_color_ids_bypass_count (count entities)
                   :u_color_ids_bypass (->> entities
                                            (mapv #(or (get % [vr/Color :color-identifier])
                                                       %)))}))

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
  #_(conj (w (vf/path [:my/model :vg.gltf/Cube])) :vg/dynamic)
  (conj (w (vf/path [:my/model #_:vg.gltf/Camera :vg.gltf/player__collider])) :vg/collide-with-static :lala)
  (conj (w (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001])) :vg/kinematic)

  ;; Accept inputs (mouse + WASD) to move the camera (or any other character via
  ;; a tag).
  (vg/camera-move! w {:sensitivity 0.5
                      :rot-sensitivity 1.5
                      :entity-tag #_:vg/camera-active :lala
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
        (assoc w (vf/path [:my/model :vg.gltf/Camera.001]) [:vg/camera-active])
        (assoc w (vf/path [:my/model :vg.gltf/Camera]) [:vg/camera-active]))))

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
    (vg/with-fx-default w {:flip-y true
                           :rt (get (::screen-rt w) vr/RenderTexture2D)}
      (vr.c/clear-background (vr/Color [20 20 20 0]))
      (vr.c/draw-rectangle-pro (vr/Rectangle [320 325 225 120]) (vt/Vector2 [0 0]) 0
                               (vr/Color [20 30 40 255]))
      (vr.c/draw-rectangle-pro (vr/Rectangle [320 320 215 115]) (vt/Vector2 [0 0]) 0
                               (vr/Color [255 234 235 255]))
      (vr.c/gui-group-box (vr/Rectangle [330 330 200 100]) (condp #(< (mod %2 6) %1) (vr.c/get-time)
                                                             1 "MONSTER +++"
                                                             3 "MONSTER ++"
                                                             5 "MONSTER +"
                                                             "MONSTER"))
      (vr.c/gui-dummy-rec (vr/Rectangle [340 340 180 80]) (condp #(< (mod %2 5) %1) (vr.c/get-time)
                                                            1 "Que tu quer???????????\n??? Por quê?"
                                                            "Que tu quer???????????")))

    (vg/with-fx (get (::screen-rt-2 w) vr/RenderTexture2D) {:shaders [[(get (::vg/shader-solid w) vt/Shader)
                                                                     {:u_color (vg/color-identifier w ::message-color)}]]}
      (let [{:keys [width height] :as tex} (get-in (::screen-rt w) [vr/RenderTexture2D :texture])
            rect (vr/Rectangle [0 0 width height])]
        (vr.c/draw-texture-rec tex (update rect :height -) (vr/Vector2 [0 0]) (vr/Color [255 255 255 255]))))

    ;; Track
    (vg/draw-lights w {:scene :vg.gltf.scene/track_scene})
    (let [draw (fn [{:keys [use-color-ids]
                     :or {use-color-ids true}}]
                 (vg/with-camera (get (w (vf/path [:my/model :vg.gltf/track_camera])) vt/Camera)
                   (vg/draw-scene w {:scene :vg.gltf.scene/track_scene
                                     :use-color-ids use-color-ids})
                   (vg/draw-billboard (w (vf/path [:my/model :vg.gltf/track_camera]))
                                      (:texture (get (if use-color-ids
                                                       (::screen-rt-2 w)
                                                       (::screen-rt w))
                                                     vr/RenderTexture2D))
                                      (-> (vp/clone (get (w (vf/path [:my/model :vg.gltf/pilot_d])) vt/Translation))
                                          (update :y + 2.2)
                                          (update :x + 0)
                                          (update :z + 0))
                                      {:scale 8})))]
      ;; Bypass some entities.
      (when turned-on
        (shader-bypass-entities
         w {:draw draw
            :shader (get (::vg/shader-edge-2d w) vt/Shader)
            :entities [(w (vf/path [:my/model :vg.gltf/track_path.001]))
                       (vg/color-identifier w ::message-color)]}))
      ;; Draw to RT.
      (vg/with-target {:target (w (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001 :vg.gltf/screen]))
                       :rt (get (::screen-rt w) vr/RenderTexture2D)
                       :shaders [[(get (::vg/shader-edge-2d w) vt/Shader)
                                  {:edge_fill 1.0}]]}
        (if turned-on
          (do (vr.c/clear-background (vr/Color [100 100 200 255]))
              (draw {:use-color-ids false}))
          (vr.c/clear-background (vr/Color [10 10 10 255])))))

    ;; General.
    #_(vg/draw-lights w {:scene :vg.gltf.scene/Scene})
    (vg/draw-lights w {:scene :vg.gltf.scene/Scene :shader (get (::vg/shader-default w) vt/Shader)})

    (vg/with-drawing

      (let [fx (vg/fx-painting w {:dither-radius 0.5})]

        (vr.c/clear-background (vr/Color [15 15 17 255]))
        (vf/with-query w [_ :vg/camera-active
                          camera vt/Camera]

          ;; Copy cursor's entity path to the clipboard.
          (when (vg/key-pressed? :z)
            (when-let [path (scene->entity-path w camera :vg.gltf.scene/Scene)]
              (println (format "Copied path %s to clipboard" path))
              (vr.c/set-clipboard-text (pr-str path))))

          (shader-bypass-entities
           w {:draw (fn [_]
                      (vg/with-camera camera
                        (vg/draw-scene w {:scene :vg.gltf.scene/Scene
                                          :use-color-ids true})))
              :shader (get (::vg/shader-edge-2d w) vt/Shader)
              :entities [(w (vf/path [:my/model :vg.gltf/office :vg.gltf/table2 :vg.gltf/tv.001 :vg.gltf/screen]))
                         (w (vf/path [:my/model :vg.gltf/office :vg.gltf/floor :vg.gltf/_nomad_unskew :vg.gltf/Boîte.003 :vg.gltf/_nomad_unskew.001 :vg.gltf/button_red]))
                         (w (vf/path [:my/model :vg.gltf/office :vg.gltf/floor :vg.gltf/_nomad_unskew :vg.gltf/Boîte.003 :vg.gltf/_nomad_unskew.002 :vg.gltf/button_green]))]})

          ;; Edge shader (no tv screen).
          (vg/with-drawing-fx w (concat [[(get (::vg/shader-edge-2d w) vt/Shader)
                                          {:edge_fill 1.0}]]
                                        fx)
            (vg/with-camera camera
              (vg/draw-scene w (merge {:scene :vg.gltf.scene/Scene}
                                      #_{:colliders true}))

              #_(vr.c/draw-grid 10 0.5)
              #_(vg/draw-debug w {:scene :vg.gltf.scene/Scene})))))

      ;; Text
      (vg/with-drawing-fx w [[(get (::vg/shader-dither w) vt/Shader)
                              {:u_radius 1.0
                               :u_offsets (vt/Vector3 (mapv #(* % (+ 0.1
                                                                     (vg/wobble 0.2))
                                                                0.5)
                                                            [0.02 (+ 0.016 (vg/wobble 0.01))
                                                             (+ 0.040 (vg/wobble 0.01))]))}]

                             [(get (::vg/shader-noise-blur w) vt/Shader)
                              {:u_radius (+ 1.0 (rand 1))}]]
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
                                     (vg/render-texture ::screen-rt-2 600 600)
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
