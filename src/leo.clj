(ns leo
  (:require
   [vybe.game :as vg]
   [vybe.raylib :as vr]
   [vybe.panama :as vp]
   [vybe.raylib.c :as vr.c]
   [vybe.flecs :as vf]
   [vybe.flecs.c :as vf.c]
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [vybe.jolt :as vj]
   [vybe.jolt.c :as vj.c]
   #_[overtone.live :refer :all]
   #_[clj-java-decompiler.core :refer [decompile disassemble]])
  (:import
   (org.vybe.flecs flecs)
   (org.vybe.raylib raylib)
   (org.vybe.jolt jolt)))

#_(init)

#_(l/demo 0.2 (l/sin-osc 800))

(set! *warn-on-reflection* true)

(defonce env (vg/make-env))

(defn load-shadowmap-render-texture
  "From https://github.com/raysan5/raylib/blob/master/examples/shaders/shaders_shadowmap.c#L202."
  [width height]
  (let [rt (vr/RenderTexture2D)
        id (vr.c/rl-load-framebuffer)
        _ (assoc rt :id id)
        _ (vr.c/rl-enable-framebuffer id)
        tex-depth-id (vr.c/rl-load-texture-depth width height false)]
    (merge rt {:texture {:width width, :height height}
               :depth {:id tex-depth-id, :width width, :height height,
                       :format 19, :mipmaps 1}})
    (vr.c/rl-framebuffer-attach id tex-depth-id (raylib/RL_ATTACHMENT_DEPTH) (raylib/RL_ATTACHMENT_TEXTURE2D) 0)
    (when-not (vr.c/rl-framebuffer-complete id)
      (throw (ex-info "Couldn't create frame buffer" {})))
    (vr.c/rl-disable-framebuffer)

    rt))

#_(load-shadowmap-render-texture 600 600)

(defn wobble
  ([v]
   (wobble v 1.0))
  ([v freq]
   (* v (math/sin (* (vr.c/get-time) freq)))))

(defn wobble-rand
  ([v]
   (wobble-rand v 1.0))
  ([v freq]
   (let [f #(wobble v (* % freq))]
     (+ (f 2) (* (f 3) (f 4.5))))))

#_(init)
(defn- d
  [v]
  (println :___DEBUG v)
  v)

#_ (:id (vg/Shader))

#_(init)

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

#_(do

    (defonce my-bus
      (audio-bus 1))

    (defonce main-g (group "get-on-the-bus main"))
    (defonce early-g (group "early birds" :head main-g))
    (defonce later-g (group "latecomers" :after early-g))

    (defsynth-load bass-drum
      "resources/sc/compiled/sonic-pi-sc808_bassdrum.scsyndef")
    #_ (bass-drum)

    (defonce b (sample "~/Downloads/wrapping-paper-rustle-72405.mp3"))

    (defsynth ddd
      [freq 300, mul 0.5, out_bus 0]
      (out out_bus
           #_(* mul (sin-osc 260) (saw 3) 0.04)
           #_(play-buf 1 b (buf-rate-scale:ir b) :loop 1)
           (* mul (lpf (pink-noise 0.4) 500))))

    (defsynth-load directional
      "resources/sc/compiled/directional.scsyndef")
    #_ (def sound-d (directional [:tail later-g] :in my-bus :out_bus 0)))

(comment

  (def b (sample "/Users/pfeodrippe/Library/Application Support/ATK/sounds/stereo/Aurora_Surgit-Dies_Irae.wav"))

  (demo 1 (lpf (pink-noise 0.4) 400))

  (do
    (stop)
    (def aaa (ddd [:tail early-g] :out_bus my-bus))
    (def sound-d (directional [:tail later-g] :in my-bus :out_bus 0)))

  (stop)

  (ctl sound-d :azim (/ math/PI 2))
  (ctl sound-d :elev (/ math/PI 2))

  (pp-node-tree)

  (ctl aaa :mul 0.2)

  ())

(defn lerp
  [p1 p2]
  (+ p1 (* 0.6 (- p2 p1))))

#_ (init)

(defn draw
  []
  (vp/with-arena _
    (let [{:keys [vf/w view-2 shadowmap-shader
                  dither-shader noise-blur-shader
                  depth-rts default-shader kuwahara-shader]}
          env

          _ (do ;; For dev mode.
              (vg/run-reloadable-commands!)
              (vg/default-systems w)
              (vf/progress w (vr.c/get-frame-time)))

          p (fn [& ks]
              (vf/path (vec (concat [:my/model] ks))))

          phys (get-in w [:vg/phys vj/PhysicsSystem])

          _ (do (def w w)
                (def p p)
                (def shadowmap-shader shadowmap-shader)
                (def dither-shader dither-shader)
                (def phys phys))

          #_ (init)]

      ;; -- Animation
      (let [step (let [key #(vr.c/is-key-down %1)]
                   (cond
                     (key (raylib/KEY_W))
                     1.5

                     (key (raylib/KEY_S))
                     -1.5

                     :else
                     1))]
        (vf/with-each w [player vg/AnimationPlayer
                         _ :vg/active
                         loop [:maybe :vg.anim/loop]
                         started [:maybe :vg.anim/started]
                         e :vf/entity]
          (if (and started (== (:current_time player) 0))
            (-> e
                (disj :vg/active :vg.anim/started)
                (conj :vg/selected))
            (do
              (conj e :vg.anim/started)
              (update player :current_time + (* (vr.c/get-frame-time) step))))))

      #_ (init)

      (vf/with-each w [[_ node] [:vg.anim/target-node :*]
                       [_ c] [:vg.anim/target-component :*]
                       {:keys [timeline_count values timeline]} vg/AnimationChannel
                       player [:meta {:flags #{:up :cascade}}
                               vg/AnimationPlayer]
                       _ [:meta {:flags #{:up :cascade}} :vg/active]
                       e :vf/entity
                       [_ n] [:vf/child-of :*]]
        #_(def e e)
        #_(def kk (vf/get-name w n))

        (let [values (vp/arr values timeline_count c)
              timeline (vp/arr timeline timeline_count :float)
              idx* (first (indices #(>= % (:current_time player)) timeline))
              idx (max (dec (or idx* (count timeline))) 0)]
          (if idx*
            (when (= c vg/Translation)
              ;; Play some sound.
              #_(let [d (vr.c/vector-3-distance
                         (vg/matrix->translation (get-in w [:vg/camera-active [vg/Transform :global]]))
                         (vg/matrix->translation (get-in w [:vg.gltf/Sphere [vg/Transform :global]])))
                      [azim elev] (let [cam-transform (get-in w [:vg/camera-active [vg/Transform :global]])
                                        sphere-transform (get-in w [:vg.gltf/Sphere [vg/Transform :global]])
                                        {:keys [x y z] :as _v} (-> sphere-transform
                                                                   (vr.c/matrix-multiply (vr.c/matrix-invert cam-transform))
                                                                   vg/matrix->translation)]
                                    (if (> z 0)
                                      [(- (vr.c/atan-2 x z))
                                       (vr.c/atan-2 y z)
                                       _v]
                                      [(vr.c/atan-2 x z)
                                       (vr.c/atan-2 y z)
                                       _v]))
                      amp (if (zero? d)

                            1
                            (/ 1 (* d d 1)))]
                  #_(ctl sound-d :azim azim :elev elev :amp amp :distance d)))
            (assoc player :current_time 0))

          #_(def node node)
          #_(vf/make-entity w node)

          (merge w {node [(nth values idx)]})

          ;; For blending.
          #_(merge w {node {:vg.anim/frame-animation [[(nth values idx) n]]}})))

      ;; Blending.
      #_(vf/with-each w [_ :vg.anim/joint
                         e :vf/entity]
          #_(merge w {e [a b c]})
          (let [frame-anim (w (vf/path [e :vg.anim/frame-animation]))
                translations (-> frame-anim (get [vg/Translation :*]))
                rotations (-> frame-anim (get [vg/Rotation :*]))
                scales (-> frame-anim (get [vg/Scale :*]))
                t1 (first translations)
                t2 (last translations)
                r1 (first rotations)
                r2 (last rotations)
                s1 (first scales)
                s2 (last scales)]
            #_(merge w {e (last translations)})
            (merge w {e [(vg/Translation [(lerp (:x t1) (:x t2))
                                          (lerp (:y t1) (:y t2))
                                          (lerp (:z t1) (:z t2))])
                         #_(+ (* 0.5 (last translation
                                           s))
                              (* 0.5 (last translations)))
                         (vg/Rotation [(lerp (:x r1) (:x r2))
                                       (lerp (:y r1) (:y r2))
                                       (lerp (:z r1) (:z r2))
                                       (lerp (:w r1) (:w r2))])
                         (vg/Scale [(lerp (:x s1) (:x s2))
                                    (lerp (:y s1) (:y s2))
                                    (lerp (:z s1) (:z s2))])]})
            (dissoc w (vf/path [e :vg.anim/frame-animation]))))

      #_ (init)

      ;; -- Keyboard
      (let [key #(vr.c/is-key-pressed %1)]
        (cond
          (key (raylib/KEY_C))
          (merge w
                 (->> (range 32)
                      (mapv (fn [idx]
                              (let [id (vj/body-add phys
                                                    (vj/BodyCreationSettings
                                                     {:position (vj/Vector4 [(+ 1 (* idx 0.05))
                                                                             (+ 4.2 (* idx 0.1))
                                                                             (- 2.2 (* idx 0.1))
                                                                             1])
                                                      :rotation (vj/Vector4 [0 0 0 1])
                                                      :shape (-> (vj/box-settings (vj/HalfExtent [0.25 0.25 0.25]))
                                                                 vj/shape)
                                                      :motion_type (jolt/JPC_MOTION_TYPE_DYNAMIC)
                                                      :object_layer :vj.layer/moving}))
                                    {:keys [mesh material]} (vg/gen-cube {:x 0.5 :y 0.5 :z 0.5} (rand-int 10))]
                                #_(println :AAAA (vg/BodyPointer {:p (vj/body-get phys id)}))
                                #_(vj/optimize-broad-phase phys)
                                [(vf/path [phys (keyword (str "vj-" id))])
                                 [mesh material [(vg/Int id) :vj/body-id]]])))
                      (into {})))

          (key (raylib/KEY_D))
          (merge w {:vg/debug (if (get-in w [:vg/debug :vg/enabled])
                                [(vf/del :vg/enabled)]
                                [:vg/enabled])})

          (key (raylib/KEY_SPACE))
          (let [[old-entity new-entity] (if (contains? (w (p :vg/camera-active)) (p :vg.gltf/CameraFar))
                                          [(p :vg.gltf/CameraFar) (p :vg.gltf/Camera)]
                                          [(p :vg.gltf/Camera) (p :vg.gltf/CameraFar)])]
            (-> w
                ;; TODO Use a union or similar here.
                (merge {(p :vg/camera-active)
                        [new-entity
                         (vf/del old-entity)
                         (vf/ref new-entity [vg/Transform :global])
                         (vf/ref new-entity vg/Camera)]})))

          (key (raylib/KEY_M))
          (-> w
              (merge
               {(p :vg.gltf/Sphere :vg.gltf.anim/Right) [(vf/del :vg/active)]
                (p :vg.gltf/Sphere :vg.gltf.anim/Left)[:vg/active]}
               (let [c (fn [k] (p :vg.gltf/Cube.002 k))]
                 (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
                   {(c :vg.gltf.anim/CubeDown) [(vf/del :vg/active) (vf/del :vg/selected)]
                    (c :vg.gltf.anim/CubeUp) [:vg/active]}
                   {(c :vg.gltf.anim/CubeDown) [:vg/active]
                    (c :vg.gltf.anim/CubeUp) [(vf/del :vg/active) (vf/del :vg/selected)]}))
               (let [c (fn [k] (p :vg.gltf/Cube k))]
                 (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
                   {(c :vg.gltf.anim/CubeDown) [(vf/del :vg/active) (vf/del :vg/selected)]
                    (c :vg.gltf.anim/CubeUp) [:vg/active]}
                   {(c :vg.gltf.anim/CubeDown) [:vg/active]
                    (c :vg.gltf.anim/CubeUp) [(vf/del :vg/active) (vf/del :vg/selected)]}))))))

      #_(vf/with-each w [_ :vg/ray-casted
                         material vr/Material
                         translation vg/Translation]
          #_(-> material
                :maps
                (vp/arr 1 vr/MaterialMap)
                (nth (raylib/MATERIAL_MAP_DIFFUSE))
                (assoc :color (vr/Color [200 155 255 1.0])))
          #_(println :a)
          (update translation :x + 0.2))

      ;; Running animation.
      #_(let [key #(vr.c/is-key-down %1)]
          (cond
            (key (raylib/KEY_X))
            (-> w
                (merge {(p :vg.gltf/Armature :vg.gltf.anim/Running) [:vg/active]
                        (p :vg.gltf/Armature :vg.gltf.anim/Idle) [(vf/del :vg/active)]})
                (update-in [(p :vg.gltf/Armature) vg/Translation :z] + 0.018))

            (key (raylib/KEY_Z))
            (-> w
                (merge {(p :vg.gltf/Armature :vg.gltf.anim/Running) [:vg/active]
                        (p :vg.gltf/Armature :vg.gltf.anim/Idle) [(vf/del :vg/active)]})
                (update-in [(p :vg.gltf/Armature) vg/Translation :z] - 0.018))

            :else
            (-> w
                (merge {(p :vg.gltf/Armature :vg.gltf.anim/Idle) [:vg/active]
                        (p :vg.gltf/Armature :vg.gltf.anim/Running) [(vf/del :vg/active)]}))))

      (comment

        (w (p :vg.gltf/Cube))

        (w (p :vg.gltf/Armature))

        (vf/hierarchy-no-path (w :my/model))
        (keys (vf/hierarchy (w (p :vg.gltf/Armature))))
        (vf/hierarchy-no-path (w (p :vg.gltf/Armature)))

        (count (vj/bodies phys))
        (count (vf/with-each w [translation vg/Translation] translation))

        (get (w (vf/path [:my/model :vg.gltf/Plane])) vg/Translation)

        (w (vf/path [:my/model :vg.gltf/Sphere]))

        (vj/body-ids phys)
        (mapv :position (vj/bodies phys))

        (count (vf/with-each w [_ :vg/dynamic
                                e :vf/entity]
                 (vf/get-name e)))

        (let [body (second (take 2 (drop 2 (vj/bodies phys))))]
          (vj/body-move body (vg/Vector3 [0 1 0]) 1/60))

        (let [p (vj/Vector4)]
          (vj.c/jpc-body-get-position (first (vj/bodies phys)) p)
          p
          (vj.c/jpc-body-interface-get-position (vj/body-interface phys)
                                                33554432
                                                p)
          p)

        ())

      #_ (init)

      ;; -- Mouse.
      (let [{:keys [position direction]} (-> (vr.c/get-mouse-position)
                                             (vr.c/vy-get-screen-to-world-ray
                                              (get-in w [(p :vg/camera-active) vg/Camera])))
            direction (mapv #(* % 10000) (vals direction))
            body (vj/cast-ray phys position direction)]
        (if-let [pos (some-> (:position body)
                             vg/Translation
                             (assoc :y (+ (nth (:bounds_max body) 1) 0.3)))]
          #_(println :pos pos)
          #_(println :PARENT (vf/parent (str "vj-" (:id body))))

          (do #_(def a (get-in w [(vf/path [phys (keyword (str "vj-" (:id body)))])
                                  #_[:vg/refers :_]]))
              #_(println :pos-1 pos)
              (when-let [e (some->> (get-in w [(vf/path [phys (keyword (str "vj-" (:id body)))])
                                               [:vg/refers :_]])
                                    first
                                    last
                                    (vf/make-entity w))]
                #_(println :pos pos)
                (if (get e [:vg/raycast :vg/enabled])
                  (do (merge w {(p :vg.gltf/Sphere)
                                [pos]})
                      (when (vr.c/is-mouse-button-pressed (raylib/MOUSE_BUTTON_LEFT))
                        (let [c (fn [k] (vf/path [(vf/parent w e) k]))]
                          (merge w (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
                                     {(c :vg.gltf.anim/CubeDown) [(vf/del :vg/active) (vf/del :vg/selected)]
                                      (c :vg.gltf.anim/CubeUp) [:vg/active]}
                                     {(c :vg.gltf.anim/CubeDown) [:vg/active]
                                      (c :vg.gltf.anim/CubeUp) [(vf/del :vg/active) (vf/del :vg/selected)]})))))
                  (merge w {(p :vg.gltf/Sphere) [(vg/Translation [-10 -10 -10])]}))))
          (when-not (= (get-in w [(p :vg.gltf/Sphere) vg/Translation])
                       (vg/Translation [-10 -10 -10]))
            (merge w {(p :vg.gltf/Sphere) [(vg/Translation [-10 -10 -10])]}))))

      #_(init)

      ;; -- Physics.
      (vj/update! phys (vr.c/get-frame-time))

      ;; Update model meshes from the Jolt bodies.
      (vf/with-each w [translation [:meta {:flags #{:up} :inout :out} vg/Translation]
                       rotation [:meta {:flags #{:up} :inout :out} vg/Rotation]
                       _ [:meta {:flags #{:up}} :vg/dynamic]
                       {id :i} [vg/Int :vj/body-id]]
        (let [pos (vj/body-position phys id)
              rot (vj/body-rotation phys id)]
          (when (and pos rot)
            (merge rotation (vg/Rotation rot))
            (merge translation (vg/Translation pos))))
        #_(when-let [body (vj/body-get phys id)]
          (merge rotation (vg/Rotation (:rotation body)))
          (merge translation (vg/Translation (:position body)))))

      ;; Update jolt meshes (for debugging).
      (merge w
             (->> (vj/body-ids phys)
                  #_(filter (partial vj/body-active? phys))
                  #_(take 2)
                  (keep (fn [id]
                          (let [position (vj/body-position phys id)
                                rotation (vj/body-rotation phys id)
                                translation (vg/Translation position)]
                            (if (< (:y translation) -20)
                              (do (println :REMOVVVV id :position position :rotation rotation)
                                  (dissoc w (vf/path [phys (keyword (str "vj-" id))])))
                              {(vf/path [phys (keyword (str "vj-" id))])
                               [translation (vg/Rotation rotation)
                                (vg/Scale [1 1 1])
                                vg/Transform [vg/Transform :global]]}))))
                  (into {})))

      (let [draw-scene (fn [w]
                         #_(vr.c/draw-grid 30 0.5)
                         (if (get-in w [:vg/debug :vg/enabled])
                           (vg/draw-debug w)
                           (vg/draw-scene w)))]

        ;; -- Drawing
        (vg/draw-lights w #_default-shader shadowmap-shader depth-rts draw-scene)

        (vg/with-multipass view-2 {:shaders [[noise-blur-shader {:u_radius (+ 1.0 (rand 1))}]
                                             [dither-shader {:u_offsets (vg/Vector3 (mapv #(* % (+ 0.6 (wobble 0.3)))
                                                                                          [0.02 (+ 0.016 (wobble 0.01))
                                                                                           (+ 0.040 (wobble 0.01))]))}] ]}
          (vr.c/clear-background (vr/Color "#A98B39"))
          (vg/with-camera (get-in w [(p :vg/camera-active) vg/Camera])
            (draw-scene w)))

        ;; Draw to the screen.
        (vg/with-drawing
          (vr.c/clear-background (vr/Color [255 20 100 255]))

          (vr.c/draw-texture-pro (:texture view-2)
                                 (vr/Rectangle [0 0 600 -600]) (vr/Rectangle [0 0 600 600])
                                 (vr/Vector2 [0 0]) 0 vg/color-white)

          #_(vg/with-camera (get-in w [:vg/camera-active vg/Camera])
              (draw-scene w))

          (vr.c/draw-fps 510 570))))))

#_(init)
#_(vr.c/set-target-fps 10)

(defn init
  []
  (when-not (vr.c/is-window-ready)
    (vr.c/set-config-flags (raylib/FLAG_MSAA_4X_HINT))
    (vr.c/init-window 600 600 "Opa")
    (vr.c/set-window-state (raylib/FLAG_WINDOW_UNFOCUSED))
    (vr.c/set-target-fps 60)
    #_ (vr.c/set-target-fps 30)
    #_ (vr.c/set-target-fps 10)
    #_ (vr.c/set-target-fps 120)
    (vr.c/set-window-position 1120 200)
    (vr.c/clear-background (vr/Color [10 100 200 255]))
    (vr.c/draw-rectangle 30 50 100 200 (vr/Color [255 100 10 255]))
    (vr.c/draw-rectangle 300 50 100 200 (vr/Color [255 100 10 255])))

  (reset! env {})
  #_(init)
  (swap! env merge {:vf/w (let [w (vf/make-world)
                                path (.getPath (io/resource "models.glb"))]
                            ;; For enabling the REST interface (explorer).
                            #_(vf.c/ecs-set-id w
                                               (flecs/FLECS_IDEcsRestID_)
                                               (flecs/FLECS_IDEcsRestID_)
                                               (.byteSize (.layout vf/Rest))
                                               (vf/Rest))
                            (vg/reloadable {:game-id :my/model :resource-paths [path]}
                              (-> w
                                  (vg/load-model :my/model path))))})

  #_(def w (:vf/w env))

  (swap! env merge { ;; Create 10 depth render textures for reuse.
                    :depth-rts (pmap #(do % (load-shadowmap-render-texture 600 600))
                                     (range 10))

                    :shadowmap-shader (vg/shader-program :shadowmap-shader
                                                         {::vg/shader.vert "shaders/shadowmap.vs"
                                                          ::vg/shader.frag "shaders/shadowmap.fs"})
                    :kuwahara-shader (vg/shader-program :kuwahara-shader
                                                        {::vg/shader.frag "shaders/kuwahara_2d.fs"})
                    :dither-shader (vg/shader-program :dither-shader
                                                      {::vg/shader.frag "shaders/dither.fs"})
                    :noise-blur-shader (vg/shader-program :noise-blur-shader
                                                          {::vg/shader.frag "shaders/noise_blur_2d.fs"})
                    :edge-shader (vg/shader-program :edge-shader
                                                    {::vg/shader.frag "shaders/edge_2d.fs"})
                    :process-shader (vg/shader-program :process-shader
                                                       {::vg/shader.frag "shaders/process_2d.fs"})
                    :dof-shader (vg/shader-program :dof-shader
                                                   {::vg/shader.frag "shaders/dof.fs"})
                    :default-shader (vg/shader-program :default-shader {})

                    :view-2 (vr.c/load-render-texture 600 600)})

  (alter-var-root #'vr/draw (constantly #'draw)))

#_(alter-var-root #'vr/draw (constantly (fn [] (Thread/sleep 10))))
#_(init)
