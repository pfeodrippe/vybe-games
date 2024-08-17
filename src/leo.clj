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
   [overtone.sc.machinery.server.connection :as ov.conn]
   [vybe.util :as vy.u]
   [vybe.type :as vt]
   [vybe.network :as vn]
   [clojure.edn :as edn]
   #_[overtone.core :refer :all]
   #_[overtone.live :refer :all]
   #_[clj-java-decompiler.core :refer [decompile disassemble]])
  (:import
   (org.vybe.flecs flecs)
   (org.vybe.raylib raylib)
   (org.vybe.jolt jolt)))

(defonce *audio-enabled? (atom false))

(defn audio-enable!
  []
  (try
    (ov.conn/scsynth-path)
    (when-not @*audio-enabled?
      (require '[overtone.core :refer :all])
      (eval '(boot-server))
      (reset! *audio-enabled? true))
    (catch Exception e#
      (println e#)
      (println "\n\n ----- WARNING -----\nIf you want audio working for this game, download SuperCollider at\nhttps://supercollider.github.io/downloads.html"))))

;; Try to enable audio.
#_(audio-enable!)

(defmacro sound
  "Macro used to wrap audio calls so we can use it safely for users who
  have overtone installed."
  [& body]
  (when @*audio-enabled?
    `(do ~@body)))

#_(init)

#_ (sound (demo 0.2 (sin-osc 400)))

(set! *warn-on-reflection* true)

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

  (boot-server)

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
  ([a b]
   (lerp a b 0.6))
  ([a b t]
   (+ a (* t (- b a)))))

(defn lerp-p
  [p1 p2 t]
  (let [c (vp/component p1)]
    (c (cond
         (vp/layout-equal? c vt/Vector3)
         (vr.c/vector-3-lerp p1 p2 t)

         (vp/layout-equal? c vt/Rotation)
         (vr.c/quaternion-slerp p1 p2 t)

         :else
         (mapv (fn [field]
                 (lerp (get p1 field)
                       (get p2 field)
                       t))
               (keys p1))))))

#_ (init)

(def screen-width 600)
(def screen-height 600)

(defn update-jolt-meshes
  [w phys]
  ;; Update jolt meshes (for debugging).
  (merge w
         (->> (vj/bodies-active phys)
              #_(vj/bodies phys)
              (keep (fn [body]
                      (let [position (vj/position body)
                            rotation (vj/rotation body)
                            translation (vt/Translation position)]
                        (if (< (:y translation) -20)
                          (do #_(println :REMOVVVV id :position position :rotation rotation)
                              #_(println (w (vf/path [:vg/phys (keyword (str "vj-" id))])))
                              (dissoc w (vg/body-path body)))
                          [(vg/body-path body)
                           [translation (vt/Rotation rotation)
                            (vt/Scale [1 1 1])
                            vt/Transform [vt/Transform :global]]]))))
              (into {}))

         #_(init)))

(defonce puncher nil)

(defn draw
  [w delta-time]
  (let [{:keys [render-texture shadowmap-shader dither-shader noise-blur-shader]}
        w

        phys (get-in w [(vg/root) vj/PhysicsSystem])

        _ (do
            ;; This will set and reevaluate the default systems (you can call this
            ;; during setup if you are not going to modify any of these later).
            (vg/default-systems w)
            (vf/with-deferred w
              (vj/update! phys delta-time))
            (update-jolt-meshes w phys)
            ;; Progress by running the systems.
            (vf/progress w delta-time))

        p (fn [& ks]
            (vf/path (vec (concat [:my/model] ks))))

        _ (do (def w w)
              (def p p)
              (def shadowmap-shader shadowmap-shader)
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
      (vf/with-each w [player [:mut vt/AnimationPlayer]
                       {speed :v} [:maybe {:flags #{:up}} [vt/Scalar :vg.anim/speed]]
                       _ :vg.anim/active
                       _loop [:maybe :vg.anim/loop]
                       stop [:maybe :vg.anim/stop]
                       e :vf/entity]
        (if stop
          (do (assoc player :current_time 0)
              (-> e
                  (disj :vg.anim/active :vg.anim/started :vg.anim/stop)
                  (conj :vg/selected)))
          (do
            (conj e :vg.anim/started)
            (update player :current_time + (* delta-time step (or speed 1)))))))

    #_ (init)

    (vf/with-each w [[_ node] [:vg.anim/target-node :*]
                     [_ c] [:vg.anim/target-component :*]
                     {:keys [timeline_count values timeline]} vt/AnimationChannel
                     player [:meta {:flags #{:up :cascade}
                                    :inout :mut}
                             vt/AnimationPlayer]
                     parent-e [:vf/entity {:flags #{:up}} :vg.anim/active]
                     e :vf/entity
                     [_ n] [:vf/child-of :*]]
      #_(def e e)
      #_(def kk (vf/get-name w n))
      #_(println :aaa (vf/get-name parent-e))

      (let [values (vp/arr values timeline_count c)
            timeline (vp/arr timeline timeline_count :float)
            idx* (first (indices #(>= % (:current_time player)) timeline))
            idx (max (dec (or idx* (count timeline))) 0)
            t (when idx*
                (/ (- (:current_time player)
                      (nth timeline idx))
                   (- (nth timeline (inc idx))
                      (nth timeline idx))))]

        (if idx*
          (when (= c vt/Translation)
            ;; Play some sound.
            #_(let [d (vr.c/vector-3-distance
                       (vg/matrix->translation (get-in w [:vg/camera-active [vt/Transform :global]]))
                       (vg/matrix->translation (get-in w [:vg.gltf/Sphere [vt/Transform :global]])))
                    [azim elev] (let [cam-transform (get-in w [:vg/camera-active [vt/Transform :global]])
                                      sphere-transform (get-in w [:vg.gltf/Sphere [vt/Transform :global]])
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
          (conj parent-e :vg.anim/stop))

        (merge w {node [(if t
                          (lerp-p (nth values idx)
                                  (nth values (inc idx))
                                  t)
                          (nth values idx))]})

        ;; For blending.
        #_(merge w {node {:vg.anim/frame-animation [[(nth values idx) n]]}})))

    ;; Blending.
    #_(vf/with-each w [_ :vg.anim/joint
                       e :vf/entity]
        #_(merge w {e [a b c]})
        (let [frame-anim (w (vf/path [e :vg.anim/frame-animation]))
              translations (-> frame-anim (get [vt/Translation :*]))
              rotations (-> frame-anim (get [vt/Rotation :*]))
              scales (-> frame-anim (get [vt/Scale :*]))
              t1 (first translations)
              t2 (last translations)
              r1 (first rotations)
              r2 (last rotations)
              s1 (first scales)
              s2 (last scales)]
          #_(merge w {e (last translations)})
          (merge w {e [(vt/Translation [(lerp (:x t1) (:x t2))
                                        (lerp (:y t1) (:y t2))
                                        (lerp (:z t1) (:z t2))])
                       #_(+ (* 0.5 (last translation
                                         s))
                            (* 0.5 (last translations)))
                       (vt/Rotation [(lerp (:x r1) (:x r2))
                                     (lerp (:y r1) (:y r2))
                                     (lerp (:z r1) (:z r2))
                                     (lerp (:w r1) (:w r2))])
                       (vt/Scale [(lerp (:x s1) (:x s2))
                                  (lerp (:y s1) (:y s2))
                                  (lerp (:z s1) (:z s2))])]})
          (dissoc w (vf/path [e :vg.anim/frame-animation]))))

    #_ (init)

    (vf/with-each w [_ [:or
                        :vg.gltf.anim/my-cubeAction.005
                        #_:vg.gltf.anim/ProjectAction]
                     _ :vg/animation
                     e :vf/entity]
      (conj e :vg.anim/active))

    ;; -- Keyboard
    (let [key #(vr.c/is-key-pressed %1)
          cubes! (fn [w]
                   (merge w
                          (->> (range 10 24)
                               #_(range 32)
                               (mapv (fn [idx]
                                       (let [settings (vj/BodyCreationSettings
                                                       {:position (vt/Vector4 [(+ -1 (* idx 0.1))
                                                                               (+ 4.2 (* idx 0.2))
                                                                               (- 2.2 (* idx 0.1))
                                                                               1])
                                                        :rotation (vr.c/vector-4-normalize
                                                                   (vr.c/quaternion-from-axis-angle
                                                                    (vt/Vector3 [1 1 0])
                                                                    (* 0.5 idx)))
                                                        :shape (vj/box (vj/HalfExtent [0.25 0.25 0.25]))
                                                        :motion_type (jolt/JPC_MOTION_TYPE_DYNAMIC)
                                                        :object_layer :vj.layer/moving})
                                             body (vj/body-add phys settings)
                                             {:keys [mesh material]} (vg/gen-cube {:x 0.5 :y 0.5 :z 0.5} (rand-int 10))]
                                         [(vg/body-path body) [mesh material body phys]])))
                               (into {}))))]
      #_(vc/swap assoc :Button (fn [] (vf/event! w :vg.clerk/on-click)))
      (vf/with-observer w [:vf/name :observer.clerk/click
                           _ [:event :vg.clerk/on-click]]
        (cubes! w))
      (cond
        (key (raylib/KEY_C))
        (cubes! w)

        (key (raylib/KEY_D))
        (merge w {:vg/debug (if (get-in w [:vg/debug :vg/enabled])
                              [(vf/del :vg/enabled)]
                              [:vg/enabled])})

        (key (raylib/KEY_SPACE))
        (vf/with-each w [_ :vg/camera-active
                         e :vf/entity]
          (if (= e (w (p :vg.gltf/CameraFar)))
            (assoc w (p :vg.gltf/Camera) [:vg/camera-active])
            (assoc w (p :vg.gltf/CameraFar) [:vg/camera-active]))
          #_(println :aa (= e (vf/eid w (p :vg.gltf/CameraFar))))
          #_(if (contains? (= e (vf/eid w (p :vg.gltf/CameraFar))))
              [(p :vg.gltf/CameraFar) (p :vg.gltf/Camera)]
              [(p :vg.gltf/Camera) (p :vg.gltf/CameraFar)]))

        #_ (w (p :vg.gltf/CameraFar :vg.gltf.anim/CameraFarAction))

        (key (raylib/KEY_M))
        (-> w
            (merge
             {#_ #_ (p :vg.gltf/Sphere :vg.gltf.anim/Right) [(vf/del :vg.anim/active)]
              #_ #_ (p :vg.gltf/Sphere :vg.gltf.anim/Left)[:vg.anim/active]
              (p :vg.gltf/CameraFar :vg.gltf.anim/CameraFarAction) [:vg.anim/active]}
             #_(let [c (fn [k] (p :vg.gltf/Cube.002 k))]
                 (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
                   {(c :vg.gltf.anim/CubeDown) [(vf/del :vg.anim/active) (vf/del :vg/selected)]
                    (c :vg.gltf.anim/CubeUp) [:vg.anim/active]}
                   {(c :vg.gltf.anim/CubeDown) [:vg.anim/active]
                    (c :vg.gltf.anim/CubeUp) [(vf/del :vg.anim/active) (vf/del :vg/selected)]}))
             #_(let [c (fn [k] (p :vg.gltf/Cube k))]
                 (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
                   {(c :vg.gltf.anim/CubeDown) [(vf/del :vg.anim/active) (vf/del :vg/selected)]
                    (c :vg.gltf.anim/CubeUp) [:vg.anim/active]}
                   {(c :vg.gltf.anim/CubeDown) [:vg.anim/active]
                    (c :vg.gltf.anim/CubeUp) [(vf/del :vg.anim/active) (vf/del :vg/selected)]}))))))

    #_(vf/with-each w [_ :vg/ray-casted
                       material vr/Material
                       translation vt/Translation]
        #_(-> material
              :maps
              (vp/arr 1 vr/MaterialMap)
              (nth (raylib/MATERIAL_MAP_DIFFUSE))
              (assoc :color (vr/Color [200 155 255 1.0])))
        #_(println :a)
        (update translation :x + 0.2))

    (let [key #(vr.c/is-key-down %1)]
      (when (key (raylib/KEY_UP))
        (-> w
            (update-in [(p :vg.gltf/monster_parent :vg.gltf/monster) vt/Translation :z] + 0.018)))

      (when (key (raylib/KEY_DOWN))
        (-> w
            (update-in [(p :vg.gltf/monster_parent :vg.gltf/monster) vt/Translation :z] - 0.018)))

      (when (key (raylib/KEY_I))
        (-> w
            (update-in [(p :vg.gltf/monster_parent.001 :vg.gltf/monster.001) vt/Translation :z] + 0.018)))

      (when (key (raylib/KEY_K))
        (-> w
            (update-in [(p :vg.gltf/monster_parent.001 :vg.gltf/monster.001) vt/Translation :z] - 0.018))))

    ;; Running animation.
    #_(let [key #(vr.c/is-key-down %1)]
        (cond
          (key (raylib/KEY_X))
          (-> w
              (merge {(p :vg.gltf/Armature :vg.gltf.anim/Running) [:vg.anim/active]
                      (p :vg.gltf/Armature :vg.gltf.anim/Idle) [(vf/del :vg.anim/active)]})
              (update-in [(p :vg.gltf/Armature) vt/Translation :z] + 0.018))

          (key (raylib/KEY_Z))
          (-> w
              (merge {(p :vg.gltf/Armature :vg.gltf.anim/Running) [:vg.anim/active]
                      (p :vg.gltf/Armature :vg.gltf.anim/Idle) [(vf/del :vg.anim/active)]})
              (update-in [(p :vg.gltf/Armature) vt/Translation :z] - 0.018))

          :else
          (-> w
              (merge {(p :vg.gltf/Armature :vg.gltf.anim/Idle) [:vg.anim/active]
                      (p :vg.gltf/Armature :vg.gltf.anim/Running) [(vf/del :vg.anim/active)]}))))

    (comment

      (w (p :vg.gltf/Cube))

      (w (p :vg.gltf/Armature))

      (vf/hierarchy-no-path (w :my/model))
      (keys (vf/hierarchy (w (p :vg.gltf/Armature))))
      (vf/hierarchy-no-path (w (p :vg.gltf/Armature)))


      (init)

      (count (vj/bodies phys))
      (count (vj/bodies-active phys))
      (count (vf/with-each w [translation vt/Translation] translation))


      (get (w (vf/path [:my/model :vg.gltf/Plane])) vt/Translation)

      (w (vf/path [:my/model :vg.gltf/Sphere]))

      (vj/body-ids phys)
      (mapv :position (vj/bodies phys))

      ())

    #_ (init)

    ;; -- Window.
    (vf/with-observer w [:vf/name :observer.window/on-close
                         _ [:event :vg.window/on-close]]
      (System/exit 0))

    ;; -- Raycast.
    (vf/with-observer w [:vf/name :observer/on-raycast-click
                         _ [:event :vg.raycast/on-click]
                         {:keys [id]} [:filter vt/Eid]]
      (sound (demo 0.2 (mapv (comp sin-osc midi->hz)
                             (repeatedly 3 #(+ (rand-int 20) 55)))))
      (let [c (fn [k] (vf/path [id k]))]
        (merge w
               (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
                 {(c :vg.gltf.anim/CubeDown) [(vf/del :vg.anim/active) (vf/del :vg/selected)]
                  (c :vg.gltf.anim/CubeUp) [:vg.anim/active]}
                 {(c :vg.gltf.anim/CubeDown) [:vg.anim/active]
                  (c :vg.gltf.anim/CubeUp) [(vf/del :vg.anim/active) (vf/del :vg/selected)]}))))

    (vf/with-observer w [:vf/name :observer/on-raycast-hover
                         _ [:event :vg.raycast/on-hover]
                         body [:filter vj/VyBody]]
      (merge w {(p :vg.gltf/Sphere) [(assoc (-> body vj/position vt/Translation)
                                            :y (+ (:y (:max (vj/world-bounds body)))
                                                  0.3))]}))

    (vf/with-observer w [:vf/name :observer/on-raycast-enter
                         _ [:event :vg.raycast/on-enter]
                         body [:filter vj/VyBody]]
      (sound (demo 0.1 (sin-osc (midi->hz (+ (rand-int 20) 50))))))

    (vf/with-observer w [:vf/name :observer/on-raycast-leave
                         _ [:event :vg.raycast/on-leave]]
      (merge w {(p :vg.gltf/Sphere) [(vt/Translation [-100 -100 -100])]}))

    #_(vr/t
        (vf/with-each w [_ :vg/camera-active
                         e :vf/entity]
          (vf/get-name e)))

    ;; Camera systems.
    (vf/with-system w [:vf/name :system/update-camera
                       _ :vg/camera-active
                       camera [:out vt/Camera]
                       translation vt/Translation
                       rotation vt/Rotation
                       e :vf/entity]
      (let [cam-pos (get-in camera [:camera :position])
            vel (vt/Velocity (mapv #(/ % delta-time)
                                   [(- (:x translation)
                                       (:x cam-pos))
                                    (- (:y translation)
                                       (:y cam-pos))
                                    (- (:z translation)
                                       (:z cam-pos))]))]
        (conj e vel))

      (-> camera
          (assoc-in [:camera :position] translation)
          (assoc-in [:rotation] rotation)))

    #_(init)

    #_(vf/with-observer w [:vf/name :observer/on-contact-added
                           {:keys [body-1 body-2]} [:event vg/OnContactAdded]]
        #_(println [(:id body-1) (:id body-2)]))

    ;; -- Physics.
    ;; Update model meshes from the Jolt bodies.
    (vf/with-system w [:vf/name :system/update-model-meshes
                       translation [:out vt/Translation]
                       rotation [:out vt/Rotation]
                       body vj/VyBody
                       :vf/always true ; TODO We shouldn't need this if we get the activate/deactivate events
                       #_ #_:vf/disabled true
                       _ :vg/dynamic
                       it :vf/iter]
      (let [pos (vj/position body)
            rot (vj/rotation body)]
        (when (and pos rot)
          (merge rotation (vt/Rotation rot))
          (merge translation (vt/Translation pos)))))

    ;; -- Network.
    (conj (vf/ent w vt/Translation) :vg/networked)
    (conj (vf/ent w vt/Rotation) :vg/networked)
    (conj (vf/ent w vt/Scale) :vg/networked)

    ;; Sync.
    (vf/with-each w [_ :vg/networked
                     c-eid :vf/eid]
      (when (not= c-eid (vf/eid w :vg/networked))
        (vf/with-system w [:vf/name (vf/path [c-eid :system/network-sync])
                           c-value c-eid
                           _ [:meta {:flags #{:up :self}} :vg/sync]
                           synced [:maybe [:vg.sync/synced c-eid]]
                           e :vf/entity]
          ;; We don't want to send data that we just received, we use :vg.sync/synced
          ;; to flag that.
          #_(println :SS c-eid (:vg.sync/synced e))
          (if synced
            (vf/disable e synced)
            (vn/send! puncher c-value {:entity e})))))

    ;; Receive network data.
    (->> (vn/update! puncher delta-time)
         (mapv (fn [{:keys [data entity-name]}]
                 (when (vp/pmap? data)
                   #_(println :RECEIVED entity-name data)
                   (let [c-eid (vf/eid w (vp/component data))]
                     (merge w {entity-name [data [:vg.sync/synced c-eid]]})
                     (vf/enable w entity-name [:vg.sync/synced c-eid]))))))

    ;; -- Drawing
    (let [draw-scene (do (fn [w]
                           #_(vr.c/draw-grid 30 0.5)
                           (if (get-in w [:vg/debug :vg/enabled])
                             (vg/draw-debug w)
                             (vg/draw-scene w)))
                         #_(fn [w]
                             (vg/draw-debug w)
                             (vg/draw-scene w)))]

      (vg/draw-lights w #_default-shader (get shadowmap-shader vt/Shader) draw-scene)

      (vf/with-each w [_ :vg/camera-active
                       camera vt/Camera]
        (vg/with-multipass (get render-texture vr/RenderTexture2D) {:shaders
                                                                    [[(get noise-blur-shader vt/Shader)
                                                                      {:u_radius (+ 1.0
                                                                                    #_(* (vr.c/vector-3-length velocity) 0.1)
                                                                                    (rand 1))}]

                                                                     [(get dither-shader vt/Shader)
                                                                      {:u_offsets (vt/Vector3 (mapv #(* % (+ 0.6
                                                                                                             (wobble 0.3)))
                                                                                                    [0.02 (+ 0.016 (wobble 0.01))
                                                                                                     (+ 0.040 (wobble 0.01))]))}]]}
          (vr.c/clear-background (vr/Color "#A98B39"))
          (vg/with-camera camera
            (draw-scene w))))

      ;; Draw to the screen.
      (vg/with-drawing
        (vr.c/clear-background (vr/Color [255 20 100 255]))

        (vr.c/draw-texture-pro (:texture (get render-texture vr/RenderTexture2D))
                               (vr/Rectangle [0 0 screen-width (- screen-height)])
                               (vr/Rectangle [0 0  screen-width screen-height])
                               (vr/Vector2 [0 0]) 0 vg/color-white)

        #_(vg/with-camera (get-in w [:vg/camera-active vt/Camera])
            (draw-scene w))

        (vr.c/draw-fps 510 570)))))

#_(init)

(defn host-init!
  []
  (let [session-id   "gamecode40"
        client-id    30
        server-ip    "147.182.133.53"
        server-port  8080
        host (vn/make-hole-puncher server-ip server-port
                                   {:session-id session-id
                                    :client-id client-id
                                    :num-of-players 2
                                    :is-host true})]
    host))

(defn client-init!
  []
  (let [session-id   "gamecode40"
        client-id    31
        server-ip    "147.182.133.53"
        server-port  8080
        client (vn/make-hole-puncher server-ip server-port
                                     {:session-id session-id
                                      :client-id client-id})]
    client))

(defn init
  []
  (when-not (vr.c/is-window-ready)
    (vr.c/set-config-flags (raylib/FLAG_MSAA_4X_HINT))
    (vr.c/init-window screen-width screen-height "Opa")
    (vr.c/set-window-state (raylib/FLAG_WINDOW_UNFOCUSED))
    (vr.c/set-target-fps 60)
    #_ (vr.c/set-target-fps 30)
    #_ (vr.c/set-target-fps 10)
    #_ (vr.c/set-target-fps 120)
    (vr.c/set-window-position 1120 200)

    ;; "Loading screen"
    (vr.c/clear-background (vr/Color [10 100 200 255]))
    (vr.c/draw-rectangle 30 50 100 200 (vr/Color [255 100 10 255]))
    (vr.c/draw-rectangle 300 50 100 200 (vr/Color [255 100 10 255])))

  (def puncher
    (if (System/getenv "VYBE_CLIENT")
      (client-init!)
      (host-init!)))

  #_ (init)

  (let [w (vf/make-world)]
    ;; If you want to enable debugging (debug messages + clerk + flecs explorer),
    ;; uncomment line below.
    #_(vg/debug-init! w)

    (merge w {:vg.sync/synced [(flecs/EcsPairIsTag) (flecs/EcsCanToggle)]})

    (vg/start! w screen-width screen-height #'draw
               (fn [w]
                 (-> w
                     (vg/model :my/model (vg/extract-resource "models.glb"))
                     (vg/shader-program :shadowmap-shader "shaders/shadowmap.vs" "shaders/shadowmap.fs")
                     (vg/shader-program :dither-shader "shaders/dither.fs")
                     (vg/shader-program :noise-blur-shader "shaders/noise_blur_2d.fs")
                     (vg/shader-program :default-shader)
                     (merge {:render-texture [(vr/RenderTexture2D (vr.c/load-render-texture screen-width screen-height))]}))))))
#_(init)

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main threaad and it will loop things
  ;; for us.
  (future (init))
  (vr/-main))
