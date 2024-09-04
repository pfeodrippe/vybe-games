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
   (org.vybe.jolt jolt)
   (java.lang.foreign MemorySegment ValueLayout)))

(defonce *audio-enabled? (atom false))

(defn audio-enable!
  []
  (try
    (ov.conn/scsynth-path)
    (when-not @*audio-enabled?
      (require '[overtone.core :refer :all])
      (eval '(boot-server))
      (require '[overtone.inst.synth :as synth])
      (reset! *audio-enabled? true))
    (catch Exception e#
      (println e#)
      (println "\n\n ----- WARNING -----\nIf you want audio working for this game, download SuperCollider at\nhttps://supercollider.github.io/downloads.html"))))

;; Try to enable audio.
(audio-enable!)

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

(defonce init-sound
  (sound

    (stop)

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
           (* mul (lpf (pink-noise 0.8) 500))))

    (defsynth-load directional
      "resources/sc/compiled/directional.scsyndef")
    (ddd [:tail early-g] :out_bus my-bus)
    (def sound-d (directional [:tail later-g] :in my-bus :out_bus 0))))

(comment

  (boot-server)

  (def b (sample "/Users/pfeodrippe/Library/Application Support/ATK/sounds/stereo/Aurora_Surgit-Dies_Irae.wav"))

  (ddd)

  (demo 1 [(lpf (pink-noise 0.4) 400)
           (lpf (pink-noise 0.4) 400)])

  (demo 1 (fx-echo (pink-noise 0.4) 1 100 1))

  (adsr 1 0.7)

  (do
    (stop)
    (def aaa (ddd [:tail early-g] :out_bus my-bus))
    (def sound-d (directional [:tail later-g] :in my-bus :out_bus 0)))

  (definst example [freq 440 gate 1 amp 1]
    (* (env-gen (adsr) :gate gate :action FREE)
       amp))

  (synth/ping :note 44)
  (synth/vintage-bass  :t 1)
  (synth/ks1 :note 48)

  (ctl sound-d :azim (/ math/PI 4))
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

(defonce *puncher (atom nil))

(defn host-init!
  ([]
   (host-init! "gamecode40"))
  ([gamecode]
   (let [session-id   gamecode
         client-id    (rand-int 999999)
         server-ip    "147.182.133.53"
         server-port  8080
         host (vn/make-hole-puncher server-ip server-port
                                    {:session-id session-id
                                     :client-id client-id
                                     :num-of-players 2
                                     :is-host true})]
     host)))

(defn client-init!
  ([]
   (client-init! "gamecode40"))
  ([gamecode]
   (let [session-id   gamecode
         client-id    (rand-int 999999)
         server-ip    "147.182.133.53"
         server-port  8080
         client (vn/make-hole-puncher server-ip server-port
                                      {:session-id session-id
                                       :client-id client-id})]
     client)))

(declare cube)

(def particles
  (memoize
   (fn [shader]
     (vp/with-arena-root
       (let [transforms (vp/arr 30000 vr/Matrix)
             {:keys [mesh material]} (vg/gen-cube {} 2)
             _ (def cube mesh)
             _ (assoc material :shader shader)
             _ (doseq [[idx transform] (mapv vector (range) transforms)]
                 (merge transform (vg/matrix-transform (vt/Translation
                                                        (mapv #(+ % (wobble-rand 0.5 (rand 0.25)))
                                                              [(- (* (mod idx 10) 0.5)
                                                                  2)
                                                               (- (* (mod idx 99) 0.1)
                                                                  0)
                                                               (- (* (mod idx 7) 0.4)
                                                                  1)]))
                                                       #_(vt/Rotation [0 0 0 1])
                                                       (vr.c/quaternion-from-euler (rand Math/TAU)
                                                                                   (rand Math/TAU)
                                                                                   (rand Math/TAU))
                                                       (vt/Scale (mapv #(Math/abs ^double (+ % (wobble-rand (* % 0.9) 10000)))
                                                                       [0.01 0.01 0.01])))))]
         [transforms material])))))

(defn ambisonic
  [sound-source pov-transform obj-transform]
  (let [d (vr.c/vector-3-distance
           (vg/matrix->translation pov-transform)
           (vg/matrix->translation obj-transform))
        [azim elev] (let [{:keys [x y z] :as _v} (-> obj-transform
                                                     (vr.c/matrix-multiply (vr.c/matrix-invert pov-transform))
                                                     vg/matrix->translation)]
                      (if (> z 0)
                        [(- (Math/atan2 x z))
                         (Math/atan2 y z)
                         _v]
                        [(Math/atan2 x z)
                         (Math/atan2 y z)
                         _v]))
        amp (if (zero? d)
              1
              (/ 1 (* d d)))]
    (sound
      (ctl sound-source :azim azim :elev elev :amp (* amp 100) :distance d))))

#_ (init)

(defn draw
  [w delta-time]
  (let [{:keys [render-texture shadowmap-shader dither-shader noise-blur-shader default-shader]}
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
              (def default-shader default-shader)
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
            (vf/with-each w [_ :vg/camera-active
                             camera vt/Camera
                             transform [vt/Transform :global]
                             sound-source-transform [:src (p :vg.gltf/sound_source) [vt/Transform :global]]]
              (ambisonic sound-d transform sound-source-transform)))
          (conj parent-e :vg.anim/stop))

        ;; FIXME: This `merge` takes a lot of CPU.
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
                        :vg.gltf.anim/sound_sourceAction
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
      (sound (mapv synth/ks1-demo
                   (repeatedly 3 #(+ (rand-int 20) 55))))
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
      (sound (synth/ks1 (+ (rand-int 20) 50))))

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
            (vn/send! @*puncher c-value {:entity e})))))

    ;; Receive network data.
    (->> (vn/update! @*puncher delta-time)
         (mapv (fn [{:keys [data entity-name]}]
                 (when (vp/pmap? data)
                   #_(println :RECEIVED entity-name data)
                   (let [c-eid (vf/eid w (vp/component data))]
                     (merge w {entity-name [data [:vg.sync/synced c-eid]]})
                     (vf/enable w entity-name [:vg.sync/synced c-eid]))))))


    #_ (init)

    ;; -- Drawing
    (let [shader (get shadowmap-shader vt/Shader)
          [transforms material] (particles shader)
          #_ #_mesh (get (w (vf/path [:my/model :vg.gltf/alphabet :vg.gltf/B :vg.gltf.mesh/data])) vr/Mesh)
          draw-scene (do (fn [w]
                           #_(vr.c/draw-grid 30 0.5)

                           (.setAtIndex (vp/mem (:locs shader))
                                        (vp/type->layout :int)
                                        (raylib/SHADER_LOC_MATRIX_MODEL)
                                        (int (vr.c/get-shader-location-attrib shader "instanceTransform")))
                           (vr.c/draw-mesh-instanced cube material transforms (count transforms))

                           (.setAtIndex (vp/mem (:locs shader))
                                        (vp/type->layout :int)
                                        (raylib/SHADER_LOC_MATRIX_MODEL)
                                        (int (vr.c/get-shader-location shader "matModel")))

                           (if (get-in w [:vg/debug :vg/enabled])
                             (vg/draw-debug w)
                             (vg/draw-scene w)))
                         #_(fn [w]
                             (vg/draw-debug w)
                             (vg/draw-scene w)))]

      (vg/draw-lights w #_(get default-shader vt/Shader) (get shadowmap-shader vt/Shader) draw-scene)

      (vf/with-each w [_ :vg/camera-active
                       camera vt/Camera]
        (vg/with-multipass (get render-texture vr/RenderTexture2D) {:shaders
                                                                    #_[(get default-shader vt/Shader)]
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
            (draw-scene w))

          #_(vr.c/gui-group-box (vr/Rectangle [330 330 200 100])
                                "Monster")

          #_(vr.c/gui-dummy-rec (vr/Rectangle [340 340 180 80])
                                "Que tu quer???????????\n???")

          #_(vr.c/gui-label (vr/Rectangle [340 340 180 80])
                            "Que tu quer?")))

      #_(merge w
               {:klk [(vf/is-a (vf/path [:my/model :vg.gltf/alphabet :vg.gltf/B]))]})
      #_(:klk2 w)
      #_(merge w
               {:klk2 [(vf/is-a (vf/path [:my/model :vg.gltf/alphabet :vg.gltf/C]))]})

      ;; -- Draw to the screen.
      (vg/with-drawing
        (vr.c/clear-background (vr/Color [255 20 100 255]))

        (vr.c/draw-texture-pro (:texture (get render-texture vr/RenderTexture2D))
                               (vr/Rectangle [0 0 screen-width (- screen-height)])
                               (vr/Rectangle [0 0  screen-width screen-height])
                               (vr/Vector2 [0 0]) 0 vg/color-white)

        #_(vg/with-camera (get-in w [:vg/camera-active vt/Camera])
            (draw-scene w))

        ;; ---- UI.
        (let [is-host-mem (vp/mem ::is-host (vp/bool* false))
              is-host (vp/p->value is-host-mem :boolean)
              expanded-mem (vp/mem ::gamecode-ui-expanded (vp/bool* false))
              expanded? (vp/p->value expanded-mem :boolean)

              x-offset 10
              y-offset 10]

          (if expanded?
            (do
              (vr/gui-text-input-box
               ::gamecode
               {:title "Gamecode"
                :message "Generate a gamecode (as a host) and\n share it with your friends"
                :rect [x-offset y-offset 350 170]
                :on-close (fn [_]
                            (sound (synth/ks1-demo :note 65))
                            (vp/set-mem expanded-mem (vp/bool* false)))
                :buttons [{:label (vr/gui-icon (raylib/ICON_STAR) "Gen")
                           :on-click (fn [mem]
                                       (vp/set-mem mem (str "gamecode-" (random-uuid))))}
                          {:label (vr/gui-icon (raylib/ICON_FILE_COPY) "Copy")
                           :on-click (fn [mem]
                                       (vr.c/set-clipboard-text mem))}
                          {:label (vr/gui-icon (raylib/ICON_FILE_PASTE) "Paste")
                           :on-click (fn [mem]
                                       (vp/set-mem mem (vp/->string (vr.c/get-clipboard-text))))}
                          {:label (vr/gui-icon (raylib/ICON_OK_TICK) "Submit")
                           :on-click (fn [mem]
                                       (let [gamecode (vp/->string mem)]
                                         (when (seq gamecode)
                                           (reset! *puncher
                                                   (if (vp/p->value is-host-mem :boolean)
                                                     (host-init! (vp/->string mem))
                                                     (client-init! (vp/->string mem)))))))}]})

              (vr.c/gui-panel (vr/Rectangle [x-offset (+ 175 y-offset) 350 100])
                              "Network Connection")

              (vr.c/gui-check-box (vr/Rectangle [(+ x-offset 10) (+ 220 y-offset) 30 30])
                                  "Connected?"
                                  (vp/bool* (vn/connected? @*puncher)))

              (vr.c/gui-toggle (vr/Rectangle [(+ 240 x-offset) (+ 220 y-offset) 100 30])
                               (vr/gui-icon (if is-host
                                              (raylib/ICON_ZOOM_ALL)
                                              (raylib/ICON_ZOOM_CENTER))
                                            "Host?")
                               is-host-mem))
            (when (pos? (vr.c/gui-button (vr/Rectangle [0 0 25 25])
                                         (vr/gui-icon (if (vn/connected? @*puncher)
                                                        (raylib/ICON_HEART)
                                                        (raylib/ICON_DEMON)))))
              (sound (synth/ks1-demo :note 70))
              (vp/set-mem expanded-mem (vp/bool* true)))))

        (vr.c/draw-fps 510 570)))))

#_(init)

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

  (vr.c/gui-load-style-sunny)

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

(comment

  (def cube (vg/gen-cube {:x 10 :y 10 :z 10} 4))

  (let [transforms (vp/arr 1 vr/Matrix)]
    (doseq [transform transforms]
      (merge transform (vg/matrix-transform (vt/Translation [1 0 0]) (vt/Rotation) (vt/Scale [1 1 1]))))
    (vr.c/draw-mesh-instanced (:mesh cube) (:material cube) transforms (count transforms))
    transforms)




  (def w2 (vf/make-world))

  (merge w2 {:a [:vf/prefab
                 {:c [:vf/prefab :d]}
                 :f]})

  (merge w2 {:a [{:c [(vf/slot-of :a)]}]})

  (merge w2 {:g [(vf/is-a :a)]})

  (vf/target (:g w2) (vf/path [:a :c]))

  ())

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))
  (vr/-main))
