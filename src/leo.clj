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
   [vybe.util :as vy.u]
   [vybe.type :as vt]
   [vybe.network :as vn]
   #_[vybe.audio :as va]
   [vybe.math :as vm]
   #_[overtone.core :refer :all])
  (:import
   (org.vybe.flecs flecs)
   (org.vybe.raylib raylib)
   (org.vybe.jolt jolt)
   (java.lang.foreign MemorySegment ValueLayout)
   (com.github.psambit9791.jdsp.transform FastFourier)))

(set! *warn-on-reflection* true)

;; Enable audio and require synth after it.
#_(when-not *compile-files*
    (va/audio-enable!)
    (eval '(require '[overtone.inst.synth :as synth])))

#_(init)

#_ (va/sound (demo 0.2 (sin-osc 400)))

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

#_(defonce init-sound
  (va/sound

    (stop)

    (defonce my-bus
      (audio-bus 1))

    (defonce main-g (group "get-on-the-bus main"))
    (defonce early-g (group "early birds" :head main-g))
    (defonce later-g (group "latecomers" :after early-g))

    (def bass-drum
      (synth-load (vy.u/app-resource "resources/sc/compiled/sonic-pi-sc808_bassdrum.scsyndef")))
    #_ (bass-drum)

    #_(defonce b (sample "~/Downloads/wrapping-paper-rustle-72405.mp3"))

    (defsynth my-noise
      [freq 300, mul 0.5, out_bus 0]
      (out out_bus
           #_(* mul (sin-osc 260) (saw 3) 0.04)
           #_(play-buf 1 b (buf-rate-scale:ir b) :loop 1)
           (* mul (lpf (* 0.8 (pink-noise)) 500))))

    (def directional
      (synth-load (vy.u/app-resource "com/pfeodrippe/vybe/overtone/directional.scsyndef"))
      #_(synth-load (app-resource "/resources/sc/compiled/directional.scsyndef")))

    (my-noise [:tail early-g] :out_bus my-bus)
    (def sound-d (directional [:tail later-g] :in my-bus :out_bus 0))))

(declare music-bg my-music)

(comment

  (def kk (sample "resources/audio/keyboard.mp3"))
  (def kk (sample "resources/audio/speech_1.mp3"))

  (definst my-music
    [rate 1.0]
    (* 8 (play-buf 2 kk (buf-rate-scale:ir kk) :loop 1 :rate rate)))

  (my-music)

  (do (clear-fx my-music)
      (def music-bg (inst-fx! my-music
                              (defsynth my-fx
                                [bus 0 freq 240]
                                (let [input (in bus)
                                      bpf-snd (bpf input freq (/ freq 1000.0))
                                      snd (select (>= freq 100)
                                                  [bpf-snd
                                                   (+ (* (/ (- 2400 freq) 2000.0)
                                                         0.1
                                                         (lf-noise0 2800))
                                                      bpf-snd)
                                                   bpf-snd])]
                                  (replace-out bus (+ snd bpf-snd)))))))

  (stop)

  (init)







  (boot-server)

  (def b (sample "/Users/pfeodrippe/Library/Application Support/ATK/sounds/stereo/Aurora_Surgit-Dies_Irae.wav"))

  (my-noise)

  (demo 1 [(lpf (pink-noise 0.4) 400)
           (lpf (pink-noise 0.4) 400)])

  (demo 1 (fx-echo (pink-noise 0.4) 1 100 1))

  (adsr 1 0.7)

  (do
    (stop)
    (def aaa (my-noise [:tail early-g] :out_bus my-bus))
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

#_ (init)

(def screen-width 600)
(def screen-height 600)

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

(defonce particles
  (memoize
   (fn [shader]
     (vp/with-arena-root
       (let [transforms (vp/arr 2000 vr/Matrix)
             {:keys [mesh material]} (vg/gen-cube {:x 1 :y 0.5 :z 0.3} 2)
             _ (def cube mesh)
             _ (assoc material :shader shader)
             _ (doseq [[idx transform] (mapv vector (range) transforms)]
                 (merge transform (vm/matrix-transform (vt/Translation
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
                                                                       [0.03 0.03 0.03]
                                                                       #_[0.05 0.05 0.05]
                                                                       #_[0.1 0.1 0.1])))))]
         [transforms material])))))

(defn ambisonic
  [sound-source source-transform target-transform]
  (let [d (vr.c/vector-3-distance
           (vm/matrix->translation target-transform)
           (vm/matrix->translation source-transform))
        [azim elev] (let [{:keys [x y z] :as _v} (-> source-transform
                                                     (vr.c/matrix-multiply (vr.c/matrix-invert target-transform))
                                                     vm/matrix->translation)]
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
    #_(va/sound
      (ctl sound-source :azim azim :elev elev :amp (* amp 10) :distance d))))

#_ (init)

(defn- p
  [& ks]
  (vf/path (vec (concat [:my/model] ks))))

;; ---- Raycast.
(vf/defobserver on-raycast-click w
  [_ [:event :vg.raycast/on-click]
   {:keys [id]} [:filter vt/Eid]]
  #_(va/sound (mapv synth/ks1-demo
                    (repeatedly 3 #(+ (rand-int 20) 55))))
  (let [c (fn [k] (vf/path [id k]))]
    (merge w
           (if (contains? (w (c :vg.gltf.anim/CubeDown)) :vg/selected)
             {(c :vg.gltf.anim/CubeDown) [(vf/del :vg.anim/active) (vf/del :vg/selected)]
              (c :vg.gltf.anim/CubeUp) [:vg.anim/active :vg/selected]}
             {(c :vg.gltf.anim/CubeDown) [:vg.anim/active :vg/selected]
              (c :vg.gltf.anim/CubeUp) [(vf/del :vg.anim/active) (vf/del :vg/selected)]}))))

(vf/defobserver on-raycast-hover w
  [_ [:event :vg.raycast/on-hover]
   body [:filter vj/VyBody]]
  (merge w {(p :vg.gltf/Sphere)
            [(assoc (-> body vj/position vt/Translation)
                    :y (+ (:y (:max (vj/world-bounds body)))
                          0.3))]}))

(vf/defobserver on-raycast-enter _w
  [_ [:event :vg.raycast/on-enter]
   _body [:filter vj/VyBody]]
  #_(va/sound (synth/ks1 (+ (rand-int 20) 50))))

(vf/defobserver on-raycast-leave w
  [_ [:event :vg.raycast/on-leave]]
  (merge w {(p :vg.gltf/Sphere) [(vt/Translation [-100 -100 -100])]}))

(defn- key-down?
  [k]
  (vr.c/is-key-down k))

(defn- key-pressed?
  [k]
  (vr.c/is-key-pressed k))

(vf/defsystem update-sound-sources _w
  [_ :vg/sound-source
   source-transform [vt/Transform :global]
   _ [:src '?e :vg/camera-active]
   target-transform [:src '?e [vt/Transform :global]]]
  #_(va/sound (ambisonic sound-d source-transform target-transform)))

(defn update-jolt-meshes
  [w]
  (let [phys (get-in w [(vg/root) vj/PhysicsSystem])]
    ;; Update jolt meshes (for debugging).
    (merge w
           (->> (vj/bodies-active phys)
                #_(vj/bodies phys)
                (keep (fn [body]
                        (let [position (vj/position body)
                              rotation (vj/rotation body)
                              translation (vt/Translation position)]
                          (if (< (:y translation) -20)
                            ;; We will delete the entity itself if it's far away.
                            [(vg/body-path body) (vf/del)]
                            ;; Update the entity.
                            [(vg/body-path body)
                             [translation (vt/Rotation rotation)
                              (vt/Scale [1 1 1])
                              vt/Transform [vt/Transform :global]]]))))
                (into {}))

           #_(init))))

(defn network-handler
  [w delta-time]
  (conj (vf/ent w vt/Translation) :vg/networked)
  (conj (vf/ent w vt/Rotation) :vg/networked)
  (conj (vf/ent w vt/Scale) :vg/networked)

  ;; Sync.
  (vf/with-query w [_ :vg/networked
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
                   (vf/enable w entity-name [:vg.sync/synced c-eid])))))))

(defn input-handler
  [w]
  (let [phys (get-in w [(vg/root) vj/PhysicsSystem])
        cubes! (fn [w]
                 (merge w
                        (->> (range 10 24)
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
                                       [(vg/body-path body) [mesh material body phys :vg.gltf.scene/main_scene]])))
                             (into {}))))
        cam :vg.gltf/CameraFar #_:vg.gltf/Camera.001]
    (cond
      (key-pressed? (raylib/KEY_C))
      (cubes! w)

      (key-pressed? (raylib/KEY_D))
      (merge w {:vg/debug (if (get-in w [:vg/debug :vg/enabled])
                            [(vf/del :vg/enabled)]
                            [:vg/enabled])})

      (key-pressed? (raylib/KEY_SPACE))
      (vf/with-query w [_ :vg/camera-active
                        e :vf/entity]
        (if (= e (w (p cam)))
          (assoc w (p :vg.gltf/Camera) [:vg/camera-active])
          (assoc w (p cam) [:vg/camera-active])))

      (key-pressed? (raylib/KEY_M))
      (-> w
          (merge
           {(p cam :vg.gltf.anim/CameraFarAction) [:vg.anim/active]})))))

#_(defn input-monster
    [w]
    (let [key-down? #(vr.c/is-key-down %1)]
      (when (key-down? (raylib/KEY_UP))
        (-> w
            (update-in [(p :vg.gltf/monster_parent :vg.gltf/monster) vt/Translation :z] + 0.018)))

      (when (key-down? (raylib/KEY_DOWN))
        (-> w
            (update-in [(p :vg.gltf/monster_parent :vg.gltf/monster) vt/Translation :z] - 0.018)))

      (when (key-down? (raylib/KEY_I))
        (-> w
            (update-in [(p :vg.gltf/monster_parent.001 :vg.gltf/monster.001) vt/Translation :z] + 0.018)))

      (when (key-down? (raylib/KEY_K))
        (-> w
            (update-in [(p :vg.gltf/monster_parent.001 :vg.gltf/monster.001) vt/Translation :z] - 0.018)))))

(defn render-ui
  []
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
                      #_(va/sound (synth/ks1-demo :note 65))
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
        #_(va/sound (synth/ks1-demo :note 70))
        (vp/set-mem expanded-mem (vp/bool* true))))))

(defonce transform-identity
  (vm/matrix-transform
   (vt/Translation [0 0 0])
   (vt/Rotation [0 0 0 1])
   (vt/Scale [1 1 1])))

(def char->name
  {\, :vg.gltf/comma
   \. :vg.gltf/dot
   \; :vg.gltf/semicolon
   \: :vg.gltf/colon
   \( :vg.gltf/parenthesis_left
   \) :vg.gltf/parenthesis_right
   \" :vg.gltf/quote_left
   \! :vg.gltf/exclamation
   \? :vg.gltf/interrogation})

(defn- draw-text-3d-meshes
  [w mat text translation-vec]
  (let [translation (vt/Translation translation-vec)
        {:keys [x y z]} translation]
    (loop [[c & text-rest] text
           x-idx 0.0
           y-idx 0.0]
      (when c
        (if-let [char-ent (cond
                            (Character/isLetter ^Character c)
                            (w (p :vg.gltf/alphabet
                                  (keyword "vg.gltf" (str/upper-case c))
                                  :vg.gltf.mesh/data))

                            (Character/isDigit ^Character c)
                            (w (p :vg.gltf/digits_punctuation
                                  (keyword "vg.gltf" (str "d_" c))
                                  :vg.gltf.mesh/data))

                            :else
                            (when-let [c-name (char->name c)]
                              (w (p :vg.gltf/digits_punctuation c-name :vg.gltf.mesh/data))))]
          (let [mesh (get-in char-ent [vr/Mesh])
                r (fn custom-rand
                    ([v]
                     (custom-rand v 0.01))
                    ([v factor]
                     (+ v (wobble-rand factor (* (math/cos (* (+ x-idx y-idx) factor)) 1.0)))))
                lower? (^[char] Character/isLowerCase c)
                new-transform (-> transform-identity
                                  (vr.c/matrix-multiply (vr.c/matrix-translate (* (+ x x-idx)
                                                                                  0.6)
                                                                               (+ y y-idx (if lower?
                                                                                            -0.32
                                                                                            0))
                                                                               z)))
                transform (cond->> (-> new-transform
                                       (vr.c/matrix-multiply (vr.c/matrix-rotate (vm/matrix->translation new-transform)
                                                                                 (r 0 0.15))))
                            true
                            (vr.c/matrix-multiply (vr.c/matrix-scale 0.7 (if lower? 0.5 1) 1)))]
            (vr.c/draw-mesh mesh mat transform)
            (recur text-rest (inc x-idx) y-idx))
          (case c
            \newline (recur text-rest 0.0 (- y-idx 1.3))

            (recur text-rest (inc x-idx) y-idx)))))))

(defn draw-text-3d
  [w shader texts-and-translations]
  (let [mat (get-in (w (p :vg.gltf/alphabet :vg.gltf/G :vg.gltf.mesh/data))
                    [vr/Material])]
    (assoc mat :shader shader)
    (doseq [[text translation-vec {:keys [scale] :or {scale 1.0}}] texts-and-translations]
      (vr.c/rl-push-matrix)
      (vr.c/rl-scalef scale scale scale)
      (draw-text-3d-meshes w mat text translation-vec)
      (vr.c/rl-pop-matrix))))

#_(init)

#_(defn get-data
  []
  (let [{:keys [arr timeline]} (last @va/*buffers)
        data (->> (mapv vector arr timeline)
                  (sort-by last)
                  (mapv first))
        fft (FastFourier. (double-array data))]
    (.transform fft)
    {:data data
     :fft (.getMagnitude fft true)}))

(defn render
  [{:keys [::vg/shader-mixer]
    :as w}]
  (let [shader (vg/->shader w ::vg/shader-shadowmap)
        [transforms material] (particles shader)
        render-texture-2 (vg/rt-get ::render-texture-2 screen-width screen-height)
        draw-scene (fn [w]
                     ;; Particles.
                     (vg/set-uniform shader {:shaderType 1})
                     (.setAtIndex (vp/mem (:locs shader))
                                  (vp/type->layout :int)
                                  (raylib/SHADER_LOC_MATRIX_MODEL)
                                  (int (vr.c/get-shader-location-attrib shader "instanceTransform")))
                     (vr.c/draw-mesh-instanced cube #_(get-in (w (p :vg.gltf/alphabet :vg.gltf/G :vg.gltf.mesh/data))
                                                              [ vr/Mesh])
                                               material transforms (count transforms))

                     ;; Model.
                     (vg/set-uniform shader {:shaderType 0})
                     (.setAtIndex (vp/mem (:locs shader))
                                  (vp/type->layout :int)
                                  (raylib/SHADER_LOC_MATRIX_MODEL)
                                  (int (vr.c/get-shader-location shader "matModel")))

                     ;; 3d Text.
                     #_(draw-text-3d w (get shadowmap-shader vt/Shader)
                                     [["Pito. Co:\nGuima;raes\nFeodrippe" [-2.5 10 4]]
                                      ["Hey! OMG,\nWhat (The)\nHell?\nLOL. Ma! Que cosa\nAB012 \"345\" 6789" [-3.5 5 2]]
                                      #_["Hey let's \nstart a new\ngame" [-5.5 10 2] {:scale 0.8}]
                                      #_[(str "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n"
                                              "recur arg for primitive\nlocal: y_idx is not\n")
                                         [-9.5 30 2]
                                         {:scale 0.3}]]
                                     #_[["AABCDEFGHI" [-3.5 6 2]]
                                        ["AABCDEFGHI" [-3.5 -1 5]]
                                        ["JKLMMNOPQR" [-3.5 4 1]]
                                        ["JKLMMNOPQR" [-3.5 8 1]]
                                        ["JKLMMNOPQR" [-1.5 1 1]]
                                        ["JKLMMNOPQR" [1.5 1 1]]
                                        ["JKLMMNOPQR" [-3.5 8 4]]
                                        ["JKLMMNOPQR" [-3.5 2 -4]]
                                        ["JKLMMNOPQR" [-3.5 10 1]]
                                        ["STUVWXYZ"  [-3.5 2 0]]
                                        ["STUVWXYZ"  [-5.5 4 0]]

                                        ["AABCDEFGHI" [-3.5 6 2]]
                                        ["AABCDEFGHI" [-3.5 -1 5]]
                                        ["JKLMMNOPQR" [-3.5 4 1]]
                                        ["JKLMMNOPQR" [-3.5 8 1]]
                                        ["JKLMMNOPQR" [-1.5 1 1]]
                                        ["JKLMMNOPQR" [1.5 1 3]]
                                        ["JKLMMNOPQR" [-3.5 8 4]]
                                        ["JKLMMNOPQR" [-3.5 2 -4]]
                                        ["JKLMMNOPQR" [-3.5 10 1]]
                                        ["STUVWXYZ"  [-3.5 2 3]]
                                        ["STUVWXYZ"  [-5.5 4 5]]])

                     (if (get-in w [:vg/debug :vg/enabled])
                       (vg/draw-debug w)
                       (vg/draw-scene w {:scene :vg.gltf.scene/main_scene})))]

    ;; We don't need to run this everytime, we just have it here because it's convenient.
    (-> (w (p :vg.gltf/tv :vg.gltf/screen :vg.gltf.mesh/data))
        (get vr/Material)
        (vr/material-get (raylib/MATERIAL_MAP_DIFFUSE))
        (assoc-in [:texture] (:texture (vg/->rt w ::vg/render-texture))))

    (let [[camera] (vf/with-query w [_ :vg/camera-active
                                     camera vt/Camera]
                     camera)]

      (vf/disable (w (p :vg.gltf/dark_world)))

      ;; For the track.
      (vg/draw-lights w {:shader ::vg/shader-shadowmap :draw draw-scene :scene :vg.gltf.scene/track_scene})
      (vg/with-fx w {:flip-y true}
        (vr.c/clear-background (vr/Color [5 5 5 255]))
        (vg/with-camera (get (w (p :vg.gltf/track_camera)) vt/Camera)
          (vg/draw-scene w {:scene :vg.gltf.scene/track_scene}))

        (vr.c/gui-group-box (vr/Rectangle [330 330 200 100]) "Monster")
        (vr.c/gui-dummy-rec (vr/Rectangle [340 340 180 80]) "Que tu quer???????????\n???"))

      #_ (init)

      ;; General.
      (vg/draw-lights w {:shader ::vg/shader-shadowmap :draw draw-scene :scene :vg.gltf.scene/main_scene})
      #_(vg/draw-lights w {:shader ::vg/shader-default :draw draw-scene :scene :vg.gltf.scene/main_scene})
      (vg/with-fx w {:shaders
                     [[::vg/shader-noise-blur
                       {:u_radius (+ 1.0
                                     #_(* (vr.c/vector-3-length velocity) 0.1)
                                     (rand 1))}]

                      [::vg/shader-dither
                       {:u_offsets (vt/Vector3 (mapv #(* % (+ 0.6
                                                              (wobble 0.3)))
                                                     [0.02 (+ 0.016 (wobble 0.01))
                                                      (+ 0.040 (wobble 0.01))]))
                        #_ #_:u_radius 0.1}]]}

        #_(va/sound (ctl  music-bg :freq (vr.c/remap (get-in (w (p :vg.gltf/Cube))
                                                             [vt/Translation
                                                              :y])
                                                     -0.058608275 0.096363540
                                                     2400 1000)))

        #_(va/sound (ctl my-music :rate (vr.c/remap (get-in (w (p :vg.gltf/Cube))
                                                            [vt/Translation
                                                             :y])
                                                    -0.058608275 0.096363540
                                                    1.0 (/ (math/pow 2 11/12)
                                                           2))))

        #_(vr.c/clear-background (vr/Color "#000000")
                               #_(vr/Color "#A98B39"))
        (vg/with-camera camera
          (draw-scene w))

        #_(vr.c/gui-group-box (vr/Rectangle [330 330 200 100])
                              "Monster")

        #_(vr.c/gui-dummy-rec (vr/Rectangle [340 340 180 80])
                              "Que tu quer???????????\n???"))

      ;; General again.
      (vg/with-fx w {:rt render-texture-2
                     :shaders [[::vg/shader-noise-blur {:u_radius (+ 1.0
                                                                     #_(* (vr.c/vector-3-length velocity) 0.1)
                                                                     (rand 1))}]

                               [::vg/shader-dither {:u_offsets (vt/Vector3 (mapv #(* % (+ 0.6
                                                                                          (wobble 0.3)))
                                                                                 [0.02 (+ 0.016 (wobble 0.01))
                                                                                  (+ 0.040 (wobble 0.01))]))
                                                    #_ #_:u_radius 0.5}]

                               [::vg/shader-edge-2d {:edge_fill 1.0}]]}

        (vr.c/clear-background (vr/Color "#A98B39"))

        (vf/enable (w (p :vg.gltf/dark_world)))
        (vg/with-camera camera
          (draw-scene w)))

      ;; Mix render textures.
      (vg/with-fx w {:rt render-texture-2
                     :shaders [[::vg/shader-mixer {:u_fill (vr.c/remap (get-in (w (p :vg.gltf/Cube))
                                                                               [vt/Translation :y])
                                                                       -0.058608275 0.096363540
                                                                       -0.5 1)
                                                   :u_time (vr.c/get-time)}]]}
        (let [tex-id (-> render-texture-2 :texture :id)]
          (vr.c/rl-active-texture-slot tex-id)
          (vr.c/rl-enable-texture tex-id)
          (vg/set-uniform (get shader-mixer vt/Shader) {:texture1 tex-id}))

        ;; This will be texture0.
        (vr.c/draw-texture-pro (:texture (vg/->rt w ::vg/render-texture))
                               (vr/Rectangle [0 0 screen-width (- screen-height)])
                               (vr/Rectangle [0 0  screen-width screen-height])
                               (vr/Vector2 [0 0]) 0 vg/color-white))

      ;; -- Draw to the screen.
      (vg/with-drawing
        (vr.c/clear-background (vr/Color [255 20 100 255]))

        (vr.c/draw-texture-pro (:texture render-texture-2)
                               (vr/Rectangle [0 0 screen-width (- screen-height)])
                               (vr/Rectangle [0 0  screen-width screen-height])
                               (vr/Vector2 [0 0]) 0 vg/color-white)

        #_(vg/with-camera camera
          (draw-scene w))

        (render-ui)

        (vr.c/draw-fps 510 570)))))

#_ (vy.u/debug-set! true)
#_ (vr.c/set-target-fps 480)
#_ (vr.c/set-target-fps 240)
#_ (vr.c/set-target-fps 60)

#_(init)

(defn draw
  [w delta-time]
  (def w w)
  (let [phys (get-in w [(vg/root) vj/PhysicsSystem])]

    ;; This will set and reevaluate the default systems (you can call this
    ;; during setup if you are not going to modify any of these later).
    (vg/default-systems w)

    ;; Observers.
    (on-raycast-click w)
    (on-raycast-enter w)
    (on-raycast-leave w)
    (on-raycast-hover w)

    ;; Systems.
    (update-sound-sources w)

    ;; Normal functions.
    (update-jolt-meshes w)
    (vf/with-deferred w
      (vj/update! phys delta-time))
    (network-handler w delta-time)
    (input-handler w)
    #_(input-monster w)

    ;; Progress world by running the systems.
    (vf/progress w delta-time)

    ;; -- Trigger some animations continuously so they never stop.
    (vf/with-query w [_ [:or
                         :vg.gltf.anim/my-cubeAction.005
                         :vg.gltf.anim/my-cubeAction.004
                         :vg.gltf.anim/sound_sourceAction
                         :vg.gltf.anim/pilot_axisAction.001
                         :vg.gltf.anim/pilot_axisAction.002
                         :vg.gltf.anim/pilot_axisAction.003
                         :vg.gltf.anim/pilot_axisAction.004
                         #_:vg.gltf.anim/ProjectAction]
                      _ :vg/animation
                      e :vf/entity]
      (conj e :vg.anim/active))

    ;; Draw... things.
    (render w)
    #_(println (.*state (vp/default-arena)))))

#_ (init)
#_ (vr/t (vg/debug-init! w))

(defn init
  []
  (let [w (vf/make-world)]
    ;; If you want to enable debugging (debug messages + clerk + flecs explorer),
    ;; uncomment line below.
    #_ (vg/debug-init! w)

    (vg/start! w {:screen-size [screen-width screen-height]
                  :draw-var #'draw
                  :init-fn  (fn [w]
                              (vr.c/gui-load-style-sunny)
                              (-> w
                                  (merge {#_ #_ #_ #_:render-texture [(vr/RenderTexture2D (vr.c/load-render-texture screen-width screen-height))]
                                          :render-texture-2 [(vr/RenderTexture2D (vr.c/load-render-texture screen-width screen-height))]
                                          :vg.sync/synced [(flecs/EcsPairIsTag) (flecs/EcsCanToggle)]})
                                  ;; `models.glb` comes from https://www.icloud.com/iclouddrive/03bfmWIccIucY5aa4j4CpCvwA#leo
                                  (vg/model :my/model (vy.u/extract-resource "models.glb"))))
                  :window-name "Leo"})))

#_ (init)

(defn -main
  [& _args]
  ;; We start `init` in a future so it's out of the main thread,
  ;; `vr/-main` will be in the main thread and it will loop the game draw
  ;; function for us.
  (future (init))
  (vr/-main))

#_(defn aa []
    (let [{:keys [data fft]} (get-data)
          ;; Get the peak of the time data.
          max-data (apply max (take 3900 data))
          max-idx (.indexOf data max-data)
          data (->> data
                    (drop max-idx)
                    ;; Normalize.
                    (map #(/ % max-data))
                    (partition 2 1)
                    vec)
          ;; Freq.
          fft (vec (partition 2 1 fft))]

      ;; Time domain.
      (doseq [idx (range 0 (min 105 (count data)))]
        (let [[v1 v2] (get data idx)]
          (vr.c/draw-line-ex (vt/Vector2 [(+ (* idx 5) 20)
                                          (- 80 (* v1 50))])
                             (vt/Vector2 [(+ (* (inc idx) 5) 20)
                                          (- 80 (* v2 50))])
                             2
                             (vr/Color [255 200 155 255]))))

      ;; Frequency domain.
      (doseq [idx (range (count fft))]
        (let [[f1 f2] (get fft idx)]
          (vr.c/draw-line-ex (vt/Vector2 [(+ (* idx 0.13) 20)
                                          (- 200 (* f1 1))])
                             (vt/Vector2 [(+ (* (inc idx) 0.13) 20)
                                          (- 200 (* f2 1))])
                             2
                             (vr/Color [200 255 155 255]))))))
#_(aa)
