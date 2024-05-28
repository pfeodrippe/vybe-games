(ns leo
  (:require
   [vybe.game :as vg]
   [vybe.raylib :as vr]
   [vybe.panama :as vp]
   [vybe.raylib.c :as vr.c]
   [vybe.flecs :as vf]
   [clojure.math :as math]
   [clojure.string :as str]
   [clojure.java.io :as io]
   #_[overtone.live :refer :all]
   #_[vybe.audio :as va]
   #_[clj-java-decompiler.core :refer [decompile disassemble]])
  (:import
   (org.vybe.flecs flecs)
   (org.vybe.raylib raylib)))

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

#_ (init)

(defn draw
  []
  (vp/with-arena _

    #_ (reset! vp/*default-arena (java.lang.foreign.Arena/ofShared))

    (let [{:keys [vf/world view-2 shadowmap-shader
                  dither-shader noise-blur-shader
                  depth-rts default-shader]}
          env

          w world
          _ (do (vg/run-reloadable-commands!)
                (vg/default-systems w)
                ;; For dev mode.
                (vf/progress w (vr.c/get-frame-time)))
          #_ #__ (do (def w w)
                     (def shadowmap-shader shadowmap-shader)
                     (def dither-shader dither-shader))

          #_ (init)]

      #_(when (vr.c/is-gamepad-available 0)
          (when (vr.c/is-gamepad-button-pressed 0 (raylib/GAMEPAD_BUTTON_RIGHT_FACE_UP))
            (println :aaa 2))
          #_(println :aaa (vr.c/get-gamepad-axis-movement 0 0)))

      ;; -- Animation
      (let [step (let [key #(vr.c/is-key-down %1)]
                   (cond
                     (key (raylib/KEY_W))
                     1.5

                     (key (raylib/KEY_S))
                     -1.5

                     :else
                     1))]
        (vf/with-each w [player vg/AnimationPlayer]
          (update player :current_time + (* (vr.c/get-frame-time) step))))

      (vf/with-each w [[_ node] [:vg.anim/target-node :*]
                       [_ c] [:vg.anim/target-component :*]
                       {:keys [timeline_count values timeline]} vg/AnimationChannel
                       player [:meta {:flags #{:up :cascade}}
                               vg/AnimationPlayer]
                       e :vf/entity
                       [_ n] [:vf/child-of :*]]
        #_(def e e)

        (comment

          (get e [:vf/child-of :*])

          (vf/with-each w [e :vf/entity,
                           c [:meta {:term {:src {:id (.id e)
                                                  #_ #_:name (vf/get-name e)}}
                                     #_ #_:flags #{:is-entity}}
                              :vg/channel]]
            e)

          (vf/with-each w [e :vf/entity,
                           c [:meta {:term {:src {:id (flecs/EcsIsName)
                                                  :name (vp/try-string "vf_gltf/model.vg_gltf_anim/CubeAction.vf/ANOM_29739")}}
                                     #_ #_:flags #{:is-entity}}
                              :vg/channel]]
            e)

          (vp/->string (vybe.flecs.c/ecs-entity-str w (.id e)))

          ())

        (when (not= n :vg.gltf.anim/Right)
          (let [values (vp/arr values timeline_count c)
                timeline (vp/arr timeline timeline_count :float)
                idx* (first (indices #(>= % (:current_time player)) timeline))
                idx (max (dec (or idx* (count timeline))) 0)]
            (if idx*
              (do (when (= c vg/Translation)
                    (let [d (vr.c/vector-3-distance
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
                      #_(ctl sound-d :azim azim :elev elev :amp amp :distance d))))
              (assoc player :current_time 0))

            (merge w {node [(nth values idx)]}))))

      #_ (init)

      ;; -- Keyboard
      (let [key #(vr.c/is-key-pressed %1)]
        (cond
          (key (raylib/KEY_SPACE))
          (let [new-entity (if (contains? (:vg/camera-active w) (vf/is-a :vg.gltf/CameraFar))
                             :vg.gltf/Camera
                             :vg.gltf/CameraFar)]
            (-> w
                (update :vg/camera-active disj (vf/is-a :*))
                (assoc :vg/camera-active [(vf/is-a new-entity)])))))

      ;; -- Drawing
      (vg/draw-lights w #_default-shader shadowmap-shader depth-rts)

      #_(init)

      (vg/with-multipass view-2 {:shaders [[noise-blur-shader {:u_radius (+ 1.0 (rand 1))}]
                                           [dither-shader {:u_offsets (vg/Vector3 (mapv #(* % 0.3)
                                                                                        [0.02 (+ 0.016 (wobble 0.005))
                                                                                         (+ 0.040 (wobble 0.01))]))}]]}
        (vr.c/clear-background (vr/Color "#A98B39"))
        (vg/with-camera #_(get-in w [:vg.gltf/Light vg/Camera])
                          (get-in w [:vg/camera-active vg/Camera])
                          (vg/draw-scene w)
                          #_(vg/draw-debug w)))

      #_(get (:vg.gltf/ball-path w) [vg/Transform :global])

      ;; Draw to the screen.
      (vg/with-drawing
        (vr.c/clear-background (vr/Color [255 20 100 255]))

        (vr.c/draw-texture-pro (:texture view-2)
                               (vr/Rectangle [0 0 600 -600]) (vr/Rectangle [0 0 600 600])
                               (vr/Vector2 [0 0]) 0 vg/color-white)

        #_(vr.c/clear-background (vr/Color "#A98B39"))
        #_(vg/with-camera (get-in w [:vg.gltf/Camera vg/Camera])
            (vg/draw-scene w)
            (vg/draw-debug w))

        (vr.c/draw-fps 510 570)))

    #_ (.close (vp/default-arena))))

#_(init)
#_(vr.c/set-target-fps 10)

(defn init
  []
  (when-not (vr.c/is-window-ready)
    (vr.c/set-config-flags (raylib/FLAG_MSAA_4X_HINT))
    (vr.c/init-window 600 600 "Opa")
    (vr.c/set-window-state (raylib/FLAG_WINDOW_UNFOCUSED))
    (vr.c/set-target-fps 60)
    #_(vr.c/set-target-fps 30)
    #_(vr.c/set-target-fps 10)
    #_(vr.c/set-target-fps 120)
    (vr.c/set-window-position 1120 200)
    (vr.c/clear-background (vr/Color [10 100 200 255]))
    (vr.c/draw-rectangle 30 50 100 200 (vr/Color [255 100 10 255]))
    (vr.c/draw-rectangle 300 50 100 200 (vr/Color [255 100 10 255])))

  (reset! env {})
  (swap! env merge {:vf/world (-> (vf/make-world)
                                  (vg/gltf->flecs :flecs (.getPath (io/resource "models.glb")))
                                  #_(vg/gltf->flecs :limbs "/Users/pfeodrippe/Downloads/models.glb"))})

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
