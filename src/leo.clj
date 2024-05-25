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
   [overtone.live :refer :all]
   [vybe.audio :as va]
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
  [freq 20000, mul 0.5, out_bus 0]
  (out out_bus
       (* mul (sin-osc 260) (saw 3) 0.04)
       #_(play-buf 1 b (buf-rate-scale:ir b) :loop 1)
       #_(* mul (lpf (pink-noise 0.4) freq))))

(defsynth-load directional
  "resources/sc/compiled/directional.scsyndef")
#_ (def sound-d (directional [:tail later-g] :in my-bus :out_bus 0))

(comment

  (def b (sample "/Users/pfeodrippe/Library/Application Support/ATK/sounds/stereo/Aurora_Surgit-Dies_Irae.wav"))

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
  (let [{:keys [vf/world view-2 shadowmap-shader
                dither-shader noise-blur-shader
                depth-rts default-shader]}
        env

        w world
        _ (do (vg/run-reloadable-commands!)
              (vg/default-systems w)
              ;; For dev mode.
              (vf/progress w (vr.c/get-frame-time)))
        _ (do (def w w)
              (def shadowmap-shader shadowmap-shader)
              (def kuwahara-shader kuwahara-shader)
              (def dither-shader dither-shader))

        #_ (init)]

    ;; -- Animation
    (vf/with-each w [[_ node] [:vg.anim/target-node :*]
                     [_ c] [:vg.anim/target-component :*]
                     {:keys [timeline_count values timeline]} vg/AnimationChannel
                     player [:meta {:flags #{:up :cascade}}
                             vg/AnimationPlayer]]
      (let [values (vp/arr values timeline_count c)
            timeline (vp/arr timeline timeline_count :float)
            idx* (first (indices #(>= % (:current_time player)) timeline))
            idx (max (dec (or idx* (count timeline))) 0)
            step (let [key #(vr.c/is-key-down %1)]
                   (cond
                     (key (raylib/KEY_W))
                     1.5

                     (key (raylib/KEY_S))
                     -1.5

                     :else
                     0.3))]

        (if idx*
          (do (when (and (= c vg/Translation)
                         #_(zero? (mod idx* 20)))
                #_(println idx* [node c])
                (let [freq (+ 600 (* 30 idx*) #_(rand-int 187))
                      d (vr.c/vector-3-distance
                         (vg/matrix->translation (get-in w [:vf.gltf/Camera [vg/Transform :global]]))
                         (vg/matrix->translation (get-in w [:vf.gltf/Sphere [vg/Transform :global]])))
                      [azim elev] (let [{:keys [x y z]} (vr.c/vector-3-subtract
                                                         (vg/matrix->translation (get-in w [:vf.gltf/Camera [vg/Transform :global]]))
                                                         #_(-> (vg/Vector3 [0 0 -1])
                                                               (vr.c/vector-3-transform (get-in w [:vf.gltf/Camera [vg/Transform :global]])))
                                                         (vg/matrix->translation (get-in w [:vf.gltf/Sphere [vg/Transform :global]])))]
                                    (cond
                                      (< x 0)
                                      [(vr.c/atan-2 z x)
                                       (vr.c/atan-2 y x)]

                                      :else
                                      [(- (vr.c/atan-2 z x))
                                       (- (vr.c/atan-2 y x))]))
                      amp (/ 1 (* d d 1))]
                  (ctl sound-d :azim azim :elev elev :amp 2)))
              #_(update player :current_time + (* (vr.c/get-frame-time) step)))
          #_(update player :current_time + (* (vr.c/get-frame-time) 0.01))
          (assoc player :current_time 0))
        (merge w {node [(nth values idx)]})))

    #_ (init)

    ;; -- Keyboard
    #_(let [key #(vr.c/is-key-pressed %1)]
        (cond
          (key (raylib/KEY_W))
          (l/demo 0.1 [(l/sin-osc 800)
                       (l/sin-osc 800)])
          #_(update-in w [:vf.gltf/Armature vg/Translation :z] + 0.35)

          (key (raylib/KEY_S))
          (l/demo 0.1 [(l/sin-osc 400)
                       (l/sin-osc 400)])
          #_(update-in w [:vf.gltf/Armature vg/Translation :z] - 0.2)))

    ;; -- Drawing
    (vg/draw-lights w #_default-shader shadowmap-shader depth-rts)

    #_(init)

    (vg/with-multipass view-2 {:shaders [[noise-blur-shader {:u_radius (+ 1.0 (rand 1))}]
                                         [dither-shader {:u_offsets (vg/Vector3 (mapv #(* % 0.3)
                                                                                      [0.02 (+ 0.016 (wobble 0.005))
                                                                                       (+ 0.040 (wobble 0.01))]))}]]}
      (vr.c/clear-background (vr/Color "#A98B39"))
      (vg/with-camera #_(get-in w [:vf.gltf/Light vg/Camera])
        (get-in w [:vf.gltf/Camera vg/Camera])
        (vg/draw-scene w)
        (vg/draw-debug w)))

    #_(get (:vf.gltf/ball-path w) [vg/Transform :global])

    ;; Draw to the screen.
    (vg/with-drawing
      (vr.c/clear-background (vr/Color [255 20 100 255]))

      (vr.c/draw-texture-pro (:texture view-2)
                             (vr/Rectangle [0 0 600 -600]) (vr/Rectangle [0 0 600 600])
                             (vr/Vector2 [0 0]) 0 vg/color-white)

      #_(vr.c/clear-background (vr/Color "#A98B39"))
      #_(vg/with-camera (get-in w [:vf.gltf/Camera vg/Camera])
          (vg/draw-scene w)
          (vg/draw-debug w))

      (vr.c/draw-fps 510 570))))

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
