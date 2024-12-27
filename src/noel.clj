(ns noel
  (:require
   [vybe.flecs :as vf]
   [vybe.game :as vg]
   [vybe.raylib.c :as vr.c]
   [vybe.raylib :as vr]
   [vybe.type :as vt]
   [vybe.jolt :as vj]
   [vybe.audio :as va]
   #_[overtone.core :refer :all]
   #_[overtone.inst.synth :as synth]))

#_(definst simple-flute [freq 880
                       amp 0.5
                       attack 0.4
                       decay 0.5
                       sustain 0.8
                       release 1
                       gate 1
                       out 0]
  (let [env  (env-gen (adsr attack decay sustain release) gate :action FREE)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod2 (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        mod3 (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig mod2 mod3)]
    sig))

#_(when-not *compile-files*
  (va/audio-enable!)
  #_(eval '(require '[overtone.inst.synth :as synth])))

#_(va/sound
  (inst-fx! synth/ks1 #_vybe.game.system/directional
            fx-echo
            :note (+ (rand-int 3) 50)
            #_ #_:in (:bus synth/ks1))
  (def aaa
    (inst-fx! synth/ks1 vybe.game.system/directional
              :azim (- (/ 3.14 2)) :elev 0 :distance 400.0
              :in (:bus synth/ks1))))

#_(demo
    (clear-fx synth/ks1)
    (keys (synth/ks1))
    (let [v (synth/ks1 :note (+ (rand-int 3) 50)
                       :azim (- (/ 3.14 3)) :elev 400.2 :distance 400.0)]
      (ctl aaa :azim (- (/ 3.14 3)))))

#_(vf/defobserver cccc w
  [{:keys [contact-manifold body-1 body-2]} [:event vj/OnContactAdded]]
  (let [l (* (vr.c/vector-3-length
              (vr.c/vector-3-subtract (vj/linear-velocity body-1)
                                      (vj/linear-velocity body-2)))
             0.01)]

    (va/sound
     #_(merge w {(-> (get w (vybe.game.system/body-path body-1))
                     (get vt/Eid)
                     :id)
                 [:vg/sound-source
                  #_(-> (get w (vybe.game.system/body-path body-1))
                        (get vt/Eid)
                        :id)]})
     (cond
       (or (zero? (vj/motion-type body-1))
           (zero? (vj/motion-type body-2)))
       #_(synth/ks1 :note (+ (rand-int 3) 50)
                    :amp l)
       false

       (= (vj/motion-type body-1) (vj/motion-type body-2))
       #_(merge w {(vf/_)
                   [:vg/sound-source
                    [(vybe.panama/clone (get-in w [(-> (get w (vybe.game.system/body-path body-1))
                                                       (get vt/Eid)
                                                       :id)
                                                   vt/Transform]))
                     :global]]})
       #_(do #_(def body-1 body-1)

             (synth/ks1-demo :note (+ (rand-int 3) 90)
                             :amp l
                             #_(* (max (abs (:penetration_depth contact-manifold))
                                       0.02)
                                  20)))
       false

       :else
       #_(synth/ks1 :note (+ (rand-int 3) 70)
                    :amp l
                    #_(* (max (abs (:penetration_depth contact-manifold))
                              0.02)
                         20))
       false))))

#_(stop)

(defn draw
  [w delta-time]
  ;; For debugging
  (def w w)

  #_(cccc w)

  ;; Progress the systems (using Flecs).
  (vg/default-systems w)
  (vf/progress w delta-time)

  ;; Update physics (using Jolt).
  (vg/physics-update! w delta-time)

  ;; Add some lights (from the blender model).
  (vg/draw-lights w)

  ;; Render stuff into the screen (using Raylib).
  (vg/with-drawing-fx w (vg/fx-painting w)
    (vr.c/clear-background (vr/Color [255 20 100 255]))

    ;; Here we do a query for the active camera (it's setup when loading the model).
    (vf/with-query w [_ :vg/camera-active
                      camera vt/Camera]
      (vg/with-camera camera
        (vg/draw-scene w)))

    (vr.c/draw-fps 510 570)))

#_ (init)

(defn init
  []
  (let [w (vf/make-world)]
    ;; If you want to enable debugging (debug messages + clerk + flecs explorer),
    ;; uncomment line below.
    #_(vg/debug-init! w)

    (vg/start! w 600 600 #'draw
               (fn [w]
                 (-> w
                     ;; Load model (as a resource).
                     (vg/model :my/model (vg/resource "noel.glb")))))))
