(ns noel
  (:require
   [vybe.flecs :as vf]
   [vybe.game :as vg]
   [vybe.raylib.c :as vr.c]
   [vybe.raylib :as vr]
   [vybe.type :as vt]
   [vybe.jolt :as vj]
   [vybe.audio :as va]))

(when-not *compile-files*
  (va/audio-enable!)
  (eval '(require '[overtone.inst.synth :as synth])))

#_ (init)

(vf/defobserver cccc _w
  [{:keys [contact-manifold body-1 body-2]} [:event vj/OnContactAdded]]
  (let [l (* (vr.c/vector-3-length
              (vr.c/vector-3-subtract (vj/linear-velocity body-1)
                                      (vj/linear-velocity body-2)))
             0.01)]
    (va/sound
      (cond
        (or (zero? (vj/motion-type body-1))
            (zero? (vj/motion-type body-2)))
        (synth/ks1 :note (+ (rand-int 3) 50)
                        :amp l)

        (= (vj/motion-type body-1) (vj/motion-type body-2))
        (synth/ks1-demo :note (+ (rand-int 3) 90)
                        :amp l
                        #_(* (max (abs (:penetration_depth contact-manifold))
                                  0.02)
                             20))

        :else
        (synth/ks1 :note (+ (rand-int 3) 70)
                        :amp l
                        #_(* (max (abs (:penetration_depth contact-manifold))
                                  0.02)
                             20))))))

#_(stop)

(defn draw
  [w delta-time]
  ;; For debugging
  (def w w)

  (cccc w)

  ;; Progress the systems (using Flecs).
  #_(vg/default-systems w)
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
