# Clojure Game Development Reference

## Raylib Drawing Functions

### Shapes
```clojure
;; Triangles
(vr.c/draw-triangle point1 point2 point3 color)

;; Circles
(vr.c/draw-circle center radius color)          ;; Filled
(vr.c/draw-circle-lines center radius color)    ;; Outline

;; Lines
(vr.c/draw-line-ex point1 point2 thickness color)

;; Rectangles
(vr.c/draw-rectangle-pro rect origin rotation color)
```

### Colors
```clojure
;; RGB + Alpha (0-255)
(vr/Color [r g b a])

;; Common patterns
(vr/Color [255 255 255 255])  ;; White, opaque
(vr/Color [0 0 0 100])         ;; Black, semi-transparent
(vr/Color [210 190 200 255])  ;; Pink, opaque
```

## Animation Frequencies

### Recommended Frequencies (Hz)
- **Subtle pulse**: 1.0-2.0 Hz (one cycle per 1-2 seconds)
- **Normal animation**: 2.0-3.0 Hz (smooth, noticeable)
- **Fast twitch**: 3.0-5.0 Hz (urgent, frenetic)
- **Slow sweep**: 0.5 Hz (grand, majestic)

### Frequency Formula
```clojure
(let [frequency 2.0  ;; Hz
      time (vr.c/get-time)
      oscillation (Math/sin (* time frequency Math/PI))]
  ;; oscillation ranges from -1 to 1
)
```

## Vector Types

```clojure
(vt/Vector2 [x y])           ;; 2D point
(vt/Vector3 [x y z])         ;; 3D point
(vt/Vector4 [x y z w])       ;; Quaternion or RGBA
(vt/Rotation [x y z w])      ;; Quaternion rotation
(vt/Translation [x y z])     ;; 3D position
```

## Quaternion Rotations

### Common Axes

**Y-axis (vertical spin):**
```clojure
[0 (sin(θ/2)) 0 (cos(θ/2))]
```

**X-axis (pitch):**
```clojure
[(sin(θ/2)) 0 0 (cos(θ/2))]
```

**Z-axis (roll):**
```clojure
[0 0 (sin(θ/2)) (cos(θ/2))]
```

Where θ = angle in radians

## Color Interpolation

```clojure
(let [base [r1 g1 b1]
      target [r2 g2 b2]
      t 0.5  ;; 0.0 = base, 1.0 = target
      result (mapv #(+ %1 (* (- %2 %1) t)) base target)]
  ;; result is interpolated color
)
```

## Entity Component System (ECS)

### Entity Access
```clojure
(let [entity (w :entity-key)]
  entity)

(get entity vt/Rotation)      ;; Get component
(assoc entity vt/Rotation ...)  ;; Set component
```

### Entity Update
```clojure
(update w :entity-key 
  (fn [ent] (assoc ent vt/Rotation rotation)))

(merge w {:entity-key [component1 component2]})
```

## Screen Coordinates

```clojure
;; Get screen dimensions
(vr.c/get-screen-width)
(vr.c/get-screen-height)

;; Calculate center
(let [center-x (/ (vr.c/get-screen-width) 2.0)
      center-y (/ (vr.c/get-screen-height) 2.0)]
  [center-x center-y])

;; Note: Y increases downward, origin at top-left
```

## Time & Delta

```clojure
;; Get absolute time (seconds since start)
(vr.c/get-time)

;; Get frame delta time (seconds since last frame)
(vg/get-delta-time)

;; Frame-rate independent movement
(swap! position + (* velocity (vg/get-delta-time)))
```

## Clamping & Interpolation

```clojure
;; Clamp value to range
(max (min value max) min)

;; Lerp (linear interpolation)
(+ start (* (- end start) t))

;; Example: fade value from 1.0 to 0.0
(max (min @*factor 1.0) 0.0)
```

## Common Mistakes & Fixes

### ❌ Random animation
```clojure
(vg/wobble 0.2)  ;; Unpredictable!
```
**✅ Use time-based:**
```clojure
(Math/sin (* time 2.0 Math/PI))  ;; Smooth & predictable
```

### ❌ Thin glow
```clojure
(vr.c/draw-circle-lines center radius color)
```
**✅ Add thickness with multiple rings:**
```clojure
(vr.c/draw-circle-lines center (- radius 2) color)
(vr.c/draw-circle-lines center radius color)
(vr.c/draw-circle-lines center (+ radius 2) lighter-color)
```

### ❌ Color construction fails
```clojure
(apply vr/Color [r g b a])  ;; Wrong!
```
**✅ Use vector:**
```clojure
(vr/Color [r g b a])  ;; Correct!
```

### ❌ Unbalanced brackets
**✅ Solution:** Use structural editing tools, reload incrementally

### ❌ Forgetting reload
**✅ Always:** `(require 'noel :reload)` after file changes

## Performance Notes

- Trigonometric functions: ~microseconds per call
- Drawing 10-20 primitives per frame: negligible cost
- Screen resolution: 600x600 is fast; optimize shaders if needed
- Animation loop: ~60 FPS target, delta-time compensates

