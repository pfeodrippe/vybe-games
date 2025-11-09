---
name: clojure-game-dev
description: Develop games with Clojure using Vybe game engine, Raylib rendering, and Flecs ECS. Use when building game features, adding animations, creating UI elements, handling entity rotations, or working with real-time graphics in Clojure. Handles cursor design, visual effects, and interactive game mechanics.
---

# Clojure Game Development with Vybe

Expertise in developing game features with Clojure, focusing on real-time rendering, entity management, and smooth animations using the Vybe game engine.

## Key Capabilities

### REPL-Driven Interactive Development
- Live reload namespaces with `(require 'noel :reload)` after every change
- Test visual changes immediately in the running game
- Avoid side effects (audio init) by reloading only changed functions when possible
- Safe Flecs interactions wrapped with `vybe.raylib/t`

### Smooth Animation Systems
- Time-based sine/cosine waves for predictable animations: `(+ 1.0 (* amplitude (Math/sin (* time frequency Math/PI))))`
- Multi-layer animations with phase differences for visual depth
- Pulses, oscillations, and continuous smooth motion
- Never use random functions like `wobble` for deterministic animations

### 3D Rotation with Quaternions
- Y-axis rotation from angle: `[0 (sin(angle/2)) 0 (cos(angle/2))]`
- Continuous rotation using `vr.c/get-time`
- Smooth entity rotation updates in the ECS system

### Advanced Visual Effects
- Layered drawing: shadows, main shapes, outlines, glows
- Concentric circle rings for thick glows
- Color interpolation (lerp) for smooth transitions
- Multi-primitive effects (triangles, lines, circles)

### Clojure Structural Editing
- Incremental function development: build features step-by-step, testing each addition
- Use bracket balancer tool before finalizing complex functions
- Always verify function balance after edits

## Instructions

### Starting a New Feature

1. **Plan the structure**: Identify all `let` bindings you'll need
2. **Create base version**: Define minimal working version
3. **Test incrementally**: Add one feature at a time, reload, verify
4. **Verify brackets**: Use bracket balancer before final commit
5. **Reload and confirm**: `(require 'noel :reload)` and check visual output

### Adding Animations

1. **Use time-based math**: `(let [time (vr.c/get-time)] ...)`
2. **Choose frequency**: Common: 1.0-3.0 Hz for UI, slower for large objects
3. **Apply lerp**: Use interpolation for smooth state transitions
4. **Layer effects**: Add multiple animations with different frequencies

Example:
```clojure
(let [time (vr.c/get-time)
      pulse (+ 1.0 (* 0.1 (Math/sin (* time 2.0 Math/PI))))
      scale (* base-size pulse)]
  ;; use scale in rendering
)
```

### Creating Visual Effects

**Glow rings:**
```clojure
(vr.c/draw-circle-lines center radius-1 color)
(vr.c/draw-circle-lines center (- radius-1 2) color)
(vr.c/draw-circle-lines center (+ radius-1 2) lighter-color)
```

**Drop shadow:**
```clojure
(vr.c/draw-triangle shadow-v1 shadow-v2 shadow-v3 dark-color)
(vr.c/draw-triangle v1 v2 v3 main-color)
```

**Color transition:**
```clojure
(let [t (if is-hovered 1.0 0.0)
      r (int (+ base-r (* (- hover-r base-r) t)))]
  (vr/Color [r g b 255]))
```

### Rotating Entities

```clojure
(let [time (vr.c/get-time)
      angle (* time rotation-speed)
      half-angle (/ angle 2.0)
      quaternion (vt/Rotation [0 (Math/sin half-angle) 0 (Math/cos half-angle)])]
  (update w :entity-key (fn [ent] (assoc ent vt/Rotation quaternion))))
```

### Handling Coordinates

- Screen origin: top-left (0, 0)
- Center: `[(/ width 2.0) (/ height 2.0)]`
- Y increases downward
- All float calculations for smooth positioning

## Examples

### Building an Animated Cursor
1. Define arrow shape with vertices
2. Add scale animation based on hover state
3. Lerp color from base to hover color
4. Add concentric glow rings with phase-offset animations
5. Use drop shadow for depth

### Creating a Rotating Object
1. Get current time with `vr.c/get-time`
2. Convert angle to quaternion
3. Update entity's `vt/Rotation` component
4. Reload namespace to see smooth rotation

### Multi-Layer Visual Effect
1. Draw shadow (offset, dark)
2. Draw main shape (on top, bright)
3. Draw outline (bright white)
4. Add glow layer (transparent, animated)

## Common Patterns

### State Management
```clojure
;; Use atoms for game state
(def *factor (atom 0.0))

;; Update in draw loop
(if condition
  (swap! *factor + (* (vg/get-delta-time) rate))
  (swap! *factor - (* (vg/get-delta-time) rate)))
(reset! *factor (max (min @*factor 1.0) 0.0))
```

### Conditional Features
```clojure
(when condition
  ;; render or update feature
)

;; Or use if for branching
(if turned-on
  ;; render one state
  ;; render other state)
```

### Entity Access
```clojure
(let [entity (w :entity-key)
      current-rotation (get entity vt/Rotation)]
  ;; use entity and components
)
```

## Debugging Checklist

- [ ] Function has matching opening/closing parens
- [ ] All `let` bindings properly closed
- [ ] Used `(require 'noel :reload)` after changes
- [ ] Checked game window for visual feedback
- [ ] Animation frequencies appropriate (1-3 Hz for UI)
- [ ] Colors are 0-255 range integers
- [ ] Vectors use `vt/Vector2`, `vt/Vector3` syntax

## Performance Tips

- Trigonometric functions are cheap; use liberally
- Multiple primitives per frame is fine
- Avoid heavy computations in draw loop
- Cache computed values when possible
- Test frame rate with complex effects

## Related Concepts

- **Flecs ECS**: Entity component system for managing game objects
- **Raylib**: Low-level graphics rendering (C interop via Vybe)
- **Quaternions**: 3D rotations without gimbal lock
- **Delta time**: `vg/get-delta-time` for frame-rate independent movement

## References

- Clojure math: `Math/sin`, `Math/cos`, `Math/PI`
- Vybe types: `vt/Vector2`, `vt/Rotation`, `vt/Translation`, `vt/Color`
- Raylib functions: `vr.c/draw-triangle`, `vr.c/draw-circle-lines`, `vr.c/draw-line-ex`
- Game time: `vr.c/get-time`, `vg/get-delta-time`

