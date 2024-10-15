(ns neilyio.visual
  (:require [quil.core :as q]))

(def bpm 123)
(def pulse-rate (/ (* bpm 2) 60.0))  ;; Convert BPM to pulses per second
(def grid-width 5)
(def grid-height 4)
(def circle-diam 100)
(def color-range 255)

(defn setup []
  (q/frame-rate 60)                 ;; Set framerate to 60 FPS
  (q/background 0 0 128))           ;; Set deep underwater dark blue background

(defn calc-index [x y grid-width]
  (+ (* y grid-width) x))

(defn pulse [t offset amount]
  (+ amount (* (Math/sin (+ (* t pulse-rate) offset)) 0.5 amount)))

(defn draw []
  (q/background 0 0 128)            ;; Reset background to deep blue
  (q/no-stroke)

  ;; Calculate circle positions
  (let [circle-spacing (/ (q/width) grid-width)
        time (q/millis)
        mouse-x-index (int (/ (q/mouse-x) circle-spacing))
        mouse-y-index (int (/ (q/mouse-y) circle-spacing))]

    (doseq [y (range grid-height)
            x (range grid-width)]
      (let [x-pos (+ (* x circle-spacing) (/ circle-spacing 2))
            y-pos (+ (* y circle-spacing) (/ circle-spacing 2))
            index (calc-index x y grid-width)
            selected (= [x y] [mouse-x-index mouse-y-index])
            dist (q/dist (q/mouse-x) (q/mouse-y) x-pos y-pos)
            neighbor (and (not selected) (< dist (* circle-spacing 1.5)))

            ;; Pulse effects for selected and neighboring circles
            pulse-amount (cond
                           selected (pulse time index 30)
                           neighbor (pulse time index 10)
                           :else circle-diam)

            ;; Set bioluminescent color for the circles
            r (q/random 50 100)
            g (q/random 100 200)
            b (q/random 200 255)]

        ;; Set color and draw circle
        (q/fill r g b)
        (q/ellipse x-pos y-pos pulse-amount pulse-amount)))))

(q/defsketch example
  :title "Bioluminescent Grid"
  :setup setup
  :draw draw
  :size :fullscreen)

