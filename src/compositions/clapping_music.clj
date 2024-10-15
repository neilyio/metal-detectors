;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Something like Steve Reich - Clapping Music ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns compositions.clapping-music
  (:use overtone.live))

(def clap (freesound 436674))
(def clap2 (freesound 3718))
; (clap)
; (clap2)

(def pattern-1 [1 1 1 0 1 1 0 1 0 1 1 0])
(def pattern-2 [1 1 1 0 1 1 0 1 0 1 1 0])

(defn my-cycle []
  (def pattern-2 (let [e (last pattern-2)
                       p (drop-last pattern-2)]
                   (vec (conj p e)))))

(defn play-beat [metro beat]
  (dorun (map (fn [i]
                (let [v (if (= 0 (mod i 3)) 1 0.5)]
                  (when (= (pattern-1 i) 1)
                    (at (metro (+ (/ i 12.0) beat)) (clap 1 0 1 0 (* v 0.33))))
                  (when (= (pattern-2 i) 1)
                    (at (metro (+ (/ i 12.0) beat)) (clap2 1 0 1 0 v)))))
              (range 12))))

(defn player [metro]
  (let [beat (metro)]
    (when (= (mod beat 4) 0) (my-cycle)
          (play-beat metro beat)
          (apply-by (metro (inc beat)) #'player (inc beat) []))))

;;(stop)
