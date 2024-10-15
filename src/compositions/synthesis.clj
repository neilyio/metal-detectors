(ns compositions.synthesis
  (:require [neilyio.macros :refer [defpart]])
  (:use overtone.live))

(defsynth fm [carrier 440 divisor 2.0 depth 1.0 out-bus 0]
  (let [modulator (/ carrier divisor)
        mod-env   (env-gen (lin 1 0 6))
        amp-env   (env-gen (lin 1 1 5) :action FREE)]
    (out out-bus (pan2 (* 0.5 amp-env
                          (sin-osc (+ carrier
                                      (* mod-env  (* carrier depth) (sin-osc modulator)))))))))
(defn rand-wail [& {:keys [root-note bus] :or {bus 0}}]
  (let [root-freq (-> root-note note midi->hz)]
    (rand-nth [#(fm :carrier root-freq :out-bus bus)
               #(fm :carrier root-freq :divisor :depth 3 :out-bus bus)
               #(fm :carrier root-freq :divisor :depth 10 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor 2 :depth 4 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor 2 :depth 8 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor 4 :depth 4 :out-bus bus)
               #(fm :carrier root-freq :divisor 4 :depth 8 :out-bus bus)
               #(fm :carrier (* 4 root-freq) :divisor 4 :depth 4 :out-bus bus)
               #(fm :carrier (/ root-freq 2) :divisor 4 :depth 4 :out-bus bus)
               #(fm :carrier root-freq :divisor 2 :depth 4 :out-bus bus)
               #(fm :carrier root-freq :divisor 2 :depth 8 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor 8 :depth 8 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor 8 :depth 2 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor (/ 4 3) :depth 2 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor (/ 5 3) :depth 2 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor (/ 7 3) :depth 2 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor (/ 4 3) :depth 4 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor (/ 5 3) :depth 4 :out-bus bus)
               #(fm :carrier (* 2 root-freq) :divisor (/ 7 3) :depth 4 :out-bus bus)
               #(fm :carrier root-freq :divisor (/ 7 5) :depth 2 :out-bus bus)
               #(fm :carrier root-freq :divisor (/ 7 5) :depth 4 :out-bus bus)
               #(fm :carrier (/ root-freq 2) :divisor 4 :depth 2 :out-bus bus)
               #(fm :carrier (/ root-freq 2) :divisor 4 :depth 4 :out-bus bus)
               #(fm :carrier (/ root-freq 2) :divisor 4 :depth 8 :out-bus bus)]))
  #_(rand-nth [#(fm)
               #(fm 220)
               #(fm 220 3)
               #(fm 220 10)
               #(fm 440)
               #(fm 440 2 4)
               #(fm 440 2 8)
               #(fm 440 4 4)
               #(fm 220 4 8)
               #(fm 880 4 4)
               #(fm 110 4 4)
               #(fm 220 2 4)
               #(fm 220 2 8)
               #(fm 440 8 8)
               #(fm 440 8 2)
               #(fm 440 (/ 4 3) 2)
               #(fm 440 (/ 5 3) 2)
               #(fm 440 (/ 7 3) 2)
               #(fm 440 (/ 4 3) 4)
               #(fm 440 (/ 5 3) 4)
               #(fm 440 (/ 7 3) 4)
               #(fm 220 (/ 7 5) 2)
               #(fm 220 (/ 7 5) 4)
               #(fm 110 4 2)
               #(fm 110 4 4)
               #(fm 110 4 8)]))

(def metro (metronome 20))

#_(defn play
    [& {:keys [metro root-note subd beat] :or {subd 1} :as args}]
    (let [beat (or beat (metro))]
      (at (metro beat) ((rand-wail :root-note root-note)))
      (apply-by (metro (+ beat (* 1 subd))) play [(assoc args :beat (+ subd beat))])))

(defpart play [& {:as args}]
  ((rand-wail args)))

; ;; Function for recursion and dynamic execution
; (defn begin-fn
;   [& {:keys [metro subd beat body] :or {subd 1} :as args}]
;   (let [beat (or beat (metro))]
;     (at (metro beat) (body))  ;; Call the body passed in
;     (apply-by (metro (+ beat (* 1 subd))) begin-fn [(assoc args :beat (+ subd beat))])))

; ;; Macro for initial call setup
; (defmacro begin
;   [{:keys [metro root-note subd beat] :or {subd 1}} & body]
;   `(begin-fn :metro ~metro
;              :root-note ~root-note
;              :subd ~subd
;              :beat ~beat
;              :body (fn [] ~@body)))

(comment
  (metro :bpm 40))

(comment
  (stop)
  (:bpm metro)
  (do
    (kill-server)
    (boot-server)))


