(ns neilyio.macros)

(defmacro defpart
  "Recursively run a musical body.
  
  Expects an initialized metronome on :metro, and optionally a 
  :subd subdivision value to adjust timing."
  {:clj-kondo/lint-as 'clojure.core/defn}
  [fn-name args & body]
  `(defn ~fn-name [~@args]
     (let [beat# (or (:beat ~'args) ((:metro ~'args)))
           subd# (or (:subd ~'args) 1)]  ;; Default subd to 1 if not provided
       ;; Execute the body at the current beat
       (overtone.sc.server/at
        ((:metro ~'args) beat#)
        (do ~@body))
       ;; Schedule the next call using apply-by
       (overtone.music.time/apply-by
        ((:metro ~'args) (+ beat# (* 1 subd#)))  ;; Use the default or provided subd
        ~fn-name
        [(assoc ~'args :beat (+ subd# beat#))]))))  ;; Pass the updated beat
