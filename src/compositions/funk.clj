(ns compositions.funk
  "This example creates a simple drum and bass pattern, based off of
    the James Brown classic 'Licking Stick', with Bootsy Collins on bass,
    and John Jab'o Starks on drums"
  (:use [overtone.live]))

; model a plucked string, we'll use this for our bass
(definst string [note 60 amp 1.0 dur 0.5 decay 30 coef 0.3 gate 1]
  (let [freq (midicps note)
        noize (* 0.8 (white-noise))
        dly   (/ 1.0 freq)
        plk   (pluck noize gate dly dly decay coef)
        dist  (distort plk)
        filt  (rlpf dist (* 12 freq) 0.6)
        clp   (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action 0) reverb)))

; define a simple drumkit using freesound samples
(def snare (sample (freesound-path 26903)))
(def kick (sample (freesound-path 2086)))
(def close-hihat (sample (freesound-path 802)))
(def open-hihat (sample (freesound-path 26657)))

(defn subdivide
  "subdivide two time intervals by 4, and return the time interval
    at position. this is a cheap hack to schedule 16th notes without
    defining the whole pattern with the metronome firing every 16th note."
  [a b position]
  (+ a (* position (/ (- b a) 4))))

(defn drums [nome]
  (let [beat (nome)]
        ; hi-hat pattern
    (at (nome beat) (close-hihat))
    (at (nome (+ 1 beat)) (open-hihat))
    (at (nome (+ 2 beat)) (close-hihat))
    (at (nome (+ 3 beat)) (close-hihat))
    (at (nome (+ 4 beat)) (close-hihat))
    (at (nome (+ 5 beat)) (open-hihat))
    (at (nome (+ 6 beat)) (close-hihat))
    (at (nome (+ 7 beat)) (close-hihat))

        ; snare pattern
    (at (nome (+ 2 beat)) (snare))
    (at (subdivide (nome (+ 2 beat)) (nome (+ 4 beat)) 3) (snare))
    (at (subdivide (nome (+ 4 beat)) (nome (+ 6 beat)) 1) (snare))
    (at (nome (+ 6 beat)) (snare))
    (at (subdivide (nome (+ 6 beat)) (nome (+ 8 beat)) 3) (snare))

        ; kick drum pattern
    (at (nome beat) (kick))
    (at (nome (+ 5 beat)) (kick))
    (at (nome (+ 7 beat)) (kick))
    (apply-by (nome (+ 8 beat)) drums nome [])))

(defn bass [nome root]
  (let [root-midi (note root)  ;; Convert the root note to its MIDI value
        beat (nome)]
    ;; First phrase of the bassline
    (at (nome beat) (string root-midi))
    (at (subdivide (nome beat) (nome (+ 2 beat)) 1) (string root-midi))
    (at (subdivide (nome beat) (nome (+ 2 beat)) 3) (string root-midi))
    (at (subdivide (nome (+ beat 1)) (nome (+ 3 beat)) 1) (string root-midi))
    (at (subdivide (nome (+ beat 1)) (nome (+ 3 beat)) 3) (string root-midi))

    ;; Second phrase of the bassline
    (at (nome (+ 4 beat)) (string root-midi))
    (at (subdivide (nome (+ 4 beat)) (nome (+ 6 beat)) 1) (string (- root-midi 2)))  ;; Transposed by a minor second
    (at (nome (+ 5 beat)) (string (- root-midi 5)))  ;; Transposed by a perfect fourth
    (at (nome (+ 6 beat)) (string root-midi))
    (at (subdivide (nome (+ 6 beat)) (nome (+ 8 beat)) 1) (string (- root-midi 2)))
    (at (nome (+ 7 beat)) (string (- root-midi 5)))

    ;; Final phrase of the bassline
    (at (nome (+ 8 beat)) (string root-midi))
    (at (nome (+ 12 beat)) (string root-midi))
    (at (subdivide (nome (+ 12 beat)) (nome (+ 14 beat)) 1) (string root-midi))

    ;; Recursive call to keep the bassline repeating
    (apply-by (nome (+ 16 beat)) bass nome root [])))

(defn section [nome root-note]
  (drums nome)
  (bass nome root-note))

;; define a metronome that will fire every eighth note
;; at 100 bpm

; (def met (metronome (* 100 2)))
;; to play the beat, just run
; (section met)
;; (stop)
