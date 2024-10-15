(ns compositions.piano-phase
  (:use overtone.live
        overtone.inst.sampled-piano))

;; Steve Reich's Piano Phase

(def piece [:E4 :F#4 :B4 :C#5 :D5 :F#4 :E4 :C#5 :B4 :F#4 :D5 :C#5])

(defn transpose-note
  "Transpose a single note by the given number of semitones, keeping the output as a keyword note."
  [n semitones]
  (-> n
      note  ;; Convert to MIDI note number
      (+ semitones)  ;; Add the semitone transposition
      find-note-name))  ;; Convert back to keyword note

(defn transpose-piece
  "Transpose all the notes in the piece by a given number of semitones based on a scale."
  [piece root-note]
  (let [interval (note root-note) ;; Get the MIDI value of the root note
        original-root (note (first piece)) ;; Get the MIDI value of the first note in the piece
        semitones (- interval original-root)] ;; Calculate the difference in semitones
    (map #(transpose-note % semitones) piece)))

(defn player
  ([metro notes]
   (player metro notes 1))
  ([metro notes subd]
   (player metro notes subd (metro)))
  ([metro notes subd beat]
   (let [n     (first notes)
         notes (next notes)]
     (when n
       (at (metro beat)
           (sampled-piano (note n)))
       (apply-by (metro (+ beat (* 1 subd))) #'player [metro notes subd (+ beat subd)])))))

(def num-notes 1000)

(defn play
  "Play the transposed piece based on the provided root note."
  ([metro root-note]
   (play metro root-note 1))
  ([metro root-note subd]
   (let [transposed-notes (transpose-piece piece root-note)]
     (player metro (cycle transposed-notes) subd (metro)))))

(comment
  (def nome (metronome 120))
  (player nome [:E4 :E5] 2)

  (take num-notes (transpose-piece piece :E4))
  (play nome :E4)
  (stop)
  (stop-all)

  (:bpm nome))


