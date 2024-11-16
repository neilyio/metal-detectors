(ns neilyio.sound
  {:clj-kondo/config '{:linters {:unresolved-var {:exclude [overtone.live]}
                                 :use {:level :off}}}}
  (:use [overtone.live])
  (:require
   [datalevin.core :as d]
   [neilyio.cache :as cache]
   [neilyio.db :as db]
   [neilyio.events :as events]
   [overtone.live :as live]))

; (defn ^:export before-ns-unload []
;   (live/clear)
;   (init! (db/get-conn) cache/conn))

(live/defsynth timeline
  "Takes the id of the buffer to play, and the normalized (0 - 1) start/end,
   which loops within the buffer. Also takes a state-bus which can be polled
   to get the absolute position in frames within the buffer."
  [buffer 0 in 0 out 1 state-bus 0 out-bus 0 rate 1]
  (let [sample-rate (live/buf-sample-rate:kr buffer)
        rate-scale (* (live/buf-rate-scale:kr buffer) rate)
        frames (live/buf-frames:kr buffer)
        start-pos (* in frames)
        end-pos   (* out frames)
        start-pos-delta (abs (- in (live/last-value in :diff 0)))
        ptr (live/phasor:ar :start start-pos :end end-pos
                            :reset-pos start-pos
                            :rate rate-scale :trig start-pos-delta)]
    (live/out:kr state-bus [(/ (live/a2k ptr) frames)
                            (live/a2k ptr)
                            sample-rate
                            rate-scale
                            frames
                            start-pos
                            end-pos
                            in
                            out])
    (live/out:ar out-bus (live/buf-rd 2 buffer ptr))))

(live/defsynth playcontrol [id 0 play 1 state-bus 0]
  (live/out:kr state-bus [id play])
  (live/pause :id id :gate play))

(live/defsynth master
  [selected 0 north 0 east 0 south 0 west 0]
  (let [mtr-filter #(* 0.6 %)
        vol-filter #(* 0.2 %)
        hpf-filter #(hpf % 50)
        rvb-filter #(free-verb % :mix 0.6 :room 0.6)]
    (->> (+ (-> (in:ar selected 2) mtr-filter)
            (-> (in:ar north 2) hpf-filter vol-filter rvb-filter)
            (-> (in:ar east 2)  hpf-filter vol-filter rvb-filter)
            (-> (in:ar south 2) hpf-filter vol-filter rvb-filter)
            (-> (in:ar west 2)  hpf-filter vol-filter rvb-filter))
         (* 0.5)
         (out:ar 0))))

(defn init! [db cache]
  (let [[selected north east south west] (repeatedly #(live/audio-bus 2))]
    (cache/transact! cache [{:synth/selected (timeline :out-bus selected)
                             :synth/north    (timeline :out-bus north)
                             :synth/east     (timeline :out-bus east)
                             :synth/south    (timeline :out-bus south)
                             :synth/west     (timeline :out-bus west)
                             :synth/master   (master :selected selected
                                                     :north north
                                                     :east east
                                                     :south south
                                                     :west west)}])))

(defn ctx [db cache]
  (let [[selected north east south west] (db/selected-speaker-and-neighbors (d/db db))
        in-by-speaker     #(->> % :speaker/loop :loop/in)
        out-by-speaker    #(->> % :speaker/loop :loop/out)
        sample-by-speaker #(->> % :speaker/loop :loop/source :db/id (cache/sample-by-source @cache))]
    (merge
     (select-keys (cache/synths @cache) [:synth/selected
                                         :synth/north
                                         :synth/east
                                         :synth/south
                                         :synth/west
                                         :synth/master])
     {:in/selected (->> selected in-by-speaker)
      :in/north (->> selected in-by-speaker)
      :in/east (->> selected in-by-speaker)
      :in/south (->> selected in-by-speaker)
      :in/west (->> selected in-by-speaker)
      :out/selected (->> selected out-by-speaker)
      :out/north (->> selected out-by-speaker)
      :out/east (->> selected out-by-speaker)
      :out/south (->> selected out-by-speaker)
      :out/west (->> selected out-by-speaker)
      :buffer/selected (->> selected sample-by-speaker)
      :buffer/north (->> north sample-by-speaker)
      :buffer/east (->> east sample-by-speaker)
      :buffer/south (->> south sample-by-speaker)
      :buffer/west (->> west sample-by-speaker)})))

(doseq [event [:loop-beats-4
               :loop-beats-half
               :loop-beats-double
               :loop-beats-left-1
               :loop-beats-right-1
               :loop-beats-left-4
               :loop-beats-right-4
               :loop-beats-left-16
               :loop-beats-right-16
               :loop-beats-left-01
               :loop-beats-right-01
               :select-prev-speaker
               :select-next-speaker
               :select-north-speaker
               :select-south-speaker
               :select-east-speaker
               :select-west-speaker
               :select-prev-source
               :select-next-source
               :location]]
  (defmethod events/handle [:sound event]
    [ctx]

    (when-let [buffer (and (:synth/selected ctx) (:buffer/selected ctx))]
      (live/ctl (:synth/selected ctx) :buffer buffer))
    (when-let [buffer (and (:synth/north ctx) (:buffer/north ctx))]
      (live/ctl (:synth/north ctx) :buffer buffer))
    (when-let [buffer (and (:synth/east ctx) (:buffer/east ctx))]
      (live/ctl (:synth/east ctx) :buffer buffer))
    (when-let [buffer (and (:synth/south ctx) (:buffer/south ctx))]
      (live/ctl (:synth/south ctx) :buffer buffer))
    (when-let [buffer (and (:synth/west ctx) (:buffer/west ctx))]
      (live/ctl (:synth/west ctx) :buffer buffer))

    (when-let [in (and (:synth/selected ctx) (:in/selected ctx))]
      (live/ctl (:synth/selected ctx) :in in))
    (when-let [in (and (:synth/north ctx) (:in/north ctx))]
      (live/ctl (:synth/north ctx) :in in))
    (when-let [in (and (:synth/east ctx) (:in/east ctx))]
      (live/ctl (:synth/east ctx) :in in))
    (when-let [in (and (:synth/south ctx) (:in/south ctx))]
      (live/ctl (:synth/south ctx) :in in))
    (when-let [in (and (:synth/west ctx) (:in/west ctx))]
      (live/ctl (:synth/west ctx) :in in))

    (when-let [out (and (:synth/selected ctx) (:out/selected ctx))]
      (live/ctl (:synth/selected ctx) :out out))
    (when-let [out (and (:synth/north ctx) (:out/north ctx))]
      (live/ctl (:synth/north ctx) :out out))
    (when-let [out (and (:synth/east ctx) (:out/east ctx))]
      (live/ctl (:synth/east ctx) :out out))
    (when-let [out (and (:synth/south ctx) (:out/south ctx))]
      (live/ctl (:synth/south ctx) :out out))
    (when-let [out (and (:synth/west ctx) (:out/west ctx))]
      (live/ctl (:synth/west ctx) :out out))))

(comment
  nil)
