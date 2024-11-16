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

(declare init!)

(defn ^:export before-ns-unload []
  (live/clear)
  (init! (db/get-conn) cache/conn))

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

(defn timeline-info [timeline-info-bus play-info-bus]
  (assert (and timeline-info-bus play-info-bus) "engine info bus not initialized")
  (let [[current current-frame sample-rate rate-scale total-frames
         loop-start-frame loop-end-frame loop-start loop-end]
        (live/control-bus-get timeline-info-bus)
        [playing-buffer playing?]
        (live/control-bus-get play-info-bus)]
    {::playing?         playing?
     ::playing-buffer   playing-buffer
     ::current          current
     ::current-frame    current-frame
     ::sample-rate      sample-rate
     ::rate-scale       rate-scale
     ::total-frames     total-frames
     ::loop-start-frame loop-start-frame
     ::loop-end-frame   loop-end-frame
     ::loop-start       loop-start
     ::loop-end         loop-end}))

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

(defn get-loop-buses [db cache]
  (let [[selected north east south west] (db/selected-speaker-and-neighbors (d/db db))
        selected-timeline (->> selected :db/id (cache/timeline-by-speaker @cache))
        north-timeline (->> north :db/id (cache/timeline-by-speaker @cache))
        east-timeline (->> east :db/id (cache/timeline-by-speaker @cache))
        south-timeline (->> south :db/id (cache/timeline-by-speaker @cache))
        west-timeline (->> west :db/id (cache/timeline-by-speaker @cache))]
    (merge
     {::selected (-> selected-timeline :timeline/looper-out)
      ::north (-> north-timeline :timeline/looper-out)
      ::east (-> east-timeline :timeline/looper-out)
      ::south (-> south-timeline :timeline/looper-out)
      ::west (-> west-timeline :timeline/looper-out)})))

(defn setup-master! [db cache]
  (let [{::keys [selected north east south west]} (get-loop-buses db cache)
        master-synth  (master :selected (or selected 0)
                              :north    (or north 0)
                              :east     (or east 0)
                              :south    (or south 0)
                              :west     (or west 0))
        master-info    (live/control-bus 2)
        master-control (playcontrol :id (:id master-synth) :play 1 :state-bus master-info)]
    (cache/transact!
     cache
     [{:master/synth master-synth :master/control master-control :master/info master-info}])))

(defn init!
  "Return an event handler function that takes state and returns new state.
   Args:
   - conn - database connection to use
   Returns a function that takes:
   - state - current application state
   - event - event to handle
   Returns updated state with ::conn and ::db added."
  [db cache]
  (doseq [speaker     (db/find-all-speakers (d/db db))]
    (let [master-bpm 123
          looper-info (live/control-bus 8)
          looper-out  (live/audio-bus 2)
          {:loop/keys [source in out bpm]} (-> speaker :speaker/loop :loop/source)
          sample      (cache/sample-by-source @cache (-> source :db/id))
          rate        (if (and bpm master-bpm)
                        (/ bpm master-bpm)
                        1)
          looper      (timeline :buffer    (or sample 0)
                                :in        (or in 0)
                                :out       (or out 1)
                                :state-bus looper-info
                                :out-bus   looper-out
                                :rate      rate)]
      (cache/timeline! cache (:db/id speaker) looper looper-info looper-out)))

  ;; Master must be added AFTER  above (or properly deal with addActions)
  (setup-master! db cache))

(defn ctx [db cache]
  (let [selected (db/selected-speaker (d/db db))
        selected-timeline (->> selected :db/id (cache/timeline-by-speaker @cache))
        master (cache/master @cache)]
    (tap> [:sound-ctx {:selected selected :timline-keys (keys selected-timeline)}])
    (assert master "no master")
    (merge
     (-> selected :speaker/loop (select-keys [:loop/in :loop/out]))
     (get-loop-buses db cache)
     (timeline-info
      (->> selected-timeline :timeline/looper-status)
      (->> (cache/master @cache) :master/info))
     {::sample (->> selected :speaker/loop :loop/source :db/id (cache/sample-by-source @cache))
      ::looper (-> selected-timeline :timeline/looper)
      ; ::player (-> selected-timeline :timeline/player)
      ::master (-> master :master/synth)})))

; (defmethod events/handle [:sound :play-toggle]
;   [{::keys [playing? all-players all-loopers]}]
;   (doseq [[player looper] (map vector all-players all-loopers)]
;     (live/ctl player :id looper :play (if (zero? playing?) 1 0))))

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
    [{:loop/keys [in out] ::keys [sample looper master selected north south east west]}]

    (when in (live/ctl looper :in in))
    (when out (live/ctl looper :out out))
    (when sample (live/ctl looper :buffer sample))

    (live/ctl master
              :selected (or selected 0)
              :north (or north 0)
              :east (or east 0)
              :west (or west 0)
              :south (or south 0))))

(comment
  nil)
