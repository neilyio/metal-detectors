(ns user
  {:clj-kondo/ignore [:unused-namespace :use]}
  (:require [neilyio.tasks]
            [neilyio.sound :as sound]
            [neilyio.cache :as cache]
            [neilyio.db :as db]
            [overtone.linter]
            [neilyio.events :as events]
            [neilyio.config :as config]
            [neilyio.repl :as repl]
            [neilyio.utils :as utils]
            [clj-reload.core]
            [clojure.edn :as edn]
            [portal.api]
            [babashka.fs :as fs]
            [datascript.core :as ds]
            [datalevin.core :as d]))

(comment
  (require '[overtone.live :as live])
  (->> (d/q '[:find [?e ...] :where [?e :speaker/x]]
            (d/db (db/get-conn)))
       (sort))
  (db/select-speaker-by-id! (db/get-conn) 1)
  (d/transact! (db/get-conn) [{:db/id (d/tempid -1) :selected/speaker 29}])
  (db/selected-speaker (d/db (db/get-conn)))

  (sort (map :db/id (db/find-all-speakers (d/db (db/get-conn)))))

  (def selected-speaker (db/selected-speaker (d/db (db/get-conn))))
  (def selected-timeline (cache/timeline-by-speaker @cache/conn (:db/id selected-speaker)))
  (def selected-out-bus (:timeline/looper-out selected-timeline))
  (def selected-sample (cache/sample-by-source @cache/conn (-> selected-speaker :speaker/loop :loop/source :db/id)))
  (def master (cache/master @cache/conn))

  (tap> (:master/synth master))

  (keys selected-sample)
  (keys (:master/synth master))
  (keys (sound/master))
  (def new-master (sound/master))

  (tap> new-master)

  (live/ctl sound/master-synth :selected selected-out-bus)
  (live/ctl (sound/master) :selected selected-out-bus)

  (nil? master)
  (def synth (sound/timeline :buffer selected-sample :in 0 :out-bus selected-out-bus))
  (sound/master selected-out-bus 2)

  (keys selected-timeline)
  ((live/synth
    (live/out:ar 0 (live/in:ar selected-out-bus 2))))

  (live/stop)

  nil)

(comment
  ;; Create a 6x10 grid of speakers if they don't exist yet
  (let [conn (db/get-conn)
        db (d/db conn)
        existing (d/q '[:find ?e
                        :where [?e :speaker/created-at]]
                      db)]
    (when (< (count existing) 60)
      (doseq [x (range 6)
              y (range 10)
              :let [speaker-x (* x 10)
                    speaker-y (* y 10)]]
        (d/transact! conn [{:speaker/x speaker-x
                            :speaker/y speaker-y
                            :speaker/created-at (System/currentTimeMillis)}]))))

  (count (cache/all-timelines @cache/conn))
  nil)

#_(comment
    (clj-reload.core/reload)
    (cache/count-buffers @cache/conn)
    (cache/timeline-by-speaker @cache/conn 11)
    (cache/all-timelines @cache/conn)
    (cache/q  '[:find ?e :in $ ?speaker-id
                :where [?e :timeline/speaker ?speaker-id]]
              @cache/conn 1)

    (cache/q '[:find ?buffer ?source :where [?e :buffer/sample ?buffer] [?e :buffer/source ?source]] @cache/conn)
    (let [k (cache/q '[:find ?buffer . :where [?e :buffer/sample ?buffer]] @cache/conn)])

    (require '[datascript.core :as ds])
    (let [db      (d/db (db/get-conn))
          buffers (atom {})
          label   (-> (db/selected-speaker db) :speaker/loop :loop/source :source/label)
          sources (db/find-all-sources db)

          conn (ds/create-conn)]
      (ds/transact! conn [{:name "neil"}])
      (doseq [{:source/keys [label bytes]} (take 2 sources)]
        (ds/transact! conn [{:bytes (sound/bytes->sample bytes)}]))
      (ds/q '[:find ?e ?n :in  :where [?e :bytes ?n]] @conn)

      #_(let [buffer             (get @buffers label)
              timeline-info-bus  (live/control-bus 8)
              play-info-bus      (live/control-bus 2)
              timeline           (timeline :buffer (or buffer 0) :start 0 :state-bus timeline-info-bus)
              playcontrol        (playcontrol :id (:id timeline) :play 0 :state-bus play-info-bus)]
          [:sound (fn [_]
                    (let [db            (d/db (get-conn))
                          timeline-info (timeline-info timeline-info-bus play-info-bus)
                          selected-loop (-> (db/selected-speaker db) :speaker/loop)
                          label         (-> selected-loop :loop/source :source/label)
                          buffer        (get @buffers label)]
                      (set-timeline-info! timeline-info)
                      (merge timeline-info
                             selected-loop
                             {::selected-buffer buffer
                              ::timeline timeline
                              ::playcontrol playcontrol
                              ::timeline-info-bus timeline-info-bus
                              ::play-info-bus play-info-bus})))]))
    nil)

#_(comment
    (require '[clojure.java.io :as io])

    (events/handle  {:module :db :event [:select-next-source] :state repl/state})

    (cache/all-timelines @cache/conn)
    (cache/count-buffers @cache/conn)
    (cache/q '[:find ?e :where [?e :timeline/looper]] @cache/conn)

    (db/transact! repl/state [{:loop/in 0 :loop/out 1}])
    (db/transact! repl/state [{:selected/loop 16}])
    (db/transact! repl/state [{:speaker/created-at (System/currentTimeMillis)}])
    (db/transact! repl/state [{:selected/speaker 18}])
    (db/transact! repl/state [{:selected/source 1}])

    (def conn (d/get-conn config/db-path db/schema))
    (d/q '[:find ?e :where [?e :source/label ?l]] (d/db conn))

    (doseq [path (fs/list-dir "/Users/neilhansen/Desktop/test_tracks")
            :when  (and (not (= "loaded" (fs/file-name path)))
                        (not (= (fs/extension path) "DS_Store")))
            :let [bpm (edn/read-string (fs/file-name path))
                  tracks (fs/list-dir path)]]
      (doseq [track tracks]
        (d/transact! (d/get-conn config/db-path db/schema)
                     [{:source/label (fs/file-name track)
                       :source/bytes (fs/read-all-bytes track)
                       :source/bpm bpm}])))

    (db/transact! repl/state
                  (for [source (utils/list-sources)]
                    {:source/type :audio-file :source/file-path source}))

    (db/q repl/state '[:find ?e :where [?e :selected/source]])
    (db/pull repl/state '[*] 100)
    (db/transact! repl/state
                  [[:db/retract 100 :source/type]])

    (db/q repl/state '[:find ?e :where [?e :source/type ?t]])
    (db/transact! repl/state (map #(-> [:db/retract (first %) :source/type :audio-file])
                                  (db/q repl/state '[:find ?e :where [?e :source/type ?t]])))

;; all source entities
    (for [[id] (db/q repl/state '[:find ?e
                                  :where [?e :source/type _]])]
      (db/entity repl/state id))

  ;; all speakers
    (for [[id] (db/q repl/state '[:find ?e
                                  :where [?e :speaker/created-at _]])]
      [id (db/entity repl/state id)])

  ;; select first speaker
    (for [[id] (take 1 (db/q repl/state '[:find ?e
                                          :where [?e :speaker/created-at _]]))]
      (db/transact! repl/state [{:selected/speaker id}]))

  ;; add loop to selected speaker
    (let [speaker-id (db/find-selected-speaker repl/state)]
      (db/transact! repl/state [[:db/add speaker-id :speaker/loop 33]]))

;; find selected speaker loop

    (defn find-all-loops []
      (->> (db/q repl/state '[:find ?e :where [?e :loop/source]])
           (map first)))

    (db/transact! repl/state [{:loop/in 0 :loop/out 1 :loop/source 4}])

    (find-all-loops)
    (db/find-all-sources repl/state)

    (db/context repl/state)
    (db/find-all-speakers (-> @repl/state :neilyio.db/conn d/db))
    (db/find-all-sources repl/state)

    (db/transact! repl/state
                  [{:loop/name "whole track"
                    :loop/in 0
                    :loop/out 1}])

    (db/reset-conn! repl/state)

    nil)

#_(comment
    (ns neilyio.metal-detectors
      (:use [overtone.live])
      (:require [overtone.linter]))

    (comment
  ;; Boot a server process. This will initiate a global server process,
  ;; and won't work more than once
      (boot-server)
      (kill-server)

  ;; Emit clj-kondo config (don't commit).
      (overtone.linter/emit!)

  ;; Eval to import the whole Overtone API.
      #_(use 'overtone.live))

    (comment
  ;; Load a sample and return a function that can play it.
  ;; load-sample will load a sample into a buffer.
      (def kick (sample "/Users/neilhansen/Downloads/249212__netr_si__kick-41.wav"))

      (sample "/Users/neilhansen/Downloads/249212__netr_si__kick-41.wav")

      (comment
    ;; Play the sound.
        (kick))

      (comment
    ;; Return a metronome function. Takes the bpm as an argument.
        (def nome (metronome 120))

    ;; Every time you evaluate this, it will show a different beat number. It's "ticking".
        (nome)

    ;; If you pass it a beat number, it will return you the timestamp at that beat number.
        (nome 1000)

    ;; (at ...) is a macro that takes [time-ms & body]. 
    ;; OSC messages are sent in a single bundle and executed at the specified timestamp.
    ;; Will fail silently if a server node has not yet initialized or terminated.
    ;;This allows you to schedule musical events ahead of time.
    ;; If we were to rely on the JVM for timing then we'd start to get annoyed with the thread
    ;; scheduler and garbage collector getting in the way of precise timing.
        (comment
          (at (nome 100) (kick)))

    ;; Recursively schedule a sound to play on a beat.
        (defn looper [nome sound]
          (let [beat (nome)]
            (at (nome beat) (sound))
            (apply-by (nome (inc beat)) looper nome sound [])))

        (comment
      ;; Start your beat.
          (looper nome kick))

        (comment
      ;; Stop all synths and metronomes.
          (stop))))

    (def samples (apply merge (for [[key path] tracks] {key (sample path)})))
    (def loops (apply merge (for [[key path] loop-tracks] {key (sample path)})))

    (defsynth looped [out-bus 0 bpm 120 buffer 0 beats 4 start 0 nudge 0]
      (let [sample-rate (buf-sample-rate:kr buffer)
            rate-scale (buf-rate-scale:kr buffer)
            frames-per-beat (/ (* sample-rate 60) bpm)
            start-frame (* start frames-per-beat)
            length (* beats frames-per-beat)
            end-frame (+ start-frame length)]
        (let [ptr (phasor:ar :rate rate-scale :start (+ start-frame nudge) :end end-frame)]
          (out:ar out-bus (buf-rd 2 buffer ptr)))))

    (def audio-buses (atom []))

    (defn assign-bus []
      (let [bus (audio-bus 2)]
        (swap! audio-buses conj (:id bus))
        bus))

    (def trigger-bus (control-bus))

    (defsynth triggercontrol [out-bus 0 in 0]
      (out:kr trigger-bus (trig1:kr in 0.05)))

    (definst sinth []
      (* (in:kr trigger-bus) (sin-osc:ar 800)))

    (definst testme [in 0 out 1 trig 0]
      (let [buffer (:jiminy loops)
            frames (buf-frames:kr buffer)
            scale (buf-rate-scale:kr buffer)
            start-frame (* in frames)
            end-frame (* out frames)
            ptr (phasor:ar :rate scale :start start-frame :end end-frame :trig trig)]
        (buf-rd 2 buffer ptr)))

    (comment

      (stop)
      (def t (testme))
      (ctl t :in 0.25 :out 0.5)
      (ctl t :trig 1)
      (ctl t :trig 0)

      (dotimes [n 10]
        (Thread/sleep 100)
        (def b (triggercontrol))
        (ctl b :in true))

      (ctl b :in 0)

      ((synth))
      (stop)

      (:duration (:jiminy amples))

      (defsynth timeline [buffer 0 seek-pos 0 end 1]
        (let [rate-scale (buf-rate-scale:kr buffer)
              start-pos (* seek-pos (buf-frames buffer))
              start-pos-delta (abs (- seek-pos (last-value seek-pos :diff 0)))
              end-pos    (* end (- (buf-frames buffer) 1))
              ptr (phasor:ar :start start-pos :end end-pos :rate  rate-scale :trig start-pos-delta)]
          (out:ar 0 (buf-rd 2 buffer ptr))))

      (def m (timeline :buffer (:jiminy loops) :end 0.5))

      (defsynth daw-timeline [out-bus 0 buffer 0 bpm 120 play-head 0 paused 0 loop-start 0 loop-end -1 loop-enabled 0]
        (let [sample-rate (buf-sample-rate:kr buffer)
              rate-scale (buf-rate-scale:kr buffer)
              frames-per-beat (/ (* sample-rate 60) bpm)

        ;; Calculate the total length of the buffer in frames
              total-frames (buf-frames:kr buffer)

        ;; Adjust playhead and loop region
              play-frame (+ (* play-head frames-per-beat) (phasor:ar :rate rate-scale :start 0 :end total-frames :trig (not paused) :reset-pos 0))
              loop-start-frame (* loop-start frames-per-beat)
              loop-end-frame (if (>= loop-end 0) (* loop-end frames-per-beat) total-frames)

        ;; Use phasor for looping within the specified subregion
              ptr (if (and loop-enabled (> loop-end 0))
                    (phasor:ar :rate rate-scale :start loop-start-frame :end loop-end-frame :trig (not paused) :reset-pos 0)
                    play-frame)

        ;; Audio output
              signal (buf-rd 2 buffer ptr)]

    ;; Output the sound
          (out:ar out-bus signal)))

      (def playing (atom false))
      (def daw-synth (atom nil))

      (defn start-timeline []
        (reset! daw-synth (daw-timeline :buffer (:jiminy samples) :bpm 120)))

      (defn toggle-play-pause []
        (when @daw-synth
          (swap! playing not)
          (ctl @daw-synth :paused (if @playing 0 1))))

      (stop)
      @daw-synth
      @playing
      (start-timeline)
      (toggle-play-pause)

      (comment
        (do
          (stop)
          (at (now)
              (reset! audio-buses [])
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:rocket loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:meadow loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:visions loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:jiminy loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:lost loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:uplifting loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:sunset loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:ashes loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:geoform loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:nobody loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:dream loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:angels loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:cosmic loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:rampa loops) :beats 16)
              (looped :bpm 123 :out-bus (assign-bus) :buffer (:breeze loops) :beats 16))))

      (comment
        (do
          (when (bound? #'master)
            (kill master))

;; No smoothing
          (def master
            ((synth
              (let [x (mouse-x 0 5)
                    y (mouse-y 0 3)
                    x-index (floor x)
                    y-index (floor y)
                    grid-width 5
                    grid-height 4
                    idx (+ (* y-index grid-width) x-index)

          ;; Calculate neighboring indices (ensure they don't go out of bounds)
                    left   (if (> x-index 0)           (- idx 1) nil)
                    right  (if (< x-index (dec grid-width)) (+ idx 1) nil)
                    above  (if (> y-index 0)           (- idx grid-width) nil)
                    below  (if (< y-index (dec grid-height)) (+ idx grid-width) nil)

          ;; Select current bus
                    main-bus (select idx @audio-buses)

          ;; Select adjacent buses (ignoring nil values)
                    left-bus (if left   (select left @audio-buses) 0)
                    right-bus (if right (select right @audio-buses) 0)
                    above-bus (if above (select above @audio-buses) 0)
                    below-bus (if below (select below @audio-buses) 0)

                    vol-filter #(* 0.5 %)
                    hpf-filter #(hpf % 50)
                    rvb-filter #(free-verb % :mix 0.6 :room 0.6)

;; Combine all signals, adjust volume for neighbors
                    signal (+ (in:ar main-bus 2)
                              (-> (in:ar left-bus 2)  hpf-filter vol-filter rvb-filter)
                              (-> (in:ar right-bus 2) hpf-filter vol-filter rvb-filter)
                              (-> (in:ar above-bus 2) hpf-filter vol-filter rvb-filter)
                              (-> (in:ar below-bus 2) hpf-filter vol-filter rvb-filter))]

      ;; Output the combined signal to speakers
                (out:ar 0 signal)))))

  ;; No neighbours.
          #_(def master
              ((synth
                (let [x (mouse-x 0 5)
                      y (mouse-y 0 3)
                      x-index (floor x)
                      y-index (floor y)
                      grid-width 5
                      idx (+ (* y-index grid-width) x-index)
                      bus (select idx @audio-buses)]
                  (out:ar 0 (in:ar bus 2))))))

          nil)))

    nil)
