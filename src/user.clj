(ns user
  {:clj-kondo/ignore [:unused-namespace :use]}
  (:require [neilyio.tasks]
            [neilyio.sound :as sound]
            [neilyio.db :as db]
            [overtone.linter]
            [neilyio.events :as events]
            [neilyio.config :as config]
            [neilyio.repl :as repl]
            [neilyio.utils :as utils]
            [clj-reload.core]
            [portal.api]
            [babashka.fs :as fs]
            [datalevin.core :as d]))

#_(comment
    (require '[clojure.java.io :as io])

    (events/handle  {:module :db :event [:select-next-source] :state repl/state})

    (db/transact! repl/state [{:loop/in 0 :loop/out 1}])
    (db/transact! repl/state [{:selected/loop 16}])
    (db/transact! repl/state [{:speaker/created-at (System/currentTimeMillis)}])
    (db/transact! repl/state [{:selected/speaker 18}])
    (db/transact! repl/state [{:selected/source 1}])

    (def conn (d/get-conn config/db-path db/schema))

    (let [e (ffirst (d/q '[:find ?e :where [?e :source/label ".DS_Store"]] (d/db conn)))]
      (d/transact! conn [[:db.fn/retractEntity e]]))

    (doseq [path (fs/list-dir "/Users/neilhansen/Desktop/test_tracks")
            :when  (= (fs/extension path) "DS_Store")]
      (d/transact! (d/get-conn config/db-path db/schema)
                   [{:source/label (fs/file-name path)
                     :source/bytes (fs/read-all-bytes path)
                     :source/bpm 123}]))

    (db/find-all-sources repl/state)

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
                    left-bus (if left (select left @audio-buses) 0)
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
