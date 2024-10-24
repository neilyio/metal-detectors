(ns neilyio.metal-detectors
  (:use [overtone.live])
  (:require [overtone.linter]
            [clojure.core.async :refer [chan go-loop >! <! put! timeout close!]]))

(comment
  ;; Boot a server process. This will initiate a global server process,
  ;; and won't work more than once
  (boot-server)
  (kill-server)
  (stop)


  ;; Emit clj-kondo config (don't commit).
  (overtone.linter/emit!)

  ;; Eval to import the whole Overtone API.
  #_(use 'overtone.live))

(comment
  ;; Load a sample and return a function that can play it.
  ;; load-sample will load a sample into a buffer.
  (def kick (sample "./tracks/Dark Sky - Angels_1.1.aif"))

  ((sample "./tracks/Dark Sky - Angels_1.1.aif"))
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

(def _tracks
  {:rocket "/test_tracks/Worst Case - Rocket.mp3"
   :meadow "/test_tracks/Township Rebellion - Infinite Meadow.mp3"
   :visions "/test_tracks/Stereo.type, Felix Raphael - Visions - Bebetta & Cioz Remix.mp3"
   :jiminy "/test_tracks/Polo & Pan - Jiminy.mp3"
   :lost "/test_tracks/Oliver Koletzki, Monolink - We Are All Lost.mp3"
   :uplifting "/test_tracks/Of Norway, Linnea Dale - Favourite Mistake - Karim Sahraoui Uplifting Remix.mp3"
   :sunset "/test_tracks/Mosley Jr - SunSet.mp3"
   :ashes "/test_tracks/Lauren Mia - Ashes in Paradise.mp3"
   :geoform "/test_tracks/HunterGame - Geoform.mp3"
   :nobody "/test_tracks/Felix Jaehn, Jasmine Thompson - Ain't Nobody (Loves Me Better) - Gunes Ergun Remix.mp3"
   :dream "/test_tracks/Dominik Eulberg, Essáy - Dream Machine.mp3"
   :angels "/test_tracks/Dark Sky - Angels.mp3"
   :cosmic "/test_tracks/CIOZ - Cosmic Noise.mp3"
   :rampa "/test_tracks/Âme - No War - Rampa Remix.mp3"
   :breeze "/test_tracks/alt-J - Breezeblocks - Tinlicker Extended Mix.mp3"})

;; TODO : remove this in favor of _tracks
(def tracks
  {:rocket "./tracks/Worst Case - Rocket_1.aif"
   :meadow "./tracks/Township Rebellion - Infinite Meadow_1.1.aif"
   :visions "./tracks/Stereo.type, Felix Raphael - Visions - Bebetta & Cioz Remix_1.aif"
   :jiminy "./tracks/Polo & Pan - Jiminy_1.aif"
   :lost "./tracks/Oliver Koletzki, Monolink - We Are All Lost_1.aif"
   :uplifting "./tracks/Of Norway, Linnea Dale - Favourite Mistake - Karim Sahraoui Uplifting Remix_1.aif"
   :sunset "./tracks/Mosley Jr - SunSet_1.aif"
   :ashes "./tracks/Lauren Mia - Ashes in Paradise_1.aif"
   :geoform "./tracks/HunterGame - Geoform_1.1.aif"
   :nobody "./tracks/Felix Jaehn, Jasmine Thompson - Ain't Nobody (Loves Me Better) - Gunes Ergun Remix_1.1.aif"
   :dream "./tracks/Dominik Eulberg, Essáy - Dream Machine_1.aif"
   :angels "./tracks/Dark Sky - Angels_1.1.aif"
   :cosmic "./tracks/CIOZ - Cosmic Noise_1.1.aif"
   :rampa "./tracks/Âme - No War - Rampa Remix_1.aif"
   :breeze "./tracks/alt-J - Breezeblocks - Tinlicker Extended Mix_1.1.aif"})

(def loop-tracks
  {:rocket "./tracks/Worst Case - Rocket_1.aif"
   :meadow "./tracks/Township Rebellion - Infinite Meadow_1.1.aif"
   :visions "./tracks/Stereo.type, Felix Raphael - Visions - Bebetta & Cioz Remix_1.aif"
   :jiminy "./tracks/Polo & Pan - Jiminy_1.aif"
   :lost "./tracks/Oliver Koletzki, Monolink - We Are All Lost_1.aif"
   :uplifting "./tracks/Of Norway, Linnea Dale - Favourite Mistake - Karim Sahraoui Uplifting Remix_1.aif"
   :sunset "./tracks/Mosley Jr - SunSet_1.aif"
   :ashes "./tracks/Lauren Mia - Ashes in Paradise_1.aif"
   :geoform "./tracks/HunterGame - Geoform_1.1.aif"
   :nobody "./tracks/Felix Jaehn, Jasmine Thompson - Ain't Nobody (Loves Me Better) - Gunes Ergun Remix_1.1.aif"
   :dream "./tracks/Dominik Eulberg, Essáy - Dream Machine_1.aif"
   :angels "./tracks/Dark Sky - Angels_1.1.aif"
   :cosmic "./tracks/CIOZ - Cosmic Noise_1.1.aif"
   :rampa "./tracks/Âme - No War - Rampa Remix_1.aif"
   :breeze "./tracks/alt-J - Breezeblocks - Tinlicker Extended Mix_1.1.aif"})

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
          Frames-per-beat (/ (* sample-rate 60) bpm)

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

;; TODO change this function name
(defn greet [_]
  (boot-server))


;;; Pablo code below
(defn play-all-tracks
  "Plays all tracks simultaneously at the specified BPM.
   Optional parameters:
   - bpm: Beats per minute (default 123)
   - beats: Number of beats in loop (default 16)
   Returns: Vector of created synth instances"
  [& {:keys [bpm beats]
      :or {bpm 123 beats 16}}]
  (do
    ;; Stop any currently playing audio
    (stop)
    ;; Reset audio buses
    (reset! audio-buses [])
    ;; Play all tracks simultaneously
    (at (now)
        (doall  ; Force evaluation of lazy sequence
          (for [track-key (keys loops)]
            (looped :bpm bpm
                   :out-bus (assign-bus)
                   :buffer (track-key loops)
                   :beats beats))))))

(defn play-all-tracks-simple []
  (println "Stopping any existing playback...")
  (stop)

  (println "Starting playback of all tracks...")
  (doseq [[track-name sample-fn] samples]
    (println "Playing track:" track-name)
    (sample-fn))

  (println "All tracks triggered"))

;; Alternative version using loops map
(defn play-all-loops []
  (println "Stopping any existing playback...")
  (stop)

  (println "Starting playback of all loops...")
  (doseq [[track-name buffer] loops]
    (println "Playing loop:" track-name)
    (looped :bpm 123
            :out-bus 0
            :buffer buffer
            :beats 16))

  (println "All loops triggered"))

;; To test individual tracks:
(comment
  ;; Try playing a single track first
  ((get samples :angels))

  ;; Then try all tracks
  (play-all-tracks-simple)

  ;; Or try the loops version
  (play-all-loops)

  (play-all-tracks)
  
  ;; Stop playback
  (stop))


;; Example usage:
(comment
  (play-all-tracks)  ; Play with default settings
  (play-all-tracks :bpm 128 :beats 32)  ; Custom BPM and beat length
  (stop)  ; Stop playback
  )


;;; tryign to adjust volume based on core.async channels
;;;
;;; Create a map of volume control channels for each track
(def volume-channels
  (into {}
        (for [track-key (keys loops)]
          [track-key (chan)])))

;; Create atoms to store our synth instances
(def track-synths (atom {}))

(defn start-volume-controlled-tracks
  "Start all tracks with volume control via channels.
   Returns a map of track names to their volume control channels."
  [& {:keys [bpm beats]
      :or {bpm 123 beats 16}}]

  ;; Stop any existing playback
  (stop)
  (reset! track-synths {})

  ;; Start each track with volume control
  (doseq [[track-key buffer] loops]
    (let [bus (audio-bus 2)  ; Create stereo bus for each track
          vol-chan (get volume-channels track-key)

          ;; Create synth with volume control
          synth (synth [vol 0.0]
                  (out:ar 0
                    (* vol
                       (pan2
                         (scaled-play-buf 2 buffer
                           :rate (buf-rate-scale:kr buffer)
                           :loop 1)))))

          ;; Store synth instance
          synth-instance (synth)]

      ;; Store in our atom for later control
      (swap! track-synths assoc track-key synth-instance)

      ;; Start a go-loop to handle volume changes
      (go-loop []
        (when-let [new-vol (<! vol-chan)]
          (ctl synth-instance :vol new-vol)
          (recur)))))

  ;; Return the channels map for external control
  volume-channels)

(defn set-track-volume!
  "Set volume for a specific track (0.0 to 1.0)"
  [track-key volume]
  (when-let [chan (get volume-channels track-key)]
    (put! chan volume)))

(defn stop-volume-controlled-tracks!
  "Stop all tracks and clean up channels"
  []
  (stop)
  (doseq [chan (vals volume-channels)]
    (close! chan))
  (reset! track-synths {}))

;; Example usage and test functions
(defn test-volume-fade
  "Test function to demonstrate volume control"
  []
  (let [chans (start-volume-controlled-tracks)]

    ;; Start with all volumes at 0
    (doseq [track-key (keys loops)]
      (set-track-volume! track-key 0.0))

    ;; Fade each track in and out in sequence
    (go-loop [[track & remaining] (keys loops)]
      (when track
        ;; Fade in
        (println "Fading in" track)
        (doseq [vol (range 0 1.1 0.1)]
          (set-track-volume! track vol)
          (<! (timeout 100)))

        ;; Hold
        (<! (timeout 1000))

        ;; Fade out
        (println "Fading out" track)
        (doseq [vol (range 1 -0.1 -0.1)]
          (set-track-volume! track vol)
          (<! (timeout 100)))

        ;; Move to next track
        (recur remaining)))))

;; Example of random volume changes
(defn random-volume-changes
  "Randomly change volumes of all tracks"
  []
  (let [chans (start-volume-controlled-tracks)]
    (go-loop []
      (doseq [track-key (keys loops)]
        (set-track-volume! track-key (rand)))
      (<! (timeout 500))  ; Change every 500ms
      (recur))))

;; Example of rhythmic volume patterns
(defn rhythmic-volumes
  "Create rhythmic patterns with volume changes"
  [bpm]
  (let [chans (start-volume-controlled-tracks)
        beat-time (/ 60000 bpm)]  ; milliseconds per beat
    (go-loop [beat 0]
      (doseq [track-key (keys loops)]
        (let [volume (if (zero? (mod (+ beat (hash track-key)) 4))
                      1.0  ; Accent on every 4th beat (different for each track)
                      0.3)]  ; Background volume
          (set-track-volume! track-key volume)))
      (<! (timeout beat-time))
      (recur (inc beat)))))

(comment
  ;; Start the tracks
  (def channels (start-volume-controlled-tracks))

  ;; Try some individual volume controls
  (set-track-volume! :angels 0.8)
  (set-track-volume! :rocket 0.5)

  ;; Run the test fade sequence
  (test-volume-fade)

  ;; Try random volume changes
  (random-volume-changes)

  ;; Try rhythmic patterns
  (rhythmic-volumes 120)  ; 120 BPM

  ;; Stop everything
  (stop-volume-controlled-tracks!)
)
