(ns ^:clj-reload/no-reload neilyio.sound
  {:clj-kondo/config '{:linters {:unresolved-var {:exclude [overtone.live]}
                                 :use {:level :off}}}}
  (:use [overtone.live])
  (:require
   [datalevin.core :as d]
   [neilyio.cache :as cache]
   [neilyio.db :as db]
   [neilyio.events :as events]
   [overtone.live :as live]))

(live/defsynth timeline
  "Takes the id of the buffer to play, and the normalized (0 - 1) start/end,
   which loops within the buffer. Also takes a state-bus which can be polled
   to get the absolute position in frames within the buffer."
  [buffer 0 in 0 out 1 state-bus 0]
  (let [sample-rate (live/buf-sample-rate:kr buffer)
        rate-scale (live/buf-rate-scale:kr buffer)
        frames (live/buf-frames:kr buffer)
        start-pos (* in frames)
        end-pos   (* out frames)
        ;; using the last-value of start to determine if we should trigger reset-pos.
        start-pos-delta (abs (- in (live/last-value in :diff 0)))
        ;; so important! both start-pos and reset-pos MUST be set here, or seeking will be off.
        ptr (live/phasor:ar :start start-pos :end end-pos
                            :reset-pos start-pos
                            :rate rate-scale :trig start-pos-delta)]
    ;; record state in a control bus
    ;; remember you have to update the bus channel count
    (live/out:kr state-bus [(/ (live/a2k ptr) frames) ;; current
                            (live/a2k ptr)            ;; current-frame
                            sample-rate               ;; sample-rate
                            rate-scale                ;; rate-scale
                            frames                    ;; total-frames
                            start-pos                 ;; loop-start-frame
                            end-pos                   ;; loop-end-frame
                            in                        ;; loop-start
                            out                       ;; loop-end
                            ])
    (live/out:ar 0 (live/buf-rd 2 buffer ptr))))

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

(defn module
  "Return an event handler function that takes state and returns new state.
   Args:
   - conn - database connection to use
   Returns a function that takes:
   - state - current application state
   - event - event to handle
   Returns updated state with ::conn and ::db added."
  [get-conn set-timeline-info!]
  (let [db                 (d/db (get-conn))
        source-id          (-> (db/selected-speaker db) :speaker/loop :loop/source :db/id)
        buffer             (cache/get-buffer (:cache/conn (events/get-state)) source-id)
        timeline-info-bus  (live/control-bus 8)
        play-info-bus      (live/control-bus 2)
        timeline           (timeline :buffer (or buffer 0) :start 0 :state-bus timeline-info-bus)
        playcontrol        (playcontrol :id (:id timeline) :play 0 :state-bus play-info-bus)]
      [:sound (fn [_]
                (let [db            (d/db (get-conn))
                      timeline-info (timeline-info timeline-info-bus play-info-bus)
                      selected-loop (-> (db/selected-speaker db) :speaker/loop)
                      source-id     (-> selected-loop :loop/source :db/id)
                      buffer        (cache/get-buffer (:cache/conn (events/get-state)) source-id)]
                  (tap> [:sound-module buffer label selected-loop timeline-info])
                  (set-timeline-info! timeline-info)
                  (merge timeline-info
                         selected-loop
                         {::selected-buffer buffer
                          ::timeline timeline
                          ::playcontrol playcontrol
                          ::timeline-info-bus timeline-info-bus
                          ::play-info-bus play-info-bus})))])))

(defmethod events/handle [:sound :play]
  [{::keys [playcontrol timeline selected-buffer]}]
  (when selected-buffer
    (live/ctl playcontrol :id timeline :play 1)))

(defmethod events/handle [:sound :pause]
  [{::keys [playcontrol timeline selected-buffer]}]
  (when selected-buffer
    (live/ctl playcontrol :id timeline :play 0)))

(defmethod events/handle [:sound :play-toggle]
  [{::keys [playcontrol timeline playing? selected-buffer]}]
  (when selected-buffer
    (live/ctl playcontrol :id timeline :play (if (zero? playing?) 1 0))))

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
               :select-prev-source
               :select-next-source
               :location]]

  (defmethod events/handle [:sound event] [{:loop/keys [in out] ::keys [timeline selected-buffer]}]
    (when selected-buffer
      (when in  (live/ctl timeline :buffer selected-buffer :in in))
      (when out (live/ctl timeline :buffer selected-buffer :out out)))))

