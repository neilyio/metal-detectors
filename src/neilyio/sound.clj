(ns neilyio.sound
  {:clj-kondo/config '{:linters {:unresolved-var {:exclude [overtone.live]}
                                 :use {:level :off}}}}
  (:use [overtone.live])
  (:require
   [babashka.fs :as fs]
   [datalevin.core :as d]
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
  (let [[current current-frame sample-rate total-frames
         loop-start-frame loop-end-frame loop-start loop-end]
        (live/control-bus-get timeline-info-bus)
        [playing-buffer playing?]
        (live/control-bus-get play-info-bus)]
    {::playing?         playing?
     ::playing-buffer   playing-buffer
     ::current          current
     ::current-frame    current-frame
     ::sample-rate      sample-rate
     ::total-frames     total-frames
     ::loop-start-frame loop-start-frame
     ::loop-end-frame   loop-end-frame
     ::loop-start       loop-start
     ::loop-end         loop-end}))

(defn bytes->sample [bytes]
  (fs/with-temp-dir [dir {}]
    (let [path (str dir "/" (random-uuid))]
      (fs/write-bytes path bytes)
      (live/sample path))))

(defn module
  "Return an event handler function that takes state and returns new state.
   Args:
   - conn - database connection to use
   Returns a function that takes:
   - state - current application state
   - event - event to handle
   Returns updated state with ::conn and ::db added."
  [get-conn set-timeline-info!]
  (let [db (d/db (get-conn))
        buffers (atom {})
        label (-> (db/selected-speaker db) :speaker/loop :loop/source :source/label)
        sources (->> (db/find-all-sources db) (filter #(= label (:source/label %))) (take 1))]
    (println "Loading" (count sources) "sound files into memory, please wait...")
    (doseq [{:source/keys [label bytes]} sources]
      (print (str (count @buffers) "... "))
      (flush)
      (swap! buffers assoc label (bytes->sample bytes)))
    (println "Done loading buffers!")
    (println "Current selected buffer: " label)
    (let [buffer (get @buffers label)
          _ (assert buffer (str "buffer not loaded for " label))
          timeline-info-bus (live/control-bus 8)
          play-info-bus (live/control-bus 2)
          timeline (timeline :buffer buffer :start 0 :state-bus timeline-info-bus)
          playcontrol (playcontrol :id (:id timeline) :play 0 :state-bus play-info-bus)]
      [:sound (fn [_]
                (let [timeline-info (timeline-info timeline-info-bus play-info-bus)
                      label (-> (db/selected-speaker db) :speaker/loop :loop/source :source/label)
                      buffer (get @buffers label)]
                  (assert buffer (str "no buffer loaded for " label))
                  (set-timeline-info! timeline-info)
                  (merge timeline-info
                         {::selected-buffer buffer
                          ::timeline timeline
                          ::playcontrol playcontrol
                          ::timeline-info-bus timeline-info-bus
                          ::play-info-bus play-info-bus})))])))

(defmethod events/handle [:sound :play] [_ #_{::keys [playcontrol timeline]}]
  (live/ctl playcontrol :id timeline :play 1))

(defmethod events/handle [:sound :pause] [_ #_{::keys [playcontrol timeline]}]
  (live/ctl playcontrol :id timeline :play 0))

(defmethod events/handle [:sound :play-toggle] [{::keys [playcontrol timeline playing?]}]
  (live/ctl playcontrol :id timeline :play (if (zero? playing?) 1 0)))

; (defmethod events/handle [:sound :stop] [{state :state}]
;   (swap! state assoc ::timeline nil ::playcontrol nil)
;   (stop))

; (defmethod events/handle [:sound :seek-left] [{state :state}]
;   (let [{::keys [timeline current seek-size]} (context state)]
;     (ctl timeline :in (clamp (- current seek-size)))
;     (output (context state) [:seek-size :current])))

; (defmethod events/handle [:sound :seek-right] [{state :state}]
;   (let [{::keys [timeline current seek-size]} (context state)]
;     (ctl timeline :in (clamp (+ current seek-size)))
;     (output (context state) [:seek-size :current])))

; (defmethod events/handle [:sound :seek-to] [{state :state [_ in] :event}]
;   (let [{::keys [timeline]} (context state)]
;     (ctl timeline :in (clamp in))))

; (defmethod events/handle [:sound :select-next-source] [{state :state}]
;   (let [{::keys [timeline selected-buffer]} (context state)]
;     (ctl timeline :buffer selected-buffer)
;     (output (context state) [:current])))

; (defmethod events/handle [:sound :select-prev-source] [{state :state}]
;   (let [{::keys [timeline selected-buffer]} (context state)]
;     (ctl timeline :buffer selected-buffer)
;     (output (context state) [:current])))
