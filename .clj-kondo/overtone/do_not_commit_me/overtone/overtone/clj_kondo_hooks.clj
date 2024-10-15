(ns
 overtone.overtone.clj-kondo-hooks
 (:require [clj-kondo.hooks-api :as api]))

(def overloaded-ugens '[= < <= * min not= > mod - or / >= + max and])

(defn
 defsynth
 [{:keys [node]}]
 (let
  [var-sym
   (second (:children node))
   binding-vec-or-docstring+body
   (drop 2 (:children node))
   [_docstring binding-vec body]
   (if
    (api/vector-node? (first binding-vec-or-docstring+body))
    [nil
     (first binding-vec-or-docstring+body)
     (vec (rest binding-vec-or-docstring+body))]
    [(first binding-vec-or-docstring+body)
     (second binding-vec-or-docstring+body)
     (vec (drop 2 binding-vec-or-docstring+body))])
   new-node
   (api/list-node
    (list
     (api/token-node 'defn)
     var-sym
     (api/coerce '[& _])
     (api/list-node
      (list*
       (api/token-node 'let)
       (api/coerce
        (vec
         (concat
          (mapcat (fn [v] [v (constantly nil)]) overloaded-ugens)
          (:children binding-vec))))
       (api/coerce overloaded-ugens)
       body))))]
  {:node new-node}))

(defn
 demo
 [{:keys [node]}]
 (let
  [body
   (rest (:children node))
   new-node
   (api/list-node
    (list*
     (api/token-node 'let)
     (api/coerce
      (vec (mapcat (fn [v] [v (constantly nil)]) overloaded-ugens)))
     (api/coerce overloaded-ugens)
     body))]
  {:node new-node}))

(defn
 defunk
 [{:keys [node]}]
 (let
  [[var-sym _docstring binding-vec & body]
   (rest (:children node))
   new-node
   (api/list-node
    (list
     (api/token-node 'defn)
     var-sym
     (api/coerce '[& _])
     (api/list-node (list* (api/token-node 'let) binding-vec body))))]
  {:node new-node}))

(defn
 defunk-env
 [{:keys [node]}]
 (let
  [[var-sym docstring binding-vec & body]
   (rest (:children node))
   new-node
   (api/list-node
    (list
     (api/token-node 'do)
     (api/list-node
      (list*
       (api/token-node 'overtone.helpers.lib/defunk)
       var-sym
       docstring
       binding-vec
       body))
     (api/list-node
      (list*
       (api/token-node 'overtone.helpers.lib/defunk)
       (api/token-node (symbol (str "env-" (:string-value var-sym))))
       docstring
       binding-vec
       body))))]
  {:node new-node}))

(defn
 defcgen
 [{:keys [node]}]
 (let
  [var-sym
   (second (:children node))
   [_docstring binding-vec & body]
   (drop 2 (:children node))
   rates
   (api/sexpr
    (->>
     body
     (filter
      (fn*
       [p1__11640#]
       (and
        (= (api/tag p1__11640#) :list)
        (contains?
         #{:ar :dr :ir :kr}
         (:k (first (:children p1__11640#)))))))
     (mapv (fn* [p1__11641#] (:k (first (:children p1__11641#)))))
     set))
   new-node
   (api/list-node
    (list*
     (api/token-node 'do)
     (->>
      (conj rates nil)
      (mapv
       (fn
        [rate]
        (api/list-node
         (list
          (api/token-node 'defn)
          (api/token-node
           (symbol
            (str
             (:string-value var-sym)
             (when rate (str ":" (name rate))))))
          (api/coerce '[& _])
          (api/list-node
           (list*
            (api/token-node 'let)
            (api/coerce
             (api/vector-node
              (->>
               (vec (concat overloaded-ugens (:children binding-vec)))
               api/coerce
               :children
               (filter
                (fn* [p1__11642#] (= (api/tag p1__11642#) :token)))
               (mapcat
                (fn
                 [token]
                 [token
                  (api/list-node
                   (list
                    (api/token-node 'eval)
                    (api/token-node nil)))]))
               vec)))
            (api/coerce overloaded-ugens)
            body)))))))))]
  {:node new-node}))

(comment
 (->
  {:node
   (api/parse-string
    (str
     '(defcgen
       varlag
       "Variable shaped lag"
       [in
        {:default 0, :doc "Input to lag"}
        time
        {:default 0.1, :doc "Lag time in seconds"}
        curvature
        {:default 0,
         :doc
         "Control curvature if shape input is 5 (default). 0 means linear, positive and negative numbers curve the segment up and down."}
        shape
        {:default 5,
         :doc
         "Shape of curve. 0: step, 1: linear, 2: exponential, 3: sine, 4: welch, 5: custom (use curvature param), 6: squared, 7: cubed, 8: hold"}]
       "Similar to Lag but with other curve shapes than exponential. A change on the input will take the specified time to reach the new value. Useful for smoothing out control signals."
       (:kr
        (let
         [gate
          (+
           (+ (impulse:kr 0 0) (> (abs (hpz1 in)) 0))
           (> (abs (hpz1 time)) 0))]
         (env-gen [in 1 -99 -99 in time shape curvature] gate))))))}
  defcgen
  :node
  api/sexpr)
 ())

(defn
 defrecord-ifn
 [{:keys [node]}]
 (let
  [var-sym
   (second (:children node))
   [fields invoke-fn & body]
   (drop 2 (:children node))
   new-node
   (api/list-node
    (list*
     (api/token-node 'defrecord)
     var-sym
     fields
     (list* (api/token-node 'Object) invoke-fn body)))]
  {:node new-node}))

(defn
 defsynth-load
 [{:keys [node]}]
 (let
  [var-sym
   (second (:children node))
   string
   (first (drop 2 (:children node)))
   new-node
   (api/list-node
    (list (api/token-node 'defn) var-sym (api/coerce '[& _]) string))]
  {:node new-node}))

(defn
 gen-stringed-synth
 [{:keys [node]}]
 (let
  [var-sym
   (second (:children node))
   binding-vec-or-docstring+body
   (drop 2 (:children node))
   new-node
   (api/list-node
    (list
     (api/token-node 'defn)
     var-sym
     (api/coerce '[& _])
     (api/list-node
      (list (api/token-node 'eval) (api/token-node nil)))))]
  {:node new-node}))

(defn
 defcheck
 [{:keys [node]}]
 (let
  [[var-sym params default-message & body]
   (rest (:children node))
   body-node
   (api/list-node
    (list*
     (api/token-node 'let)
     (api/coerce
      (vec
       (mapcat
        (fn [v] [v (constantly nil)])
        '[rate num-outs inputs ugen spec])))
     (api/coerce '[rate num-outs inputs ugen spec])
     default-message
     body))
   new-node
   (api/list-node
    (list
     (api/token-node 'defn)
     var-sym
     (api/list-node (list params body-node))
     (api/list-node
      (list
       (api/coerce (vec (conj (:children params) 'message)))
       (api/token-node 'message)
       body-node))))]
  {:node new-node}))

(defn
 defspec
 [{:keys [node]}]
 (let
  [[var-sym & body]
   (rest (:children node))
   new-node
   (api/list-node
    (list (api/token-node 'def) var-sym (api/vector-node body)))]
  {:node new-node}))

(defn
 defexamples
 [{:keys [node]}]
 (let
  [[var-sym & body]
   (rest (:children node))
   new-node
   (api/list-node
    (list*
     (api/token-node 'def)
     var-sym
     (map
      (fn
       [{:keys [children]}]
       (let
        [[name
          _summary
          _long-doc
          _rate-sym
          _rate
          _params
          _body-str
          _contributor-sym
          _contributor]
         children]
        name))
      body)))]
  {:node new-node}))

(comment
 (->
  {:node
   (api/parse-string
    "(defexamples send-reply\n        (:count\n         \"Short\"\n         \"Full\"\n\n         rate :kr\n         [rate {:default 3 :doc \"Rate of count in times per second. Increase to up the count rate\"}]\n         \"\n  (let [tr   (impulse rate)\n        step (stepper tr 0 0 12)]\n    (send-reply tr \\\"/count\\\" [step] 42))\"\n         contributor \"Sam Aaron\"))")}
  defexamples
  :node
  str)
 ())

(defn
 defclib
 [{:keys [node]}]
 (let
  [[var-sym & body]
   (rest (:children node))
   new-node
   (api/list-node
    (list
     (api/token-node 'def)
     var-sym
     (api/list-node (list 'quote body))))]
  {:node new-node}))

(comment
 (str
  (:node
   (defsynth
    {:node
     (api/parse-string
      "\n(defsynth ppp2\n  [bus 0\n   eita 3\n   amp 1]\n  (out bus (* amp (mda-piano :freq (midicps 50)\n                             #_ #_ #_ #_:release 0.5\n                             :decay 0))))")})))
 ())

(def
 vars
 '{overtone.sc.ugens/impulse:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dbrown2:dr {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-out-stream? {:arglists ([bs])},
   overtone.sc.ugens/drive-noise {:arglists ([& _]), :special? true},
   overtone.studio.midi-player/midi-poly-player
   {:arglists
    ([play-fn] [play-fn player-key] [play-fn device-key player-key])},
   overtone.sc.ugens/FREE-AND-AFTER {},
   overtone.music.pitch/interval-freq
   {:arglists ([base-freq n] [base-freq n mode tuning])},
   overtone.sc.envelope/step-shape {:arglists ([pos y1 y2])},
   overtone.sc.ugens/lpz2:ar {:arglists ([& _]), :special? true},
   overtone.sc.synth/synth-args {:arglists ([synth])},
   overtone.sc.ugens/disintegrator:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/round-up {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-output-buses
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dust:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rlpfd:ar {:arglists ([& _]), :special? true},
   overtone.music.rhythm/IMetronome {},
   overtone.sc.info/server-num-output-buses {:arglists ([])},
   overtone.sc.ugens/os-fold4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman2-dc:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/index-in-between:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.synth/modify-synth-params
   {:arglists ([s & params-vals])},
   overtone.sc.ugens/double-well:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.synth/run {:arglists ([& body]), :macro true},
   overtone.sc.info/buffer-count* {},
   overtone.libs.asset/asset-path {:arglists ([url] [url name])},
   overtone.sc.ugens/ratiomidi {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-local-max:kr
   {:arglists ([& _]), :special? true},
   overtone.samples.freesound/freesound-searchm
   {:arglists ([ks* q* & params]), :macro true},
   overtone.config.store/store {:arglists ([])},
   overtone.config.store/__LOAD-CONFIG__ {},
   overtone.sc.ugens/lpfvs6:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lfd-noise1:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sqrt {:arglists ([& _]), :special? true},
   overtone.sc.node/node-place* {:arglists ([node position target])},
   overtone.sc.ugens/spring:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard-trig:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nested-allpass-n:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/set-buf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/apf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-scale:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-user-group
   {:arglists ([])},
   overtone.sc.cgens.demand/dseries
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/slew:ar {:arglists ([& _]), :special? true},
   overtone.sc.sample/cached-samples* {},
   overtone.sc.ugens/select {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-tri {:arglists ([& _]), :special? true},
   overtone.config.store/CONFIG-DEFAULTS {},
   overtone.sc.ugens/pv-hainsworth-foote:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-n {:arglists ([& _]), :special? true},
   overtone.sc.node/node-control* {:arglists ([node name-values])},
   overtone.sc.ugens/dwg-sound-board:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ramp:kr {:arglists ([& _]), :special? true},
   overtone.osc/without-osc-bundle {:arglists ([& body]), :macro true},
   overtone.sc.cgens.tap/tap:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-shift:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-brick-wall:kr
   {:arglists ([& _]), :special? true},
   overtone.libs.event/map->LossyWorker
   {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/bpz2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-samples:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan-az:ar {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-distortion2
   {:arglists ([& _]), :special? true},
   overtone.repl.ugens/print-ugen-docs {:arglists ([specs])},
   overtone.sc.ugens/b-hi-pass:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-wrap8 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-common-mul:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nl-filt-l:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-saw:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/moog-ladder:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dc:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dn:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-soft-wipe {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-inst-controller
   {:arglists ([state-atom handler mapping])},
   overtone.sc.ugens/beat-track {:arglists ([& _]), :special? true},
   overtone.algo.chance/chosen-from {:arglists ([coll] [coll limit])},
   overtone.sc.ugens/median:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/klang:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/log {:arglists ([& _]), :special? true},
   overtone.version/OVERTONE-VERSION-STR {},
   overtone.sc.ugens/fold:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-all-pass {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-player-stop
   {:arglists ([] [player-or-key])},
   overtone.sc.ugens/mda-piano {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise0
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/cubed {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mda-piano:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-pluck:ar {:arglists ([& _]), :special? true},
   overtone.sc.trig/on-sync-trigger
   {:arglists ([trig-id handler key] [node trig-id handler key])},
   overtone.sc.ugens/demand-env-gen:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-find-connected-devices
   {:arglists ([search])},
   overtone.sc.ugens/internal:scope-out2:kr
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/scale-field {:arglists ([skey & [sname]])},
   overtone.sc.ugens/v-osc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sweep:ar {:arglists ([& _]), :special? true},
   overtone.sc.envelope/perc {:arglists ([& _]), :special? true},
   overtone.sc.bus/control-bus
   {:arglists ([] [n-channels-or-name] [n-channels name])},
   overtone.sc.ugens/henon-trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:d-gauss
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dwhite:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/index:ir {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/dbrown {:arglists ([& _]), :special? true},
   overtone.sc.cgens.io/scaled-v-disk-in:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-x-fade2 {:arglists ([& _]), :special? true},
   overtone.music.pitch/scale
   {:arglists ([root scale-name] [root scale-name degrees])},
   overtone.sc.ugens/detect-index:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/neg {:arglists ([& _]), :special? true},
   overtone.sc.node/node-pause {:arglists ([this])},
   overtone.studio.mixer/recording-stop {:arglists ([])},
   overtone.sc.ugens/pv-max:kr {:arglists ([& _]), :special? true},
   overtone.studio.transport/DEFAULT-BPM {},
   overtone.sc.ugens/breakcore {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-moog {:arglists ([& _]), :special? true},
   overtone.sc.ugens/wel-window {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dbrown:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/radians-per-sample:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-allpass-l {:arglists ([& _]), :special? true},
   overtone.music.pitch/unison {:arglists ([freq__7472__auto__])},
   overtone.sc.ugens/brf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/demand-env-gen
   {:arglists ([& _]), :special? true},
   overtone.libs.event/remove-event-handler {:arglists ([key])},
   overtone.sc.server/server-status {:arglists ([])},
   overtone.sc.ugens/squared {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy5:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/running-max:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-l:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/vbap {:arglists ([& _]), :special? true},
   overtone.sc.synth/synth {:arglists ([sname & args]), :macro true},
   overtone.sc.ugens/brz2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/record-buf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/subsample-offset:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.freq/add-cents:ar
   {:arglists ([& _]), :special? true},
   overtone.libs.event/->LossyWorker
   {:arglists ([queue worker current-val])},
   overtone.sc.synth/demo {:arglists ([& body]), :macro true},
   overtone.sc.ugens/two-zero {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-dur:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-par:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/*node-tree-data* {},
   overtone.sc.ugens/clip:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/floor {:arglists ([& _]), :special? true},
   overtone.studio.mixer/clear-instruments {:arglists ([])},
   overtone.sc.ugens/exp-rand:ir {:arglists ([& _]), :special? true},
   overtone.music.pitch/find-scale-name {:arglists ([scale])},
   overtone.sc.ugens/ay {:arglists ([& _]), :special? true},
   overtone.sc.node/->SynthGroup
   {:arglists ([group id target position status loaded?])},
   overtone.studio.inst/pre-inst {:arglists ([& args]), :macro true},
   overtone.sc.buffer/buffer-stream-close {:arglists ([buf-stream])},
   overtone.sc.ugens/t-exp-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/normalizer {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-buffers:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/key-state:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpf1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/demand:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/balance2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-rate-scale
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/i-env-gen:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hpf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ringz:ar {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/without-namespace-in-synthdef
   {:arglists ([& body]), :macro true},
   overtone.sc.ugens/buf-comb-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-low-shelf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-comb-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stepper {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pink-noise:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fft:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-rd:kr {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-noise-gate
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:poll:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gravity-grid2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pulse:ar {:arglists ([& _]), :special? true},
   overtone.sc.cgens.beq-suite/b-low-pass4:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/shaper:kr {:arglists ([& _]), :special? true},
   overtone.music.pitch/chord
   {:arglists ([root chord-name] [root chord-name inversion])},
   overtone.sc.sample/load-samples {:arglists ([& glob-paths])},
   overtone.sc.cgens.demand/dseries:dr
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/ONLY-MIDI-NOTE-RE {},
   overtone.sc.ugens/trig1:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/node-place {:arglists ([this position dest-node])},
   overtone.sc.ugens/dfm1:ar {:arglists ([& _]), :special? true},
   overtone.repl.debug/expand-control-ugs
   {:arglists ([control-ugs sdef])},
   overtone.sc.cgens.tap/validate-tap! {:arglists ([label freq src])},
   overtone.music.pitch/find-chord {:arglists ([notes])},
   overtone.sc.info/output-bus-count* {},
   overtone.sc.ugens/smooth-decimator:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-grains2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amp-comp:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-moog:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fb-sine-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-trunc4:ar {:arglists ([& _]), :special? true},
   overtone.sc.clock/server-clock-drift {:arglists ([])},
   overtone.sc.ugens/double-well3 {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-mk-full-device-key {:arglists ([dev])},
   overtone.sc.ugens/vbap:ar {:arglists ([& _]), :special? true},
   overtone.sc.sample/stereo-partial-player
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon-trig:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gauss-trig:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/brusselator:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.sample/sample? {:arglists ([s])},
   overtone.sc.cgens.mix/sum {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dswitch1 {:arglists ([& _]), :special? true},
   overtone.algo.chance/sputter
   {:arglists
    ([list]
     [list prob]
     [list prob max]
     [[head & tail] prob max result])},
   overtone.sc.ugens/frame-compare:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman2-dl:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/local-in:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-modal-bar:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hypot {:arglists ([& _]), :special? true},
   overtone.sc.ugens/warp1:ar {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-rlpf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hpf {:arglists ([& _]), :special? true},
   overtone.sc.envelope/envelope
   {:arglists
    ([levels durations]
     [levels durations curves]
     [levels durations curves release-node]
     [levels durations curves release-node loop-node])},
   overtone.sc.sample/free-all-loaded-samples {:arglists ([])},
   overtone.sc.ugens/iir-filter {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-c {:arglists ([& _]), :special? true},
   overtone.sc.server/server-connecting? {:arglists ([])},
   overtone.sc.ugens/b-moog {:arglists ([& _]), :special? true},
   overtone.sc.ugens/exp-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bl-buf-rd {:arglists ([& _]), :special? true},
   overtone.sc.ugens/midicps {:arglists ([& _]), :special? true},
   overtone.sc.ugens/round-down {:arglists ([& _]), :special? true},
   overtone.sc.buffer/map->BufferFile {:arglists ([m__7972__auto__])},
   overtone.sc.buffer/buffer-wave-fill!
   {:arglists ([buf option flag partials-vector])},
   overtone.sc.ugens/brown-noise:kr
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-rm-listener {:arglists ([peer key])},
   overtone.sc.envelope/env-triangle
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/offset-out {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard2-dn:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.event/padd
   {:arglists
    ([k
      pattern
      &
      {:keys [quant clock offset],
       :as opts,
       :or {quant 1, offset 0, clock transport/*clock*}}])},
   overtone.sc.ugens/mid-eq:ar {:arglists ([& _]), :special? true},
   overtone.music.rhythm/beat-ms {:arglists ([b bpm])},
   overtone.sc.ugens/white-noise {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dl {:arglists ([& _]), :special? true},
   overtone.sc.trig/on-trigger
   {:arglists ([trig-id handler key] [node trig-id handler key])},
   overtone.sc.ugens/free-self {:arglists ([& _]), :special? true},
   overtone.libs.asset/registered-assets
   {:arglists ([key] [key name])},
   overtone.sc.cgens.demand/t-duty:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-channels:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.server/connection-info {:arglists ([])},
   overtone.sc.buffer/assert-less-than-max-buffers {:arglists ([key])},
   overtone.sc.ugens/grain-sin {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag3 {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-full-device-key {:arglists ([dev])},
   overtone.sc.ugens/rlpf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/two-zero:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/spec-pcile {:arglists ([& _]), :special? true},
   overtone.studio.inst/inst-channels {:arglists ([inst & _args])},
   overtone.sc.bus/control-bus? {:arglists ([bus])},
   overtone.sc.ugens/set-reset-ff:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-compander:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sample-rate {:arglists ([& _]), :special? true},
   overtone.sc.ugens/vibrato {:arglists ([& _]), :special? true},
   overtone.sc.ugens/local-out:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/play-buf:kr {:arglists ([& _]), :special? true},
   overtone.music.pitch/resolve-chord {:arglists ([chord])},
   overtone.sc.ugens/POWER {},
   overtone.music.time/interspaced
   {:arglists
    ([ms-period fun]
     [ms-period fun initial-delay]
     [ms-period fun initial-delay description])},
   overtone.sc.ugens/henon-c {:arglists ([& _]), :special? true},
   overtone.sc.node/group
   {:arglists
    ([]
     [name-or-position]
     [name-or-position position-or-target]
     [name position target]
     [name id position target])},
   overtone.sc.ugens/quad-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/demand-env-gen:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/toggle-ff {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hpz1:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag-ud:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/INF {},
   overtone.sc.ugens/cpsoct {:arglists ([& _]), :special? true},
   overtone.config.store/config {:arglists ([])},
   overtone.sc.ugens/stk-banded-wg {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-frames:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/ensure-node-active!
   {:arglists ([node] [node err-msg])},
   overtone.sc.ugens/pv-add {:arglists ([& _]), :special? true},
   overtone.studio.event/pclear {:arglists ([])},
   overtone.sc.ugens/lorenz2-dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-diffuser {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rand-seed:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free-self:kr {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-find-connected-receiver
   {:arglists ([search])},
   overtone.sc.ugens/lfd-clip-noise:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/__INTERN-UGENS__ {},
   overtone.sc.ugens/delay-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-pulse {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ti-rand {:arglists ([& _]), :special? true},
   overtone.libs.asset/asset-bundle-path {:arglists ([url name])},
   overtone.sc.ugens/with-overloaded-ugens
   {:arglists ([& body]), :macro true},
   overtone.studio.inst/mono-inst-mixer
   {:arglists ([& _]), :special? true},
   overtone.sc.synth/synth-arg-index {:arglists ([synth arg-name])},
   overtone.studio.midi/midi-capture-next-control-input
   {:arglists ([] [with-key?])},
   overtone.sc.ugens/henon2-dn:kr {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-chorus {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dn {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in-feedback {:arglists ([& _]), :special? true},
   overtone.music.pitch/MIDDLE-C {},
   overtone.studio.fx/fx-sustainer {:arglists ([& _]), :special? true},
   overtone.sc.ugens/compander:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clipper4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/set-reset-ff:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-mk-control-key-keyword
   {:arglists ([prefix control-key])},
   overtone.sc.buffer/supported-file-types {},
   overtone.sc.cgens.demand/dbrown:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dfm1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz2-dn {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ring3 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-low-shelf:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/radians-per-sample
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-brown-rand:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sinh {:arglists ([& _]), :special? true},
   overtone.sc.synth/control-proxy-cache {},
   overtone.sc.ugens/send-trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/shaper {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/convolution2-l:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/out {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-div:kr {:arglists ([& _]), :special? true},
   overtone.sc.bus/map->ControlBus {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/allpass-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/local-out:kr {:arglists ([& _]), :special? true},
   overtone.sc.clock/server-clock-uptime {:arglists ([])},
   overtone.sc.ugens/dwg-bowed-simple:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-soft-wipe:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-control-agents* {},
   overtone.sc.ugens/internal:local-buf
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bl-buf-rd:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak-follower:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decay2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-pulse:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hpf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/del-tap-wr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag2-ud:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-min:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stepper:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sweep:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/v-osc3 {:arglists ([& _]), :special? true},
   overtone.sc.node/NODE-POSITION {},
   overtone.sc.cgens.tap/tap {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-dur:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-beta-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/record-buf:ar {:arglists ([& _]), :special? true},
   overtone.algo.chance/choose-n {:arglists ([n col])},
   overtone.sc.ugens/b-band-pass:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/quad-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-output-buses:ir
   {:arglists ([& _]), :special? true},
   overtone.studio.inst/__MIXER-SYNTHS__ {},
   overtone.sc.ugens/median:kr {:arglists ([& _]), :special? true},
   overtone.studio.event/handle-chord {:arglists ([e])},
   overtone.sc.ugens/pv-bin-shift:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.io/sound-in {:arglists ([& _]), :special? true},
   overtone.sc.ugens/svf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-exp {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-distortion-tubescreamer
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-debug {:arglists ([& [on-off]])},
   overtone.sc.ugens/latoocarfian2-dl:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-sample-rate
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sin-osc-fb:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-fold8:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ti-rand:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/status {},
   overtone.osc/zero-conf-off {:arglists ([])},
   overtone.sc.ugens/lpfvs6:kr {:arglists ([& _]), :special? true},
   overtone.samples.freesound/*api-key* {},
   overtone.sc.ugens/convolution2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.info/sample-dur* {},
   overtone.sc.buffer/map->Buffer {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/dust2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/brusselator {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE {},
   overtone.sc.ugens/t-grains3:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/node-map-n-controls
   {:arglists ([this start-control start-bus n])},
   overtone.sc.ugens/grain-fm {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ceil {:arglists ([& _]), :special? true},
   overtone.sc.sample/load-sample {:arglists ([path & args])},
   overtone.music.rhythm/metronome {:arglists ([bpm])},
   overtone.music.pitch/amp->db {:arglists ([amp])},
   overtone.sc.ugens/nl-filt-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/control-rate:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/del-tap-rd:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpfvs6 {:arglists ([& _]), :special? true},
   overtone.sc.node/node-get-controls
   {:arglists ([node control-names])},
   overtone.music.pitch/invert-chord {:arglists ([notes shift])},
   overtone.sc.cgens.env/hold {:arglists ([& _]), :special? true},
   overtone.sc.node/group-append-node {:arglists ([group node])},
   overtone.osc/in-unested-osc-bundle
   {:arglists ([client timestamp & body]), :macro true},
   overtone.sc.ugens/trig1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/osc:ar {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-rhpf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/convolution {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/diwhite:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.info/server-radians-per-sample {:arglists ([])},
   overtone.sc.ugens/RCOMPLEX {},
   overtone.sc.ugens/last-value:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/trig:ar {:arglists ([& _]), :special? true},
   overtone.studio.mixer/wait-until-mixer-booted {:arglists ([])},
   overtone.sc.ugens/onsets {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag3-ud:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clip-amp8
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/resolve-degrees {:arglists ([degrees])},
   overtone.sc.ugens/wrap:ar {:arglists ([& _]), :special? true},
   overtone.libs.asset/asset-bundle-dir {:arglists ([url])},
   overtone.sc.ugens/LINEAR {},
   overtone.sc.envelope/env-asr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/glitch-rhpf:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-limiter {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-low-pass:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-hi-shelf:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/find-pitch-class-name {:arglists ([note])},
   overtone.sc.ugens/leak-dc:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard2-dn:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-cong-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lfd-noise0:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rotate2:ar {:arglists ([& _]), :special? true},
   overtone.studio.inst/inst {:arglists ([sname & args]), :macro true},
   overtone.sc.ugens/stk-bowed:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-squared:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/disk-in:ar {:arglists ([& _]), :special? true},
   overtone.sc.bus/->AudioBus {:arglists ([id n-channels rate name])},
   overtone.sc.ugens/allpass-l:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/grain-fm:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pause-self {:arglists ([& _]), :special? true},
   overtone.sc.ugens/control-dur {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gray-noise:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sos {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ifft:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-phase-shift90:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-samples {:arglists ([& _]), :special? true},
   overtone.sc.ugens/done:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/detect-silence
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-line:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-noise:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-wrap4:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dn:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/grain-in:ar {:arglists ([& _]), :special? true},
   overtone.sc.buffer/->Buffer
   {:arglists ([id size n-channels rate status name])},
   overtone.sc.ugens/num-input-buses:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/NO-ACTION {},
   overtone.sc.ugens/klank {:arglists ([& _]), :special? true},
   overtone.samples.freesound/access-token {:arglists ([code])},
   overtone.sc.ugens/lfd-noise1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-alloc-read-channel
   {:arglists
    ([path channel]
     [path channel start]
     [path channel start n-frames])},
   overtone.sc.ugens/peak-eq4:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-running-synths:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/atan2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-c:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/g-verb {:arglists ([& _]), :special? true},
   overtone.sc.sample/sample-player {:arglists ([smpl & pargs])},
   overtone.sc.dyn-vars/inactive-buffer-modification-error
   {:arglists ([])},
   overtone.studio.event/pname-mapping {},
   overtone.sc.ugens/gravity-grid:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-n:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.oscillators/pm-osc:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/RECT {},
   overtone.sc.ugens/demand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pulse-divider:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/analyse-events2
   {:arglists ([& _]), :special? true},
   overtone.osc/zero-conf-on {:arglists ([])},
   overtone.sc.ugens/gate:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lfd-noise3 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clear-buf {:arglists ([& _]), :special? true},
   overtone.sc.info/radians-per-sample* {},
   overtone.sc.ugens/ball:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/saw {:arglists ([& _]), :special? true},
   overtone.sc.info/server-info {:arglists ([])},
   overtone.sc.ugens/lag2-ud {:arglists ([& _]), :special? true},
   overtone.sc.ugens/integrator {:arglists ([& _]), :special? true},
   overtone.algo.trig/tanr {:arglists ([idx range centre period])},
   overtone.sc.envelope/env-cutoff {:arglists ([& _]), :special? true},
   overtone.sc.cgens.freq/freq-spread:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-n
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/least-change:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/reciprocal {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-allpass-n:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-rand-wipe {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lfd-noise3:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.mix/mix:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-morph:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-cong-l:ar {:arglists ([& _]), :special? true},
   overtone.music.time/apply-by {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-control
   {:arglists ([rcv ctl-num val] [rcv ctl-num val channel])},
   overtone.sc.ugens/fos:kr {:arglists ([& _]), :special? true},
   overtone.osc/osc-send {:arglists ([client path & args])},
   overtone.sc.ugens/spec-centroid {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dser {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-mk-full-device-event-key
   {:arglists ([dev command])},
   overtone.sc.ugens/osc-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clipper8 {:arglists ([& _]), :special? true},
   overtone.sc.envelope/sine {:arglists ([& _]), :special? true},
   overtone.sc.cgens.mix/splay {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ramp:ar {:arglists ([& _]), :special? true},
   overtone.music.time/after-delay
   {:arglists ([ms-delay fun] [ms-delay fun description])},
   overtone.sc.ugens/FREE-PAUSE-BEFORE {},
   overtone.sc.ugens/nl-filt-n:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/play-buf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/index:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clipper4 {:arglists ([& _]), :special? true},
   overtone.studio.pattern/pnext {:arglists ([s] [s dur])},
   overtone.sc.ugens/running-min:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clipper4:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ifft {:arglists ([& _]), :special? true},
   overtone.sc.ugens/key-state {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy5:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-flute:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dn:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/spec-centroid:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rotate2:kr {:arglists ([& _]), :special? true},
   overtone.sc.sample/mono-partial-player
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nl-filt-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/one-zero:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sync-saw:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clip-noise {:arglists ([& _]), :special? true},
   overtone.repl.ugens/ugen-doc {:arglists ([ug-name]), :macro true},
   overtone.sc.ugens/w-amp {:arglists ([& _]), :special? true},
   overtone.studio.pattern/pfirst {:arglists ([s])},
   overtone.sc.ugens/t-beta-rand:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.info/control-rate* {},
   overtone.sc.ugens/freq-shift {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dl:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpz1 {:arglists ([& _]), :special? true},
   overtone.music.pitch/nth-equal-tempered-freq
   {:arglists ([base-freq interval])},
   overtone.sc.ugens/trapezoid:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pink-noise:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/node-free* {:arglists ([node])},
   overtone.sc.ugens/lf-saw:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-rand:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bpf:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/node-block-until-ready* {:arglists ([node])},
   overtone.sc.ugens/stk-shakers:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.line/lin-lin:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/thresh {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nl-filt-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-mandolin {:arglists ([& _]), :special? true},
   overtone.samples.freesound/freesound-samples {:arglists ([& ids])},
   overtone.studio.event/pplay {:arglists ([k pattern & args])},
   overtone.sc.ugens/trig1:kr {:arglists ([& _]), :special? true},
   overtone.sc.server/clear-all {:arglists ([])},
   overtone.sc.ugens/stk-voic-form:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/del-tap-wr:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rlpfd:kr {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer? {:arglists ([buf])},
   overtone.sc.synth/pre-synth {:arglists ([& args]), :macro true},
   overtone.sc.ugens/wrap-index:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/slew:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-phase-shift
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clipper4:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-c
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman2-dl {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rhpf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/coin-gate:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/white-noise:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-channels {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amplitude:ar {:arglists ([& _]), :special? true},
   overtone.sc.synth/synth-player
   {:arglists ([sdef params this & args])},
   overtone.osc/osc-rm-all-handlers {:arglists ([peer] [peer path])},
   overtone.sc.ugens/clip {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-reverb {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-clip-noise {:arglists ([& _]), :special? true},
   overtone.repl.shell/grep {:arglists ([stdin match])},
   overtone.sc.ugens/dstutter:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/limiter {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ball {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-flute:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decay2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gravity-grid {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-windex:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak-follower {:arglists ([& _]), :special? true},
   overtone.music.pitch/note {:arglists ([n])},
   overtone.sc.ugens/internal:dgeom
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/g-verb:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-freeze:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag2:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/node-free {:arglists ([this])},
   overtone.sc.ugens/pulse-divider {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-bin-wipe:kr
   {:arglists ([& _]), :special? true},
   overtone.libs.event/sync-event {:arglists ([event-type & args])},
   overtone.sc.ugens/gbman2-dn:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dc
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amplitude-mod {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dc:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decay:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-delay {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lfd-noise0:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-rand-wipe:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-trig:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.server/snd {:arglists ([path & args])},
   overtone.sc.ugens/i-rand:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise1:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.bus/audio-bus
   {:arglists ([] [n-channels-or-name] [n-channels name])},
   overtone.sc.ugens/pitch-shift {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-trig:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-fill!
   {:arglists ([buf val] [buf start len val])},
   overtone.sc.buffer/num-frames {:arglists ([buf])},
   overtone.sc.ugens/demand:ar {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-id {:arglists ([b])},
   overtone.sc.ugens/set-buf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/brz2 {:arglists ([& _]), :special? true},
   overtone.sc.vbap/v= {},
   overtone.sc.ugens/pv-common-mag:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:poll {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:scope-out2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.sample/load-samples-async {:arglists ([& glob-paths])},
   overtone.music.time/apply-at {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.clock/server-clock-b {},
   overtone.sc.ugens/sin-osc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t2a {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-capture-next-controller-key
   {:arglists ([])},
   overtone.sc.ugens/lin-pan2:kr {:arglists ([& _]), :special? true},
   overtone.sc.server/connect-external-server {},
   overtone.sc.ugens/set-reset-ff {:arglists ([& _]), :special? true},
   overtone.sc.ugens/wrap2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dc:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.info/audio-bus-count* {},
   overtone.sc.ugens/buf-delay-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise0 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pause:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/group-post-tree {:arglists ([group with-args?])},
   overtone.sc.ugens/decay2:ar {:arglists ([& _]), :special? true},
   overtone.config.store/OVERTONE-ASSETS-FILE {},
   overtone.studio.mixer/add-instrument {:arglists ([inst])},
   overtone.sc.ugens/standard-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:duty:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-grains2 {:arglists ([& _]), :special? true},
   overtone.sc.node/node-map-controls*
   {:arglists ([node names-buses])},
   overtone.sc.ugens/soft-clip-amp4:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pulse-count {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-GROUP {},
   overtone.sc.ugens/formant:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mfcc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/glitch-rhpf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free {:arglists ([& _]), :special? true},
   overtone.sc.ugens/logistic {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-cong-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise0:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/crossover-distortion
   {:arglists ([& _]), :special? true},
   overtone.sc.trig/on-latest-trigger
   {:arglists ([trig-id handler key] [node trig-id handler key])},
   overtone.sc.ugens/glitch-hpf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan2 {:arglists ([& _]), :special? true},
   overtone.studio.inst/inst-mixer {:arglists ([n-chans & args])},
   overtone.sc.ugens/dswitch1:dr {:arglists ([& _]), :special? true},
   overtone.sc.node/on-node-paused {:arglists ([node f])},
   overtone.sc.ugens/smooth-decimator
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/done {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy5 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-AND-GROUP-AFTER-DEEP {},
   overtone.sc.clock/server-clock-start-time {},
   overtone.music.pitch/mk-midi-string
   {:arglists ([pitch-key octave])},
   overtone.sc.ugens/buf-samples:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-mandolin:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/send-trig:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/osc:kr {:arglists ([& _]), :special? true},
   overtone.sc.envelope/env-lin {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-copy-phase {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ring4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-x-fade2:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.synth/synth? {:arglists ([s])},
   overtone.sc.ugens/stereo-convolution2-l
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-PAUSE-AFTER {},
   overtone.sc.node/node-status {:arglists ([this])},
   overtone.sc.node/node-created-event-key {:arglists ([node])},
   overtone.music.pitch/match-note
   {:arglists ([s] [s prev-str post-str])},
   overtone.sc.ugens/lfd-noise3:ar {:arglists ([& _]), :special? true},
   overtone.osc/osc-handle {:arglists ([peer path handler])},
   overtone.sc.ugens/FREE-AND-BEFORE {},
   overtone.libs.event/registered-handlers {:arglists ([])},
   overtone.sc.defcgen/parse-cgen-params {:arglists ([params])},
   overtone.sc.ugens/octcps {:arglists ([& _]), :special? true},
   overtone.sc.bus/map->AudioBus {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/t-gauss-rand {:arglists ([& _]), :special? true},
   overtone.sc.envelope/sine-shape {:arglists ([pos y1 y2])},
   overtone.sc.ugens/pv-phase-shift90
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-n:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise2:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/membrane-circle:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/dwhite:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.envelope/triangle {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:t-duty
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard2-dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/oscy:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/disk-in {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-hi-pass {:arglists ([& _]), :special? true},
   overtone.sc.synth/reset-synth-defaults {:arglists ([synth])},
   overtone.osc/osc-recv {:arglists ([peer path handler & [timeout]])},
   overtone.sc.cgens.demand/dwhite {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-n:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-add:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/allpass-n:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/duty:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-cutoff:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/a2k {:arglists ([& _]), :special? true},
   overtone.sc.ugens/f-sin-osc:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rand-id:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-diffuser:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rhpf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-fold4:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-nested-allpass-c:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sos:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sine-shaper:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-cutoff {:arglists ([& _]), :special? true},
   overtone.sc.envelope/cutoff {:arglists ([& _]), :special? true},
   overtone.sc.ugens/white-noise:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-nested-allpass-n:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-c:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-frames:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn-trig:ar {:arglists ([& _]), :special? true},
   overtone.sc.bus/control-bus-get-channel
   {:arglists ([bus] [bus offset])},
   overtone.sc.cgens.line/range-lin
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-ball:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/slope:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mul-add:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dl
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/iir-filter:ar {:arglists ([& _]), :special? true},
   overtone.sc.bus/control-bus-get {:arglists ([bus] [bus offset])},
   overtone.sc.buffer/buffer-alloc-read
   {:arglists ([path] [path start] [path start n-frames])},
   overtone.sc.envelope/env-adsr-ng
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dc:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/midi->hz {:arglists ([note])},
   overtone.sc.sample/free-sample {:arglists ([smpl])},
   overtone.music.pitch/canonical-pitch-class-name {:arglists ([pc])},
   overtone.sc.ugens/moog-ff {:arglists ([& _]), :special? true},
   overtone.music.pitch/find-note-name {:arglists ([note])},
   overtone.sc.ugens/lf-noise1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-comb-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mouse-x {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dgeom:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/scope-out2:ar
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-reply
   {:arglists ([peer msg-to-reply-to path & args])},
   overtone.sc.ugens/lfd-clip-noise:ar
   {:arglists ([& _]), :special? true},
   overtone.algo.chance/ranged-rand {:arglists ([min max])},
   overtone.studio.inst/->Inst
   {:arglists
    ([name
      params
      args
      sdef
      group
      instance-group
      fx-group
      mixer
      bus
      fx-chain
      volume
      pan
      n-chans])},
   overtone.studio.core/studio* {},
   overtone.sc.server/external-server-log {:arglists ([])},
   overtone.sc.ugens/rand-id:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/schmidt:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/impulse {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-pulse:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in-rect {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-c:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/brown-noise {:arglists ([& _]), :special? true},
   overtone.sc.bus/IBus {},
   overtone.sc.node/node-destroyed-event-key {:arglists ([node])},
   overtone.sc.ugens/lorenz2-dn:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan-b2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/select:kr {:arglists ([& _]), :special? true},
   overtone.sc.synth/update-tap-data {:arglists ([msg])},
   overtone.studio.fx/fx-compressor
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/running-sum:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-in-stream? {:arglists ([bc])},
   overtone.music.pitch/CHORD {},
   overtone.sc.ugens/pv-conformal-map:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amplitude-mod:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/spec-pcile:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pause-self-when-done
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-dur {:arglists ([& _]), :special? true},
   overtone.osc/osc-reply-msg {:arglists ([peer msg msg-to-reply-to])},
   overtone.music.rhythm/metro-bar-start
   {:arglists ([metro] [metro start-bar])},
   overtone.sc.ugens/dseq {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag3-ud:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/par-group {:arglists ([& args])},
   overtone.sc.ugens/send-reply {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-squared
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard2-dl:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/scaled-play-buf:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.node/node-paused? {:arglists ([n])},
   overtone.samples.freesound/freesound-pack-dir {:arglists ([id])},
   overtone.sc.buffer/buffer-info {:arglists ([buf-id])},
   overtone.sc.example/example {:arglists ([gen key & params])},
   overtone.sc.ugens/pan-b:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amp-comp-a:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dbamp {:arglists ([& _]), :special? true},
   overtone.sc.ugens/check-bad-values:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/record-buf:kr {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/add-current-namespace-to-synth-name?
   {:arglists ([])},
   overtone.sc.ugens/buf-wr:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard2-dn {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:scope-out2
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/wrap-index:ir {:arglists ([& _]), :special? true},
   overtone.sc.node/node-start* {:arglists ([node])},
   overtone.sc.ugens/bi-pan-b2:kr {:arglists ([& _]), :special? true},
   overtone.repl.debug/opp {:arglists ([& args]), :macro true},
   overtone.studio.event/handle-ctl {:arglists ([e])},
   overtone.sc.ugens/toggle-ff:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-beta-rand:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/timer:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/server-connected? {:arglists ([])},
   overtone.sc.bus/bus-monitor {:arglists ([bus] [bus chan-offset])},
   overtone.sc.ugens/resonz {:arglists ([& _]), :special? true},
   overtone.sc.clock/server-clock-time {:arglists ([])},
   overtone.sc.ugens/silent {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-ball {:arglists ([& _]), :special? true},
   overtone.studio.midi/dev-num-cache {},
   overtone.sc.ugens/lpf {:arglists ([& _]), :special? true},
   overtone.sc.bus/control-bus-fill!
   {:arglists
    ([bus fill-val] [bus fill-val len] [bus fill-val len offset])},
   overtone.sc.ugens/fold2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/membrane-hexagon:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sin-osc-fb {:arglists ([& _]), :special? true},
   overtone.sc.synth/normalize-synth-args {:arglists ([args])},
   overtone.sc.ugens/moog-ladder:kr
   {:arglists ([& _]), :special? true},
   overtone.config.store/__ENSURE-STORAGE-FILES__ {},
   overtone.sc.ugens/standard-trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/var-saw:kr {:arglists ([& _]), :special? true},
   overtone.sc.synth/with-ugen-debugging
   {:arglists ([& body]), :macro true},
   overtone.sc.ugens/ti-rand:kr {:arglists ([& _]), :special? true},
   overtone.sc.envelope/asr {:arglists ([& _]), :special? true},
   overtone.sc.node/node-status* {:arglists ([node])},
   overtone.sc.ugens/stk-bee-three:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/svf:kr {:arglists ([& _]), :special? true},
   overtone.sc.bindings/*ugens* {},
   overtone.sc.ugens/dwg-bowed:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/allpass-c:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/resonz:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sign {:arglists ([& _]), :special? true},
   overtone.studio.wavetable/wavetable
   {:arglists ([num-waves] [num-waves size])},
   overtone.libs.event/event-debug-on {:arglists ([])},
   overtone.sc.ugens/pan4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-fold8 {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/dbufwr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/trapezoid {:arglists ([& _]), :special? true},
   overtone.sc.node/ISynthNodeStatus {},
   overtone.sc.ugens/lpf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mouse-x:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/i-env-gen {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz-trig {:arglists ([& _]), :special? true},
   overtone.sc.sample/stereo-stream-player
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/defratio
   {:arglists ([rname ratio]), :macro true},
   overtone.sc.ugens/pv-hainsworth-foote
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sqrsum {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/local-buf
   {:arglists ([& _]), :special? true},
   overtone.studio.inst/definst
   {:arglists ([name doc-string? params ugen-form]), :macro true},
   overtone.sc.ugens/resonz:ar {:arglists ([& _]), :special? true},
   overtone.sc.envelope/adsr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/frame-compare {:arglists ([& _]), :special? true},
   overtone.sc.clock/__SERVER-CLOCK-SYNTH__ {},
   overtone.sc.ugens/clip:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/blip:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/offset-out:ar {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-feedback-distortion
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-rect-comb:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/env-gen:kr {:arglists ([& _]), :special? true},
   overtone.algo.chance/weighted-coin {:arglists ([n])},
   overtone.sc.ugens/internal:diwhite
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dswitch {:arglists ([& _]), :special? true},
   overtone.sc.ugens/c-osc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/scope-out:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-phase-shift:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t2k:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clear-buf:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stereo-convolution2-l:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.synth/defsynth
   {:arglists ([s-name & s-form]), :macro true},
   overtone.sc.ugens/i-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dn
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/log10 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bpz2:ar {:arglists ([& _]), :special? true},
   overtone.studio.event/ppause {:arglists ([k])},
   overtone.sc.ugens/buf-delay-l:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.node/ctl {:arglists ([node & args])},
   overtone.sc.ugens/PAUSE {},
   overtone.sc.ugens/lag-in {:arglists ([& _]), :special? true},
   overtone.sc.ugens/allpass-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/key-track:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak-eq4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-trunc4 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decay:ar {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-agent-for-control
   {:arglists ([control-key])},
   overtone.studio.midi/midi-find-connected-receivers
   {:arglists ([search])},
   overtone.sc.node/node-pause* {:arglists ([node])},
   overtone.sc.node/node-start {:arglists ([this])},
   overtone.sc.ugens/buf-rd {:arglists ([& _]), :special? true},
   overtone.samples.freesound/freesound {:arglists ([id & args])},
   overtone.sc.ugens/breakcore:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free-verb {:arglists ([& _]), :special? true},
   overtone.sc.ugens/beat-track:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-x-fade:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pulse-count:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.dyn/dyn-klang:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz2-dl:ar {:arglists ([& _]), :special? true},
   overtone.osc/osc-send-bundle {:arglists ([client bundle])},
   overtone.algo.scaling/round-to {:arglists ([n div])},
   overtone.sc.ugens/drand:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-gauss {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bl-buf-rd:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/node-paused-event-key {:arglists ([node])},
   overtone.sc.ugens/detect-index {:arglists ([& _]), :special? true},
   overtone.sc.ugens/not-pos? {:arglists ([& _]), :special? true},
   overtone.sc.ugens/v-osc3:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stepper:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/shaper:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-blow-hole:kr
   {:arglists ([& _]), :special? true},
   overtone.samples.freesound/*access-token* {},
   overtone.sc.ugens/v-osc:ar {:arglists ([& _]), :special? true},
   overtone.sc.example/get-example {:arglists ([gen key])},
   overtone.sc.ugens/acos {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/drand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bpf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t2a:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/quad-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rect-window {:arglists ([& _]), :special? true},
   overtone.studio.event/derivations {},
   overtone.sc.cgens.dyn/dyn-klang:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.pattern/pchoose {:arglists ([coll])},
   overtone.studio.event/schedule-next {:arglists ([k])},
   overtone.repl.graphviz/graphviz {:arglists ([s])},
   overtone.sc.ugens/b-all-pass:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ifft:kr {:arglists ([& _]), :special? true},
   overtone.sc.vbap/v+ {},
   overtone.sc.cgens.buf-io/local-buf:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.server/clear-msg-queue {:arglists ([])},
   overtone.sc.ugens/nl-filt-c:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-gate:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/formlet {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in-range:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-bin-wipe {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free-verb2:ar {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-distortion
   {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-overtone-group
   {:arglists ([])},
   overtone.repl.shell/ls {:arglists ([path])},
   overtone.sc.ugens/lpz2:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/node-started-event-key {:arglists ([node])},
   overtone.sc.ugens/drive-noise:ar
   {:arglists ([& _]), :special? true},
   overtone.music.time/now {:arglists ([])},
   overtone.sc.ugens/amplitude {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dn:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/timer {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-frames {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak-eq2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nested-allpass-n
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/tan {:arglists ([& _]), :special? true},
   overtone.sc.vbap/round2 {:arglists ([d] [d precision])},
   overtone.config.store/OVERTONE-CONFIG-FILE {},
   overtone.sc.ugens/pv-rect-comb2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy1:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-scale {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise1:ar
   {:arglists ([& _]), :special? true},
   overtone.config.store/live-config {},
   overtone.sc.ugens/lorenz-trig:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/index-in-between
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nl-filt-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/distort {:arglists ([& _]), :special? true},
   overtone.osc/osc-listeners {:arglists ([peer])},
   overtone.sc.ugens/pan4:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/schmidt {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gravity-grid2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amp-comp-a {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-gate {:arglists ([& _]), :special? true},
   overtone.sc.ugens/integrator:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/index {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dl:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/n-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-voic-form:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.sample/defsample
   {:arglists ([s-name path & args]), :macro true},
   overtone.sc.ugens/index-in-between:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-bowed:ar {:arglists ([& _]), :special? true},
   overtone.repl.debug/pp-sdef {:arglists ([s])},
   overtone.sc.cgens.mix/sum:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dn {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-conformal-map
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/part-conv:ar {:arglists ([& _]), :special? true},
   overtone.sc.synth/active-synths {:arglists ([& [synth-filter]])},
   overtone.sc.ugens/henon-trig:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/phasor:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/leak-dc:kr {:arglists ([& _]), :special? true},
   overtone.sc.defcgen/cgen
   {:arglists ([c-name summary doc params body rate categories]),
    :macro true},
   overtone.sc.ugens/free-self-when-done:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.oscillators/square:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-grains:ar {:arglists ([& _]), :special? true},
   overtone.samples.freesound/freesound-sample
   {:arglists ([id & args])},
   overtone.libs.asset/__ENSURE-LIVE-ASSET-STORE__ {},
   overtone.sc.ugens/gbman2-dn:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decode-b2:ar {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-write!
   {:arglists
    ([buf data]
     [buf start-idx data]
     [buf start-idx length data]
     [buf start-idx offset length data])},
   overtone.sc.buffer/map->BufferOutStream
   {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/pv-mag-mul {:arglists ([& _]), :special? true},
   overtone.osc/osc-client
   {:arglists ([host port] [host port send-nested-osc-bundles?])},
   overtone.sc.ugens/buf-allpass-l:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-shakers:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latch {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pulse-divider:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.node/active-synth-nodes* {},
   overtone.sc.ugens/standard2-dc:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/LIN {},
   overtone.libs.event/event-debug-off {:arglists ([])},
   overtone.sc.ugens/shared-out:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free:kr {:arglists ([& _]), :special? true},
   overtone.libs.event/event-monitor {:arglists ([] [event-key])},
   overtone.sc.ugens/zero-crossing {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-mandolin:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/w-amp:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz2-dc:ar {:arglists ([& _]), :special? true},
   overtone.studio.event/defaults {},
   overtone.sc.bindings/*constants* {},
   overtone.samples.freesound/freesound-search
   {:arglists ([ks* q* & params])},
   overtone.sc.ugens/pink-noise {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/del-tap-rd:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/running-max {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/with-inactive-modification-error
   {:arglists ([error-type & body]), :macro true},
   overtone.sc.ugens/lin-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fb-sine-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-tri:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/p-sin-grain:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/atan {:arglists ([& _]), :special? true},
   overtone.samples.freesound/*client-id* {},
   overtone.sc.server/connect-server
   {:arglists ([] [port] [host port])},
   overtone.sc.ugens/pause-self-when-done:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-max {:arglists ([& _]), :special? true},
   overtone.sc.vbap/v-len {},
   overtone.sc.ugens/membrane-hexagon
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard-trig:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dswitch:dr {:arglists ([& _]), :special? true},
   overtone.sc.bus/audio-bus-monitor
   {:arglists ([audio-bus] [audio-bus chan-offset])},
   overtone.sc.ugens/formlet:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/boot-external-server
   {:arglists ([] [port] [port opts])},
   overtone.sc.ugens/fhn2-dc:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/skip-needle:ar
   {:arglists ([& _]), :special? true},
   overtone.music.rhythm/metro-bpb
   {:arglists ([metro] [metro new-bpb])},
   overtone.config.store/OVERTONE-USER-STORE-FILE {},
   overtone.sc.ugens/t-exp-rand:kr {:arglists ([& _]), :special? true},
   overtone.studio.mixer/master-recorder
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.env/hold:ar {:arglists ([& _]), :special? true},
   overtone.sc.envelope/ENV-SHAPES {},
   overtone.sc.synth/synth-form {:arglists ([s-name s-form])},
   overtone.sc.ugens/hpz1:kr {:arglists ([& _]), :special? true},
   overtone.repl.shell/->ShellStringList {:arglists ([strlist])},
   overtone.sc.ugens/lorenz2-dl:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag-ud:ar {:arglists ([& _]), :special? true},
   overtone.samples.freesound/authorization-instructions
   {:arglists ([])},
   overtone.studio.event/pplayers {},
   overtone.sc.ugens/cusp-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nested-allpass-c:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lfd-noise1:kr {:arglists ([& _]), :special? true},
   overtone.sc.envelope/linear-shape {:arglists ([pos y1 y2])},
   overtone.music.pitch/MIDI-NOTE-RE-STR {},
   overtone.sc.ugens/nl-filt-l {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-device-num {:arglists ([dev])},
   overtone.sc.ugens/one-zero:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/COMPLEX {},
   overtone.sc.ugens/lfd-noise0 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hilbert:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-voic-form {:arglists ([& _]), :special? true},
   overtone.algo.chance/weighted-choose
   {:arglists ([val-prob-map] [vals probabilities])},
   overtone.sc.ugens/pv-conj:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag2 {:arglists ([& _]), :special? true},
   overtone.sc.synth/map->Synth {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/x-fade2:kr {:arglists ([& _]), :special? true},
   overtone.studio.mixer/recording? {:arglists ([])},
   overtone.osc/osc-peer
   {:arglists ([] [listen? send-nested-osc-bundles?])},
   overtone.sc.ugens/dust2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/coin-gate {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-allpass-c {:arglists ([& _]), :special? true},
   overtone.music.pitch/nth-interval {:arglists ([n] [scale n])},
   overtone.sc.ugens/standard2-dl {:arglists ([& _]), :special? true},
   overtone.sc.ugens/slope:ar {:arglists ([& _]), :special? true},
   overtone.sc.cgens.oscillators/pm-osc
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.io/scaled-v-disk-in
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/wrap:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/convolution2-l
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-grains3 {:arglists ([& _]), :special? true},
   overtone.sc.cgens.line/range-lin:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-saxofony:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/cosh {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-blow-hole {:arglists ([& _]), :special? true},
   overtone.sc.info/server-sample-dur {:arglists ([])},
   overtone.sc.ugens/buf-rate-scale:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/map->BufferInfo {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/detect-silence:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:poll:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/map->SynthNode {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/spec-flatness {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dc:kr {:arglists ([& _]), :special? true},
   overtone.studio.inst/inst-pan! {:arglists ([inst pan])},
   overtone.sc.ugens/pv-mul:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/group-clear {:arglists ([group])},
   overtone.sc.ugens/stk-pluck:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dust {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-exp:kr {:arglists ([& _]), :special? true},
   overtone.libs.event/event-monitor-off {:arglists ([])},
   overtone.sc.node/node-active? {:arglists ([n])},
   overtone.music.time/stop-player {:arglists ([sched-fn])},
   overtone.studio.midi/midi-note
   {:arglists ([rcv note-num vel dur] [rcv note-num vel dur channel])},
   overtone.sc.clock/server-clock-n-ticks {:arglists ([])},
   overtone.sc.ugens/lag3:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dbufwr
   {:arglists ([& _]), :special? true},
   overtone.studio.event/octave-note {:arglists ([e octave note])},
   overtone.sc.node/to-id {:arglists ([obj])},
   overtone.sc.ugens/num-audio-buses
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-set!
   {:arglists ([buf val] [buf index val])},
   overtone.samples.freesound/freesound-info {:arglists ([id])},
   overtone.music.pitch/sharp {:arglists ([phrase notes])},
   overtone.music.time/player-pool {},
   overtone.sc.buffer/buffer-read {:arglists ([buf] [buf start len])},
   overtone.sc.ugens/max-local-bufs
   {:arglists ([& _]), :special? true},
   overtone.sc.sample/->PlayableSample
   {:arglists ([id size n-channels rate status path args name])},
   overtone.sc.ugens/lf-gauss:ar {:arglists ([& _]), :special? true},
   overtone.samples.freesound/freesound-search-paths
   {:arglists ([query* & params])},
   overtone.studio.mixer/mixer-booted? {:arglists ([])},
   overtone.music.rhythm/metro-start
   {:arglists ([metro] [metro start-beat])},
   overtone.sc.ugens/normalizer:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rlpf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/apf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fos {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-band-pass {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sample-dur:ir {:arglists ([& _]), :special? true},
   overtone.sc.node/node-live? {:arglists ([n])},
   overtone.studio.pattern/pbind {:arglists ([m repeat] [m])},
   overtone.sc.ugens/bpf:kr {:arglists ([& _]), :special? true},
   overtone.sc.vbap/vbap-speaker-array {:arglists ([speaker-defs])},
   overtone.sc.ugens/pv-mag-below {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-wrap8:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/degree->interval {:arglists ([degree scale])},
   overtone.samples.freesound/freesound-sample-pack {:arglists ([id])},
   overtone.music.rhythm/metro-bar-phase {:arglists ([metro])},
   overtone.sc.ugens/b-hi-shelf {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mouse-y {:arglists ([& _]), :special? true},
   overtone.sc.ugens/shared-in {:arglists ([& _]), :special? true},
   overtone.sc.node/ISynthGroup {},
   overtone.sc.node/group-free {:arglists ([group])},
   overtone.sc.node/on-node-destroyed {:arglists ([node f])},
   overtone.sc.ugens/wrap-index {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-line {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decimator:ar {:arglists ([& _]), :special? true},
   overtone.studio.mixer/remove-instrument {:arglists ([i-name])},
   overtone.sc.ugens/vibrato:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/last-value:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-clarinet:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/on-node-started {:arglists ([node f])},
   overtone.sc.ugens/stk-saxofony {:arglists ([& _]), :special? true},
   overtone.sc.sample/map->PlayableSample
   {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/grain-in {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ringz {:arglists ([& _]), :special? true},
   overtone.repl.ugens/pretty-ugen-doc-string
   {:arglists ([ug-spec] [ug-spec ns-str])},
   overtone.sc.ugens/gate:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/klang {:arglists ([& _]), :special? true},
   overtone.sc.ugens/glitch-hpf:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/server-opts {:arglists ([])},
   overtone.sc.ugens/round {:arglists ([& _]), :special? true},
   overtone.studio.inst/stereo-inst-mixer
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak-follower:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.envelope/env-dadsr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-l:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-exp:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fb-sine-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-wrap4 {:arglists ([& _]), :special? true},
   overtone.studio.mixer/MIXER-BOOT-DEPS {},
   overtone.sc.ugens/lorenz2-dn:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-jensen-andersen
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/c-osc:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/flat {:arglists ([phrase notes])},
   overtone.sc.server/stop {:arglists ([])},
   overtone.sc.ugens/num-buffers {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpf18 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/scale-neg {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-nested-allpass-n
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dwg-bowed-tor:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rand-seed:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/c-osc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clipper8:ar
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-close {:arglists ([peer & wait])},
   overtone.sc.ugens/dwg-bowed-tor {:arglists ([& _]), :special? true},
   overtone.algo.chance/only
   {:arglists ([phrase notes] [phrase notes result])},
   overtone.music.pitch/resolve-scale {:arglists ([scale])},
   overtone.sc.ugens/internal:dseries
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pluck {:arglists ([& _]), :special? true},
   overtone.sc.info/server-num-buffers {:arglists ([])},
   overtone.sc.ugens/disk-out {:arglists ([& _]), :special? true},
   overtone.config.store/live-store {},
   overtone.sc.ugens/lin-cong-l {:arglists ([& _]), :special? true},
   overtone.samples.freesound/map->FreesoundSample
   {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/gendy2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mfcc {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-input-group
   {:arglists ([])},
   overtone.sc.ugens/latoocarfian-l
   {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-root-group
   {:arglists ([])},
   overtone.sc.ugens/osc {:arglists ([& _]), :special? true},
   overtone.sc.cgens.beq-suite/b-hi-pass4:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpf:ar {:arglists ([& _]), :special? true},
   overtone.algo.lists/fill {:arglists ([size coll])},
   overtone.sc.buffer/write-wav {},
   overtone.sc.ugens/allpass-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rhpf {:arglists ([& _]), :special? true},
   overtone.music.pitch/note-info {:arglists ([midi-string])},
   overtone.sc.ugens/gendy1:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-clip-noise:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/node-block-until-ready {:arglists ([this])},
   overtone.sc.ugens/lag-ud {:arglists ([& _]), :special? true},
   overtone.sc.ugens/crossover-distortion:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-fade2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-modal-bar:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bi-pan-b2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman2-dc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/slope {:arglists ([& _]), :special? true},
   overtone.sc.server/sc-osc-debug-off {:arglists ([])},
   overtone.sc.ugens/pv-rand-comb {:arglists ([& _]), :special? true},
   overtone.config.store/store-set! {:arglists ([key val])},
   overtone.sc.ugens/mul-add:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/loudness:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/dgeom:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-safe-pre-default-group
   {:arglists ([])},
   overtone.sc.ugens/linen:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-comb-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pow {:arglists ([& _]), :special? true},
   overtone.sc.server/sc-debug-off {:arglists ([])},
   overtone.sc.envelope/env-sine {:arglists ([& _]), :special? true},
   overtone.sc.ugens/part-conv {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clip-noise:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-sample-rate:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/disk-out:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clip2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:t-duty:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sin {:arglists ([& _]), :special? true},
   overtone.osc/osc-send-msg {:arglists ([peer msg])},
   overtone.music.pitch/min-sixth {:arglists ([freq__7472__auto__])},
   overtone.sc.ugens/dpoll:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/k2a:ar {:arglists ([& _]), :special? true},
   overtone.studio.mixer/__BUS-MIXERS__ {},
   overtone.sc.ugens/lf-saw {:arglists ([& _]), :special? true},
   overtone.sc.info/input-bus-count* {},
   overtone.sc.ugens/quad-l {:arglists ([& _]), :special? true},
   overtone.osc/in-osc-bundle
   {:arglists ([client timestamp & body]), :macro true},
   overtone.sc.cgens.demand/t-duty:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/skip-needle {:arglists ([& _]), :special? true},
   overtone.studio.fx/MAX-DELAY {},
   overtone.sc.ugens/most-change:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/slew {:arglists ([& _]), :special? true},
   overtone.osc/osc-target {:arglists ([client host port])},
   overtone.sc.ugens/amplitude:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpf1:ar {:arglists ([& _]), :special? true},
   overtone.sc.synth/defsynth-load
   {:arglists ([def-name file-path]), :macro true},
   overtone.sc.ugens/pv-morph {:arglists ([& _]), :special? true},
   overtone.sc.ugens/detect-silence:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.fx/distortion2
   {:arglists ([& _]), :special? true},
   overtone.helpers.rand/rand-gaussian {:arglists ([])},
   overtone.sc.ugens/pan-b {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-copy-phase:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/par-group-switch {},
   overtone.sc.buffer/buffer-write-relay!
   {:arglists ([buf data] [buf start-idx data])},
   overtone.sc.ugens/internal:d-gauss:dr
   {:arglists ([& _]), :special? true},
   overtone.music.rhythm/metro-bpm
   {:arglists ([metro] [metro new-bpm])},
   overtone.sc.ugens/buf-rate-scale:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise0:ar {:arglists ([& _]), :special? true},
   overtone.repl.inst/ns-instruments {:arglists ([ns])},
   overtone.sc.dyn-vars/with-inactive-node-modification-error
   {:arglists ([error-type & body]), :macro true},
   overtone.studio.inst/inst-volume! {:arglists ([inst vol])},
   overtone.sc.synth/control-proxy-value-atom
   {:arglists ([full-name param])},
   overtone.sc.node/idify {:arglists ([col])},
   overtone.sc.ugens/running-min {:arglists ([& _]), :special? true},
   overtone.sc.cgens.oscillators/square
   {:arglists ([& _]), :special? true},
   overtone.libs.asset/unregister-assets!
   {:arglists ([key] [key & paths])},
   overtone.sc.ugens/integrator:ar {:arglists ([& _]), :special? true},
   overtone.sc.vbap/v-dist {},
   overtone.sc.ugens/osc-n:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fb-sine-l:ar {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-freeverb {:arglists ([& _]), :special? true},
   overtone.config.store/OVERTONE-DIRS {},
   overtone.sc.ugens/lag2-ud:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/crackle:kr {:arglists ([& _]), :special? true},
   overtone.music.pitch/third {:arglists ([freq__7472__auto__])},
   overtone.sc.ugens/lin-x-fade2:ar
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/nth-octave {:arglists ([freq n])},
   overtone.sc.ugens/nested-allpass-l
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz2-dc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fos:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/f-sin-osc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dust2:kr {:arglists ([& _]), :special? true},
   overtone.music.time/periodic
   {:arglists
    ([ms-period fun]
     [ms-period fun initial-delay]
     [ms-period fun initial-delay description])},
   overtone.sc.ugens/fft-trigger {:arglists ([& _]), :special? true},
   overtone.sc.ugens/asin {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dibrown:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.line/lin-lin {:arglists ([& _]), :special? true},
   overtone.libs.event/oneshot-event
   {:arglists ([event-type handler key])},
   overtone.sc.ugens/coyote {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-out:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-rand:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/two-pole:kr {:arglists ([& _]), :special? true},
   overtone.samples.freesound/with-authorization-header
   {:arglists ([b]), :macro true},
   overtone.sc.info/server-num-audio-buses {:arglists ([])},
   overtone.sc.ugens/gbman-trig:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/connect-jack-ports {},
   overtone.sc.ugens/needle-rect:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-sample-rate:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/degree-to-key:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag3:kr {:arglists ([& _]), :special? true},
   overtone.music.pitch/shift {:arglists ([phrase notes amount])},
   overtone.sc.ugens/in-trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amp-comp-a:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dser:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fb-sine-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-cub {:arglists ([& _]), :special? true},
   overtone.sc.ugens/membrane-circle
   {:arglists ([& _]), :special? true},
   overtone.studio.fx/fx-echo {:arglists ([& _]), :special? true},
   overtone.sc.ugens/grain-sin:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/check-bad-values
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-gauss:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/running-sum:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/spec-flatness:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-l:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-line:kr {:arglists ([& _]), :special? true},
   overtone.sc.info/server-sample-rate {:arglists ([])},
   overtone.sc.ugens/henon2-dl:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pitch-shift:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amclip {:arglists ([& _]), :special? true},
   overtone.sc.synth/*demo-time* {},
   overtone.music.rhythm/metro-tock {:arglists ([metro])},
   overtone.sc.ugens/INFINITE {},
   overtone.sc.ugens/lf-noise0:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:t-duty:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/wrap {:arglists ([& _]), :special? true},
   overtone.sc.ugens/loudness {:arglists ([& _]), :special? true},
   overtone.sc.ugens/one-pole {:arglists ([& _]), :special? true},
   overtone.music.rhythm/metro-beat {:arglists ([metro] [metro beat])},
   overtone.sc.ugens/leak-dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mantissa-mask:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.mixer/out-bus-mixer
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ring2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-bin-scramble
   {:arglists ([& _]), :special? true},
   overtone.sc.node/_root-group_ {},
   overtone.sc.foundation-groups/foundation-safe-post-default-group
   {:arglists ([])},
   overtone.sc.ugens/replace-out:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan-b2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay2 {:arglists ([& _]), :special? true},
   overtone.sc.cgens.line/lin-lin:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise1:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pause-self:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/group-prepend-node {:arglists ([group node])},
   overtone.sc.ugens/pv-bin-shift {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fb-sine-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/MKL {},
   overtone.sc.ugens/double-nested-allpass-c
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/excess {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-shift {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-well2 {:arglists ([& _]), :special? true},
   overtone.sc.node/node-map-n-controls*
   {:arglists ([node start-control start-bus n])},
   overtone.sc.ugens/pv-min {:arglists ([& _]), :special? true},
   overtone.sc.ugens/os-trunc8 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/limiter:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/node-loading? {:arglists ([n])},
   overtone.music.pitch/invert {:arglists ([notes & [pivot]])},
   overtone.sc.ugens/brown-noise:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-phase-shift270:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/env-gen:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-rand:ar {:arglists ([& _]), :special? true},
   overtone.algo.fn/cycle-fn {:arglists ([& fns])},
   overtone.sc.ugens/t-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/logistic:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy3:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpz1:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clipper8 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dshuf:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/local-in {:arglists ([& _]), :special? true},
   overtone.sc.envelope/lin-env {},
   overtone.sc.info/server-control-rate {:arglists ([])},
   overtone.sc.ugens/in-rect:ar {:arglists ([& _]), :special? true},
   overtone.sc.bus/control-bus-get-range
   {:arglists ([bus len] [bus len offset])},
   overtone.sc.ugens/line:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/han-window {:arglists ([& _]), :special? true},
   overtone.libs.event/oneshot-sync-event
   {:arglists ([event-type handler key])},
   overtone.sc.ugens/t-grains {:arglists ([& _]), :special? true},
   overtone.libs.asset/*cache-root* {},
   overtone.sc.ugens/cos {:arglists ([& _]), :special? true},
   overtone.sc.ugens/degree-to-key {:arglists ([& _]), :special? true},
   overtone.sc.server/at {:arglists ([time-ms & body]), :macro true},
   overtone.libs.asset/asset-seq {:arglists ([url])},
   overtone.sc.synth/with-no-ugen-checks
   {:arglists ([& body]), :macro true},
   overtone.sc.ugens/nl-filt-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gauss-trig {:arglists ([& _]), :special? true},
   overtone.sc.vbap/v- {},
   overtone.sc.bus/->ControlBus
   {:arglists ([id n-channels rate name])},
   overtone.sc.ugens/fold {:arglists ([& _]), :special? true},
   overtone.sc.ugens/brf {:arglists ([& _]), :special? true},
   overtone.sc.node/node-tree-zipper {:arglists ([] [root])},
   overtone.sc.ugens/SINE {},
   overtone.sc.ugens/lf-par:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise1
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-common-mag {:arglists ([& _]), :special? true},
   overtone.sc.ugens/set-buf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy3 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise2
   {:arglists ([& _]), :special? true},
   overtone.sc.defcgen/defcgen
   {:arglists ([c-name & c-form]), :macro true},
   overtone.studio.inst/instrument? {:arglists ([o])},
   overtone.sc.ugens/formant {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pitch {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-clarinet {:arglists ([& _]), :special? true},
   overtone.repl.debug/sdef {},
   overtone.sc.ugens/running-sum {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman-trig:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/WPHASE {},
   overtone.sc.server/boot-server {:arglists ([])},
   overtone.sc.node/group-deep-clear {:arglists ([group])},
   overtone.sc.vbap/v-dist-sqr {},
   overtone.sc.ugens/cpsmidi {:arglists ([& _]), :special? true},
   overtone.sc.info/server-control-dur {:arglists ([])},
   overtone.sc.ugens/dwg-bowed-simple
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/->BufferFile
   {:arglists ([id size n-channels rate status path])},
   overtone.music.pitch/cents {:arglists ([freq n-cents])},
   overtone.sc.ugens/pv-x-fade {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz-l {:arglists ([& _]), :special? true},
   overtone.osc/zero-conf? {:arglists ([])},
   overtone.sc.ugens/degree-to-key:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:duty:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.inst/DEFAULT-VOLUME {},
   overtone.sc.ugens/most-change {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-control-buses:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.sample/__DEFINE-PLAYERS__ {},
   overtone.sc.ugens/check-bad-values:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.info/server-num-input-buses {:arglists ([])},
   overtone.sc.node/node? {:arglists ([obj])},
   overtone.sc.ugens/saw:ar {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-sysex {:arglists ([rcv byte-seq])},
   overtone.sc.synth/synth-load {:arglists ([file-path])},
   overtone.sc.ugens/sumsqr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn2-dl {:arglists ([& _]), :special? true},
   overtone.sc.server/ensure-connected! {:arglists ([])},
   overtone.sc.info/snd-server-info
   {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-connected-devices {:arglists ([])},
   overtone.sc.buffer/buffer-save {:arglists ([buf path & args])},
   overtone.sc.buffer/buffer-get {:arglists ([buf] [buf index])},
   overtone.sc.ugens/send-reply:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/nested-allpass-c
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-rm-all-listeners {:arglists ([peer])},
   overtone.sc.ugens/a2k:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hasher:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/server-disconnected? {:arglists ([])},
   overtone.studio.mixer/__RECORDER__ {},
   overtone.sc.vbap/some-element? {},
   overtone.sc.ugens/markov-synth:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.synth/count-ugens {:arglists ([ug-tree ug-name])},
   overtone.sc.ugens/hpz2 {:arglists ([& _]), :special? true},
   overtone.algo.chance/srand {:arglists ([n])},
   overtone.sc.server/recv {:arglists ([path] [path matcher-fn])},
   overtone.studio.inst/load-instruments {:arglists ([])},
   overtone.sc.bus/control-bus-set!
   {:arglists ([bus val] [bus val offset])},
   overtone.sc.ugens/buf-allpass-c:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.mixer/reset-instruments {:arglists ([_event-info])},
   overtone.sc.ugens/crackle {:arglists ([& _]), :special? true},
   overtone.sc.ugens/impulse:kr {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-free {:arglists ([buf])},
   overtone.sc.ugens/delay1:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-bin-scramble:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lag:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/vosim:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dbrown
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/EXP {},
   overtone.sc.synth/synthdef
   {:arglists ([sname params ugens constants])},
   overtone.sc.ugens/pv-mag-above:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-low-pass {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amp-comp:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/markov-synth {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-jensen-andersen:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.clock/wall-clock-s {},
   overtone.studio.event/params-vec {:arglists ([e])},
   overtone.algo.trig/cosr {:arglists ([idx range centre period])},
   overtone.music.rhythm/->Metronome
   {:arglists ([start bar-start bpm bpb])},
   overtone.sc.ugens/absdif {:arglists ([& _]), :special? true},
   overtone.repl.debug/unified-sdef {:arglists ([s])},
   overtone.sc.ugens/ringz:kr {:arglists ([& _]), :special? true},
   overtone.sc.server/sc-debug-on {:arglists ([])},
   overtone.sc.ugens/log2 {:arglists ([& _]), :special? true},
   overtone.sc.bindings/*idle-async-timeout* {},
   overtone.sc.ugens/lf-clip-noise:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-banded-wg:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/node-get-control-range
   {:arglists ([node name-index n])},
   overtone.sc.bus/bus? {:arglists ([bus])},
   overtone.sc.synth/topological-sort-ugens {:arglists ([ugens])},
   overtone.studio.inst/clear-fx {:arglists ([inst])},
   overtone.sc.ugens/scope-out {:arglists ([& _]), :special? true},
   overtone.sc.node/node-control-range*
   {:arglists ([node ctl-start ctl-vals])},
   overtone.sc.cgens.demand/dgeom {:arglists ([& _]), :special? true},
   overtone.sc.ugens/svf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free-verb:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-pluck {:arglists ([& _]), :special? true},
   overtone.music.pitch/octave-note {:arglists ([octave interval])},
   overtone.sc.ugens/exp {:arglists ([& _]), :special? true},
   overtone.sc.node/node-map-controls {:arglists ([this names-buses])},
   overtone.sc.node/->SynthNode
   {:arglists ([synth id target position args sdef status loaded?])},
   overtone.sc.ugens/pv-copy {:arglists ([& _]), :special? true},
   overtone.sc.node/node-tree-seq {:arglists ([] [root])},
   overtone.studio.event/handle-note {:arglists ([e])},
   overtone.sc.cgens.line/varlag {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-above {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-audio-buses:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-FROM-THIS-ON {},
   overtone.sc.ugens/crackle:ar {:arglists ([& _]), :special? true},
   overtone.sc.cgens.berlach/soft-clip-amp
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/klank:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/line:ar {:arglists ([& _]), :special? true},
   overtone.algo.chance/rrand {:arglists ([min max])},
   overtone.sc.ugens/lf-cub:kr {:arglists ([& _]), :special? true},
   overtone.sc.sample/mono-stream-player
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/blip {:arglists ([& _]), :special? true},
   overtone.sc.envelope/dadsr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/vosim {:arglists ([& _]), :special? true},
   overtone.studio.wavetable/wavetable->signal {:arglists ([table])},
   overtone.sc.server/stop-all {:arglists ([])},
   overtone.sc.ugens/allpass-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/control-rate {:arglists ([& _]), :special? true},
   overtone.sc.ugens/phasor:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-modal-bar {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/scope-out2
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mid-eq {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dc:kr {:arglists ([& _]), :special? true},
   overtone.repl.debug/pp-unified-sdef {:arglists ([sdef])},
   overtone.sc.cgens.info/poll:kr {:arglists ([& _]), :special? true},
   overtone.sc.buffer/TWO-PI {},
   overtone.algo.scaling/closest-to {:arglists ([n low hi])},
   overtone.sc.ugens/quad-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in-rect:kr {:arglists ([& _]), :special? true},
   overtone.sc.bus/__BUS-MONITOR-SYNTH__ {},
   overtone.studio.fx/fx-feedback {:arglists ([& _]), :special? true},
   overtone.sc.ugens/v-disk-in:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sync-saw:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dbufrd:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-noise {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-bowed {:arglists ([& _]), :special? true},
   overtone.sc.buffer/->BufferInStream
   {:arglists ([id size n-channels rate status path open?])},
   overtone.sc.ugens/henon-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-rd:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rlpfd {:arglists ([& _]), :special? true},
   overtone.osc/osc-rm-handler {:arglists ([peer path])},
   overtone.sc.ugens/gendy4:kr {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-note-on
   {:arglists ([rcv note-num vel] [rcv note-num vel channel])},
   overtone.sc.ugens/oscy {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian2-dl:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/b-peak-eq:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/map->SynthGroup {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/double-nested-allpass-l:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-cue {:arglists ([path & args])},
   overtone.sc.ugens/fold:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-out {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/duty {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz2-dl {:arglists ([& _]), :special? true},
   overtone.sc.ugens/send-reply:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-c:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/compander {:arglists ([& _]), :special? true},
   overtone.sc.ugens/trig-avg:kr {:arglists ([& _]), :special? true},
   overtone.music.rhythm/metro-bar {:arglists ([metro] [metro bar])},
   overtone.sc.ugens/out:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.beq-suite/b-low-pass4
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/diwhite
   {:arglists ([& _]), :special? true},
   overtone.libs.event/event-monitor-timer {:arglists ([] [seconds])},
   overtone.sc.ugens/t-gauss-rand:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/dibrown:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-well2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:diwhite:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.envelope/defunk-env
   {:arglists ([fn-name docstring args & body]), :macro true},
   overtone.sc.ugens/hasher {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-AND-GROUP-BEFORE {},
   overtone.sc.ugens/os-trunc8:ar {:arglists ([& _]), :special? true},
   overtone.sc.buffer/ensure-buffer-active!
   {:arglists ([buf] [buf err-msg])},
   overtone.sc.ugens/vibrato:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak {:arglists ([& _]), :special? true},
   overtone.sc.ugens/HANN {},
   overtone.sc.ugens/stk-shakers {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clipper32:ar {:arglists ([& _]), :special? true},
   overtone.studio.pattern/ppad {:arglists ([pattern beats])},
   overtone.sc.ugens/out:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/kill {:arglists ([& nodes])},
   overtone.sc.sample/sample {:arglists ([path & args])},
   overtone.studio.fx/fx-bitcrusher
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/frac {:arglists ([& _]), :special? true},
   overtone.music.pitch/degree->int {:arglists ([degree])},
   overtone.sc.ugens/ampdb {:arglists ([& _]), :special? true},
   overtone.sc.trig/trig-id {:arglists ([])},
   overtone.sc.ugens/lag3-ud {:arglists ([& _]), :special? true},
   overtone.sc.cgens.freq/add-cents
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sync-saw {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latch:kr {:arglists ([& _]), :special? true},
   overtone.music.pitch/fourth {:arglists ([freq__7472__auto__])},
   overtone.samples.freesound/->FreesoundSample
   {:arglists
    ([id size n-channels rate status path args name freesound-id])},
   overtone.sc.ugens/amp-comp-a:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mul-add:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/convolution3:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/median {:arglists ([& _]), :special? true},
   overtone.sc.ugens/brf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sqrdif {:arglists ([& _]), :special? true},
   overtone.sc.ugens/streson:kr {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/block-node-until-ready? {:arglists ([])},
   overtone.music.pitch/DEGREE {},
   overtone.sc.server/snd-immediately
   {:arglists ([& body]), :macro true},
   overtone.sc.cgens.dyn/dyn-klang {:arglists ([& _]), :special? true},
   overtone.sc.info/control-dur* {},
   overtone.sc.ugens/pan-b2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/cusp-l {:arglists ([& _]), :special? true},
   overtone.studio.mixer/boot-server-and-mixer {:arglists ([])},
   overtone.studio.transport/*clock* {},
   overtone.sc.ugens/max-local-bufs:ir
   {:arglists ([& _]), :special? true},
   overtone.libs.event/event-monitor-keys {:arglists ([])},
   overtone.sc.node/group-node-tree {:arglists ([group])},
   overtone.sc.ugens/gbman2-dl:kr {:arglists ([& _]), :special? true},
   overtone.osc/osc-listen
   {:arglists ([peer listener] [peer listener key])},
   overtone.sc.ugens/pulse {:arglists ([& _]), :special? true},
   overtone.music.pitch/chord-degree
   {:arglists ([degree root mode] [degree root mode num-notes])},
   overtone.sc.ugens/lag-in:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/replace-out {:arglists ([& _]), :special? true},
   overtone.sc.cgens.dyn/dyn-klank:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/shared-out {:arglists ([& _]), :special? true},
   overtone.sc.ugens/v-osc {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-monitor-group
   {:arglists ([])},
   overtone.sc.envelope/squared-shape {:arglists ([pos y1 y2])},
   overtone.studio.inst/map->Inst {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/running-max:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/grain-buf:ar {:arglists ([& _]), :special? true},
   overtone.sc.cgens.berlach/soft-clip-amp:ar
   {:arglists ([& _]), :special? true},
   overtone.libs.asset/register-assets! {:arglists ([key & paths])},
   overtone.sc.ugens/beat-track2:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/two-pole:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gauss-trig:kr {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-connected-receivers {:arglists ([])},
   overtone.sc.bus/control-bus-set-range!
   {:arglists ([bus vals] [bus vals offset])},
   overtone.sc.clock/__internal-wall-clock__
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/tri-window {:arglists ([& _]), :special? true},
   overtone.sc.ugens/analyse-events2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/play-buf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t2k {:arglists ([& _]), :special? true},
   overtone.studio.event/eget {:arglists ([e k])},
   overtone.sc.ugens/in-feedback:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mouse-button {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-phase-shift270
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-pan2:ar {:arglists ([& _]), :special? true},
   overtone.sc.synth/->Synth
   {:arglists ([name ugens sdef args params instance-fn])},
   overtone.sc.ugens/pv-mag-minus {:arglists ([& _]), :special? true},
   overtone.repl.ugens/find-ugen-doc
   {:arglists ([& search-terms]), :macro true},
   overtone.sc.ugens/lag:ar {:arglists ([& _]), :special? true},
   overtone.sc.server/sc-osc-debug-on {:arglists ([])},
   overtone.sc.cgens.freq/add-cents:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/subsample-offset
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.beq-suite/b-hi-pass4
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-bee-three:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-default-group
   {:arglists ([])},
   overtone.sc.ugens/comb-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-out:ar {:arglists ([& _]), :special? true},
   overtone.sc.synth/gather-ugens-and-constants {:arglists ([root])},
   overtone.sc.ugens/v-osc3:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.line/range-lin:kr
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-msg {:arglists ([path & args])},
   overtone.sc.envelope/env-perc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/quad-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mid-eq:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.io/sound-in:ar
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/octave {:arglists ([freq__7472__auto__])},
   overtone.sc.envelope/adsr-ng {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-trig
   {:arglists ([& _]), :special? true},
   overtone.studio.mixer/recording-start {:arglists ([path & args])},
   overtone.sc.ugens/key-track {:arglists ([& _]), :special? true},
   overtone.studio.event/premove {:arglists ([k])},
   overtone.sc.ugens/convolution3 {:arglists ([& _]), :special? true},
   overtone.sc.bus/control-bus-monitor
   {:arglists ([control-bus] [control-bus chan-offset])},
   overtone.sc.ugens/pitch:kr {:arglists ([& _]), :special? true},
   overtone.sc.info/sample-rate* {},
   overtone.sc.ugens/lpz2 {:arglists ([& _]), :special? true},
   overtone.algo.trig/sinr {:arglists ([idx range centre period])},
   overtone.sc.ugens/FREE-CHILDREN {},
   overtone.sc.ugens/stk-moog:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-well3:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-div {:arglists ([& _]), :special? true},
   overtone.sc.ugens/onsets:kr {:arglists ([& _]), :special? true},
   overtone.speech/speech-buffer {:arglists ([text & {:as options}])},
   overtone.sc.ugens/b-peak-eq {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-running-synths:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/apf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decay {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mouse-button:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.wavetable/signal->wavetable {:arglists ([signal])},
   overtone.studio.mixer/in-bus-mixer
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/zero-crossing:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gendy2:ar {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-find-connected-device
   {:arglists ([search])},
   overtone.sc.ugens/fhn2-dn:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hypot-aprox {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dseq:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-cong-c:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/MIDI-NOTE-RE {},
   overtone.sc.ugens/internal:local-buf:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.freq/freq-spread:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/pp-node-tree {:arglists ([] [root])},
   overtone.sc.sample/loaded-samples* {},
   overtone.sc.ugens/detect-index:kr
   {:arglists ([& _]), :special? true},
   overtone.music.rhythm/metro-tick {:arglists ([metro])},
   overtone.sc.ugens/needle-rect {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-wr {:arglists ([& _]), :special? true},
   overtone.music.rhythm/metro-beat-phase {:arglists ([metro])},
   overtone.sc.bus/mono-audio-bus-level
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-c:kr
   {:arglists ([& _]), :special? true},
   overtone.repl.graphviz/show-graphviz-synth {:arglists ([s])},
   overtone.sc.ugens/pulse-count:ar
   {:arglists ([& _]), :special? true},
   overtone.config.store/__ENSURE-DIRS___ {},
   overtone.sc.ugens/pv-mag-smear:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/control-dur:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.info/__SERVER-INFO__ {},
   overtone.sc.cgens.info/poll:ar {:arglists ([& _]), :special? true},
   overtone.sc.bus/audio-bus? {:arglists ([bus])},
   overtone.sc.sample/->Sample
   {:arglists ([id size n-channels rate status path args name])},
   overtone.sc.ugens/t-windex:kr {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/inactive-node-modification-error
   {:arglists ([])},
   overtone.studio.wavetable/linear-interpolate-wavetable
   {:arglists ([table idx-a idx-b])},
   overtone.sc.envelope/lin {:arglists ([& _]), :special? true},
   overtone.sc.ugens/one-pole:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/cusp-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/peak-eq2 {:arglists ([& _]), :special? true},
   overtone.libs.asset/assets* {},
   overtone.sc.cgens.demand/dibrown
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-c:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/var-saw {:arglists ([& _]), :special? true},
   overtone.sc.ugens/clipper8:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-AND-GROUP-AFTER {},
   overtone.helpers.rand/rand-signed {:arglists ([] [n])},
   overtone.sc.ugens/hpz2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bpz2 {:arglists ([& _]), :special? true},
   overtone.sc.buffer/->BufferInfo
   {:arglists
    ([id size n-channels rate n-samples rate-scale duration])},
   overtone.sc.ugens/gendy3:ar {:arglists ([& _]), :special? true},
   overtone.libs.event/event-monitor-on {:arglists ([])},
   overtone.sc.ugens/timer:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amp-comp:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-l:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/beat-track2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ramp {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn-trig:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hpz1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-saxofony:ar
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-now {:arglists ([])},
   overtone.sc.ugens/dwg-sound-board
   {:arglists ([& _]), :special? true},
   overtone.sc.server/kill-server {:arglists ([])},
   overtone.sc.ugens/brz2:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/resolve-degree
   {:arglists ([degree] [degree octave-shift semitone-shift])},
   overtone.sc.cgens.info/poll {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpf18:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-div {:arglists ([& _]), :special? true},
   overtone.sc.ugens/difsqr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/n-rand:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/glitch-rhpf:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.event/presume {:arglists ([k])},
   overtone.sc.node/on-node-created {:arglists ([node f])},
   overtone.sc.ugens/pv-mag-mul:kr {:arglists ([& _]), :special? true},
   overtone.libs.event/on-event {:arglists ([event-type handler key])},
   overtone.sc.ugens/pv-mag-clip {:arglists ([& _]), :special? true},
   overtone.config.store/store-get {:arglists ([key] [key not-found])},
   overtone.sc.ugens/pv-mag-clip:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-comb-c:ar {:arglists ([& _]), :special? true},
   overtone.repl.ugens/find-ugen
   {:arglists ([& search-terms]), :macro true},
   overtone.sc.ugens/local-in:kr {:arglists ([& _]), :special? true},
   overtone.music.time/show-schedule {:arglists ([])},
   overtone.sc.ugens/trapezoid:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/toggle-ff:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gray-noise {:arglists ([& _]), :special? true},
   overtone.sc.node/node-tree {:arglists ([] [root])},
   overtone.sc.cgens.oscillators/pm-osc:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-compander {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-moog:ar {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-mk-full-control-event-key
   {:arglists ([dev command control-id])},
   overtone.sc.node/to-sc-id {:arglists ([v])},
   overtone.sc.ugens/softclip {:arglists ([& _]), :special? true},
   overtone.sc.ugens/balance2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-rect-comb {:arglists ([& _]), :special? true},
   overtone.repl.examples/examples {:arglists ([] [gen] [gen key])},
   overtone.sc.ugens/sample-dur {:arglists ([& _]), :special? true},
   overtone.sc.ugens/streson {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-brown-rand {:arglists ([& _]), :special? true},
   overtone.sc.ugens/allpass-l {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer
   {:arglists
    ([size] [size num-channels-or-name] [size num-channels name])},
   overtone.sc.ugens/comb-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/tanh {:arglists ([& _]), :special? true},
   overtone.sc.ugens/linen {:arglists ([& _]), :special? true},
   overtone.algo.chance/choose {:arglists ([col])},
   overtone.sc.ugens/sweep {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-live? {:arglists ([b])},
   overtone.sc.ugens/loop-buf {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/with-inactive-buffer-modification-error
   {:arglists ([error-type & body]), :macro true},
   overtone.sc.ugens/EXPONENTIAL {},
   overtone.sc.ugens/fft {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-common-mul {:arglists ([& _]), :special? true},
   overtone.sc.defcgen/mk-cgen
   {:arglists
    ([c-name summary doc params body categories rate]
     [c-name summary doc params body categories rate rates])},
   overtone.sc.ugens/delay-n {:arglists ([& _]), :special? true},
   overtone.music.pitch/rand-chord
   {:arglists
    ([root chord-name num-pitches pitch-range]
     [root chord-name num-pitches pitch-range inversion])},
   overtone.sc.node/node-get-control {:arglists ([node control-name])},
   overtone.algo.scaling/scale-range
   {:arglists ([x in-min in-max out-min out-max])},
   overtone.sc.buffer/file-buffer? {:arglists ([buf])},
   overtone.sc.ugens/convolution:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.server/external-server? {:arglists ([])},
   overtone.sc.ugens/buf-comb-c {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in-trig:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dseries:dr
   {:arglists ([& _]), :special? true},
   overtone.repl.ugens/odoc {:arglists ([name]), :macro true},
   overtone.sc.ugens/internal:dwhite
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-stream {:arglists ([path & args])},
   overtone.sc.ugens/lf-par {:arglists ([& _]), :special? true},
   overtone.sc.ugens/coyote:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decimator {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-tri:ar {:arglists ([& _]), :special? true},
   overtone.sc.envelope/curve-shape
   {:arglists ([pos y1 y2 curvature])},
   overtone.osc/osc-handlers {:arglists ([peer] [peer path])},
   overtone.sc.ugens/xor {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fft-trigger:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rand-seed {:arglists ([& _]), :special? true},
   overtone.sc.cgens.dyn/dyn-klank {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:duty {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/scaled-play-buf:kr
   {:arglists ([& _]), :special? true},
   overtone.config.store/config-get
   {:arglists ([key] [key not-found])},
   overtone.sc.ugens/oscy:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/amplitude-mod:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ring1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/latoocarfian-l:ar
   {:arglists ([& _]), :special? true},
   overtone.studio.inst/DEFAULT-PAN {},
   overtone.sc.ugens/gbman-n:ar {:arglists ([& _]), :special? true},
   overtone.config.store/OVERTONE-LOG-FILE {},
   overtone.sc.cgens.freq/freq-spread
   {:arglists ([& _]), :special? true},
   overtone.samples.freesound/freesound-pack-info {:arglists ([id])},
   overtone.sc.ugens/b-band-stop:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lin-pan2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rotate2 {:arglists ([& _]), :special? true},
   overtone.version/OVERTONE-VERSION {},
   overtone.sc.ugens/sin-osc:ar {:arglists ([& _]), :special? true},
   overtone.studio.midi/poly-players* {},
   overtone.sc.ugens/mantissa-mask {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-running-synths
   {:arglists ([& _]), :special? true},
   overtone.sc.cgens.fx/distortion2:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/i-env-gen:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/del-tap-rd {:arglists ([& _]), :special? true},
   overtone.studio.event/rest? {:arglists ([o])},
   overtone.sc.ugens/last-value {:arglists ([& _]), :special? true},
   overtone.sc.ugens/comb-l {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dbufwr:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-brown-noise0:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-minus:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.midi/__watch_device_changes__ {},
   overtone.sc.ugens/pan4:ar {:arglists ([& _]), :special? true},
   overtone.sc.node/ISynthNode {},
   overtone.sc.buffer/sample-info {:arglists ([s])},
   overtone.sc.ugens/dxrand:dr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-copy:kr {:arglists ([& _]), :special? true},
   overtone.sc.defcgen/generate-full-cgen-doc
   {:arglists
    ([c-name summary doc categories rate params rates]
     [c-name
      summary
      doc
      categories
      rate
      params
      rates
      src-str
      contributor])},
   overtone.sc.ugens/moog-ff:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-control-buses
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dbrown2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/trig-avg {:arglists ([& _]), :special? true},
   overtone.sc.cgens.line/varlag:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-delay:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay2:kr {:arglists ([& _]), :special? true},
   overtone.sc.node/node-tree-matching-synth-ids
   {:arglists ([re-or-str] [re-or-str root])},
   overtone.sc.ugens/formlet:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sine-shaper {:arglists ([& _]), :special? true},
   overtone.music.pitch/MIDI-RANGE {},
   overtone.sc.buffer/buffer-cue-pos
   {:arglists ([buf-cue] [buf-cue pos])},
   overtone.sc.ugens/pan-az {:arglists ([& _]), :special? true},
   overtone.music.pitch/degrees->pitches
   {:arglists ([degrees scale root])},
   overtone.algo.lists/rotate {:arglists ([n coll])},
   overtone.sc.ugens/latch:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/line {:arglists ([& _]), :special? true},
   overtone.sc.ugens/v-disk-in {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-banded-wg:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dshuf {:arglists ([& _]), :special? true},
   overtone.osc/osc-server
   {:arglists
    ([port]
     [port zero-conf-name]
     [port zero-conf-name send-nested-osc-bundles?])},
   overtone.sc.ugens/coin-gate:ir {:arglists ([& _]), :special? true},
   overtone.sc.ugens/silent:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/grain-buf {:arglists ([& _]), :special? true},
   overtone.music.time/kill-player {:arglists ([sched-fn])},
   overtone.samples.freesound/freesound-path {:arglists ([id])},
   overtone.sc.ugens/cusp-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/least-change:kr
   {:arglists ([& _]), :special? true},
   overtone.studio.mixer/input-gain {:arglists ([] [gain])},
   overtone.sc.ugens/freq-shift:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-rand-comb:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpf1:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/var-saw:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/osc-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decode-b2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-bee-three {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sos:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/two-pole {:arglists ([& _]), :special? true},
   overtone.sc.ugens/convolution3:ar
   {:arglists ([& _]), :special? true},
   overtone.config.store/__MOVE-OLD-ROOT-DIR__ {},
   overtone.sc.ugens/gendy4:ar {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-info? {:arglists ([b-info])},
   overtone.sc.ugens/trig:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-channels:ir
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/streson:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/standard2-dc:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pause {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hpz2:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dxrand {:arglists ([& _]), :special? true},
   overtone.studio.inst/inst-fx! {},
   overtone.libs.event/on-sync-event
   {:arglists ([event-type handler key])},
   overtone.sc.ugens/delay2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/PHASE {},
   overtone.sc.cgens.mix/mix {:arglists ([& _]), :special? true},
   overtone.sc.ugens/decode-b2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/internal:dibrown
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-freeze {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-output-group
   {:arglists ([])},
   overtone.studio.midi/midi-capture-next-controller-control-key
   {:arglists ([])},
   overtone.sc.ugens/running-min:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.node/IControllableNode {},
   overtone.sc.ugens/allpass-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clip-amp4
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lorenz-trig:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/num-input-buses
   {:arglists ([& _]), :special? true},
   overtone.sc.buffer/map->BufferInStream
   {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/free-verb2 {:arglists ([& _]), :special? true},
   overtone.sc.envelope/exponential-shape {:arglists ([pos y1 y2])},
   overtone.sc.ugens/nested-allpass-l:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mouse-y:kr {:arglists ([& _]), :special? true},
   overtone.sc.buffer/buffer-size {},
   overtone.sc.ugens/pv-mag-smear {:arglists ([& _]), :special? true},
   overtone.sc.server/clear {:arglists ([])},
   overtone.sc.envelope/welch-shape {:arglists ([pos y1 y2])},
   overtone.studio.pattern/pdo {:arglists ([& body]), :macro true},
   overtone.sc.node/node
   {:arglists
    ([synth-name]
     [synth-name arg-map]
     [synth-name arg-map location]
     [synth-name arg-map location sdef])},
   overtone.sc.ugens/phasor {:arglists ([& _]), :special? true},
   overtone.studio.pattern/pseries
   {:arglists ([start step] [start step size])},
   overtone.sc.ugens/lag2:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan2:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/fifth {:arglists ([freq__7472__auto__])},
   overtone.sc.ugens/env-gen {:arglists ([& _]), :special? true},
   overtone.sc.ugens/warp1 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/moog-ff:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/free-self-when-done
   {:arglists ([& _]), :special? true},
   overtone.osc/osc-bundle {:arglists ([timestamp & items])},
   overtone.sc.ugens/gbman-trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sin-osc:kr {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-device-keys {:arglists ([])},
   overtone.sc.ugens/one-zero {:arglists ([& _]), :special? true},
   overtone.music.pitch/hz->midi {:arglists ([freq])},
   overtone.sc.ugens/lf-cub:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-gauss-rand:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dust:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.mix/splay:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-blow-hole:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-well {:arglists ([& _]), :special? true},
   overtone.music.pitch/min-third {:arglists ([freq__7472__auto__])},
   overtone.sc.ugens/standard2-dl:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sin-osc-fb:ar {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/scaled-play-buf
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/zero-crossing:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/spring {:arglists ([& _]), :special? true},
   overtone.sc.buffer/create-buffer-data
   {:arglists ([size f range-min range-max])},
   overtone.sc.ugens/del-tap-wr:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/ay:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/double-nested-allpass-l
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/send-trig:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/vbap:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rand-id {:arglists ([& _]), :special? true},
   overtone.sc.ugens/one-pole:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/convolution2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mul-add {:arglists ([& _]), :special? true},
   overtone.studio.midi/next-dev-num
   {:arglists ([counter-key device-map])},
   overtone.sc.ugens/t-exp-rand:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-AND-GROUP-BEFORE-DEEP {},
   overtone.sc.ugens/pv-rect-comb2:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/bi-pan-b2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dwg-bowed {:arglists ([& _]), :special? true},
   overtone.sc.ugens/rlpf:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman2-dn {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lf-noise1:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-brown-rand:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.dyn-vars/without-node-blocking
   {:arglists ([& body]), :macro true},
   overtone.sc.cgens.demand/duty:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dbufrd {:arglists ([& _]), :special? true},
   overtone.sc.node/to-sc-id* {},
   overtone.sc.node/node-control {:arglists ([this params])},
   overtone.sc.ugens/buf-allpass-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/glitch-hpf:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/fhn-trig {:arglists ([& _]), :special? true},
   overtone.sc.ugens/midiratio {:arglists ([& _]), :special? true},
   overtone.music.pitch/sixth {:arglists ([freq__7472__auto__])},
   overtone.sc.ugens/stk-clarinet:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay1:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gate {:arglists ([& _]), :special? true},
   overtone.sc.node/node-control-range
   {:arglists ([this ctl-start ctl-vals])},
   overtone.sc.ugens/in-range {:arglists ([& _]), :special? true},
   overtone.sc.ugens/FREE-UPTO-THIS {},
   overtone.sc.ugens/t-windex {:arglists ([& _]), :special? true},
   overtone.sc.ugens/disintegrator {:arglists ([& _]), :special? true},
   overtone.sc.ugens/in-range:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/x-fade2 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/mul-add:kr {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-safe-group
   {:arglists ([])},
   overtone.music.pitch/REVERSE-NOTES {},
   overtone.config.store/config-set! {:arglists ([key val])},
   overtone.sc.ugens/amp-comp {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mul {:arglists ([& _]), :special? true},
   overtone.sc.ugens/local-out {:arglists ([& _]), :special? true},
   overtone.sc.sample/map->Sample {:arglists ([m__7972__auto__])},
   overtone.sc.ugens/pv-brick-wall {:arglists ([& _]), :special? true},
   overtone.studio.midi/midi-note-off
   {:arglists ([rcv note-num] [rcv note-num channel])},
   overtone.sc.ugens/dpoll {:arglists ([& _]), :special? true},
   overtone.sc.cgens.buf-io/scope-out2:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/sample-rate:ir
   {:arglists ([& _]), :special? true},
   overtone.music.pitch/db->amp {:arglists ([db])},
   overtone.sc.ugens/lin-cong-n:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-local-max {:arglists ([& _]), :special? true},
   overtone.sc.cgens.dyn/dyn-klank:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/k2a {:arglists ([& _]), :special? true},
   overtone.studio.fx/BITS {},
   overtone.sc.ugens/shared-in:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/dstutter {:arglists ([& _]), :special? true},
   overtone.sc.buffer/->BufferOutStream
   {:arglists
    ([id size n-channels header samples rate status path open?])},
   overtone.sc.ugens/pv-conj {:arglists ([& _]), :special? true},
   overtone.sc.bus/___reserve-overtone-first-control-bus___ {},
   overtone.libs.event/on-latest-event
   {:arglists ([event-type handler key])},
   overtone.music.pitch/SCALE {},
   overtone.sc.ugens/pv-div:kr {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/dbufwr:dr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/stk-flute {:arglists ([& _]), :special? true},
   overtone.sc.ugens/soft-clip-amp8:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-delay-n:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/two-zero:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman-n {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pv-mag-below:kr
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/delay-n:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/p-sin-grain {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pluck:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/lpz1:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/most-change:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/buf-wr:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/gbman2-dc {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan-b:ar {:arglists ([& _]), :special? true},
   overtone.sc.bus/free-bus {:arglists ([bus])},
   overtone.sc.ugens/b-band-stop {:arglists ([& _]), :special? true},
   overtone.sc.cgens.demand/t-duty {:arglists ([& _]), :special? true},
   overtone.sc.ugens/hilbert {:arglists ([& _]), :special? true},
   overtone.sc.ugens/henon2-dl:kr {:arglists ([& _]), :special? true},
   overtone.sc.foundation-groups/foundation-timing-group
   {:arglists ([])},
   overtone.sc.ugens/clipper32 {:arglists ([& _]), :special? true},
   overtone.sc.ugens/f-sin-osc:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/moog-ladder {:arglists ([& _]), :special? true},
   overtone.sc.synth/DEFAULT-RATE {},
   overtone.sc.ugens/replace-out:ar
   {:arglists ([& _]), :special? true},
   overtone.sc.envelope/env-adsr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/select:ar {:arglists ([& _]), :special? true},
   overtone.helpers.rand/rand-in-range {:arglists ([min max])},
   overtone.libs.event/event {:arglists ([event-type & args])},
   overtone.studio.pattern/pwhite
   {:arglists ([min max] [min max repeats])},
   overtone.sc.envelope/cubed-shape {:arglists ([pos y1 y2])},
   overtone.sc.ugens/schmidt:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/t-delay:ar {:arglists ([& _]), :special? true},
   overtone.sc.ugens/least-change {:arglists ([& _]), :special? true},
   overtone.sc.ugens/loop-buf:ar {:arglists ([& _]), :special? true},
   overtone.music.pitch/NOTES {},
   overtone.studio.mixer/volume {:arglists ([] [vol])},
   overtone.sc.ugens/lfd-clip-noise
   {:arglists ([& _]), :special? true},
   overtone.sc.ugens/pan-az:kr {:arglists ([& _]), :special? true},
   overtone.sc.ugens/MAGSUM {}})

(defn
 intern-vars
 [vars]
 (let
  [new-node
   (api/list-node
    (list*
     (api/token-node 'do)
     (->>
      (keys vars)
      (group-by namespace)
      (mapv
       (fn
        [[namespace' var-syms]]
        (api/list-node
         (list*
          (api/token-node 'do)
          (api/list-node
           (list
            (api/token-node 'require)
            (api/list-node
             (list (api/token-node 'quote) (symbol namespace')))))
          (->>
           var-syms
           (mapv
            (fn
             [var-sym]
             (if
              (and
               (not (:macro (get vars var-sym)))
               (seq (:arglists (get vars var-sym))))
              (api/list-node
               (if
                (> (count (:arglists (get vars var-sym))) 1)
                (list*
                 (api/token-node 'defn)
                 (api/token-node (symbol (name var-sym)))
                 (->>
                  (:arglists (get vars var-sym))
                  (mapv
                   (fn
                    [arity]
                    (api/list-node
                     (list
                      (api/coerce
                       (if
                        (some #{'&} arity)
                        ['& '_]
                        (vec (repeat (count arity) '_))))))))))
                (list
                 (api/token-node 'defn)
                 (api/token-node (symbol (name var-sym)))
                 (->>
                  (:arglists (get vars var-sym))
                  (mapv
                   (fn
                    [arity]
                    (api/coerce
                     (if
                      (some #{'&} arity)
                      ['& '_]
                      (vec (repeat (count arity) '_))))))
                  first)
                 (api/list-node
                  (list
                   (api/token-node 'eval)
                   (api/token-node nil))))))
              (api/list-node
               (list
                (api/token-node 'def)
                (api/token-node (symbol (name var-sym))))))))))))))))]
  {:node new-node}))

(defn immigrate-overtone-api [_] (intern-vars vars))

(defn
 intern-ugens
 [_]
 (intern-vars
  (->>
   vars
   (filter (comp #{"overtone.sc.ugens"} namespace first))
   (into {}))))

(comment
 (->>
  (->
   (str (:node (immigrate-overtone-api nil)))
   (clojure.string/split #"require"))
  (remove empty?)
  (mapv
   (fn* [p1__11643#] (clojure.string/replace p1__11643# #"\(do" "")))
  (mapv clojure.string/trim)
  (remove empty?)
  (mapv
   (fn*
    [p1__11644#]
    (->>
     (clojure.string/split p1__11644# #"defn")
     (mapv clojure.string/trim)))))
 ())
