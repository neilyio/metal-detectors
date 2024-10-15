(ns compositions.blip
  (:use [overtone.live]))

;; SuperCollider original code:
;;
;; SynthDef.new(\blip, {
;;   arg out;
;;   var freq, trig, sig;
;;   freq = LFNoise0.kr(3).exprange(300,1200).round(300);
;;   sig = SinOsc.ar(freq) * 0.25;
;;   trig = Dust.kr(2);
;;   sig = sig * EnvGen.kr(Env.perc(0.01, 0.2), trig);
;;   sig = Pan2.ar(sig, LFNoise1.kr(10));
;;   Out.ar(out, sig);
;; }).add;
;;
;; SynthDef.new(\reverb, {
;;   arg in, out=0;
;;   var sig;
;;   sig = In.ar(in, 2);
;;   sig = FreeVerb.ar(sig, 0.5, 0.8, 0.2);
;;   Out.ar(out, sig);
;; }).add;
;;
;; x = Synth.new(\blip, [\out, ~reverbBus2]);
;; y = Synth.new(\reverb, [\in, ~reverbBus2], x, \addAfter);
;; x.free;
;; x = Synth.before(y, \blip, [\out, ~reverbBus2]);

;; Define a blip synth
(defsynth bleep []
  (let [freq (lin-lin (lf-noise1 3) -1 1 300 1200)
        trig (dust 2)
        env (env-gen:ar (perc 0.01 0.2) trig)
        sig  (* (sin-osc freq) 0.25)
        pan-sig (pan2 (* sig env) (lf-noise1 10))]
    (out 0 pan-sig)))

;; Define a reverb synth
(defsynth reverb [input-bus 0]
  (let [sig (in input-bus 2)
        reverb-sig (free-verb sig 0.5 0.8 0.2)]
    (out 0 reverb-sig)))

;; Setup the synth and reverb chain
(def blip-synth (bleep))
(def reverb-bus (audio-bus 2))
(def reverb-synth (reverb reverb-bus))

(audio-bus)

(kill blip-synth)
(kill reverb-synth)
