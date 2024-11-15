(ns neilyio.events)

(def keymap
  {;; Playback
   \u0000 [:play-toggle]  ; Keeping as requested (ctrl-space)

   ;; Speaker selection (left hand home row)
   ; \a [:select-prev-speaker]
   ; \s [:select-next-speaker]

   ;; Source selection (near speaker selection)
   ; \d [:select-prev-source]
   ; \f [:select-next-source]

  ;; Speaker navigation
   \h [:select-west-speaker]
   \j [:select-south-speaker]
   \k [:select-north-speaker]
   \l [:select-east-speaker]

   ;; Loop manipulation (right hand home row)
   \a [:loop-beats-left-1]
   \s [:loop-beats-right-1]
   \d [:loop-beats-left-4]
   \f [:loop-beats-right-4]

   ;; Loop in/out points (right hand above home)
   \u [:select-prev-source]
   \i [:select-next-source]
   ; \u [:loop-in-left]
   ; \i [:loop-in-right]
   ; \o [:loop-out-left]
   ; \p [:loop-out-right]

   ;; Larger movements (with shift)
   \A [:loop-beats-left-16]
   \S [:loop-beats-right-16]
   \D [:loop-beats-left-01]
   \F [:loop-beats-right-01]

   ;; Loop sizing (numbers for explicit sizes)
   \4 [:loop-beats-4]
   \2 [:loop-beats-half]
   \8 [:loop-beats-double]

   ;; Utility functions
   \c [:create-loop-from-selected]
   \x [:delete-all-loops]})

(defmulti handle (fn [{:keys [module event]}]
                   (when (sequential? event)
                     [module (first event)])))

(defmethod handle :default [& _] ::default)

