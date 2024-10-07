(ns neilyio.events)

(def keymap
  {;; Playback
   \u0000 [:play-toggle]  ; Keeping as requested (ctrl-space)

   ;; Speaker selection (left hand home row)
   \a [:select-prev-speaker]
   \s [:select-next-speaker]

   ;; Source selection (near speaker selection)
   \d [:select-prev-source]
   \f [:select-next-source]

   ;; Loop manipulation (right hand home row)
   \j [:loop-beats-left-1]
   \k [:loop-beats-right-1]
   \h [:loop-beats-left-4]
   \l [:loop-beats-right-4]

   ;; Loop in/out points (right hand above home)
   \u [:loop-in-left]
   \i [:loop-in-right]
   \o [:loop-out-left]
   \p [:loop-out-right]

   ;; Larger movements (with shift)
   \H [:loop-beats-left-16]
   \L [:loop-beats-right-16]
   \J [:loop-beats-left-01]
   \K [:loop-beats-right-01]

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

