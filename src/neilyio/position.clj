(ns neilyio.position
  (:require [lanterna.terminal :as t]))

;; Setting up a Swing terminal for REPL use
(def term (t/get-terminal :swing {:resize-listener (fn [cols rows]
                                                     (println "Resized to" cols "x" rows))}))

;; Start the terminal
(t/start term)

;; Function to write text with cursor positioning and color
(defn write-text [terminal text x y & {:keys [fg bg]}]
  (t/move-cursor terminal x y)
  (when fg (t/set-fg-color terminal fg))
  (when bg (t/set-bg-color terminal bg))
  (t/put-string terminal text)
  (t/set-fg-color terminal :default)
  (t/set-bg-color terminal :default))

;; Demonstrate writing text at various positions and colors
(write-text term "Hello, Lanterna!" 10 5 :fg :yellow)
(write-text term "Resizable Terminal" 5 8 :fg :blue)
(write-text term "Press 'q' to quit" 5 10 :fg :red)

;; Get terminal size
(defn get-terminal-size []
  (t/get-size term))

(defn draw-map [terminal points]
  (let [[term-width term-height] (get-terminal-size)
        scale-x (fn [x] (int (* x (dec term-width))))
        scale-y (fn [y] (int (* y (dec term-height))))]
    (doseq [{:keys [x y]} points]
      (let [scaled-x (scale-x x)
            scaled-y (scale-y y)]
        (write-text terminal "•" scaled-x scaled-y :fg :green))))
  (t/flush terminal))

;; Sample points to demonstrate map rendering
(def points [{:x 0.1 :y 0.1} {:x 0.5 :y 0.5} {:x 0.9 :y 0.9}])

;; Wait for user input and handle 'q' to quit
(defn wait-for-key []
  (loop []
    (let [k (t/get-key-blocking term)]
      (when (not= k :q)
        (recur)))))

;; Main function that orchestrates the setup, rendering, and cleanup
(defn -main []
  (try
    (t/start term)
    (write-text term "Hello, Lanterna!" 10 5 :fg :yellow)
    (write-text term "Resizable Terminal" 5 8 :fg :blue)
    (write-text term "Press 'q' to quit" 5 10 :fg :red)
    (draw-map term points)
    (wait-for-key)
    (finally
      ;; Clean up by stopping the terminal
      (t/stop term))))

