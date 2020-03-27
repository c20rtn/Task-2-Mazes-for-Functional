(ns client.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.json :as json]))

(defn get-maze [x]
                            (json/read-str
                              (slurp (str "http://localhost:3000/maze/" x))
                              :key-fn keyword))

(defn pp-maze [grid]  ;Prints the maze to console
  (println (apply str "+" (repeat (count (get-in grid [0])) "----+")))
  (loop [x 0]
    (when (< x (count grid))
      (println (apply str "|" (for [col (get-in grid [x])]
                                (if (= (col :east) 0) "    |" "     "))))
      (println (apply str "+" (for [col (get-in grid [x])]
                                (if (= (col :south) 0) "----+""    +"))))
      (recur (+ x 1)))))

;function to find neighbours
(defn find-neighbours [queue grid]
  (let [cell (get-in grid [((first queue) :x) ((first queue) :y)]) coords (first queue)]
    (remove nil?
            (conj queue
                  (if (= (cell :north) 1) ;if passage north
                    (if (not (contains? (get-in grid [(- (coords :x) 1) (coords :y)]) :count)) ;if not visited
                      {:x (- (coords :x) 1), :y (coords :y), :count (inc (coords :count))})) ;add coords and new incremented count to list
                  (if (= (cell :south) 1) ;if passage south
                    (if (not (contains? (get-in grid [(+ (coords :x) 1) (coords :y)]) :count)) ;if not visited
                      {:x (+ (coords :x) 1), :y (coords :y), :count (inc (coords :count))}))
                  (if (= (cell :east) 1) ;if passage east
                    (if (not (contains? (get-in grid [(coords :x) (+ (coords :y) 1)]) :count)) ;if not visited
                      {:x (coords :x), :y (+ (coords :y) 1), :count (inc (coords :count))}))
                  (if (= (cell :west) 1) ;if passage west
                    (if (not (contains? (get-in grid [(coords :x) (- (coords :y) 1)]) :count)) ;if not visited
                      {:x (coords :x) , :y (- (coords :y) 1), :count (inc (coords :count))}))))))

(defn dijkstra [maze]
  (loop [grid maze, queue [{:x 0 :y 0 :count 0}]]
    (if (empty? queue)
      grid ;if endpoint reached
      (recur (assoc-in grid [((first queue) :x) ((first queue) :y) :count] ((first queue) :count)) ;puts distance into the maze
             (vec (rest (find-neighbours queue grid)))))))

(defn pp-dijkstra-maze [grid]  ;Prints the maze to console
  (println (apply str "+" (repeat (count (get-in grid [0])) "----+")))
  (loop [x 0]
    (when (< x (count grid))
      (println (apply str "|" (for [col (get-in grid [x])]
                                (if (= (col :east) 0)
                                  (if (contains? col :count)
                                    (apply str " " (format "%02d" (col :count)) " |")
                                    "    |" )
                                  (if (contains? col :count)
                                    (apply str " " (format "%02d" (col :count)) "  ")
                                    "     " )))))
      (println (apply str "+" (for [col (get-in grid [x])]
                                (if (= (col :south) 0) "----+""    +"))))
      (recur (+ x 1)))))

(pp-dijkstra-maze (dijkstra (get-maze 10)))