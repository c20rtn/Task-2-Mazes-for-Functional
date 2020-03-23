(ns maze-task-2.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [clojure.data.json :as json])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           [com.mongodb DB WriteConcern]))

;  a. These mazes must initially square.
;  b. Your program must be capable of building square mazes in at least three different sizes.
;  c. You may extend the generator to include triangular, circular or any other shape of two-dimensional maze that you wish.
;  d. Maps are represented using structured data and held in a simple text file.

(defn make-a-row [columns]
  (loop [count 0 row []]
    (if (= columns count)
      row
      (recur (inc count) (conj row {:north 0 :east 0 :south 0 :west 0})))))

(defn make-a-grid [rows columns]
  (loop [count 0 grid []]
    (if (= rows count)
      grid
      (recur (inc count) (conj grid (make-a-row columns))))))

(def grid (atom (make-a-grid 4 4)))

(defn make-a-cell
  ([row] (make-a-cell row 0))
  ([row col]
   ; store size of grid and number of cells
   (let [size (count @grid) cells (count (first @grid))]
     ; above top row return maze
     (cond
       (= row size) @grid
       ; top row && last cell do nothing
       (and (= row (- size 1)) (= col (- cells 1))) 0
       ; top row carve east
       (=  row (- size 1)) (do
                             (swap! grid assoc-in [row col :east] 1)
                             (swap! grid assoc-in [row (+ col 1) :west] 1))
       ; not top row && last cell carve north
       (= col (- cells 1)) (do
                             (swap! grid assoc-in [row col :north] 1)
                             (swap! grid assoc-in [(+ row 1) col :south] 1))
       ; not top row carve north or east
       :else
       (if (= 0 (rand-int 2))
         (do
           (swap! grid assoc-in [row col :east] 1)
           (swap! grid assoc-in [row (+ col 1) :west] 1))
         (do
           (swap! grid assoc-in [row col :north] 1)
           (swap! grid assoc-in [(+ row 1) col :south] 1)))))))


(defn carve-passages
  ([] (carve-passages 0))
  ([row]
   (if (= row (count @grid))
     @grid
     (do
       (dotimes [col (count (first @grid))]
         (make-a-cell row col))
       (carve-passages (inc row))))))
(def maze (carve-passages))



;(defn to-s [grid]
;  (print "+")
;  (for [x (range 0 (count (first grid))) ]
;    (print "---+"))
;  (print "\n")
;  )
;
;(to-s maze)



;(defn make-a-cell
;  ([grid row] (make-a-cell grid row 0)) ;If
;  ([grid row col]
;   ; store size of grid and number of cells
;   (let [size (count grid) cells (count (first grid))]
;     ; above top row return maze
;     (cond
;       (= row size) grid
;       ; top row && last cell do nothing
;       (and (= row (- size 1)) (= col (- cells 1))) 0
;       ; top row carve east
;       (=  row (- size 1)) (do
;                             (swap! grid assoc-in [row col :east] 1)
;                             (swap! grid assoc-in [row (+ col 1) :west] 1))
;       ; not top row && last cell carve north
;       (= col (- cells 1)) (do
;                             (swap! grid assoc-in [row col :north] 1)
;                             (swap! grid assoc-in [(+ row 1) col :south] 1))
;       ; not top row carve north or east
;       :else
;       (if (= 0 (rand-int 2))
;         (do
;           (swap! grid assoc-in [row col :east] 1)
;           (swap! grid assoc-in [row (+ col 1) :west] 1))
;         (do
;           (swap! grid assoc-in [row col :north] 1)
;           (swap! grid assoc-in [(+ row 1) col :south] 1)))))))
;
;(defn carve-passages
;  ([grid] carve-passages [grid 0 []])
;  ([grid row] (if (= row (count grid))
;    grid
;    (do
;      (carve-passages
;        (dotimes [col (count (first grid))]
;          (make-a-cell grid row col))
;        (inc row)
;        )))))
;
;(defn generate-maze [rows cols]
;  (carve-passages (make-a-grid rows cols)))
;(generate-maze 4 4)