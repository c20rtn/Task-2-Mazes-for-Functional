(ns maze-task-2.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [clojure.data.json :as json]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :refer [site]]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.params :refer [wrap-params]]
            [clojure.java.jdbc :as sql]
            [stencil.core :refer [render-string]]
            [ring.adapter.jetty :as jetty])
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


(defroutes handler
           (GET "/books/:id" [id] (get_book id))
           (POST "/generate-maze" req (prn req)));[title] (book_handler title)))

(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3000}))

(loop [x 0 y 0 grid maze]
  (when (< x 4)
    (loop [_x x _y y _grid grid]
      (when (< _y 4)
        (print [x _y])
        (recur _x (inc _y) grid)))
    (println)
    (recur (inc x) y grid)))

(defn alter-cell [x y grid]
  (assoc-in grid [x y] {:north 1, :east 1, :south 1, :west 1}))

(defn maze-row [x y maze]
  (loop [count 0 grid maze]
    (if (= y count)
      grid
      (recur (inc count) (alter-cell x count grid)))))

(defn maze-grid [rows columns maze]
  (loop [count 0 grid maze]  ; loop over rows of empty grid
    (println grid)
    (if (= rows count)
      grid  ;return grid
      (recur (inc count) (maze-row count columns grid))))) ;return value of maze-row is now grid

(defn generate-maze [rows cols]
  ;uses empty grid and row col numbers
  (maze-grid rows cols (make-a-grid rows cols)))

;generate maze function
(generate-maze 4 4)




;(defn to-s [grid]
;  (print "+")
;  (for [x (range 0 (count (first grid))) ]
;    (print "---+"))
;  (print "\n")
;  )
;
;(to-s maze)

