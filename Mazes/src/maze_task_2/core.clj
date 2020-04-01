(ns maze-task-2.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [clojure.data.json :as json]
            [compojure.core :refer :all]
            [compojure.coercions :refer :all]
            [compojure.route :as route]
            [compojure.handler :refer [site]]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.params :refer [wrap-params]]
            [stencil.core :refer [render-string]]
            [ring.adapter.jetty :as jetty])
  (:use [hiccup.core])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           [com.mongodb DB WriteConcern]))

;Just a debugging function to print to console and see if algorithms return a true maze
(defn pp-maze [grid]  ;Prints the maze to console
  (println (apply str "+" (repeat (count (get-in grid [0])) "----+")))
  (loop [x 0]
    (when (< x (count grid))
      (println (apply str "|" (for [col (get-in grid [x])]
                                (if (= (col :east) 0) "    |" "     "))))
      (println (apply str "+" (for [col (get-in grid [x])]
                                (if (= (col :south) 0) "----+""    +"))))
      (recur (+ x 1)))))

;Both functions create a maze of zeros
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

;=============================== Binary Tree Maze Algorithm ===============================
;Alters the cell of the maze and carves out the maze
(defn binary-alter-cell [x y grid]
  (cond
    ; top row & last cell do nothing
    (and (= x 0) (= y (- (count(get-in grid [0])) 1))) grid
    ;if top row only carve east
    (= x 0) (assoc-in (assoc-in grid [x (+ y 1) :west] 1) [x y :east] 1)
    ;if eastern cell carve north
    (= y (- (count(get-in grid [0])) 1)) (assoc-in (assoc-in grid [(- x 1) y :south] 1) [x y :north] 1)
    ; not top row carve north or east
    :else
    (if (= 0 (rand-int 2))  ;randomly carve north or east
      (assoc-in (assoc-in grid [x (+ y 1) :west] 1) [x y :east] 1)
      (assoc-in (assoc-in grid [(- x 1) y :south] 1) [x y :north] 1))))

(defn binary-maze-row [x y maze]
  (loop [count 0 grid maze]; loop over cells
    (if (= y count)
      grid
      (recur (inc count) (binary-alter-cell x count grid)))))

(defn binary-maze-grid [rows columns maze]
  (loop [count 0 grid maze]  ; loop over rows of empty grid
    (if (= rows count)
      grid  ;return grid
      (recur (inc count) (binary-maze-row count columns grid))))) ;return value of maze-row is now grid

(defn binary-generate-maze [rows cols]
  ;uses empty grid and row col numbers
  (binary-maze-grid rows cols (make-a-grid rows cols)))
(defn binary-generate-maze-json [rows cols]
  (json/write-str (binary-generate-maze rows cols)))
;==========================================================================================

;================================ Aldous-Broder Algorithm =================================

(defn aldous-broder-cell [maze new-cell current]
  (if (some #(= 1 %) (vals (get-in maze [(new-cell :x) (new-cell :y)])));If passage has been carved to this cell already
    maze  ;returns maze if passage has been carved already
    (cond
      ;if new cell is left
      (and (= (new-cell :x)(current :x)) (= (-(new-cell :y)(current :y)) -1)) (assoc-in (assoc-in maze [(new-cell :x) (new-cell :y) :east] 1) [(current :x)(current :y) :west] 1)
      ;if new cell is right
      (and (= (new-cell :x)(current :x)) (= (-(new-cell :y)(current :y)) 1)) (assoc-in (assoc-in maze [(new-cell :x) (new-cell :y) :west] 1) [(current :x)(current :y) :east] 1)
      ;if new cell is above
      (and (= (-(new-cell :x)(current :x)) -1) (= (new-cell :y)(current :y))) (assoc-in (assoc-in maze [(new-cell :x) (new-cell :y) :south] 1) [(current :x)(current :y) :north] 1)
      ;if new cell is below
      (and (= (-(new-cell :x)(current :x)) 1) (= (new-cell :y)(current :y))) (assoc-in (assoc-in maze [(new-cell :x) (new-cell :y) :north] 1) [(current :x)(current :y) :south] 1))))

(defn aldous-broder-choose-cell [cell rows cols]
  ;Used cond to make sure not to carve out of the maze
  (cond
    ;if top left corner
    (and (= (cell :x) 0) (= (cell :y) 0)) (let [direction (rand-int 2)]
                                            (cond (= direction 0) {:x (cell :x) :y (+ (cell :y) 1)}
                                                  (= direction 1) {:x (+ (cell :x) 1) :y (cell :y)}))
    ;if top right corner
    (and (= (cell :x) 0) (= (cell :y) (- cols 1))) (let [direction (rand-int 2)]
                                                     (cond (= direction 0) {:x (cell :x) :y (- (cell :y) 1)}
                                                           (= direction 1) {:x (+ (cell :x) 1) :y (cell :y)}))
    ;if bottom left corner
    (and (= (cell :x) (- rows 1)) (= (cell :y) 0)) (let [direction (rand-int 2)]
                                                     (cond (= direction 0) {:x (cell :x) :y (+ (cell :y) 1)}
                                                           (= direction 1) {:x (- (cell :x) 1) :y (cell :y)}))
    ;if bottom right corner
    (and (= (cell :x) (- rows 1)) (= (cell :y) (- cols 1))) (let [direction (rand-int 2)]
                                                              (cond (= direction 0) {:x (cell :x) :y (- (cell :y) 1)}
                                                                    (= direction 1) {:x (- (cell :x) 1) :y (cell :y)}))
    ;if top row
    (= (cell :x) 0) (let [direction (rand-int 3)]
                     (cond (= direction 0) {:x (cell :x) :y (+ (cell :y) 1)}
                           (= direction 1) {:x (cell :x) :y (- (cell :y) 1)}
                           (= direction 2) {:x (+ (cell :x) 1) :y (cell :y)}))
    ;if right col
    (= (cell :y) (- cols 1))(let [direction (rand-int 3)]
                              (cond (= direction 0) {:x (cell :x) :y (- (cell :y) 1)}
                                    (= direction 1) {:x (- (cell :x) 1) :y (cell :y)}
                                    (= direction 2) {:x (+ (cell :x) 1) :y (cell :y)}))
    ;if bottom row
    (= (cell :x) (- rows 1))(let [direction (rand-int 3)]
                              (cond (= direction 0) {:x (cell :x) :y (+ (cell :y) 1)}
                                    (= direction 1) {:x (cell :x) :y (- (cell :y) 1)}
                                    (= direction 2) {:x (- (cell :x) 1) :y (cell :y)}))
    ;if left col
    (= (cell :y) 0)(let [direction (rand-int 3)]
                     (cond (= direction 0) {:x (cell :x) :y (+ (cell :y) 1)}
                           (= direction 1) {:x (- (cell :x) 1) :y (cell :y)}
                           (= direction 2) {:x (+ (cell :x) 1) :y (cell :y)}))
    :else
    (let [direction (rand-int 4)]
      (cond (= direction 0) {:x (cell :x) :y (+ (cell :y) 1)}
            (= direction 1) {:x (cell :x) :y (- (cell :y) 1)}
            (= direction 2) {:x (+ (cell :x) 1) :y (cell :y)}
            (= direction 3) {:x (- (cell :x) 1) :y (cell :y)}))))

(defn aldous-broder [rows cols grid]
  (let [limit (* rows cols)] ;sets the limit of
    (loop
      [maze grid count 1 current {:x (rand-int rows) :y (rand-int cols)}] ;start with random maze position
      (if (= count limit) ;once all
        maze
        (let [new-cell (aldous-broder-choose-cell current rows cols) ;sets the
              new-maze (aldous-broder-cell maze new-cell current)] ;sets the
           (recur new-maze
                  (if (= maze new-maze) count (inc count))  ;if been carved the increment the count
                  new-cell))))))

(defn aldous-broder-generate-maze [rows cols]
  ;uses empty grid and row col numbers
  (aldous-broder rows cols (make-a-grid rows cols)))
(defn aldous-broder-generate-maze-json [rows cols]
  (json/write-str (aldous-broder-generate-maze rows cols)))

;==========================================================================================

;Sends the json of a named maze
(defn get-maze-by-name [name]
  (let [conn (mg/connect)
        db   (mg/get-db conn "MazeDB")
        coll "Mazes"]
    ((mc/find-one-as-map db coll {:name name}) :maze)))

;Sends the json of a random maze
(defn get-random-maze []
  (let [conn (mg/connect)
        db   (mg/get-db conn "MazeDB")
        coll "Mazes"]
    (:maze (rand-nth (mc/find-maps db coll)))))

;Creates a new aldous broder maze and stores it
(defn create-ad-maze-by-name [name x y]
  (let [conn (mg/connect)
        db   (mg/get-db conn "MazeDB")
        coll "Mazes"]
    (mc/insert db coll {:name name :maze (aldous-broder-generate-maze-json x y)}))
  (str "POSTED " name))

;Creates a new binary maze and stores it
(defn create-binary-maze-by-name [name x y]
  (let [conn (mg/connect)
        db   (mg/get-db conn "MazeDB")
        coll "Mazes"]
    (mc/insert db coll {:name name :maze (binary-generate-maze-json x y)}))
  (str "POSTED " name))

;returns a list of maze names
(defn list-mazes []
  (clojure.string/join "+" (let [conn (mg/connect)
                                 db   (mg/get-db conn "MazeDB")
                                 coll "Mazes"]
                             (map #(% :name)(mc/find-maps db coll)))))

(defroutes handler
    (GET "/get/:name" [name]
        (get-maze-by-name name))
    (GET "/binary/:name/:x/:y" [name x y]
        (create-binary-maze-by-name name (as-int x) (as-int y)))
    (GET "/ad/:name/:x/:y" [name x y]
        (create-ad-maze-by-name name (as-int x) (as-int y)))
    (GET "/maze/:x/:y" [x y]
        (binary-generate-maze-json (as-int x) (as-int y)))
    (GET "/random" []
        (get-random-maze))
    (GET "/list" []
        (list-mazes))
    (route/not-found [:h1 "Page not found"]))

(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3000}))


