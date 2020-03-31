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

;Creates a new maze and stores it
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
    (GET "/maze/:x/:y" [x y]
        (binary-generate-maze-json (as-int x) (as-int y)))
    (GET "/random" []
        (get-random-maze))
    (GET "/list" []
        (list-mazes))
    (route/not-found [:h1 "Page not found"]))

(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3000}))