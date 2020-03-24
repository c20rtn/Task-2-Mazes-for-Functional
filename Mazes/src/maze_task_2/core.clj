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
            [clojure.java.jdbc :as sql]
            [stencil.core :refer [render-string]]
            [ring.adapter.jetty :as jetty])
  (:use [hiccup.core])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           [com.mongodb DB WriteConcern]))

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
    (if (= 0 (rand-int 2))
      (assoc-in (assoc-in grid [x (+ y 1) :west] 1) [x y :east] 1)
      (assoc-in (assoc-in grid [(- x 1) y :south] 1) [x y :north] 1))))

(defn binary-maze-row [x y maze]
  (loop [count 0 grid maze]
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

(defn to-s [grid]  ;Returns the maze as a string
  (loop [x 0 output (apply str "+" (apply str(for [col (get-in grid [0])] "----+")) "\n")]
    (if (>= x (count grid))
      output
      (recur (+ x 1) (apply str output "|" (apply str(for [col (get-in grid [x])]
                                                       (if (= (col :east) 0) "    |" "     "))) "\n"
                                       "+" (apply str (for [col (get-in grid [x])]
                                             (if (= (col :south) 0) "----+""    +"))) "\n")))))

(defn pp-maze [grid]  ;Prints the maze to console
  (println (apply str "+" (repeat (count (get-in grid [0])) "----+")))
  (loop [x 0]
    (when (< x (count grid))
      (println (apply str "|" (for [col (get-in grid [x])]
                                (if (= (col :east) 0) "    |" "     "))))
      (println (apply str "+" (for [col (get-in grid [x])]
                                (if (= (col :south) 0) "----+""    +"))))
      (recur (+ x 1)))))

(defn to-html [s]
  (html [:pre (clojure.string/replace s #"\r\n|\n|\r" "<br />\n")]))

(defn binary-generate-maze-json [rows cols]
  (json/write-str (binary-generate-maze rows cols)))

(defroutes handler
    (GET "/gen-maze" []
        (binary-generate-maze-json 10 10))
    (GET "/maze/:size" [size :<< as-int] (binary-generate-maze-json size size)))

(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3000}))