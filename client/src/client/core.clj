(ns client.core
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
            [ring.adapter.jetty :as jetty]
            [hiccup.core]
            [hiccup.form])
  (:use [hiccup.core])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           [com.mongodb DB WriteConcern]))

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

(defn get-maze [name]
  (json/read-str
    (slurp (str "http://localhost:3000/get/" (clojure.string/replace name #" " "%20")))
    :key-fn keyword))

(defn get-random []
  (json/read-str
    (slurp (str "http://localhost:3000/random"))
    :key-fn keyword))

(defn solve-new-maze [x y]
  (json/read-str
    (slurp (str "http://localhost:3000/maze/" x "/" y))
    :key-fn keyword))

(defn create-maze [x y name]
  (slurp (str "http://localhost:3000/binary/" (clojure.string/replace name #" " "%20") "/" x "/" y)))

(defn list-mazes []
  (slurp (str "http://localhost:3000/list")))

(defn dijkstra-col [x y maze]
  (loop [col 0 grid maze]
    (if (= y col)
      grid
      (recur (inc col) (assoc-in (assoc-in grid [x col :x] x) [x col :y] col)))))
(defn dijkstra-row [x y maze]
  (loop [row 0 grid maze]  ; loop over rows of empty grid
    (if (= x row)
      grid  ;return grid
      (recur (inc row) (dijkstra-col row y grid))))) ;return value of maze-row is now grid
(defn dijkstra-prep [grid]
  (dijkstra-row (count grid) (count (get-in grid [0])) grid))

;function to find neighbours
(defn dijkstra-find-neighbours [queue grid]
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
             (vec (rest (dijkstra-find-neighbours queue grid)))))))

(defn path-neighbours [grid queue]
  (let [cell (get-in grid [((last queue) :x) ((last queue) :y)]) coords (last queue)]
    (remove nil?
            (conj queue
                  (if (= (cell :north) 1) ;if passage north
                    (if (= ((get-in grid [(- (coords :x) 1) (coords :y)]) :count) (- (coords :count) 1))
                      {:x (- (coords :x) 1), :y (coords :y), :count (- (coords :count) 1)}))
                  (if (= (cell :south) 1) ;if passage south
                    (if (= ((get-in grid [(+ (coords :x) 1) (coords :y)]) :count) (- (coords :count) 1))
                      {:x (+ (coords :x) 1), :y (coords :y), :count (- (coords :count) 1)}))
                  (if (= (cell :east) 1) ;if passage east
                    (if (= ((get-in grid [(coords :x) (+ (coords :y) 1)]) :count) (- (coords :count) 1))
                      {:x (coords :x), :y (+ (coords :y) 1), :count (- (coords :count) 1)}))
                  (if (= (cell :west) 1) ;if passage west
                    (if (= ((get-in grid [(coords :x) (- (coords :y) 1)]) :count) (- (coords :count) 1))
                      {:x (coords :x) , :y (- (coords :y) 1), :count (- (coords :count) 1)}))))))

(defn get-shortest-path [maze x y]
  (loop [queue [{:x x :y y :count (:count (get-in maze [x y]))}]]
    (if (= {:x 0 :y 0 :count 0} (last queue))
      (reverse queue) ;if endpoint reached
      (recur (vec (path-neighbours maze queue))))))

(defn get-max [maze]
  (if (contains? (first(flatten maze)) :count)
    (apply max (map #(% :count) (flatten maze)))
    0))

(defn to-html-table [grid]  ;Prints the maze to console
  (let [ratio (+(get-max grid)1)
        path (get-shortest-path grid (- (count grid) 1) 0)
        maze (dijkstra-prep grid)]
    (html
      [:h1 {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"} "Maze solver" ]
      [:br]
      [:table {:style "border-collapse: collapse; margin:20px auto"}
       (for [row maze]
         [:tr (for [col row]
                [:td
                 {:style (str "width:60px; height:60px; text-align:center; font-family: Helvetica, sans-serif;"
                              (if (some true? (map #(if (and (= (% :x) (col :x))(= (% :y) (col :y))) true false) path))
                                "background: rgba(255,215,0, 1);"
                                (str "background: rgba(255, 110, 17," (str (double (/ (if (contains? col :count) (+ (col :count) 1) 1) ratio)) ");")))
                              (if (= (col :north) 0) "border-top: 2px solid black;" "")
                              (if (= (col :east) 0) "border-right: 2px solid black;" "")
                              (if (= (col :south) 0) "border-bottom: 2px solid black;" "")
                              (if (= (col :west) 0) "border-left: 2px solid black;" ""))}
                 (if (contains? col :count) (str (col :count)) "X")])])])))


(defn home-page []
  (html
    [:h1 {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"} "Maze Solver" ]

    [:div {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"}
     [:br]
     [:h2 {:style "margin:10px;"} "Maze Solver"]
     [:form
      {:action "/solve"}
         [:select {:id "name"
                   :type "text"
                   :name "name"
                   :style "color: darkcyan; border: 1px solid darkcyan; background-color: lightcyan; padding: 15px; margin: 10px; border-radius:10px;" }
          (hiccup.form/select-options (clojure.string/split
                                        (list-mazes)
                                        #"\+"))]
          [:br]
          [:input {:style "background-color: darkcyan; border-radius:10px; border: none; color: white; padding: 15px 32px; margin: 10px;" :type "submit" :value "Solve maze"}]]]

    [:div {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"}
     [:h2 {:style "margin:10px;"} "Random Maze"]
     [:form
      {:action "/random"}
      [:br]
     [:input {:style "background-color: darkcyan; border-radius:10px; border: none; color: white; padding: 15px 32px; margin: 10px;" :type "submit"  :value "Solve Random Maze"}]]]

    [:div {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"}
     [:h2 {:style "margin:10px;"} "Solve Newly Generated Maze"]
     [:form
      {:action "/solve-new"}
     [:input {:id "x"
              :type "text"
              :name "x"
              :placeholder "Enter x"
              :style "color: darkcyan; border: 1px solid darkcyan; background-color: lightcyan; padding: 15px; margin: 10px; border-radius:10px;" }]
      [:br]
     [:input {:id "y"
              :type "text"
              :name "y"
              :placeholder "Enter y"
              :style "color: darkcyan; border: 1px solid darkcyan; background-color: lightcyan; padding: 15px; margin: 10px; border-radius:10px;" }]
      [:br]
     [:input {:style "background-color: darkcyan; border-radius:10px; border: none; color: white; padding: 15px 32px; margin: 10px;" :type "submit" :value "Solve new maze"}]]]

    [:div {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"}
     [:h2 {:style "margin:10px;"} "Create Maze"]
     [:form
      {:action "/create"}
     [:input {:id "name"
              :type "text"
              :name "name"
              :placeholder "Enter name"
              :style "color: darkcyan; border: 1px solid darkcyan; background-color: lightcyan; padding: 15px; margin: 10px; border-radius:10px;" }]
      [:br]
     [:input {:id "x"
              :type "text"
              :name "x"
              :placeholder "Enter x"
              :style "color: darkcyan; border: 1px solid darkcyan; background-color: lightcyan; padding: 15px; margin: 10px; border-radius:10px;" }]
      [:br]
     [:input {:id "y"
              :type "text"
              :name "y"
              :placeholder "Enter y"
              :style "color: darkcyan; border: 1px solid darkcyan; background-color: lightcyan; padding: 15px; margin: 10px; border-radius:10px;" }]
      [:br]
     [:input {:style "background-color: darkcyan; border-radius:10px; border: none; color: white; padding: 15px 32px; margin: 10px;" :type "submit" :value "Solve new maze"}]]]))

(defroutes handler
           (GET "/solve" [name :as {u :uri rm :request-method}]
             (to-html-table (dijkstra (get-maze name))))
           (GET "/random" []
             (to-html-table (dijkstra (get-random))))
           (GET "/solve-new" [x y :as {u :uri rm :request-method}]
             (to-html-table (dijkstra (solve-new-maze (as-int x) (as-int y)))))
           (GET "/create" [name x y :as {u :uri rm :request-method}]
             (create-maze (as-int x) (as-int y) name))
           (GET "/" []
             (home-page))
           (route/not-found (html [:h1 {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"} "Page not found"])))

(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3001}))