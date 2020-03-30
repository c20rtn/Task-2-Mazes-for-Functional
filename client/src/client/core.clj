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
            [ring.adapter.jetty :as jetty])
  (:use [hiccup.core])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           [com.mongodb DB WriteConcern]))

(defn get-maze [name]
  (json/read-str
    (slurp (str "http://localhost:3000/get/" name))
    :key-fn keyword))

(defn get-random []
  (json/read-str
    (slurp (str "http://localhost:3000/random"))
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

(defn to-s [grid]  ;Returns the maze as a string
  (loop [x 0 output (apply str "+" (apply str(for [col (get-in grid [0])] "----+")) "\n")]
    (if (>= x (count grid))
      output
      (recur (+ x 1) (apply str output "|" (apply str (for [col (get-in grid [x])]
                                                        (if (= (col :east) 0)
                                                          (if (contains? col :count)
                                                            (apply str " " (format "%02d" (col :count)) " |")
                                                            "    |" )
                                                          (if (contains? col :count)
                                                            (apply str " " (format "%02d" (col :count)) "  ")
                                                            "     " )))) "\n"
                            "+" (apply str (for [col (get-in grid [x])]
                                             (if (= (col :south) 0) "----+""    +"))) "\n")))))

(defn to-html [maze]
  (html [:pre {:style "text-align:center"} (clojure.string/replace (to-s maze) #"\r\n|\n|\r" "<br />\n")]))
(def maze (dijkstra [[{:north 0, :east 1, :south 0, :west 0}
            {:north 0, :east 1, :south 1, :west 1}
            {:north 0, :east 1, :south 1, :west 1}
            {:north 0, :east 1, :south 1, :west 1}
            {:north 0, :east 0, :south 1, :west 1}]
           [{:north 0, :east 1, :south 1, :west 0}
            {:north 1, :east 0, :south 0, :west 1}
            {:north 1, :east 0, :south 0, :west 0}
            {:north 1, :east 0, :south 1, :west 0}
            {:north 1, :east 0, :south 1, :west 0}]
           [{:north 1, :east 0, :south 1, :west 0}
            {:north 0, :east 1, :south 1, :west 0}
            {:north 0, :east 1, :south 1, :west 1}
            {:north 1, :east 0, :south 1, :west 1}
            {:north 1, :east 0, :south 1, :west 0}]
           [{:north 1, :east 0, :south 0, :west 0}
            {:north 1, :east 0, :south 1, :west 0}
            {:north 1, :east 0, :south 1, :west 0}
            {:north 1, :east 0, :south 1, :west 0}
            {:north 1, :east 0, :south 1, :west 0}]
           [{:north 0, :east 1, :south 0, :west 0}
            {:north 1, :east 0, :south 0, :west 1}
            {:north 1, :east 0, :south 0, :west 0}
            {:north 1, :east 0, :south 0, :west 0}
            {:north 1, :east 0, :south 0, :west 0}]]))

(defn get-max [maze]
  (if (contains? (first(flatten maze)) :count)
    (apply max (map #(% :count) (flatten maze)))
    0))

(defn to-html-table [grid]  ;Prints the maze to console
  (html
    [:h1 {:style "font-family: Helvetica, sans-serif; text-align:center; margin:20px;"} "Maze solver" ]
    [:br]
    [:table {:style "border-collapse: collapse; margin:20px auto"}
     (for [row maze]
       [:tr (for [col row]
              [:td
               {:style (str "width:60px; height:60px; text-align:center; font-family: Helvetica, sans-serif; background-color: lightcyan;"
                            (if (= (col :north) 0) "border-top: 1px solid black;" "")
                            (if (= (col :east) 0) "border-right: 1px solid black;" "")
                            (if (= (col :south) 0) "border-bottom: 1px solid black;" "")
                            (if (= (col :west) 0) "border-left: 1px solid black;" ""))}
               (if (contains? col :count) (str (col :count)) "X")])])]))

(defroutes handler
           (GET "/view" []
             (to-html-table maze))
           (route/not-found [:h1 "Page not found"]))

(defn -main []
  (jetty/run-jetty (wrap-params handler (assoc site-defaults :security false)) {:port 3001}))