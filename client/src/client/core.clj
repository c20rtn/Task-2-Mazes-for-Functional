(ns client.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.json :as json]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse x y 100 100))))

(q/defsketch client
             :title "You spin my circle right round"
             :size [500 500]
             ; setup function called only once, during sketch initialization.
             :setup setup
             ; update-state is called on each iteration before draw-state.
             :update update-state
             :draw draw-state
             :features [:keep-on-top]
             ; This sketch uses functional-mode middleware.
             ; Check quil wiki for more info about middlewares and particularly
             ; fun-mode.
             :middleware [m/fun-mode])




(def mazey [[{:north 0, :east 1, :south 0, :west 0}
             {:north 0, :east 1, :south 1, :west 1}
             {:north 0, :east 1, :south 1, :west 1}
             {:north 0, :east 1, :south 0, :west 1}
             {:north 0, :east 0, :south 1, :west 1}]
            [{:north 0, :east 1, :south 0, :west 0}
             {:north 1, :east 0, :south 0, :west 1}
             {:north 1, :east 0, :south 1, :west 0}
             {:north 0, :east 1, :south 1, :west 0}
             {:north 1, :east 0, :south 1, :west 1}]
            [{:north 0, :east 1, :south 0, :west 0}
             {:north 0, :east 1, :south 0, :west 1}
             {:north 1, :east 0, :south 1, :west 1}
             {:north 1, :east 0, :south 0, :west 0}
             {:north 1, :east 0, :south 1, :west 0}]
            [{:north 0, :east 1, :south 1, :west 0}
             {:north 0, :east 1, :south 1, :west 1}
             {:north 1, :east 0, :south 0, :west 1}
             {:north 0, :east 1, :south 0, :west 0}
             {:north 1, :east 0, :south 1, :west 1}]
            [{:north 1, :east 0, :south 0, :west 0}
             {:north 1, :east 0, :south 0, :west 0}
             {:north 0, :east 1, :south 0, :west 0}
             {:north 0, :east 1, :south 0, :west 1}
             {:north 1, :east 0, :south 0, :west 1}]])

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

(defn dijkstra-cell [x y grid]
  (assoc-in (assoc-in grid [x y :x] x) [x y :y] y))

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

(dijkstra-prep mazey)

(defn dijkstra [maze]
  (loop [grid maze, list ({:x 0 :y 0 :count 0}), count 0]


    (recur (assoc-in grid [((first list) :x) ((first list) :y) :count] ((first list) :count)) ;puts distance into the maze
           list
           count)))

;function to find neighbours
(defn find-neighbours [list grid]
  (loop [l list g grid ])

    (recur ))
(find-neighbours [{:x 0, :y 1, :count 0}] mazey)


(let [cell (get-in nmazey [((first listy) :x) ((first listy) :y)]) coords (first listy)]
  (remove nil?
          (conj listy
                (if (= (cell :north) 1) ;if passage north
                  (if (not (contains? (get-in nmazey [(- (coords :x) 1) (coords :y)]) :count)) ;if not visited
                    {:x (- (coords :x) 1), :y (coords :y), :count (inc (coords :count))})) ;add coords and new incremented count to list
                (if (= (cell :south) 1) ;if passage north
                  (if (not (contains? (get-in nmazey [(+ (coords :x) 1) (coords :y)]) :count)) ;if not visited
                    {:x (+ (coords :x) 1), :y (coords :y), :count (inc (coords :count))}))
                (if (= (cell :east) 1) ;if passage north
                  (if (not (contains? (get-in nmazey [(coords :x) (+ (coords :y) 1)]) :count)) ;if not visited
                    {:x (coords :x), :y (+ (coords :y) 1), :count (inc (coords :count))}))
                (if (= (cell :west) 1) ;if passage north
                  (if (not (contains? (get-in nmazey [(coords :x) (- (coords :y) 1)]) :count)) ;if not visited
                    {:x (coords :x) , :y (- (coords :y) 1), :count (inc (coords :count))})))))