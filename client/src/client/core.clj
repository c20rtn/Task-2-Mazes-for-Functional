(ns client.core
  (:require [clojure.data.json :as json]))

(defn get-maze [x]
  (json/read-str
    (slurp (str "http://localhost:3000/maze/" x))
    :key-fn keyword))
