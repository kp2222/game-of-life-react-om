(ns ^:figwheel-always om-tut.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)


(defn random-world []
  (mapv (fn[] (mapv (fn[] (rand-int 3)) (range 0 20))) (range 0 20)))

(defonce app-state
  (atom
   {:world (random-world)}))

(defn num-rows []
  (count (:world @app-state)))

(defn num-cols []
  (count (first (:world @app-state))))

(defn neighbours [[row col]]
  (map (fn [[row-offset col-offset]]
         (vector (mod (+ row row-offset) (num-rows))
                 (mod (+ col col-offset) (num-cols))))
  [[0 1] [0 -1] [1 0] [-1 0] [-1 -1] [-1 1] [1 -1] [1 1]]))


(defn alive-neighbours[pos]
  (count (filter #(= 1 %) (map #(get-in (:world @app-state) %) (neighbours pos)))))

(defn val-for-next-generation [pos]
  (cond
    (> 2 (alive-neighbours pos)) 0
    (= 3 (alive-neighbours pos)) 1
    (< 3 (alive-neighbours pos)) 0
    :else (get-in (:world @app-state) pos)))

(defn progress-world []
  (vec (doall (map-indexed (fn [row-index row]
                 (vec (doall(map-indexed (fn [col-index col] (val-for-next-generation [row-index col-index])) row))))
               (:world @app-state)))))

(js/setInterval (fn[] (do (println "progressing world!") (swap! app-state assoc :world (progress-world)))) 50)



;; OM VIEW STUFF!!
(defn render-cell [val]
  (if (= val 0) "." "*"))

(defn world-view [data owner]
  (reify
    om/IRender
    (render [this]
      (let [world (:world data)]
        (apply dom/table nil 
               (map (fn [row] (apply dom/tr nil
                                     (map (fn [col] (dom/td #js {:className "cell"} (render-cell col))) row))) world))))))

(om/root world-view app-state
         {:target (. js/document (getElementById "contacts"))})

