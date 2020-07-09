(ns propeller-site.scatter
  (:require [d3 :as d3]
            [reagent.core :as r]))

;;;;;;;;;;;;;;;;;;;;;
;;  SCATTER PLOTS  ;;
;;;;;;;;;;;;;;;;;;;;;

(defn format-decimal
  "format a given number to 2 decimal places"
  [num]
  (/ (.round js/Math (* 100 num)) 100))

(def empty-state [])
(def data-best-total-errors (r/atom []))
(def data-diversities (r/atom []))
(def data-genome-lengths (r/atom []))

(def height 250)
(def width 500)
(def padding 40)
(def duration 100)

(def domain-max (r/atom 1))
(def yscale-max {".scatter__error" 10000
                 ".scatter__diversity" 1
                 ".scatter__length" 50})
(defn set-maxes [x-val]
  (reset! domain-max x-val))

(defn reset-plot-data
  "resets the population data to be plotted"
  []
  (reset! data-best-total-errors empty-state)
  (reset! data-diversities empty-state)
  (reset! data-genome-lengths empty-state))

(defn update-plot-data
  "Collects population data each generation"
  [pop gen]
  (let [; previous plot data <atom> values
        prev-errors @data-best-total-errors
        prev-diversities @data-diversities
        prev-lengths @data-genome-lengths

        ; data from evaluated population
        best (if (contains? (first pop) :total-error)
               (first pop)
               (throw (js/Error. (str "Called when total-error was not calculated"))))
        best-total-error (:total-error best)
        diversity (float (/ (count (distinct (map :plushy pop))) (count pop)))
        avg-genome-length (float (/ (reduce + (map count (map :plushy pop))) (count pop)))

        total-errors (conj prev-errors [gen best-total-error])
        diversities (conj prev-diversities [gen diversity])
        genome-lengths (conj prev-lengths [gen avg-genome-length])]
    ; update plot <atom> data
    ;; (println "total errors: " total-errors)
    ;; (println "diversities: " diversities)
    ;; (println "genome-lengths: " genome-lengths)
    (reset! data-best-total-errors total-errors)
    (reset! data-diversities diversities)
    (reset! data-genome-lengths genome-lengths)))

(defn draw-scatter [state svg-id]
  (let [svg (d3/select svg-id)
        data (clj->js state)
        x-scale (-> (d3/scaleLinear)
                    (.domain #js [0 (d3/max data (fn [[x _]] x))])
                    ;; (.domain #js [0 @domain-max])
                    (.range #js [padding (- width (* padding 2))]))
        y-scale (-> (d3/scaleLinear)
                    ;; (.domain #js [0 (get yscale-max svg-id)])
                    (.domain #js [0 (d3/max data (fn [[_ y]] y))])
                    (.range #js [(- height padding) padding]))
        x-axis (-> (d3/axisBottom)
                   (.scale x-scale)
                   (.ticks 5))
        y-axis (-> (d3/axisLeft)
                   (.scale y-scale)
                   (.ticks 5))
        points (-> svg
                   (.selectAll "circle")
                   (.data data))
        labels (-> svg
                   (.selectAll "text")
                   (.data data))]
    ;; draw the circles
    (-> points
        (.enter)
        (.append "circle")
        (.attr "cx" (fn [[x _]] (x-scale x)))
        (.attr "cy" (fn [[_ y]] (y-scale y)))
        (.attr "r"  5))
    ;; draw the text
    (-> labels
        (.enter)
        (.append "text")
        (.text (fn [[_ y]] (str (format-decimal y))))
        (.attr "x" (fn [[x _]] (x-scale x)))
        ;; (.attr "y" (fn [[_ y]] (- (y-scale y) 5)))
        (.attr "y" (fn [[_ y]] (y-scale y)))
        (.attr "font-family" "sans-serif")
        (.attr "font-size" "11px")
        (.attr "fill" "red"))
    (-> svg
        (.append "g")
        (.attr "class" "axis")
        (.attr "transform" (str "translate(0," (- height padding) ")"))
        (.call x-axis))
    (-> svg
        (.append "g")
        (.attr "class" "axis")
        (.attr "transform" (str "translate(" padding ",0)"))
        (.call y-axis))))

(defn re-draw-scatter [state svg-id]
  (let [svg (d3/select svg-id)
        data (clj->js state)
        x-scale (-> (d3/scaleLinear)
                    ;; (.domain #js [0 @domain-max])
                    (.domain #js [0 (d3/max data (fn [[x _]] x))])
                    (.range #js [padding (- width (* padding 2))]))
        y-scale (-> (d3/scaleLinear)
                    (.domain #js [0 (d3/max data (fn [[_ y]] y))])
                    (.range #js [(- height padding) padding]))
        points (-> svg
                   (.selectAll "circle")
                   (.data data))
        labels (-> svg
                   (.selectAll "text")
                   (.data data))]
    ;; draw the circles
    (-> points
        (.enter)
        (.append "circle")
        (.attr "cx" (fn [[x _]] (x-scale x)))
        (.attr "cy" (fn [[_ y]] (y-scale y)))
        ;; (.attr "r"  5)
        (.merge points)
        (.transition)
        (.duration duration)
        (.attr "cx" (fn [[x _]] (x-scale x)))
        (.attr "cy" (fn [[_ y]] (y-scale y)))
        (.attr "r"  5))
    (-> points
        (.exit)
        (.transition)
        (.duration duration)
        (.remove))
    ;; draw the text
    (-> labels
        (.enter)
        (.append "text")
        (.attr "x" (fn [[x _]] (x-scale x)))
        (.attr "y" (fn [[_ y]] (- (y-scale y) 5)))
        (.text (fn [[_ y]] (str (format-decimal y))))
        (.attr "font-family" "sans-serif")
        (.attr "font-size" "11px")
        (.attr "fill" "red")
        (.merge labels)
        (.transition)
        (.duration duration)
        (.text (fn [[_ y]] (str (format-decimal y))))
        (.attr "x" (fn [[x _]] (x-scale x)))
        (.attr "y" (fn [[_ y]] (- (y-scale y) 5)))
        ;; (.attr "y" (fn [[_ y]] (y-scale y)))
        (.attr "font-family" "sans-serif")
        (.attr "font-size" "11px")
        (.attr "fill" "red"))
    (-> labels
        (.exit)
        (.transition)
        (.duration duration)
        (.remove))))

(defn scatter-plot [state svg-id]
  (let [svgid (keyword (str "svg" svg-id))]
    (r/create-class {:display-name "scatter plot"
                     :component-did-mount (fn []
                                            (draw-scatter state svg-id))
                     :component-did-update (fn [this]
                                             (let [[_ new-state] (r/argv this)]
                                               (re-draw-scatter new-state svg-id)))
                     :reagent-render (fn []
                                       [svgid {:height height
                                               :width width}])})))

(defn scatter-error []
  [:div [scatter-plot @data-best-total-errors ".scatter__error"]])

(defn scatter-diversity []
  [:div [scatter-plot @data-diversities ".scatter__diversity"]])

(defn scatter-length []
  [:div [scatter-plot @data-genome-lengths ".scatter__length"]])
