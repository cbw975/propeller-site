(ns propeller-site.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [propeller-site.propeller :as propel]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STYLES & PARAMETERS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def param-input-style {:border "#555555" :font-size "15px" :height "15px" :line-height "1px" :width "60px"})
(def evolve-button-style {:width "100px" :color "#ccff00" :font-size "20px"})
(def disabled-style {:width "100px" :color "#666666" :font-size "20px" :border "#999999" :background-color "#cccccc"})

(def reports (r/atom []))
(def result (r/atom []))
(def evolved? (r/atom false))
(def gp-gen (r/atom 0))
(def gp-pop (r/atom []))

(def error-function (r/atom "regression"))
(def generations (r/atom 10))
(def population-size (r/atom 10))
(def max-initial-plushy-size (r/atom 50))
(def step-limit (r/atom 100))
(def parent-selection (r/atom :lexicase))
(def tournament-size (r/atom 5))

(def error-functions {"regression" propel/regression-error-function
                      "string-classification" propel/string-classification-error-function})

(def param-max {:generations 300
                :population-size 500
                :max-initial-plushy-size 100
                :step-limit 200
                :tournament-size @population-size})
(def param-min {:generations 0
                :population-size 1
                :max-initial-plushy-size 1
                :step-limit 1
                :tournament-size @population-size})

;;;;;;;;;;;;;;;;;;;;;;
;;  GP FUNCTION(S)  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn report-start []
  (swap! reports conj (str @error-function " errror function for " @generations " generations with " @population-size " population size."))
  (swap! reports conj (str "Running Propeller GP!"))
  (swap! reports conj (str ".....................................")))

(defn make-population [instr mips]
  (repeatedly (int @population-size)
              #(hash-map :plushy 
                         (propel/make-random-plushy instr mips))))

(defn evaluate-pop [argmap]
  (map (partial (:error-function argmap) argmap)
                             @gp-pop))

(defn sort-pop
  "Returns the curent population of items sorted by their :total-error's"
  [argmap]
  (sort-by :total-error (evaluate-pop argmap)))

(defn propel-gp
  "Runs genetic programming to solve, or approximately solve, a problem in the
   context (error-function, gp) of the given population-size, # generations, etc."
  [{:keys [population-size max-generations error-function
           instructions max-initial-plushy-size] :as argmap}]
  (let [evaluated-pop (sort-pop argmap) ; NOTE: sort-pop does not compile
        curr-best (first evaluated-pop)]
    (swap! reports conj (propel/report evaluated-pop (int @gp-gen)))
    (if (or (< (:total-error curr-best) 0.1) 
            (>= (int @gp-gen) max-generations))
      (do ;(swap! gp-gen inc)
          (swap! result conj "Ended")
          (swap! result conj (str "Best genome: " (:plushy curr-best) 
                                  ", Total error: " (int (:total-error curr-best)))))
      (do (reset! gp-pop (repeatedly population-size 
                                     #(propel/new-individual evaluated-pop argmap)))
          (swap! gp-gen inc))))) ; TODO: PAUSE CASE ??? make the above true until you pause...

;;;;;;;;;;;;;;;;;;;;;
;;  USER CONTROLS  ;;
;;;;;;;;;;;;;;;;;;;;;

(def anFrame (r/atom {}))

(defn param-change
  "Returns the parameter (<str> name) value <int> inputted, corrected if out-of-bounds or casted if non-int"
  [param-name param-value]
  (cond (< (int param-value) (get param-min param-name))
        (int (get param-min param-name))

        (> (int param-value) (get param-max param-name))
        (int (get param-max param-name))

        :else
        (int param-value)))

(defn get-argmap []
  {:instructions propel/default-instructions
   :error-function (get error-functions @error-function)
   :max-generations (param-change :generations (int @generations))
   :population-size (param-change :population-size (int @population-size))
   :max-initial-plushy-size (param-change :max-initial-plushy-size (int @max-initial-plushy-size))
   :step-limit (param-change :step-limit (int @step-limit))
   :parent-selection (keyword @parent-selection)
   :tournament-size (param-change :tournament-size (int @tournament-size))})

(defn beginning [argmap]
  (report-start)
  (reset! gp-pop (make-population (:instructions argmap)
                                  (:max-initial-plushy-size argmap))))
(defn step-evolve []
  (let [argmap (get-argmap)]
    (if (= (int @gp-gen) 0)
      (beginning argmap))
    (propel-gp argmap)
    (if (or (< (:total-error (first (sort-pop argmap))) 0.1)
            (>= (int @gp-gen) (:max-generations argmap)))
      (do (propel-gp argmap)
          (reset! evolved? true)))))

(defn run-evolve []
  (let [argmap (get-argmap)]
    (if (= (int @gp-gen) 0)
            (beginning argmap))
    (propel-gp argmap)
    (if (or (< (:total-error (first (sort-pop argmap))) 0.1)
            (>= (int @gp-gen) (:max-generations argmap)))
      (do (propel-gp argmap)
          (reset! evolved? true))
      (reset! anFrame (.requestAnimationFrame js/window run-evolve)))))

(defn stop-evolve []
  (.cancelAnimationFrame js/window @anFrame))

(defn visualize-evolve []
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  COMPONENT BUILDING  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn output-report []
  (vec (cons :div (mapv #((fn [n] [:div (str n)]) %) @reports))))

(defn output-result []
  (vec (cons :div (mapv #((fn [n] [:div [:b (str n)]]) %) @result))))

(defn error-function-input []
  [:select {:value @error-function :style (assoc param-input-style :width "150px" :height "18px")
            :on-change #(reset! error-function (-> % .-target .-value))}
   [:option {:value "regression"} "Regression"]
   [:option {:value "string-classification"} "String Classification"]])

(defn parent-selection-input []
  [:select {:value @parent-selection :style (assoc param-input-style :width "150px" :height "18px")
            :on-change #(reset! parent-selection (-> % .-target .-value))}
   [:option {:value :tournament} "Tournament"]
   [:option {:value :lexicase} "Lexicase"]])

(defn generations-input []
  [:input {:type "number" :value @generations :style param-input-style
           :min (get param-min "generations") :max (get param-max "generations")
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! generations (-> % .-target .-value))}])

(defn population-size-input []
  [:input {:type "number" :value @population-size :style param-input-style
           :min (get param-min "population-size") :max (get param-max "population-size")
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! population-size (-> % .-target .-value))}])

(defn max-initial-plushy-size-input []
  [:input {:type "number" :value @max-initial-plushy-size :style param-input-style
           :min (get param-min "max-initial-plushy-size") :max (get param-max "max-initial-plushy-size")
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! max-initial-plushy-size (-> % .-target .-value))}])

(defn step-limit-input []
  [:input {:type "number" :value @step-limit :style param-input-style
           :min (get param-min "step-limit") :max (get param-max "step-limit")
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! step-limit (-> % .-target .-value))}])

(defn tournament-size-input []
  [:input {:type "number" :value @tournament-size :style param-input-style
           :min (get param-min "tournament-size") :max (get param-max "tournament-size")
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! tournament-size (-> % .-target .-value))}])

(defn run-button []
  [:input {:type "button" :value "Run" :disabled @evolved?
           :style (if @evolved? disabled-style evolve-button-style)
           :on-click #(run-evolve)}])

(defn pause-button []
  [:input {:type "button" :value "Pause" :disabled @evolved?
           :style (if @evolved? disabled-style evolve-button-style)
           :on-click #(stop-evolve)}])

(defn step-button []
  [:input {:type "button" :value "Step" :disabled @evolved?
           :style (if @evolved? disabled-style evolve-button-style)
           :on-click #(step-evolve)}])

(defn reset-button []
  [:input {:type "button" :value "Reset" :style evolve-button-style
           :on-click #(do (reset! reports [])
                          (reset! gp-gen 0)
                          (reset! result [])
                          (reset! evolved? false)
                          (stop-evolve))}])

(defn visualize-button []
  [:input {:type "button" :value "Visualize" :disabled (false? @evolved?)
           :style (if @evolved? evolve-button-style disabled-style)
           :on-click #(visualize-evolve)}])

;;;;;;;;;;;;;
;;  Views  ;;
;;;;;;;;;;;;;

(defn inputs-component []
  [:div
   [:table {:width "100%"}
    [:tbody
     [:tr [:td "Error function = " [error-function-input]] [:td "the evaluation of performance with a behavior"]]
     [:tr [:td "Max generations = " [generations-input]] [:td "the number of generations for which it will run evolution"]]
     [:tr [:td "Population size = " [population-size-input]] [:td "the number of individuals in the population"]]
     [:tr [:td "Max initial plushy size = " [max-initial-plushy-size-input]] [:td "the limit on how large the program plushy/genome can become"]]
     [:tr [:td "Step limit = " [step-limit-input]] [:td "the limit on how long steps, i.e. due to loop structures, may go"]]
     [:tr [:td "Parent selection = " [parent-selection-input]] [:td "the process/method by which individuals/programs are chosen to reproduce"]]
     [:tr [:td "Tournament size = " [tournament-size-input]] [:td "the number of individuals per tournament bracket"]]]]])

(defn buttons-component []
  [:div
   [:table.center {:width "100%" :height "35px"}
    [:tbody
     [:tr
      [:td [run-button] [:spacer-button]]
      [:td [pause-button] [:spacer-button]]
      [:td [step-button] [:spacer-button]]
      [:td [reset-button] [:spacer-button]]
      [:td [visualize-button] [:spacer-button]]]]]])

(defn reports-component []
  [:div
   [:table {:width "100%" :border "1px solid #ccc"}
    [:tbody
     [:tr.center {:width "50%" :height "35px"}
      [:td {:width "70%"} [:h3 [:b [:em "Report: "]]]]
      [:td [:h3 [:b [:em "Visual: "]]]]]
     [:tr
      [:td [:div.scroll-container
            [output-result]
            [output-report]]]
      [:td [:h4 "COMING SOON"]]]]]])

(defn footer-component []
  [:div.footer
   [:p "Chloe Wohlgemuth (cbw975@gmail.com) \t Sung Kwak (skwak22@amherst.edu) \t Lee Spector (lspector@amherst.edu) (MORE TO BE ADDED)"]])

(defn home-page []
  [:div
   [:div.header
    [:h1 "Propeller!"]
    [:h2 "INSERT DESCRIPTION HERE"]]
   [inputs-component]
   [:div.spacer-vertical]
   [buttons-component]
   [reports-component]
   [footer-component]])

;;;;;;;;;;;;;;;;;;;;;;
;;  Initialize app  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
