(ns propeller-site.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [propeller-site.propeller :as propel]))
; NOTE: Change the problem argument passed to evolve function specified in the run-button

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STYLES & PARAMETERS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def param-input-style {:border "#555555" :font-size "15px" :height "15px"
                        :text-align "center" :line-height "1px" :width "60px"})

(def reports (r/atom []))
(def result (r/atom []))
(def evolved? (r/atom false))
(def gp-gen (r/atom 0))
(def gp-pop (r/atom []))

(def error-function (r/atom :regression-error-function))
(def generations (r/atom 10))
(def population-size (r/atom 10))
(def max-initial-plushy-size (r/atom 5))
(def step-limit (r/atom 10))
(def parent-selection (r/atom :tournament))
(def tournament-size (r/atom 5))

(def error-functions {:regression-error-function propel/regression-error-function})
(def param-max {"generations" 200
                "population-size" 500
                "max-initial-plushy-size" 100
                "step-limit" 200
                "tournament-size" @population-size})
(def param-min {"generations" 0
                "population-size" 1
                "max-initial-plushy-size" 1
                "step-limit" 1
                "tournament-size" @population-size})

;;;;;;;;;;;;;;;;;;;;;;
;;  GP FUNCTION(S)  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn make-population [instr mips]
  (repeatedly @population-size #(hash-map :plushy (propel/make-random-plushy instr mips))))

(defn sort-pop
  "Returns the curent population of items sorted by their :total-error's"
  [argmap]
  (println "2 - start sort-pop" (partial (:error-function argmap) argmap))
  (sort-by :total-error (map (partial (:error-function argmap) argmap)
                             @gp-pop)))

(defn propel-gp
  "Runs genetic programming to solve, or approximately solve, a problem in the
   context (error-function, gp) of the given population-size, # generations, etc."
  [{:keys [population-size max-generations error-function
           instructions max-initial-plushy-size] :as argmap}]
  (let [evaluated-pop (sort-pop argmap) ; NOTE: sort-pop does not compile
        curr-best (first evaluated-pop)]
    (println "3 - evaluated pop, in let")
    (swap! reports conj (propel/report evaluated-pop (int @gp-gen)))
    (if
     (or (zero? (:total-error curr-best)) (< (:total-error curr-best) 0.1) (>= (int @gp-gen) max-generations))
      (do (swap! result conj "Ended")
          (swap! result conj (str "Best genome: " (:plushy curr-best) ", error: " (int (:error curr-best)))))
      (do (reset! gp-pop (repeatedly population-size #(propel/new-individual evaluated-pop argmap)))
          (swap! gp-gen inc))))) ; TODO: PAUSE CASE ??? make the above true until you pause...

;;;;;;;;;;;;;;;;;;;;;
;;  USER CONTROLS  ;;
;;;;;;;;;;;;;;;;;;;;;

(def anFrame (r/atom {}))

(defn param-change
  "Returns the parameter (<str> name) value <int> inputted, corrected if out-of-bounds"
  [param-name param-value]
  (cond (< (int param-value) (get param-min param-name)) (get param-min param-name)
        (> (int param-value) (get param-max param-name)) (get param-max param-name)
        :else param-value))

(defn get-argmap []
  {:instructions propel/default-instructions
   :error-function (get error-functions @error-function)
   :max-generations (param-change "generations" (int @generations))
   :population-size (param-change "population-size" (int @population-size))
   :max-initial-plushy-size (param-change "max-initial-plushy-size" (int @max-initial-plushy-size))
   :step-limit (param-change "step-limit" (int @step-limit))
   :parent-selection @parent-selection
   :tournament-size (param-change "tournament-size" (int @tournament-size))})

(defn start-evolve
  "{false = RUN; true = STEP 1 gen}"
  [isStep]
  (let [argmap (get-argmap)]
    (if (= (int @gp-gen) 0)
      (reset! gp-pop (make-population (:instructions argmap) (:max-initial-plushy-size argmap))))
    (println "1 - made population" @gp-pop)
    (propel-gp argmap)
    (if (or (< (:total-error (first (sort-pop argmap))) 0.1) (>= (int @gp-gen) (:max-generations argmap)))
      (do (propel-gp argmap)
          (reset! evolved? true))
      (if (not isStep)
        (reset! anFrame (.requestAnimationFrame js/window start-evolve))))))

(defn stop-evolve []
  (.cancelAnimationFrame js/window @anFrame))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  COMPONENT BUILDING  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn output-report []
  (vec (cons :div (mapv #((fn [n] [:div (str n)]) %) @reports))))

(defn output-result []
  (vec (cons :div (mapv #((fn [n] [:div [:b (str n)]]) %) @result))))

(defn error-function-input []
  ;(if (contains? error-functions @error-function)
  (let [i {:type "text" :value @error-function  :style (assoc param-input-style :width "200px")
           :on-change #(reset! error-function (-> % .-target .-value))}]
    [:input.field i]));  )

(defn parent-selection-input []
  ;(if (contains? parent-selections @parent-selection)  
  [:input.field {:type "text" :value @parent-selection
                 :style (assoc param-input-style :width "150px")
                 :on-change #(reset! parent-selection (-> % .-target .-value))}]);  )

(defn generations-input []
  (let [b {:type "number" :value @generations :style param-input-style
           :min (get param-min "generations") :max (get param-max "generations")
           :on-change #(reset! generations (-> % .-target .-value))}]
    (if (= (int @gp-gen) 0)
      [:input b]
      [:input (assoc b :disabled "true")])))

(defn population-size-input []
  (let [b {:type "number" :value @population-size :style param-input-style
           :min (get param-min "population-size") :max (get param-max "population-size")
           :on-change #(reset! population-size (-> % .-target .-value))}]
    (if (= (int @gp-gen) 0)
      [:input b]
      [:input (assoc b :disabled "true")])))

(defn max-initial-plushy-size-input []
  (let [b {:type "number" :value @max-initial-plushy-size :style param-input-style
           :min (get param-min "max-initial-plushy-size") :max (get param-max "max-initial-plushy-size")
           :on-change #(reset! max-initial-plushy-size (-> % .-target .-value))}]
    (if (= (int @gp-gen) 0)
      [:input b]
      [:input (assoc b :disabled "true")])))

(defn step-limit-input []
  (let [b {:type "number" :value @step-limit :style param-input-style
           :min (get param-min "step-limit") :max (get param-max "step-limit")
           :on-change #(reset! step-limit (-> % .-target .-value))}]
    (if (= (int @gp-gen) 0)
      [:input b]
      [:input (assoc b :disabled "true")])))

(defn tournament-size-input []
  (let [b {:type "number" :value @tournament-size :style param-input-style ;:disabled (str (= (int @gp-prog-gen) 0))
           :min (get param-min "tournament-size") :max (get param-max "tournament-size")
           :on-change #(reset! tournament-size (-> % .-target .-value))}]
    (if (= (int @gp-gen) 0)
      [:input b]
      [:input (assoc b :disabled "true")])))

(defn run-button []
  [:input
   (if (false? @evolved?)
     {:type "button" :value "Run" :on-click #(start-evolve false)
      :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}}
     {:type "button" :value "Run" :disabled "true" :on-click #(start-evolve false)
      :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}})])

(defn pause-button []
  [:input
   (if (false? @evolved?)
     {:type "button" :value "Pause" :on-click #(stop-evolve)
      :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}}
     {:type "button" :value "Pause" :disabled "true" :on-click #(stop-evolve)
      :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}})])

(defn step-button []
  [:input
   (if (false? @evolved?)
     {:type "button" :value "Step" :on-click #(start-evolve true)
      :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}}
     {:type "button" :value "Step" :disabled "true" :on-click #(start-evolve true)
      :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}})])

(defn reset-button []
  [:input {:type "button" :value "Reset"
           :style {:width "100px" :color "#ccff00" :font-size "24px" :text-align "center"}
           :on-click #(do (reset! reports [])
                          (reset! gp-gen 0)
                          (reset! result [])
                          (reset! evolved? false))}])

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
  [:div [:table {:width "100%" :height "35px" :text-align "center"}
         [:tbody
          [:td [run-button]] [:spacer-button]
          [:td [pause-button]] [:spacer-button]
          [:td [step-button]] [:spacer-button]
          [:td [reset-button]] [:spacer-button]
     ;[:td [plot-button]] [:spacer-button]
     ;[:td [other-button]] [:spacer-button]
          ]]])

(defn reports-component []
  [:div
   [:table {:width "100%" :border "1px solid #ccc"}
    [:tr {:width "50%" :height "35px" :text-align "center"}
     [:td {:width "60%"} [:h3 [:b [:em "Report: "]]]]
     [:td [:h3 [:b [:em "Visual: "]]]]]
    [:tr
     [:td [:div.scroll-container [output-result] [output-report]]]
     [:td [:h4 "COMING SOON"]]]]])

(defn footer-component []
  [:div.footer {:width "100%" :margin-bottom "-10px"}
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
  (println "\n")
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
