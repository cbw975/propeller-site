(ns propeller-site.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [propeller-site.propeller :as propel]
   [propeller-site.scatter :as scatter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STYLES & PARAMETERS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def param-input-style {:border "#555555"
                        :font-size "15px"
                        :height "15px"
                        :line-height "1px"
                        :width "60px"})
(def evolve-button-style {:width "100px"
                          :color "#ccff00" :font-size "20px"})
(def disabled-style {:width "100px"
                     :color "#666666"
                     :font-size "20px"
                     :border "#999999"
                     :background-color "#cccccc"})

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
(def umad-rate (r/atom 0.1))
(def umad (r/atom 0.5))
(def crossover (r/atom 0.5))
(def elitism (r/atom false))

(def error-functions {"regression" propel/regression-error-function
                      "string-classification" propel/string-classification-error-function})

(def param-max {:generations 300
                :population-size 500
                :max-initial-plushy-size 100
                :step-limit 200
                :tournament-size @population-size
                :umad-rate 1.0
                :umad 1.0
                :crossover 1.0})
(def param-min {:generations 0
                :population-size 1
                :max-initial-plushy-size 1
                :step-limit 1
                :tournament-size 1
                :umad-rate (double 0)
                :umad (double 0)
                :crossover (double 0)})

;;;;;;;;;;;;;;;;;;;;;;
;;  GENOME VISUALS  ;;
;;;;;;;;;;;;;;;;;;;;;;

(def gDisplayed? (r/atom false))
(def glength 1000)              ; length for genome visual
(def gheight 50)                ; height for genome visual
(def gene-colors (r/atom {}))   ; gene (push instruction) color (visual representation) map
(def genomes (r/atom []))       ; genomes that are being displayed
(def index (r/atom 0))          ; genome visualization we are displaying

(def test-genome '("" "A" "A" :string_drop false :boolean_and :boolean_and 0))

(defn update-gene-map
  "Returns the gene (push instruction) color (visual representation) map
  for a coll of given plushies/genomes"
  [coll-of-plushies]
  (let [genes (distinct (reduce concat coll-of-plushies)) ; instructions/genes that could display
        count (count genes)
        get-color (fn []
                    (into [] (take 3 (repeatedly #(rand-int 256)))))
        colors (into [] (take count (repeatedly #(get-color))))]
    ; TODO: Change get-color to be generate more distinctive colors based on which stack, etc



    (reset! gDisplayed? true)




    (reset! gene-colors (zipmap genes colors))
    (reset! genomes [])))    ; Assign RGB values (colors) to genes in gene-colors map

(defn get-genome-colors
  "Converts a genome of instructions to vector of colors. Each gene replaced with its color (RGB values)"
  [genome]
  (into [] (map #(get @gene-colors %) genome)))

(defn setup []
  (q/frame-rate 30)       ; FPS
  (q/color-mode :hsb)     ; HSB (HSV) rather than default RGB
  {:color 0 :angle 0 :gene-count 0})    ; setup returns initial state, containing the circle color and position

(defn update-state
  "Update the sketch state by changing the (current) gene color and position"
  [state]
  (if @gDisplayed?
    state
    (let [curr-genome (get @genomes @index)
          n-genes (count curr-genome)
          gene-width (/ glength n-genes)]            ; to convert bin index to pixel offset in the x-dimension
      (if (= (:gene-count state) n-genes)
        (do (if (< @index (count @genomes))
              (swap! index inc)
              (swap! gDisplayed? not))
            {:color (nth (get @genomes @index) 0)
             :pos 0
             :gene-count 0})
        {:color (nth curr-genome (:gene-count state))
         :pos (* (:gene-count state) gene-width)
         :gene-count (inc (:gene-count state))}))))

(defn draw-state
  "(Quil) Sketch a genome (rectangular row) of color-coded genes."
  [state]
  (q/background 240)      ; Clear the sketch by filling it with light-grey color.   
  (q/fill (:color state) 255 255) ; Set circle color.
  (let [curr-genome (get @genomes @index)
        n-genes (count curr-genome)
        gene-width (/ glength n-genes)             ; to convert bin index to pixel offset in the x-dimension
        x-pos (* (:gene-count state) gene-width)]
    (q/rect x-pos 0 gene-width gheight)))

(q/defsketch display-genomes
  :host "display-genomes"
  :size [glength gheight]
  :setup setup            ; setup function called only once, during sketch initialization
  :update update-state    ; update-state is called on each iteration before draw-state
  :draw draw-state
  :middleware [m/fun-mode])

;;;;;;;;;;;;;;;;;;;;
;;  PLOT VISUALS  ;;
;;;;;;;;;;;;;;;;;;;;

(def curr-best-program (r/atom []))
(def curr-best-errors (r/atom []))

(defn get-generation-data
  "Collects population data each generation"
  [pop gen]
  (let [; data from evaluated population
        best (if (contains? (first pop) :total-error)
               (first pop)
               (throw (js/Error. (str "Called when total-error was not calculated"))))
        best-errors (:errors best)
        best-plushy (:plushy best)
        best-program (propel/plushy->push (:plushy best))]
    (reset! curr-best-program best-program)
    (reset! curr-best-errors best-errors)

    (scatter/update-plot-data pop gen)

    {:best-program best-program
     :best-plushy best-plushy
     :best-errors best-errors}))

;;;;;;;;;;;;;;;;;;;;;;
;;  GP FUNCTION(S)  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn report-start [argmap]
  (swap! reports conj (str "***************************"))
  (swap! reports conj (str "  ~  Error function:  " @error-function "  for  " (:max-generations argmap) "  generations with  " (:population-size argmap) "  individuals"))
  (swap! reports conj (str "  ~  Max Initial Plushy Size of  " (:max-initial-plushy-size argmap) "  with a step limit of  " (:step-limit argmap)))
  (swap! reports conj (str "  ~  Parent Selection via  " (:parent-selection argmap) "  with tournament size of  " (:tournament-size argmap)))
  (swap! reports conj (str "  ~  Umad-rate:  " (:umad-rate argmap) "  , umad:  " (get-in argmap [:variation :umad]) "  ,  crossover:  " (get-in argmap [:variation :crossover]) "  ,  elitism  " (:elitism argmap)))
  (swap! reports conj (str "RUNNING PROPELLER GP!"))
  (swap! reports conj (str " ***************************")))

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
  (let [evaluated-pop (sort-pop argmap)
        curr-best (first evaluated-pop)]
    (swap! reports conj (propel/report evaluated-pop (int @gp-gen)))
    (if (or (< (:total-error curr-best) 0.1)
            (>= (int @gp-gen) max-generations))
      (do (get-generation-data evaluated-pop @gp-gen)
          ; (get-genomes evaluated-pop)
          (swap! result conj "Ended")
          (swap! result conj (str "Best genome: " (:plushy curr-best)
                                  ", Total error: " (int (:total-error curr-best)))))
      (do (get-generation-data evaluated-pop @gp-gen)
          ; (get-genomes evaluated-pop)
          (reset! gp-pop (repeatedly population-size
                                     #(propel/new-individual evaluated-pop argmap)))
          (swap! gp-gen inc)))))

;;;;;;;;;;;;;;;;;;;;;
;;  USER CONTROLS  ;;
;;;;;;;;;;;;;;;;;;;;;

(def anFrame (r/atom {}))

(defn param-change
  "Returns the parameter (<str> name) value <int> inputted, corrected if out-of-bounds or casted if non-int"
  [param-name param-value]
  (cond (< param-value (get param-min param-name))
        (get param-min param-name)
        (> param-value (get param-max param-name))
        (get param-max param-name)
        :else
        param-value))

(defn get-argmap []
  {:instructions propel/default-instructions
   :error-function (get error-functions @error-function)
   :max-generations (param-change :generations (int @generations))
   :population-size (param-change :population-size (int @population-size))
   :max-initial-plushy-size (param-change :max-initial-plushy-size (int @max-initial-plushy-size))
   :step-limit (param-change :step-limit (int @step-limit))
   :parent-selection (keyword @parent-selection)
   :tournament-size (param-change :tournament-size (int @tournament-size))
   :umad-rate (param-change :umad-rate (double @umad-rate))
   :variation {:umad (param-change :umad (double @umad))
               :crossover (param-change :crossover (double @crossover))}
   :elitism (boolean @elitism)})

(defn beginning [argmap]
  (report-start argmap)
  (scatter/set-maxes (:max-generations argmap))
  (assoc scatter/yscale-max ".scatter__length" (:max-initial-plushy-size argmap))
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

(defn elitism-input []
  [:select {:value @elitism :style (assoc param-input-style :height "18px")
            :on-change #(reset! elitism (-> % .-target .-value))}
   [:option {:value true} "True"]
   [:option {:value false} "False"]])

(defn generations-input []
  [:input {:type "number" :value @generations :style param-input-style
           :min (get param-min :generations) :max (get param-max :generations)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! generations (-> % .-target .-value))}])

(defn population-size-input []
  [:input {:type "number" :value @population-size :style param-input-style
           :min (get param-min :population-size) :max (get param-max :population-size)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! population-size (-> % .-target .-value))}])

(defn max-initial-plushy-size-input []
  [:input {:type "number" :value @max-initial-plushy-size :style param-input-style
           :min (get param-min :max-initial-plushy-size) :max (get param-max :max-initial-plushy-size)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! max-initial-plushy-size (-> % .-target .-value))}])

(defn step-limit-input []
  [:input {:type "number" :value @step-limit :style param-input-style
           :min (get param-min :step-limit) :max (get param-max :step-limit)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! step-limit (-> % .-target .-value))}])

(defn tournament-size-input []
  [:input {:type "number" :value @tournament-size :style param-input-style
           :min (get param-min :tournament-size) :max (get param-max :tournament-size)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! tournament-size (-> % .-target .-value))}])

(defn umad-rate-input []
  [:input {:type "number" :value @umad-rate :style param-input-style
           :min (get param-min :umad-rate) :max (get param-max :umad-rate)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! umad-rate (-> % .-target .-value))}])

(defn umad-input []
  [:input {:type "number" :value @umad :style param-input-style
           :min (get param-min :umad) :max (get param-max :umad)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! umad (-> % .-target .-value))}])

(defn crossover-input []
  [:input {:type "number" :value @crossover :style param-input-style
           :min (get param-min :crossover) :max (get param-max :crossover)
           :disabled (not= (int @gp-gen) 0)
           :on-change #(reset! crossover (-> % .-target .-value))}])

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
                          (scatter/reset-plot-data)
                          (stop-evolve))}])

;;;;;;;;;;;;;
;;  Views  ;;
;;;;;;;;;;;;;

(defn inputs-component []
  [:div
   [:table [:tbody
            [:tr [:td.parameter "Error function = " [error-function-input]] [:td "the evaluation of performance with a behavior"]]
            [:tr [:td.parameter "Max generations = " [generations-input]] [:td "the number of generations for which it will run evolution"]]
            [:tr [:td.parameter "Population size = " [population-size-input]] [:td "the number of individuals in the population"]]
            [:tr [:td.parameter "Max initial plushy size = " [max-initial-plushy-size-input]] [:td "the limit on how large the program plushy/genome can become"]]
            [:tr [:td.parameter "Step limit = " [step-limit-input]] [:td "the limit on how long steps, i.e. due to loop structures, may go"]]
            [:tr [:td.parameter "Parent selection = " [parent-selection-input]] [:td "the process/method by which individuals/programs are chosen to reproduce"]]
            [:tr [:td.parameter "Tournament size = " [tournament-size-input]] [:td "the number of individuals per tournament bracket"]]
            [:tr [:td.parameter "UMAD rate = " [umad-rate-input]] [:td " "]]
            [:tr [:td.parameter "Variation: umad = " [umad-input]] [:td " "]]
            [:tr [:td.parameter "Variation: crossover = " [crossover-input]] [:td " "]]
            [:tr [:td.parameter "Elitism = " [elitism-input]] [:td " "]]]]])

(defn plots-component
  "Generates and updates plots as evolution progresses with each generation"
  []
  [:table.vis [:tbody
               [:tr.header
                [:td {:colSpan 2}
                 [:h3 "Plots"]]]
               [:tr
                [:td
                 [:h4 "Best Total Error vs Generation"]
                 [scatter/scatter-error]]
                [:td
                 [:h4 "Genotypic diversity vs Generation"]
                 [scatter/scatter-diversity]]]
               [:tr
                [:td [:h4 "Average genome length vs Generation"]
                 [scatter/scatter-length]]
                [:td]]]])

(defn buttons-component []
  [:div
   [:table.center {:height "35px"}
    [:tbody
     [:tr
      [:td [run-button] [:spacer-button]]
      [:td [pause-button] [:spacer-button]]
      [:td [step-button] [:spacer-button]]
      [:td [reset-button] [:spacer-button]]]]]])

(defn report-component
  "Displays generational reports in <text> format 
   (Same as whne run on the terminal)"
  []
  [:table.vis [:tbody
               [:tr.header
                [:td [:h3 "Textual Report"]]]
               [:tr
                [:td [:div.scroll-container
                      [output-result]
                      [output-report]]]]]])

(defn population-component
  "Visualizes the first INSERT genomes/plushies of the population with
   the genes as color-coded segments"
  []
  [:table.vis [:tbody
               [:tr.header [:td [:h3 "Population"]]]
               [:tr
                [:td [:div
                      [:canvas#display-genomes]
            ;; [:canvas#display-genomes]
                      ]]]]])

(defn push-component
  "Generates and updates plots as evolution progresses with each generation"
  []
  [:table.vis [:tbody
               [:tr.header
                [:td
                 [:h3 "Push Program Interpreter"]]]
               [:tr
                [:td "COMING SOON"]]]])

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
   [report-component]
   [population-component]
   [plots-component]
   [push-component]
   [footer-component]])

;;;;;;;;;;;;;;;;;;;;;;
;;  Initialize app  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
