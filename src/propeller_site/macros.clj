(ns propeller-site.macros
  (:require [propeller-site.globals :as globals]))

(defmacro def-instruction
  [instruction definition]
  `(swap! globals/instruction-table assoc '~instruction ~definition))

(defmacro generate-functions [stacks functions]
  `(do ~@(for [stack stacks
               function functions
               :let [instruction-name (keyword (str (name stack) function))]]
           `(def-instruction ~instruction-name (partial ~function ~stack)))))


