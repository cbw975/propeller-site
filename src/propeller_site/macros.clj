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

;; Given a sequence of stacks, e.g. [:float :integer], and a sequence of suffix
;; function strings, e.g. [_add, _mult, _eq], automates the generation of all
;; possible combination instructions, which here would be :float_add, :float_mult,
;; :float_eq, :integer_add, :integer_mult, and :integer_eq, also transferring
;; and updating the generic function's stack-type metadata
(defmacro generate-instructions [stacks functions]
  `(do ~@(for [stack stacks
               func functions
               :let [instruction-name (keyword (str (name stack) func))
                     metadata `(update-in (meta ~func) [:stacks] #(conj % ~stack))
                     new-func `(with-meta (partial ~func ~stack) ~metadata)]]
           `(def-instruction ~instruction-name ~new-func))))