(ns propeller-site.propeller
  (:require-macros [propeller-site.macros :refer [def-instruction
                                                  generate-instructions]])
  (:require [clojure.string]
            [propeller-site.globals :as globals]))

;; ============================================================================
;; Tools
;; ============================================================================

(defn abs
  "Returns the absolute value of a number."
  [x]
  (if (neg? x) (- x) x))

(defn cos
  "Returns the cosine of an angle (specified in radians)."
  [x]
  ;; #?(:clj  (cos x)
  ;;    :cljs (js/Math.cos x))
  (js/Math.cos x))

(defn sin
  "Returns the sine of an angle (specified in radians)."
  [x]
  ;; #?(:clj  (sin x)
  ;;    :cljs (js/Math.sin x))
  (js/Math.sin x))

(defn tan
  "Returns the tangent of an angle (specified in radians)."
  [x]
  ;; #?(:clj  (tan x)
  ;;    :cljs (js/Math.tan x))
  (js/Math.tan x))

;; (defn is-letter
;;   "Returns true if the given character is a letter, A-Z or a-z."
;;   [c]
;;   (<= (int \A) (int c) (int \z)))

;; (defn is-digit
;;   "Returns true if the given character is a digit, 0-9."
;;   [c]
;;   (<= (int \0) (int c) (int \9)))

;; (defn is-whitespace
;;   "Returns true if the given character is whitespace (newline, space, tab)."
;;   [c]
;;   (contains? #{(int \newline) (int \tab) (int \space)} (int c)))

;; =============================================================================
;; 
;; Utils
;; 
;; =============================================================================

(defn get-vector-literal-type
  "Returns the literal stack corresponding to some vector stack."
  [vector-stack]
  (keyword (clojure.string/replace (str vector-stack) ":vector_" "")))

(defn indexof
  "Returns the first index of an element in a collection. If the element is not
  present in the collection, returns -1."
  [element coll]
  (or (first (keep-indexed #(if (= element %2) %1) coll)) -1))

(defn not-lazy
  "Returns lst if it is not a seq, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn ensure-list
  "Returns a non-lazy list if passed a seq argument. Othwrwise, returns a list
  containing the argument."
  [thing]
  (if (seq? thing)
    (not-lazy thing)
    (list thing)))

(defn random-instruction
  "Returns a random instruction from a supplied pool of instructions, evaluating
  ERC-producing functions to a constant literal."
  [instructions]
  (let [instruction (rand-nth instructions)]
    (if (fn? instruction)
      (instruction)
      instruction)))

;; =============================================================================
;; PushGP Instructions
;;
;; Instructions are represented as keywords, and stored in an atom.
;;
;; Instructions must all be either functions that take one Push state and
;; return another, or constant literals.
;;
;; TMH: ERCs?
;; =============================================================================

;; Set of original propel instructions
(def default-instructions
  (list :in1
        :integer_add
        :integer_subtract
        :integer_mult
        :integer_quot
        :integer_eq
        :exec_dup
        :exec_if
        :boolean_and
        :boolean_or
        :boolean_not
        :boolean_eq
        :string_eq
        :string_take
        :string_drop
        :string_reverse
        :string_concat
        :string_length
        :string_includes?
        'close
        0
        1
        true
        false
        ""
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "A"
        "C"
        "G"
        "T"))

;; Number of blocks opened by instructions (default = 0)
(def opens {:exec_dup 1
            :exec_if  2})

;; Set of all stacks used by the Push interpreter
(defonce stacks #{:auxiliary
                  :boolean
                  ;; :char
                  :code
                  :environment
                  :exec
                  :float
                  :genome
                  :gtm
                  :input
                  :integer
                  :output
                  :return
                  :string
                  :tag
                  :vector_boolean
                  :vector_float
                  :vector_integer
                  :vector_string
                  ;; :zip
                  })

;; Record-based states for performance

;; Empty push state - each stack type is nil
;(defonce empty-state (map->State {}))
(defonce empty-state {:auxiliary      '()
                      :boolean        '()
                      :char           '()
                      :code           '()
                      :exec           '()
                      :float          '()
                      :input          {}
                      :integer        '()
                      :output         '()
                      :string         '()
                      :vector_boolean '()
                      :vector_float   '()
                      :vector_integer '()
                      :vector_string  '()})

(def example-push-state
  {:exec    '()
   :integer '(1 2 3 4 5 6 7)
   :string  '("abc")
   :input   {:in1 4}})

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (let [working-stack (get state stack)]
    (if (empty? working-stack)
      :no-stack-item
      (first working-stack))))

(defn peek-stack-multiple
  "Returns the top n items on a stack. If there are less than n items on the
  stack, returns the entire stack."
  [state stack n]
  (take n (get state stack)))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn pop-stack-multiple
  "Removes the top n items of a stack. If there are less than n items on the
  stack, pops the entire stack."
  [state stack n]
  (update state stack #(drop n %)))

(defn push-to-stack
  "Pushes item(s) onto stack."
  [state stack items]
  (let [items-list (if (coll? items) items (list items))
        items-list-no-nil (filter #(not (nil? %)) items-list)]
    (update state stack into items-list-no-nil)))

(defn push-to-stack-multiple
  "Pushes a list of items onto a stack, leaving them in the order they are in."
  [state stack items]
  (let [items-list (if (coll? items) items (list items))
        items-list-no-nil (filter #(not (nil? %)) items-list)]
    (update state stack into (reverse items-list-no-nil))))

(defn get-args-from-stacks
  "Takes a state and a collection of stacks to take args from. If there are
  enough args on each of the desired stacks, returns a map with keys
  {:state :args}, where :state is the new state and :args is a list of args
  popped from the stacks. If there aren't enough args on the stacks, returns
  :not-enough-args without popping anything."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [current-stack (first stacks)]
        (if (empty-stack? state current-stack)
          :not-enough-args
          (recur (pop-stack state current-stack)
                 (rest stacks)
                 (conj args (peek-stack state current-stack))))))))


;; A utility function for making Push instructions. Takes a state, a function
;; to apply to the args, the stacks to take the args from, and the stack to
;; return the result to. Applies the function to the args (popped from the
;; given stacks), and pushes the result onto the return-stack
(defn make-instruction
  [state function arg-stacks return-stack]
  (let [popped-args (get-args-from-stacks state arg-stacks)]
    (if (= popped-args :not-enough-args)
      state
      (let [result (apply function (:args popped-args))
            new-state (:state popped-args)]
        (push-to-stack new-state return-stack result)))))

;; Given a sequence of stacks, e.g. [:float :integer], and a sequence of suffix
;; function strings, e.g. [_+, _*, _=], automates the generation of all possible
;; combination instructions, which here would be :float_+, :float_*, :float_=,
;; :integer_+, :integer_*, and :integer_=

;; If a piece of data is a literal, return its corresponding stack name, e.g.
;; :integer. Otherwise, return nil"
(defn get-literal-type
  [data]
  (let [literals {:boolean        (fn [thing] (or (true? thing) (false? thing)))
                  :char           char?
                  :float          float?
                  :integer        integer?
                  :string         string?
                  :vector_boolean (fn [thing] (and (vector? thing)
                                                   (or (true? (first thing))
                                                       (false? (first thing)))))
                  :vector_float   (fn [thing] (and (vector? thing)
                                                   (float? (first thing))))
                  :vector_integer (fn [thing] (and (vector? thing)
                                                   (integer? (first thing))))
                  :vector_string  (fn [thing] (and (vector? thing)
                                                   (string? (first thing))))
                  :generic-vector (fn [thing] (= [] thing))}]
    (first (for [[stack function] literals
                 :when (function data)]
             stack))))

;; Pretty-prints a Push state, for logging or debugging purposes
(defn print-state
  [state]
  (doseq [stack stacks]
    ;(printf "%-15s = " stack)
    (prn (if (get state stack) (get state stack) '()))
    (flush)))

;; =============================================================================
;; INPUT Instructions
;; =============================================================================

;; Allows Push to handle input instructions of the form :inN, e.g. :in2, taking
;; elements thus labeled from the :input stack and pushing them onto the :exec
;; stack. We can tell whether a particular inN instruction is valid if N-1
;; values are on the input stack.
(defn handle-input-instruction
  [state instruction]
  (if-let [input (instruction (:input state))]
    (push-to-stack state :exec input)
    (throw (js/Error. (str "Undefined input instruction " instruction)))))

;; =============================================================================
;; OUTPUT Instructions
;; =============================================================================

(def-instruction
  :print_newline
  ^{:stacks [:print]}
  (fn [state]
    (let [current-output (peek-stack state :output)
          popped-state (pop-stack state :output)]
      (push-to-stack popped-state :output (str current-output \newline)))))

(def _print
  ^{:stacks [:print]}
  (fn [stack state]
    (if (empty-stack? state stack)
      state
      (let [top-item (peek-stack state stack)
            top-item-str (if (or (string? top-item)
                                 (char? top-item))
                           top-item
                           (pr-str top-item))
            current-output (peek-stack state :output)
            popped-state (pop-stack (pop-stack state stack) :output)]
        (push-to-stack popped-state
                             :output
                             (str current-output top-item-str))))))

(generate-instructions
 [:boolean :char :code :exec :float :integer :string
  :vector_boolean :vector_float :vector_integer :vector_string]
 [_print])

;; =============================================================================
;; FLOAT and INTEGER Instructions (polymorphic)
;; =============================================================================

;; Pushes TRUE onto the BOOLEAN stack if the second item is greater than the top
;; item, and FALSE otherwise
(def _gt
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state > [stack stack] :boolean)))

;; Pushes TRUE onto the BOOLEAN stack if the second item is greater than or
;; equal to the top item, and FALSE otherwise
(def _gte
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state >= [stack stack] :boolean)))

;; Pushes TRUE onto the BOOLEAN stack if the second item is less than the top
;; item, and FALSE otherwise
(def _lt
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state < [stack stack] :boolean)))

;; Pushes TRUE onto the BOOLEAN stack if the second item is less than or equal
;; to the top item, and FALSE otherwise
(def _lte
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state <= [stack stack] :boolean)))

;; Pushes the sum of the top two items onto the same stack
(def _add
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state + [stack stack] stack)))

;; Pushes the difference of the top two items (i.e. the second item minus the
;; top item) onto the same stack
(def _subtract
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state - [stack stack] stack)))

;; Pushes the product of the top two items onto the same stack
(def _mult
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state * [stack stack] stack)))

;; Pushes the quotient of the top two items (i.e. the second item divided by the
;; top item) onto the same stack. If the top item is zero, pushes 1
(def _quot
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state #(if (zero? %2) 1 (quot %1 %2)) [stack stack] stack)))

;; Pushes the second item modulo the top item onto the same stack. If the top
;; item is zero, pushes 1. The modulus is computed as the remainder of the
;; quotient, where the quotient has first been truncated towards negative
;; infinity.
(def _mod
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state #(if (zero? %2) 1 (mod %1 %2)) [stack stack] stack)))

;; Pushes the maximum of the top two items
(def _max
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state max [stack stack] stack)))

;; Pushes the minimum of the top two items
(def _min
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state min [stack stack] stack)))

;; Pushes 1 / 1.0 if the top BOOLEAN is TRUE, or 0 / 0.0 if FALSE
(def _fromboolean
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state
                      #((if (= stack :integer) int float) (if % 1 0))
                      [:boolean]
                      stack)))

;; Pushes the ASCII value of the top CHAR
(def _fromchar
  ^{:stacks #{:char}}
  (fn [stack state]
    (make-instruction state (if (= stack :integer) int float) [:char] stack)))

;; Pushes the value of the top STRING, if it can be parsed as a number.
;; Otherwise, acts as a NOOP
(def _fromstring
  ^{:stacks #{:string}}
  (fn [stack state]
    (make-instruction state
                      #(try ((if (= stack :integer) int float) (js/Number %))
                            (catch js/Error e))
                      [:string]
                      stack)))

;; Pushes the increment (i.e. +1) of the top item of the stack
(def _inc
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state inc [stack] stack)))

;; Pushes the decrement (i.e. -1) of the top item of the stack
(def _dec
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state dec [stack] stack)))

;; 2 types x 16 functions = 32 instructions
(generate-instructions
 [:float :integer]
 [_gt _gte _lt _lte _add _subtract _mult _quot _mod _max _min _inc _dec
  _fromboolean _fromchar _fromstring])

;; =============================================================================
;; FLOAT Instructions only
;; =============================================================================

;; Pushes the cosine of the top FLOAT
(def-instruction
  :float_cos
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state js/Math.cos [:float] :float)))

;; Pushes the sine of the top FLOAT
(def-instruction
  :float_sin
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state js/Math.sin [:float] :float)))

;; Pushes the tangent of the top FLOAT
(def-instruction
  :float_tan
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state js/Math.tan [:float] :float)))

;; Pushes the floating point version of the top INTEGER
(def-instruction
  :float_frominteger
  ^{:stacks #{:float :integer}}
  (fn [state]
    (make-instruction state float [:integer] :float)))

;; =============================================================================
;; INTEGER Instructions only
;; =============================================================================

;; Pushes the result of truncating the top FLOAT towards negative infinity
(def-instruction
  :integer_fromfloat
  ^{:stacks #{:float :integer}}
  (fn [state]
    (make-instruction state int [:float] :integer)))

;; =============================================================================
;; Polymorphic Instructions
;; =============================================================================

(def _noop
  ^{:stacks #{}}
  (fn [stack state] state))

(generate-instructions [:exec :code] [_noop])

;; =============================================================================
;; CODE Instructions
;; =============================================================================

;; Concatenates the top two instructions on the :code stack and pushes the
;; result back onto the stack
(def-instruction
  :code_append
  ^{:stacks #{:code}}
  (fn [state]
    (make-instruction state
                      #(not-lazy
                        (concat (ensure-list %2)
                                (ensure-list %1)))
                      [:code :code]
                      :code)))

;; Unfinished...

;; =============================================================================
;; EXEC Instructions
;; =============================================================================

;; Executes the top EXEC instruction (i.e. loops) a number of times determined
;; by the top two INTEGERs, while also pushing the loop counter onto the INTEGER
;; stack. The top INTEGER is the "destination index" and the second INTEGER is
;; the "current index". If the integers are equal, then the current index is
;; pushed onto the INTEGER stack and the code (which is the "body" of the loop)
;; is pushed onto the EXEC stack for subsequent execution. If the integers are
;; not equal, then the current index will still be pushed onto the INTEGER stack
;; but two items will be pushed onto the EXEC stack - first a recursive call to
;; :exec_do*range (with the same code and destination index, but with a current
;; index that has been either incremented or decremented by 1 to be closer to
;; the destination index) and then the body code. Note that the range is
;; inclusive of both endpoints; a call with integer arguments 3 and 5 will cause
;; its body to be executed 3 times, with the loop counter having the values 3,
;; 4, and 5. Note also that one can specify a loop that "counts down" by
;; providing a destination index that is less than the specified current index.
(def-instruction
  :exec_do*range
  ^{:stacks #{:exec :integer}}
  (fn [state]
    (if (or (empty-stack? state :exec)
            (< (count (:integer state)) 2))
      state
      (let [to-do (peek-stack state :exec)
            destination-index (peek-stack state :integer)
            current-index (peek-stack
                           (pop-stack state :integer) :integer)
            popped-state (pop-stack
                          (pop-stack
                           (pop-stack state :exec) :integer) :integer)
            increment (cond (< current-index destination-index) 1
                            (> current-index destination-index) -1
                            :else 0)
            continuation (if (zero? increment)
                           popped-state
                           (push-to-stack popped-state
                                                :exec
                                                (list (+ current-index increment)
                                                      destination-index
                                                      :exec_do*range
                                                      to-do)))]
        (push-to-stack
         (push-to-stack continuation :integer current-index) :exec to-do)))))

;; Executes the top EXEC instruction (i.e. loops) a number of times determined
;; by the top INTEGER, pushing an index (which runs from 0 to one less than the
;; total number of iterations) onto the INTEGER stack prior to each execution
;; of the loop body. If the top INTEGER argument is <= 0, this becomes a NOOP
(def-instruction
  :exec_do*count
  ^{:stacks #{:exec :integer}}
  (fn [state]
    (if (or (empty-stack? state :integer)
            (empty-stack? state :exec)
            (< (peek-stack state :integer) 1))
      state
      (let [to-do (peek-stack state :exec)
            index (peek-stack state :integer)
            popped-state (pop-stack (pop-stack state :exec) :integer)]
        (push-to-stack popped-state :exec (list 0
                                                      (dec index)
                                                      :exec_do*range
                                                      to-do))))))

;; Like :exec_do*count, but does not push the loop counter onto the INTEGER stack
(def-instruction
  :exec_do*times
  ^{:stacks #{:exec :integer}}
  (fn [state]
    (if (or (empty-stack? state :integer)
            (empty-stack? state :exec)
            (< (peek-stack state :integer) 1))
      state
      (let [to-do (peek-stack state :exec)
            to-do-with-pop (cons :integer_pop (ensure-list to-do))
            index (peek-stack state :integer)
            popped-state (pop-stack (pop-stack state :exec) :integer)]
        (push-to-stack popped-state :exec (list 0
                                                      (dec index)
                                                      :exec_do*range
                                                      to-do-with-pop))))))

;; If the top BOOLEAN is TRUE, removes the the second item on the EXEC stack,
;; leaving the first item to be executed. Otherwise, removes the first item,
;; leaving the second to be executed. Acts as a NOOP unless there are at least
;; two items on the EXEC stack and one item on the BOOLEAN stack
(def-instruction
  :exec_if
  ^{:stacks #{:boolean :exec}}
  (fn [state]
    (make-instruction state #(if %1 %3 %2) [:boolean :exec :exec] :exec)))

;; If the top BOOLEAN is TRUE, leaves the first item on the EXEC stack to be
;; executed. Otherwise, it removes it. Acts as a NOOP unless there is at least
;; one item on the EXEC stack and one item on the BOOLEAN stack
(def-instruction
  :exec_when
  ^{:stacks #{:boolean :exec}}
  (fn [state]
    (make-instruction state #(when %1 %2) [:boolean :exec] :exec)))

;; Keeps executing the top instruction on the EXEC stack while the top item on
;; the BOOLEAN stack is true
(def-instruction
  :exec_while
  ^{:stacks #{:boolean :exec}}
  (fn [state]
    (if (empty-stack? state :exec)
      state
      (if (empty-stack? state :boolean)
        (pop-stack state :exec)
        (if (peek-stack state :boolean)
          (let [to-do (peek-stack state :exec)
                popped-state (pop-stack state :boolean)]
            (push-to-stack
             (push-to-stack popped-state :exec :exec_while) :exec to-do))
          (pop-stack (pop-stack state :boolean) :exec))))))

;; Keeps executing the top instruction on the EXEC stack while the top item on
;; the BOOLEAN stack is true. Differs from :exec_while in that it executes
;; the top instruction at least once
(def-instruction
  :exec_do*while
  ^{:stacks #{:boolean :exec}}
  (fn [state]
    (if (empty-stack? state :exec)
      state
      (let [to-do (peek-stack state :exec)]
        (push-to-stack
         (push-to-stack state :exec :exec_while) :exec to-do)))))

;; The "K combinator" - removes the second item on the EXEC stack
(def-instruction
  :exec_k
  ^{:stacks #{:exec}}
  (fn [state]
    (make-instruction state (fn [_ first] first) [:exec :exec] :exec)))

;; The "S combinator" - pops 3 items from the EXEC stack, which we will call A,
;; B, and C (with A being the first one popped), and then pushes a list
;; containing B and C back onto the EXEC stack, followed by another instance of
;; C, followed by another instance of A
(def-instruction
  :exec_s
  ^{:stacks #{:exec}}
  (fn [state]
    (if (< (count (:exec state)) 3)
      state
      (let [[a b c] (peek-stack-multiple state :exec 3)
            popped-state (pop-stack-multiple state :exec 3)
            to-push-back (list a c (list b c))]
        (push-to-stack-multiple popped-state :exec to-push-back)))))

;; The "Y combinator" - inserts beneath the top item of the EXEC stack a new
;; item of the form "(:exec_y TOP_ITEM)"
(def-instruction
  :exec_y
  ^{:stacks #{:exec}}
  (fn [state]
    (if (empty-stack? state :exec)
      state
      (let [top-item (peek-stack state :exec)
            popped-state (pop-stack state :exec)
            to-push-back (list top-item (list :exec_y top-item))]
        (push-to-stack-multiple popped-state :exec to-push-back)))))

;; =============================================================================
;; BOOLEAN Instructions
;; =============================================================================

;; Pushes the logical AND of the top two BOOLEANs
(def-instruction
  :boolean_and
  ^{:stacks #{:boolean}}
  (fn [state]
    (make-instruction state #(and %1 %2) [:boolean :boolean] :boolean)))

;; Pushes the logical OR of the top two BOOLEANs
(def-instruction
  :boolean_or
  ^{:stacks #{:boolean}}
  (fn [state]
    (make-instruction state #(or %1 %2) [:boolean :boolean] :boolean)))

;; Pushes the logical NOT of the top BOOLEAN
(def-instruction
  :boolean_not
  ^{:stacks #{:boolean}}
  (fn [state]
    (make-instruction state not [:boolean] :boolean)))

;; Pushes the logical XOR of the top two BOOLEAN
(def-instruction
  :boolean_xor
  ^{:stacks #{:boolean}}
  (fn [state]
    (make-instruction state #(or (and %1 (not %2))
                                 (and (not %1) %2))
                      [:boolean :boolean]
                      :boolean)))

;; Pushes the logical AND of the top two BOOLEANs, after applying NOT to the
;; first one
(def-instruction
  :boolean_invert_first_then_and
  ^{:stacks #{:boolean}}
  (fn [state]
    (make-instruction state #(and %1 (not %2)) [:boolean :boolean] :boolean)))

;; Pushes the logical AND of the top two BOOLEANs, after applying NOT to the
;; second one
(def-instruction
  :boolean_invert_second_then_and
  ^{:stacks #{:boolean}}
  (fn [state]
    (make-instruction state #(and (not %1) %2) [:boolean :boolean] :boolean)))

;; Pushes FALSE if the top FLOAT is 0.0, and TRUE otherwise
(def-instruction
  :boolean_fromfloat
  ^{:stacks #{:boolean :float}}
  (fn [state]
    (make-instruction state #(not (zero? %)) [:float] :boolean)))

;; Pushes FALSE if the top INTEGER is 0, and TRUE otherwise
(def-instruction
  :boolean_frominteger
  ^{:stacks #{:boolean :integer}}
  (fn [state]
    (make-instruction state #(not (zero? %)) [:integer] :boolean)))

;; =============================================================================
;; CHAR Instructions
;; =============================================================================

;; Pushes TRUE onto the BOOLEAN stack if the popped character is a letter
;; (def-instruction
;;   :char_isletter
;;   ^{:stacks #{:boolean :char}}
;;   (fn [state]
;;     (make-instruction state is-letter [:char] :boolean)))

;; ;; Pushes TRUE onto the BOOLEAN stack if the popped character is a digit
;; (def-instruction
;;   :char_isdigit
;;   ^{:stacks #{:boolean :char}}
;;   (fn [state]
;;     (make-instruction state is-digit [:char] :boolean)))

;; ;; Pushes TRUE onto the BOOLEAN stack if the popped character is whitespace
;; ;; (newline, space, or tab)
;; (def-instruction
;;   :char_iswhitespace
;;   ^{:stacks #{:boolean :char}}
;;   (fn [state]
;;     (make-instruction state is-whitespace [:char] :boolean)))

;; Pops the FLOAT stack, converts the top item to a whole number, and pushes
;; its corresponding ASCII value onto the CHAR stack. Whole numbers larger than
;; 128 will be reduced modulo 128. For instance, 248.45 will result in x being
;; pushed.
(def-instruction
  :char_fromfloat
  ^{:stacks #{:char :float}}
  (fn [state]
    (make-instruction state #(char (mod (long %) 128)) [:float] :char)))

;; Pops the INTEGER stack and pushes the top element's corresponding ASCII
;; value onto the CHAR stack. Integers larger than 128 will be reduced modulo
;; 128. For instance, 248 will result in x being pushed
(def-instruction
  :char_frominteger
  ^{:stacks #{:char :integer}}
  (fn [state]
    (make-instruction state #(char (mod % 128)) [:integer] :char)))

;; Pops the STRING stack and pushes the top element's constituent characters
;; onto the CHAR stack, in order. For instance, "hello" will result in the
;; top of the CHAR stack being \h \e \l \l \o
(def-instruction
  :char_allfromstring
  ^{:stacks #{:char :string}}
  (fn [state]
    (if (empty-stack? state :string)
      state
      (let [top-string (peek-stack state :string)
            popped-state (pop-stack state :string)]
        (push-to-stack-multiple popped-state :char (map char top-string))))))

;; =============================================================================
;; STRING Instructions
;; =============================================================================

(def-instruction
  :string_=
  ^{:stacks #{:boolean :string}}
  (fn [state]
    (make-instruction state = [:string :string] :boolean)))

(def-instruction
  :string_concat
  ^{:stacks #{:string}}
  (fn [state]
    (make-instruction state #(apply str (concat %1 %2)) [:string :string] :string)))

(def-instruction
  :string_drop
  ^{:stacks #{:integer :string}}
  (fn [state]
    (make-instruction state #(apply str (drop %1 %2)) [:integer :string] :string)))

(def-instruction
  :string_includes?
  ^{:stacks #{:boolean :string}}
  (fn [state]
    (make-instruction state clojure.string/includes? [:string :string] :boolean)))

(def-instruction
  :string_length
  ^{:stacks #{:integer :string}}
  (fn [state]
    (make-instruction state count [:string] :integer)))

(def-instruction
  :string_reverse
  ^{:stacks #{:string}}
  (fn [state]
    (make-instruction state #(apply str (reverse %)) [:string] :string)))

(def-instruction
  :string_take
  ^{:stacks #{:integer :string}}
  (fn [state]
    (make-instruction state #(apply str (take %1 %2)) [:integer :string] :string)))

;; =============================================================================
;; Polymorphic Instructions
;;
;; (for all stacks, with the exception of non-data ones like auxiliary, input,
;; and output)
;; =============================================================================

;; Duplicates the top item of the stack. Does not pop its argument (since that
;; would negate the effect of the duplication)
(def _dup
  ^{:stacks #{}}
  (fn [stack state]
    (let [top-item (peek-stack state stack)]
      (if (empty-stack? state stack)
        state
        (push-to-stack state stack top-item)))))

;; Duplicates n copies of the top item (i.e leaves n copies there). Does not pop
;; its argument (since that would negate the effect of the duplication). The
;; number n is determined by the top INTEGER. For n = 0, equivalent to POP.
;; For n = 1, equivalent to NOOP. For n = 2, equivalent to DUP. Negative values
;; of n are treated as 0
(def _duptimes
  ^{:stacks #{:integer}}
  (fn [stack state]
    (if (or (and (= stack :integer)
                 (<= 2 (count (:integer state))))
            (and (not= stack :integer)
                 (not (empty-stack? state :integer))
                 (not (empty-stack? state stack))))
      (let [n (peek-stack state :integer)
            popped-state (pop-stack state :integer)
            top-item (peek-stack popped-state stack)
            top-item-dup (take (- n 1) (repeat top-item))]
        (cond
          (< 0 n) (push-to-stack-multiple popped-state stack top-item-dup)
          :else (pop-stack popped-state stack)))
      state)))

;; Duplicates the top n items on the stack, one time each. The number n is
;; determined by the top INTEGER. If n <= 0, no items will be duplicated. If
;; fewer than n items are on the stack, the entire stack will be duplicated.
(def _dupitems
  ^{:stacks #{:integer}}
  (fn [stack state]
    (if (empty-stack? state :integer)
      state
      (let [n (peek-stack state :integer)
            popped-state (pop-stack state :integer)
            top-items (take n (get popped-state stack))]
        (push-to-stack-multiple popped-state stack top-items)))))

;; Pushes TRUE onto the BOOLEAN stack if the stack is empty. Otherwise FALSE
(def _empty
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (push-to-stack state :boolean (empty-stack? state stack))))

;; Pushes TRUE onto the BOOLEAN stack if the top two items are equal.
;; Otherwise FALSE
(def _eq
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state = [stack stack] :boolean)))

;; Empties the given stack
(def _flush
  ^{:stacks #{}}
  (fn [stack state]
    (assoc state stack '())))

;; Pops the given stack
(def _pop
  ^{:stacks #{}}
  (fn [stack state]
    (pop-stack state stack)))

;; Rotates the top three items on the stack (i.e. pulls the third item out and
;; pushes it on top). Equivalent to (yank state stack-type 2)
(def _rot
  ^{:stacks #{}}
  (fn [stack state]
    (if (<= 3 (count (get state stack)))
      (let [top-three (peek-stack-multiple state stack 3)
            popped-state (pop-stack-multiple state stack 3)
            top-three-rot (take 3 (conj top-three (last top-three)))]
        (push-to-stack-multiple popped-state stack top-three-rot))
      state)))

;; Inserts the top item deeper into the stack, using the top INTEGER to
;; determine how deep
(def _shove
  ^{:stacks #{:integer}}
  (fn [stack state]
    (if (or (and (= stack :integer)
                 (<= 2 (count (:integer state))))
            (and (not= stack :integer)
                 (not (empty-stack? state :integer))
                 (not (empty-stack? state stack))))
      (let [index-raw (peek-stack state :integer)
            popped-state (pop-stack state :integer)
            top-item (peek-stack popped-state stack)
            popped-state (pop-stack popped-state stack)
            index (max 0 (min index-raw (dec (count (get popped-state stack)))))]
        (update popped-state stack #(not-lazy (concat (take index %)
                                                            (list top-item)
                                                            (drop index %)))))
      state)))

;; Pushes the given stack's depth onto the INTEGER stack
(def _stackdepth
  ^{:stacks #{:integer}}
  (fn [stack state]
    (let [stack-depth (count (get state stack))]
      (push-to-stack state :integer stack-depth))))

;; Swaps the top two items on the stack
(def _swap
  ^{:stacks #{}}
  (fn [stack state]
    (if (<= 2 (count (get state stack)))
      (let [top-two (peek-stack-multiple state stack 2)
            popped-state (pop-stack-multiple state stack 2)]
        (push-to-stack-multiple popped-state stack (reverse top-two)))
      state)))

;; Pushes an indexed item from deep in the stack, removing it. The top INTEGER
;; is used to determine how deep to yank from
(def _yank
  ^{:stacks #{:integer}}
  (fn [stack state]
    (if (or (and (= stack :integer)
                 (<= 2 (count (:integer state))))
            (and (not= stack :integer)
                 (not (empty-stack? state :integer))
                 (not (empty-stack? state stack))))
      (let [index-raw (peek-stack state :integer)
            popped-state (pop-stack state :integer)
            index (max 0 (min index-raw (dec (count (get popped-state stack)))))
            indexed-item (nth (get popped-state stack) index)]
        (update popped-state stack #(not-lazy
                                     (concat (list indexed-item)
                                             (take index %)
                                             (rest (drop index %))))))
      state)))

;; Pushes a copy of an indexed item from deep in the stack, without removing it.
;; The top INTEGER is used to determine how deep to yankdup from
(def _yankdup
  ^{:stacks #{:integer}}
  (fn [stack state]
    (if (or (and (= stack :integer)
                 (<= 2 (count (:integer state))))
            (and (not= stack :integer)
                 (not (empty-stack? state :integer))
                 (not (empty-stack? state stack))))
      (let [index-raw (peek-stack state :integer)
            popped-state (pop-stack state :integer)
            index (max 0 (min index-raw (dec (count (get popped-state stack)))))
            indexed-item (nth (get popped-state stack) index)]
        (push-to-stack popped-state stack indexed-item))
      state)))

;; 11 types x 13 functions = 143 instructions
(generate-instructions
 [:boolean :char :code :exec :float :integer :string
  :vector_boolean :vector_float :vector_integer :vector_string]
 [_dup _duptimes _dupitems _empty _eq _flush _pop _rot _shove
  _stackdepth _swap _yank _yankdup])

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        instruction (first (:exec state))
        literal-type (get-literal-type instruction)] ; nil for non-literals
    (cond
      ;;
      ;; Recognize functional instruction or input instruction
      (keyword? instruction)
      (if-let [function (instruction @globals/instruction-table)]
        (function popped-state)
        (handle-input-instruction popped-state instruction))
      ;;
      ;; Recognize constant literal instruction
      literal-type
      (if (= :generic-vector literal-type)
        ;; Empty vector gets pushed on all vector stacks
        (reduce #(update-in % [%2] conj []) popped-state
                [:vector_boolean :vector_float :vector_integer :vector_string])
        (push-to-stack popped-state literal-type instruction))
      ;;
      ;; Recognize parenthesized group of instructions
      (seq? instruction)
      (update popped-state :exec #(concat %2 %1) instruction)
      ;;
      :else
      (throw (js/Error. (str "Unrecognized Push instruction in program: "
                              (name instruction)))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn plushy->push
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))]     ;; [open <n>] marks opens
    (loop [push ()                                          ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)                                   ;; maybe we're done?
        (if (some opener? push)                             ;; done with plushy, but unclosed open
          (recur push '(close))                             ;; recur with one more close
          push)                                             ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push)                         ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy)))                   ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy))))))))   ;; anything else

;;;;;;;;;
;; GP

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))

(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn uniform-addition
  "Returns plushy with new instructions possibly added before or after each
  existing instruction."
  [plushy instructions umad-rate]
  (apply concat
         (map #(if (< (rand) umad-rate)
                 (shuffle [% (rand-nth instructions)])
                 [%])
              plushy)))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy umad-rate]
  (remove (fn [_] (< (rand)
                     (/ 1 (+ 1 (/ 1 umad-rate)))))
          plushy))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob (:crossover (:variation argmap)))
       (crossover (:plushy (select-parent pop argmap))
                  (:plushy (select-parent pop argmap)))
       (< prob (+ (:crossover (:variation argmap))
                  (:umad (:variation argmap)) 2))
       (uniform-deletion (uniform-addition (:plushy (select-parent pop argmap))
                                           (:instructions argmap)
                                           (:umad-rate argmap))
                         (:umad-rate argmap))
       :else (:plushy (select-parent pop argmap))))})

(defn report
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (str "Generation:   " generation ", "
         "Best total error:" (:total-error best) ", "
         ;"Best errors:" (:errors best) ", "
         "Best plushy: " (:plushy best) ", "
         ;"Best program: " (plushy->push (:plushy best)) ", "
         ;"Best behaviors:" (:behaviors best) ", "
         "Genotypic diversity:"
         (float (/ (count (distinct (map :plushy pop))) (count pop))) ", "
         "Average genome length:"
         (float (/ (reduce + (map count (map :plushy pop))) (count pop))))))

;; (defn gp
;;   "Main GP loop."
;;   [{:keys [population-size max-generations error-function instructions
;;            max-initial-plushy-size]
;;     :as   argmap}]

;;   (println "Starting GP with args: " argmap)
;;   (println "Registered instructions:")
;;   (println (sort (keys @globals/instruction-table)))

;;   (loop [generation 0
;;          population (repeatedly
;;                      population-size
;;                      #(hash-map :plushy
;;                                 (make-random-plushy instructions
;;                                                     max-initial-plushy-size)))]
;;     (let [evaluated-pop (sort-by :total-error
;;                                  (map (partial error-function argmap)
;;                                       population))]
;;       (report evaluated-pop generation)
;;       (cond
;;         (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
;;         (>= generation max-generations) nil
;;         :else (recur (inc generation)
;;                      (if (:elitism argmap)
;;                        (conj (repeatedly (dec population-size)
;;                                          #(new-individual evaluated-pop argmap))
;;                              (first evaluated-pop))
;;                        (repeatedly population-size
;;                                    #(new-individual evaluated-pop argmap))))))))

;; =============================================================================
;; String classification
;; =============================================================================

(defn string-classification-error-function
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the BOOLEAN stack."
  [argmap individual]
  (let [program (plushy->push (:plushy individual))
        inputs ["GCG" "GACAG" "AGAAG" "CCCA" "GATTACA" "TAGG" "GACT"]
        correct-outputs [false false false false true true true]
        outputs (map (fn [input]
                       (peek-stack
                        (interpret-program
                         program
                         (assoc empty-state :input {:in1 input})
                         (:step-limit argmap))
                        :boolean))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (if (= correct-output output)
                          0
                          1)))
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply + errors))))

;; =============================================================================
;; Regression
;; =============================================================================

(defn- target-function-hard
  "Target function: f(x) = 7x^2 - 20x + 13"
  [x]
  (+ (* 7 x x) (* -20 x) 13))

(defn- target-function
  "Target function: f(x) = x^3 + x + 3"
  [x]
  (+ (* x x x) x 3))

(defn regression-error-function
  "Finds the behaviors and errors of an individual. The error is the absolute
  deviation between the target output value and the program's selected behavior,
  or 1000000 if no behavior is produced. The behavior is here defined as the
  final top item on the INTEGER stack."
  [argmap individual]
  (let [program (plushy->push (:plushy individual))
        inputs (range -10 11)
        correct-outputs (map target-function inputs)
        outputs (map (fn [input]
                       (peek-stack
                        (interpret-program
                         program
                         (assoc empty-state :input {:in1 input})
                         (:step-limit argmap))
                        :integer))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (or (empty? program) (= output :no-stack-item))
                        1000000
                        (abs (- correct-output output))))
                    correct-outputs
                    outputs)]

    ;; (println (str "\n\t----------------------------------
    ;;                \n\tprogram: " program
    ;;               "\n\tinputs: " inputs
    ;;               "\n\tcorrect-outputs" correct-outputs
    ;;               "\n\toutputs: " outputs
    ;;               "\n\terrors: " errors))
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply + errors))))

;; (defn -main
;;   "Runs propel-gp, giving it a map of arguments."
;;   [& args]
;;   (gp (update-in
;;        (merge
;;         {:instructions            default-instructions
;;          :error-function          regression-error-function
;;          :max-generations         500
;;          :population-size         500
;;          :max-initial-plushy-size 50
;;          :step-limit              100
;;          :parent-selection        :tournament-selection
;;          :tournament-size         5
;;          :umad-rate               0.1
;;          :variation               {:umad 0.5 :crossover 0.5}
;;          :elitism                 false}
;;         (apply hash-map
;;                (map js/Number args)))
;;        [:error-function]
;;        #(if (fn? %) % (eval %)))))