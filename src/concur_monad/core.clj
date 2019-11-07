;;           ____
;;         ,'  , `.
;;      ,-+-,.' _ |                                       ,---,
;;   ,-+-. ;   , ||   ,---.        ,---,                ,---.'|
;;  ,--.'|'   |  ;|  '   ,'\   ,-+-. /  |               |   | :  .--.--.
;; |   |  ,', |  ': /   /   | ,--.'|'   |  ,--.--.      |   | | /  /    '
;; |   | /  | |  ||.   ; ,. :|   |  ,"' | /       \   ,--.__| ||  :  /`./
;; '   | :  | :  |,'   | |: :|   | /  | |.--.  .-. | /   ,'   ||  :  ;_
;; ;   . |  ; |--' '   | .; :|   | |  | | \__\/: . ..   '  /  | \  \    `.
;; |   : |  | ,    |   :    ||   | |  |/  ," .--.; |'   ; |:  |  `----.   \
;; |   : '  |/      \   \  / |   | |--'  /  /  ,.  ||   | '/  ' /  /`--'  /
;; ;   | |`-'        `----'  |   |/     ;  :   .'   \   :    :|'--'.     /
;; |   ;/                    '---'      |  ,     .-./\   \  /    `--'---'
;; '---'                                 `--`---'     `----'
;;         .-.           .-.
;;        /  |          /  |         What are monads?
;;        |  /          |  /         Why do we care as Clojurists?
;;     .'\|.-; _     .'\|.-; _
;;    /.-.;\  |\|   /.-.;\  |\|
;;    '   |'._/ `   '   |'._/ `
;;        |  \          |  \
;;        \  |          \  |
;;         '-'           '-'
;; Disclaimer: I am not a cat person, so if asked cat questions, I will just say:
;;             Monads are just monoids in the category of endofunctors



(ns concur-monad.core)

;; How do we assign values to names without defining stuff in the global space?
((fn [a]
   ((fn [b]
      (+ a b))
    2))
 1)

;; That's ugly! How to make it better?
(defn m-bind [value function]
  (function value))

(defn m-result [value]
  value)

(m-bind 1 (fn [a]
            (m-bind 2 (fn [b]
                        (m-bind (+ a b) (fn [c]
                                          (m-result c)))))))

;; What is a monad?
(def identity-monad
  {:m-result m-result
   :m-bind m-bind})


;; Still not good looking! Can we make it even better?
(defmacro run-monad [monad [var val & rest :as steps] end-expr]
  (if steps
     `((:m-bind ~monad) ~val (fn [~var] (run-monad ~monad ~rest ~(seq end-expr))))
     `((:m-result ~monad) ~end-expr)))


(run-monad identity-monad
  [a 1
   b 2]
  (+ a b))

;; Looks familiar?
(let [a 1
      b 2]
  (+ a b))

;; https://github.com/clojure/algo.monads
(require '[clojure.algo.monads :as m])

(m/domonad m/identity-m
  [a 1
   b 2]
  (+ a b))

;; Maybe not?
(let [result (do (println "superdoper computation failed and returns the billion dollar mistake")
                 nil)
      another-result (do (println "expensive computation that take ages.")
                      (Thread/sleep 5000)
                      result)]
  (+ another-result 42))

;; Maybe yes.
(def maybe-monad
  {:m-result identity
   :m-bind (fn [v f]
             (when-not (nil? v) (f v)))})

(run-monad maybe-monad
  [result (do (println "superdoper computation failed and returns the billion dollar mistake")
              nil)
   another-result (do (println "expensive computation that take ages.")
                      (Thread/sleep 5000)
                      result)]
  (+ another-result 42))

;; algo.m library version
(m/domonad m/maybe-m
  [result (do (println "superdoper computation failed and returns the billion dollar mistake")
              nil)
   another-result (do (println "expensive computation that take ages.")
                      (Thread/sleep 5000)
                      result)]
  (+ another-result 42))

;; nonlinearity
(def sequence-monad
  {:m-result #(list %)
   :m-bind (fn [v f]
             (flatten (map f v)))})

(run-monad sequence-monad
  [a (range 5)
   b (range a)]
  (+ a b))

;; algo.m
(m/domonad m/sequence-m
  [a (range 5)
   b (list (inc a))]
  (+ a b))

;; feeling familiar again? you should be.
(for [a (range 5)
      b (range a)]
  (+ a b))

;; Why do we care as Clojurists?
;; Because Clojure is already full of it.


;; Many more:
;; Error monad: http://funcool.github.io/cats/latest/#exception
;; Monad transformers: https://github.com/khinsen/monads-in-clojure/blob/master/PART4.md
