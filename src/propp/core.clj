(ns propp.core
  (:use clojure.walk))

(defn wrand
  "Given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (let [slice (nth slices i)]
       (if (< r (+ slice sum))
         i
         (recur (inc i) (+ slice sum)))))))

(defn pick-wrand
  "Given a sequence of pairs of slice sizes and options, it returns
  one of the options based on the slice picked by wrand."
  [options]
  (let [options (partition 2 options)
        index (wrand (map first options))]
    (second (nth options index))))

(defn pick-rand
  "Given a sequence of options, returns one in random."
  [options]
  (rand-nth options))

(defmulti cook-coll first)
(defmethod cook-coll :one-of
  [coll]
  (pick-rand (rest coll)))
(defmethod cook-coll :roulette
  [coll]
  (pick-wrand (rest coll)))
(defmethod cook-coll :scene
  [coll]
  (cook-coll (drop 2 coll)))
(defmethod cook-coll :default
  [coll]
  (when (not (keyword? (first coll))) ;;postwalk visits map pairs as vectors
    (apply str (interpose " " coll))))

(defn cook-recipe
  [recipe]
  (postwalk (fn [x]
              (if (vector? x)
                (cook-coll x) x)) recipe))

#_(def recipe
    [[:scene {:id :opening-scene}
      "Once upon a time, there was a"
      [:one-of "prince" "inn-keeper" "thief"]
      [:roulette
       90 "who was trying to make ends meet." ;;most likely
       10 "who was plotting to kill his adversary."]]
     [:scene {:id :hero-description}
      "Our hero was brave and smart, but"
      [:one-of
       "not very tall."
       "not good at sports"]]])
#_(take 10 (repeatedly #(cook-recipe recipe)))
