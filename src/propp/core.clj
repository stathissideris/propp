(ns propp.core)

(defn wrand
  "Given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (nth slices i) sum))
        i
        (recur (inc i) (+ (nth slices i) sum))))))

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
