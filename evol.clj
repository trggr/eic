(ns evol
  (:use clojure.test))

(def *width* 100)
(def *height* 40)
(def *jungle* '(45 10 10 20))
(def *plant-energy* 80)
(def *reproduction-energy* 200)
(def *plants* #{})
(def *animals* ())

(defstruct animal :x :y :energy :dir :genes)

(defn random
  "Generates a random number"
  [range]
  (rand-int range))

(defn random-plant
  "Creates a random plant within a given rectangle"
  [left top width height]
  (list (+ left (random width))
	(+ top (random height))))

(defn add-plants
  "Adds two random plants, one of them in the jungle"
   [plants]
   (let [p (random-plant 0 0 *width* *height*)
	 q (apply random-plant *jungle*)]
     (conj (conj plants p) q)))

(defn between?
  "Returns true, if x is between min and max"
  [x min max]
  (and (>= x min) (<= x max)))

(defn modify-nth
  "Replaces nth element of a coll with a val"
  [coll nth val]
  (loop [n 0 acc '() e coll]
    (if (= e '())
      (reverse acc)
      (recur (+ n 1)
	(conj acc
	  (if (= n nth) val (first e)))
	(rest e)))))

(defn move
  "Moves animal"
  [anim]
  (let [dir (:dir anim) x (:x anim) y (:y anim)]
    (assoc anim
      :x (mod (+ x (cond
		    (between? dir 2 4) 1
		    (or (= dir 1) (= dir 5)) 0
		    :else -1)) *width*)
      :y (mod (+ y (cond (between? dir 0 2) -1
			 (between? dir 4 6) 1
			 :else 0)) *height*)
      :energy (dec (:energy anim)))))

(defn- angle
  [genes x]
  (let [xnu (- x (first genes))]
    (if (< xnu 0)
      0
      (+ 1 (angle (rest genes) xnu)))))

(defn turn
  "Calculates next direction of an animal"
  [anim]
  (let [sum (random (reduce + (:genes anim)))]
    (assoc anim :dir (mod (+ (:dir anim) (angle (:genes anim) sum)) 8))))

(defn eat
  "An animal eats a plant at his location. Returns energized animal (first)
   and updated plants (second)"
  [anim plants]
  (let [pos (list (:x anim) (:y anim))]
    (if (contains? plants pos)
      (list (assoc anim :energy (+ (:energy anim) *plant-energy*))
	    (disj plants pos))
      (list anim plants))))

(defn mutate
  "Slightly mutates a random gene"
  [genes]
  (let [mutation (random 8)]
    (modify-nth genes mutation
		(max 1 (+ (nth genes mutation) (random 3) -1)))))

(defn reproduce
  "Animal tries to reproduce. Returns a list of new animals, including self"
  [anim]
  (let [e (:energy anim)]
    (if (>= e *reproduction-energy*)
      (let [nue (bit-shift-right e 1)]
	(list (assoc anim :energy nue)
	      (struct animal (:x anim) (:y anim) nue
		      (:dir anim) (mutate (:genes anim)))))
      (list anim))))

(defn meal
  "All animals have a meal"
  [animals plants]
  (loop [acc nil
	 elt animals
	 p plants]
    (if (= '() elt)
      (list acc p)
      (let [x (eat (first elt) p)]
	(recur (cons (first x) acc)
	       (rest elt)
	       (second x))))))

(defn turn-world
  "Simulates one day in a world. Returns updated
   animals (first), and updated plants (second)"
  ([world]
     (turn-world (first world) (second world)))
  ([animals plants]
     (let [alive-animals     (filter #(> (:energy %) 0) animals)
	   travelled-animals (map #(move (turn %)) alive-animals)
	   updated-world (meal travelled-animals plants)]
       (list (flatten (map reproduce (first updated-world)))
	     (add-plants (second updated-world))))))

(defn to-char
  "Prints condensed representation of an animal"
  [anim]
  (str (:x anim) "-" (:y anim) "-" (:energy anim) "-"
       (:dir anim) "-" (reduce str (:genes anim))))

(defn stats
  "Prints statistics about animals"
  [animals plants]
  (print (format "\n %s animals, %s plants\n"
		 (count animals)
		 (count plants)))
  (println "Five strongest animals:")
  (println (map to-char (take 5 (sort-by #(* -1 (:energy %)) animals))))
  (print "Groups in 20 energy increments: ")
  (println (->> animals
		(map #(int (/ (:energy %) 20)))
		sort
		(partition-by identity)
		(map (juxt first count))
		(map second))))

(defn draw-world
  "Draws the world"
  [animals plants]
  (dotimes [y *height*]
    (print (format "\n|"))
    (dotimes [x *width*]
      (print
       (cond (some #(and (= (:x %) x) (= (:y %) y)) animals) 'M
	     (contains? plants (list x y)) '*
	     :else '.))))
  (stats animals plants))

(defn step
  "Simulates life in a world"
  ([] (step 1))
  ([times]
     (dotimes [i times]
       (let [w1 (turn-world *animals* *plants*)]
	 (def *animals* (first w1))
	 (def *plants* (second w1))))
     (draw-world *animals* *plants*)))

;;----- Tests ------
;; (load-file "/tim/clojure/eic/evol.clj")

(def *plants* (-> *plants* add-plants add-plants add-plants))

(def *animals*
     (list (struct animal
		   (bit-shift-right *width* 1)
		   (bit-shift-right *height* 1)
		   1000
		   0
		   (map (fn [x] (random 10)) (take 10 (iterate inc 0))))))

(deftest test-modify-nth
  (let [a8 '(1 2 3 4 5 6 7 8)]
    (is (= '(1 2 30 4 5 6 7 8) (modify-nth a8 2 30)))
    (is (= a8 (modify-nth a8 8 30)))
    (is (= '() (modify-nth '() 0 10)))))

(deftest test-eat
  (let [a (struct animal 10 20 30 0 '(1 2 3 4 5 6 7 8))
	p '#{(10 20)}
	p1 '#{(10 20) (20 30)}
	p2 '#{}]
    (is (= (first (eat a p)) (struct animal 10 20 110 0 '(1 2 3 4 5 6 7 8))))
    (is (= (second (eat a p)) '#{}))
    (is (= (first (eat a p1)) (struct animal 10 20 110 0 '(1 2 3 4 5 6 7 8))))
    (is (= (second (eat a p1)) '#{(20 30)}))
    (is (= (first (eat a p2)) a))
    (is (= (second (eat a p2)) p2))))

(deftest test-meal
  (let [a (struct animal 10 20 30 0 '(1 2 3 4 5 6 7 8))
	b (struct animal 20 30 30 0 '(1 2 3 4 5 6 7 8))
	anim (list a b)
	plants '#{(10 20) (20 30)}]
    (is (= (first (meal anim plants))
	   (list (struct animal 20 30 110 0 '(1 2 3 4 5 6 7 8))
		 (struct animal 10 20 110 0 '(1 2 3 4 5 6 7 8)))))
    (is (= (second (meal anim plants)) #{}))))

(run-tests 'evol)
