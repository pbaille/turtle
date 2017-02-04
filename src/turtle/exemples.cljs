(ns turtle.exemples
  (:require [turtle.core :as t :refer [t>]]))

(defn cyclic-program [cycle* & [cycles-count]]
  (let [p (cycle cycle*)]
    (into [:>]
          (if cycles-count
            (take (* cycles-count (count cycle*)) p)
            p))))

(comment
  (t/draw!
    (t/drawing-turtle
      {:angle (/ 360 5)
       :step 10
       :program (cyclic-program
                  [[:turn-right]
                   [:F]
                   [:swap #(update % :step (partial + 4))]]
                  100)})))

(comment
  (t/draw!
    (t> (-> (t/drawing-turtle {:angle 60 :step 10})
            (update :tasks into t/ls-tasks)
            (assoc-in [:ls :rules]
                      #(let [angle (rand-nth (range 0 360 10))
                             step (+ 10 (rand-int 10))
                             t1 [:turn (* 1 angle)]
                             t2 [:turn (* 2 angle)]
                             t3 [:turn (* 3 angle)]
                             t4 [:turn (* 4 angle)]
                             F [:F step]
                             f [:f step]]
                         {:a [:> t1 [:d] F t4 [:b] F [:b] t4 F [:d] t1]
                          :b [:> t3 [:c] F t2 [:d] F [:d] t2 F [:c] t3]
                          :c [:> t2 [:a] F t1 [:c] F [:c] t1 F [:a] t2]
                          :d [:> t4 [:b] F t3 [:a] F [:a] t3 F [:b] t4]}))
            (assoc :program [:a]))
        [:ls/next 5]
        [:swap (fn [t] (update t :program t/cmd-rmap #(if (#{[:a] [:b] [:c] [:d]} %) [:self] %)))]
        [:sym 3])))

(defn mirror
  "takes [1 2 3] and returns [1 2 3 2 1]"
  [rule]
  (vec (concat rule (reverse (next rule)))))


(def rules-gen-defaults
  {:max-rule-len 8
   :min-rule-len 4
   :max-rules-count 6
   :min-rules-count 3
   :angles-count 4
   :steps-count 4
   :angle-gen #(rand-int 360)
   :step-gen #(rand-nth [10 5 15])})

(defn make-rule [steps angles ]
  ())

(defn rules-gen [{:keys [max-rule-len
                         min-rule-len
                         max-rules-count
                         min-rules-count
                         angles-count
                         steps-count
                         angle-gen
                         step-gen]}]
  )

(js/console.log [])