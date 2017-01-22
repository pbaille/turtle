(ns turtle.one
  (:require [cljs.pprint :refer [pprint]]))

(def extent (juxt #(apply min %) #(apply max %)))

(defn middle [width [left right]]
  (- (/ (- width (- right left)) 2) left))

(defn coords [cmds]
  (loop [cmds cmds
         [x y :as loc] [0 0]
         dir 180
         pts [[0 0]]]
    (if (seq cmds)
      (let [[cmd & params] (first cmds)
            remaining (rest cmds)]
        (condp = cmd
          :forward (let [dist (first params)
                         radians (* dir js/Math.PI (/ 180))
                         loc' [(+ x (* dist (js/Math.sin radians)))
                               (+ y (* dist (js/Math.cos radians)))]]
                     (recur remaining loc' dir (conj pts loc')))
          :turn (recur remaining loc (- dir (first params)) pts)))
      pts)))

(defn clear! [canvas-id]
  (let [canvas (js/document.getElementById canvas-id)
        ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (.clearRect ctx 0 0 w h)))

(defn draw [cmds & [opts]]
  (let [canvas (js/document.getElementById "turtle-canvas")
        ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)
        pts (coords cmds)
        [left right] (extent (map first pts))
        [bottom top] (extent (map second pts))
        sx (middle w [left right])
        sy (middle h [bottom top])]

    (set! (.-globalCompositeOperation ctx) "lighter")
    (set! (.-globalAlpha ctx) 0.5)

    (set! (.-strokeStyle ctx) (:stroke opts "rgba(0,0,0,.4)"))
    (set! (.-fillStyle ctx) (:stroke opts "rgba(0,0,0,.1)"))
    (set! (.-lineWidth ctx) (:line-width opts 1))
    (.beginPath ctx)
    (let [[x y] (first pts)]
      (.moveTo ctx (+ sx x) (+ sy y)))
    (doseq [[x y] (rest pts)]
      (.lineTo ctx (+ sx x) (+ sy y)))
    (.stroke ctx)))

;; sketch ----------------------------------------------------------

(defn rand-between [[x y]]
  (+ x (* (- y x) (rand))))

(defn hilberand [step rule turn-angle depth]
  (if (pos? depth)
    (let [forward [[:forward (rand-between step)]]
          angle (rand-between turn-angle)
          left [[:turn (- 360 angle)]]
          right [[:turn angle]]
          a (hilberand step :a turn-angle (dec depth))
          b (hilberand step :b turn-angle (dec depth))]
      (condp = rule
        :a (concat left b forward right a forward a right forward b left)
        :b (concat right a forward left b forward b left forward a right)))
    []))

(defn hilbrands [specs]
  (mapv (partial apply hilberand) specs))

(defn draw-hilbrands! [n]
  (clear! "turtle-canvas")
  (mapv #(draw % {:line-width 0.5})
        (hilbrands (repeat n [[1 5] :a [0 180] 7]))))

;; pierand --------------------------------------------------------

;; 2d
(defn pierand* [{:keys [step-fn rule angle-fn depth] :as opts}]
  (if (pos? depth)
    (let [forward [[:forward (step-fn)]]
          angle (angle-fn)
          a0 [[:turn angle]]
          a1 [[:turn (- 270 angle)]]
          new-opts (update opts :depth dec)
          a (pierand* (assoc new-opts :rule :b))
          b (pierand* (assoc new-opts :rule :a))]
      (condp = (or rule :a)
        :a (concat a0 b forward a1 a forward a a1 forward b a0)
        :b (concat a1 a forward a0 b forward b a0 forward a a1)))
    []))

(comment
  (do (clear! "turtle-canvas")
      (draw
        (pierand*
          {:step-fn #(rand-nth (range 5 20 5))
           :angle-fn #(rand-nth (range 0 360 60))
           :depth 6}))))

;; sym 3

(defn pierand** [{:keys [step-fn rule angle-fn depth] :as opts}]
  (if (pos? depth)
    (let [forward [[:forward (step-fn)]]
          angle (angle-fn)
          a0 [[:turn angle]]
          a1 [[:turn (- 270 angle)]]
          new-opts (update opts :depth dec)
          a (pierand* (assoc new-opts :rule :b))
          b (pierand* (assoc new-opts :rule :a))]
      (condp = (or rule :a)
        :a (concat a0 b forward a1 a forward
                   [[:turn 120]] a0 b forward a1 a forward
                   [[:turn 240]] a0 b forward a1 a forward)
        :b (concat a1 a forward a0 b forward
                   [[:turn 120]] a1 a forward a0 b forward
                   [[:turn 240]] a1 a forward a0 b forward)))
    []))

(comment
  (do (clear! "turtle-canvas")
      (draw
        (pierand**
          {:step-fn #(rand-nth (range 5 30 10))
           :angle-fn #(rand-nth (range 0 360 10))
           :depth 6}))))

;; syms 4 ?? no!
(defn pierand [{:keys [step-fn rule angle-fn depth] :as opts}]
  (if (pos? depth)
    (let [forward [[:forward (step-fn)]]
          angle (angle-fn)
          a1 [[:turn (- angle)]]
          a2 [[:turn (- 90 angle)]]
          a3 [[:turn (- 180 angle)]]
          a4 [[:turn (- 270 angle)]]
          new-opts (update opts :depth dec)
          a (pierand (assoc new-opts :rule :b))
          b (pierand (assoc new-opts :rule :c))
          c (pierand (assoc new-opts :rule :d))
          d (pierand (assoc new-opts :rule :a))]
      (condp = (or rule :a)
          :d (concat a2 b forward a1 a forward a a1 forward b a2)
          :b (concat a1 c forward a3 d forward d a3 forward c a1)
          :c (concat a3 a forward a4 c forward c a4 forward a a3)
          :a (concat a4 d forward a2 b forward b a2 forward d a4))
      #_(condp = (or rule :a)
          :a (concat a2 b forward a1 a forward a a1 forward b a2)
          :b (concat a1 c forward a3 b forward b a3 forward c a1)
          :c (concat a3 d forward a4 c forward c a4 forward d a3)
          :d (concat a4 a forward a2 d forward d a2 forward a a4))
      #_(condp = (or rule :a)
          :a (concat a1 b forward a2 a forward a a2 forward b a1)
          :b (concat a2 c forward a3 b forward b a3 forward c a2)
          :c (concat a3 d forward a4 c forward c a4 forward d a3)
          :d (concat a4 a forward a1 d forward d a1 forward a a4))
      (condp = (or rule :a)
        :a (concat a2 b forward a1 a forward a a2 c a2 a forward a a1 forward b a2)
        :b (concat a1 c forward a3 b forward a a1 c a1 a forward b a3 forward c a1)
        :c (concat a3 d forward a4 c forward c a4 forward d a3)
        :d c))
    []))

(comment
  (do (clear! "turtle-canvas")
      (draw
        (pierand
          {:step-fn (constantly 10) #_(rand-nth (range 0 15 2))
           :angle-fn #_(* 360 (rand)) #(rand-nth (range 0 360 120))
           :depth 6})))
  (do (clear! "turtle-canvas")
      (draw
        (pierand
          {:step-fn (constantly 10)
           :angle-fn (constantly 80)
           :depth 8}))))

;; pierand2 --------------------------------------------------------

(defn fract [{:keys [step-fn
                     rule
                     angle-fn
                     depth
                     nsym
                     rules-map
                     rules-shift]
              :as opts}]
  (if (pos? depth)
    (let [step (step-fn)
          angle (angle-fn)
          turns (into {}
                      (map-indexed
                        #(vector %1 [[:turn (- %2 angle)]])
                        (range 0 360 (/ 360 nsym))))
          new-opts (update opts :depth dec)
          recs (into {}
                     (map (fn [[from to]]
                            [from
                             (fract (assoc new-opts :rule to))])
                          rules-shift))
          cmds (merge
                 recs
                 turns
                 {:fw [[:forward step]]})]
      (mapcat #(cmds %) (rules-map rule)))
    []))

(comment

  (do (clear! "turtle-canvas")

      (draw
        (fract
          {:step-fn #(rand-nth (range 10))
           :angle-fn #(rand-nth (range 0 360 60))
           :depth 7
           :nsym 4
           :rule :a
           :rules-shift {:a :b
                         :b :c
                         :c :d
                         :d :a}
           :rules-map {:a [0 :b :fw 1 :a :fw :a 1 :fw :b 0]
                       :b [1 :c :fw 2 :d :fw :d 2 :fw :c 1]
                       :c [2 :d :fw 3 :c :fw :c 3 :fw :d 2]
                       :d [3 :a :fw 0 :d :fw :d 0 :fw :a 3]}})
        ))
  )

(defn autorules [{:keys [nrules nsym rule-max-len] :as opts}]
  (let [rules (set (repeatedly nrules gensym))
        rand-cmd (fn []
                   (first
                     (shuffle
                       (concat
                         rules
                         (range nsym)
                         (repeat (/ (+ nsym (count rules)) 3) :fw)))))
        make-rule (fn []
                    (loop [ret []]
                      (if (> (count ret) (/ (dec rule-max-len) 2))
                        (concat ret
                                (rand-nth [nil [(rand-cmd)]])
                                (reverse ret))
                        (let [cm {:rules (count (filter rules ret))
                                  :angles (count (filter number? ret))
                                  :fws (count (filter (partial = :fw) ret))}]
                          (recur (conj ret
                                       (condp = (ffirst (sort-by second < (shuffle cm)))
                                         :rules (first (shuffle rules))
                                         :angles (rand-nth (range nsym))
                                         :fws :fw)))))))]
    {:rules-shift
     (into {} (map (fn [r] [r (rand-nth (seq (disj rules r)))])
                   rules))
     :rules-map
     (into {}
           (mapv (fn [x] [x (make-rule)])
                 rules))
     :rule (first (shuffle rules))}))

(defn autofract [opts]
  (fract (into opts (autorules opts))))

(comment

  (pprint (autorules
            {:step-fn #(rand-nth [5])
             :angle-fn #(rand-nth (range 0 360 40))
             :depth 6
             :nsym 3
             :nrules 8
             :rule-max-len 6}

            ))

  (do (clear! "turtle-canvas")

      (draw
        (autofract
          {:step-fn #(rand-nth [5 10 15])
           :angle-fn #(rand-nth (range 0 360 85))
           :depth 5
           :nsym 10
           :nrules 10
           :rule-max-len 10})
        )))

;; tic ------------------------------------------------------------

(def interval (atom nil))

(defn stop! []
  (swap! interval #(js/clearInterval %)))

(defn start! [f ms]
  (stop!)
  (f)
  (reset! interval
          (js/setInterval f ms)))