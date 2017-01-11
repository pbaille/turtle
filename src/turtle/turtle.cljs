(ns turtle.turtle)

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

(defn draw [canvas-id cmds]
  (let [canvas (js/document.getElementById canvas-id)
        ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)
        pts (coords cmds)
        [left right] (extent (map first pts))
        [bottom top] (extent (map second pts))
        sx (middle w [left right])
        sy (middle h [bottom top])]
    (.clearRect ctx 0 0 w h)

    (set! (.-fillStyle ctx) "lightgray")
    (.beginPath ctx)
    (let [[x y] (first pts)]
      (.ellipse ctx (+ sx x) (+ sy y) 5 5 0 0 (* 2 js/Math.PI)))
    (.fill ctx)

    (set! (.-strokeStyle ctx) "black")
    (set! (.-lineWidth ctx) 2)
    (.beginPath ctx)
    (let [[x y] (first pts)]
      (.moveTo ctx (+ sx x) (+ sy y)))
    (doseq [[x y] (rest pts)]
      (.lineTo ctx (+ sx x) (+ sy y)))
    (.stroke ctx)))

(defn hilbert2 [step rule turn-angle depth]
  (if (pos? depth)
    (let [forward [[:forward step]]
          left [[:turn (- 360 turn-angle)]]
          right [[:turn turn-angle]]
          a (hilbert2 step :a turn-angle (dec depth))
          b (hilbert2 step :b turn-angle (dec depth))]
      (condp = rule
        :a (concat left b forward right a forward a right forward b left)
        :b (concat right a forward left b forward b left forward a right)))
    []))

(draw "turtle-canvas"
      (hilbert2 10 :a 85 4))
