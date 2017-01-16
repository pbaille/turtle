(ns turtle.two)

(enable-console-print!)

(def extent (juxt #(apply min %) #(apply max %)))

(defn middle [width [left right]]
  (- (/ (- width (- right left)) 2) left))

(def ctype first)

(defn ctype=
  ([t] #(ctype= % t))
  ([c t] (= (first c) t)))

(defn parse-turtle-cmds [cmds]
  (loop [cmds cmds
         [x y :as loc] [0 0]
         dir 180
         pts [[:goto 0 0]]]
    (if (seq cmds)
      (let [[cmd & params] (first cmds)
            remaining (rest cmds)]
        (condp = cmd
          :forward (let [dist (first params)
                         radians (* dir js/Math.PI (/ 180))
                         loc' [(+ x (* dist (js/Math.sin radians)))
                               (+ y (* dist (js/Math.cos radians)))]]
                     (recur remaining loc' dir (conj pts (into [:goto] loc'))))
          :turn (recur remaining loc (- dir (first params)) pts)
          (recur remaining loc dir (conj pts (first cmds)))))
      pts)))

(defn clear! [canvas-id]
  (let [canvas (js/document.getElementById canvas-id)
        ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (.clearRect ctx 0 0 w h)))

(defn draw [{:keys [canvas-id
                    width
                    height
                    auto-scale
                    clear
                    cmds]}]
  (let [canvas (js/document.getElementById canvas-id)
        ctx (.getContext canvas "2d")
        cmds (parse-turtle-cmds cmds)
        [left right] (extent (map second (filter (ctype= :goto) cmds)))
        [bottom top] (extent (map #(nth % 2) (filter (ctype= :goto) cmds)))

        drawing-width (- right left)
        drawing-height (- top bottom)
        scale-x (/ width drawing-width)
        scale-y (/ height drawing-height)
        scale-factor (if auto-scale (min scale-x scale-y) 1)

        _ (println
            "\n drawing-width" drawing-width
            "\n drawing-height" drawing-height
            "\n scale-x" scale-x
            "\n scale-y" scale-y
            "\n scale-factor" scale-factor
            )

        cmds (map (fn [[t a1 a2 :as cmd]]
                    (if (= t :goto)
                      [:goto (* scale-factor a1) (* scale-factor a2)]
                      cmd))
                  cmds)

        sx (middle width [left right])
        sy (middle height [bottom top])

        loc (atom nil)]

    (when clear (clear! canvas-id))


    (set! (.-width canvas) width)
    (set! (.-height canvas) height)

    (set! (.-lineWidth ctx) 0.5)
    (set! (.-strokeStyle ctx) "rgba(0,0,0,.4)")
    (set! (.-fillStyle ctx) "rgba(0,0,30,.05)")

    (.beginPath ctx)
    (let [[x y] (first (filter #(= :goto (first %)) cmds))]
      (reset! loc [(+ sx x) (+ sy y)]))
    (doseq [[ct a1 a2 & as] cmds]
      (condp = ct
        :goto (let [[x y] @loc
                    [x* y*] [(+ sx a1) (+ sy a2)]]
                (.lineTo ctx x* y*)
                (.fill ctx)
                (reset! loc [x* y*]))
        :do (a1 ctx @loc)))))

(defn color [c]
  [:do #(set! (.-strokeStyle %) c)])

(def break-path
  [:do #(do (.stroke %) (.beginPath %))])

(defn fill [c]
  [:do #(set! (.-fillStyle %) c)])

(defn maybe
  ([e]
   (when (rand-nth [true false])
     e))
  ([p e]
   (let [r (rand)]
     (when (> p r)
       e))))

(defn circle [r]
  [:do
   (fn [ctx [x y]]
     (.beginPath ctx)
     (.arc ctx x y (* r (rand)) 0 (* 2 js/Math.PI) false))])

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
        :d (concat (maybe 0.8 [break-path]) [(color "rgba(0,0,0,.2)")] a2 b forward a1 a forward a a1 forward b a2)
        :b (concat [(fill "rgba(255,200,255,.01)")] a1 c forward a3 d forward (maybe 0.3 [(circle 20)]) d a3 forward c a1)
        :c (concat [(fill "rgba(0,0,30,.01)")] a3 a forward a4 c forward c a4 forward a a3)
        :a (concat (maybe 0.8 [break-path]) (maybe [(color "rgba(0,0,0,.4)")]) a4 d forward a2 b forward b a2 forward d a4)))
    []))

(defn go []
  (draw
    {:canvas-id "turtle-canvas"
     :width 1000
     :height 1500
     :clear true
     :cmds
     (pierand
       {:step-fn #(rand-nth (range 4 13 4))
        :angle-fn #(rand-nth (range 0 360 30))
        :depth 6})}))

(set! (.-onclick js/document.body)
      go)

#_(go)
