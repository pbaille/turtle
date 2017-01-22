(ns turtle.three)

(enable-console-print!)

(def extent (juxt #(apply min %) #(apply max %)))

(defn middle [width [left right]]
  (- (/ (- width (- right left)) 2) left))

(def ctype first)

(defn log [& xs] (doseq [x xs]
                   (js/console.log x)))

(defn ctype=
  ([t] #(ctype= % t))
  ([c t] (= (first c) t)))

(defn parse-turtle-cmds [cmds]
  (loop [cmds cmds
         [x y :as loc] [0 0]
         dir 180
         pts [[:point 0 0]]]
    (if (seq cmds)
      (let [[cmd & params] (first cmds)
            remaining (rest cmds)]
        (condp = cmd
          :forward (let [dist (first params)
                         radians (* dir js/Math.PI (/ 180))
                         loc' [(+ x (* dist (js/Math.sin radians)))
                               (+ y (* dist (js/Math.cos radians)))]]
                     (recur remaining loc' dir (conj pts (into [:point] loc'))))
          :turn (recur remaining loc (- dir (first params)) pts)
          (recur remaining loc dir (conj pts (first cmds)))))
      pts)))

(defn clear! [{:keys [ctx canvas]}]
  (let [w (.-width canvas)
        h (.-height canvas)]
    (.clearRect ctx 0 0 w h)))

(defn draw [{:keys [canvas-id
                    width
                    height
                    cmds
                    init]}]
  (let [canvas (js/document.getElementById canvas-id)
        cmds (parse-turtle-cmds cmds)
        ctx (.getContext canvas "2d")
        [left right] (extent (map second (filter (ctype= :point) cmds)))
        [bottom top] (extent (map #(nth % 2) (filter (ctype= :point) cmds)))
        sx (middle width [left right])
        sy (middle height [bottom top])
        state {:ctx ctx
               :canvas canvas
               :pos [sx sy]
               :cmds cmds
               :cmd-idx 0
               :mode [:polygon 6]}]

    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (init state)

    (loop [{:keys [ctx pos cmd-idx cmds mode] :as s} state]
      (when-let [cmd (get cmds cmd-idx)]
        (let [[ct a1 a2] cmd]
          (condp = ct
            :point
            (condp = (first mode)
              :point
              (let [[nx ny :as nxt-pos] [(+ sx a1) (+ sy a2)]]
                (.strokeRect ctx nx ny 1 1)
                (recur
                  (-> s
                      (update :cmd-idx inc)
                      (assoc :pos nxt-pos))))

              :circle
              (let [[nx ny :as nxt-pos] [(+ sx a1) (+ sy a2)]]
                (.beginPath ctx)
                (.arc ctx nx ny (second mode) 0 (* 2 js/Math.PI) false)
                (.stroke ctx)
                (.fill ctx)
                (recur
                  (-> s
                      (update :cmd-idx inc)
                      (assoc :pos nxt-pos))))

              :line
              (let [[x y] pos]
                (let [[nx ny :as nxt-pos] [(+ sx a1) (+ sy a2)]]
                  (.beginPath ctx)
                  (.moveTo ctx x y)
                  (.lineTo ctx nx ny)
                  (.stroke ctx)
                  (recur (-> s
                             (update :cmd-idx inc)
                             (assoc :pos nxt-pos)))))

              :triangle
              (let [[p1 p2] (take 2 (filter (ctype= :point) (reverse (subvec cmds 0 cmd-idx))))]
                (if (and p1 p2)
                  (let [[p1x p1y] (map + (next p1) [sx sy])
                        [p2x p2y] (map + (next p2) [sx sy])
                        [nx ny :as nxt-pos] [(+ sx a1) (+ sy a2)]]
                    (.beginPath ctx)
                    (.moveTo ctx p1x p1y)
                    (.lineTo ctx p2x p2y)
                    (.lineTo ctx nx ny)
                    (.stroke ctx)
                    (.fill ctx)
                    (recur (-> s
                               (update :cmd-idx inc)
                               (assoc :pos nxt-pos))))
                  (recur (update s :cmd-idx inc))))

              :polygon
              (let [[[p1x p1y] & prst :as pts]
                    (take (second mode)
                          (map (fn [[_ x y]] [(+ sx x) (+ sy y)])
                               (filter (ctype= :point)
                                       (reverse (subvec cmds 0 cmd-idx)))))]
                (if (= (count pts) (second mode))
                  (let [[nx ny :as nxt-pos] [(+ sx a1) (+ sy a2)]]
                    (.beginPath ctx)
                    (.moveTo ctx p1x p1y)
                    (reduce
                      (fn [ctx [x y]]
                        (.lineTo ctx x y)
                        ctx)
                      ctx
                      prst)
                    (.lineTo ctx nx ny)
                    (.stroke ctx)
                    (.fill ctx)
                    (recur (-> s
                               (update :cmd-idx inc)
                               (assoc :pos nxt-pos))))
                  (recur (update s :cmd-idx inc)))))

            :do (recur (a1 s))
            :mode (recur (-> s
                             (assoc :mode a1)
                             (update :cmd-idx inc)))))))))

(defn color [c]
  [:do #(set! (.-strokeStyle %) c)])

(def break-path
  [:do #(do (.stroke %) (.beginPath %))])

(defn fill [c]
  [:do #(set! (.-fillStyle %) c)])

(defn rand-fill []
  [:do #(set! (.-fillStyle %)
              (str "rgba(" (int (* 255 (rand))) ","
                   (int (* 255 (rand))) ","
                   (int (* 255 (rand)))
                   ",1)"))])

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
        :d (concat (maybe 0.2 [[:mode [:circle (rand-nth (range 5 21 10))]]]) a2 b forward a1 a forward a a1 forward b a2)
        :b (concat (maybe [[:mode [:polygon 5]]]) a1 c forward a3 d forward d a3 forward c a1)
        :c (concat (maybe [(fill "red")]) a3 a forward a4 c forward c a4 forward a a3)
        :a (concat a4 d forward a2 b forward b a2 forward d a4)))
    []))

(defn go []
  (draw
    {:canvas-id "turtle-canvas"
     :width 500
     :height 500
     :clear true
     :init (fn [s]
             (let [ctx (:ctx s)]
               (clear! s)
               (set! (.-lineWidth ctx) 0.5)
               (set! (.-strokeStyle ctx) "rgba(0,0,0,0.1)")
               (set! (.-fillStyle ctx) "rgba(100,0,50,.05)")
               (assoc s :ctx ctx)))
     :cmds
     (pierand
       {:step-fn #(rand-nth [10 20])
        :angle-fn #(rand-nth (range 0 360 60))
        :depth 6})}))

(set! (.-onclick js/document.body)
      go)

(go)

