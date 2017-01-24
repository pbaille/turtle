(ns turtle.core
  (:require [monet.canvas :as m]))

(enable-console-print!)

;; ---------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------

; generic -

(defn concretize
  "if this is a function, call it on args, else return this"
  [this & args]
  (if (fn? this)
    (apply this args)
    this))

(defn safe-call
  "if this is a function, call it on that,
   else return that"
  [this that]
  (if (fn? this)
    (this that)
    that))

; dom -----

(defn $1 [sel]
  (js/document.querySelector sel))

(defn log [& xs]
  (doseq [x xs]
    (js/console.log x)))

; draw ----

(defn middle [width [left right]]
  (- (/ (- width (- right left)) 2) left))

(defn clear! [canvas]
  (let [ctx (m/get-context canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (m/clear-rect ctx {:x 0 :y 0 :w w :h h})))

(defn reset-transform [ctx]
  (.setTransform ctx 1 0 0 1 0 0))

; monet mappings

(def ctx-actions
  {:begin-path m/begin-path
   :close-path m/close-path
   :save m/save
   :restore m/restore
   :rotate m/rotate
   :scale m/scale
   :translate m/translate
   :transform m/transform
   :fill m/fill
   :stroke m/stroke
   :clip m/clip
   :rect m/rect
   :clear-rect m/clear-rect
   :stroke-rect m/stroke-rect
   :fill-rect m/fill-rect
   :arc m/arc
   :ellipse m/ellipse
   :circle m/circle
   :text m/text
   :font-style m/font-style
   :fill-style m/fill-style
   :stroke-style m/stroke-style
   :stroke-width m/stroke-width
   :stroke-cap m/stroke-cap
   :stroke-join m/stroke-join
   :move-to (fn [ctx [x y]] (m/move-to ctx x y))
   :line-to (fn [ctx [x y]] (m/line-to ctx x y))
   :alpha m/alpha
   :composition-operation m/composition-operation
   :text-align m/text-align
   :text-baseline m/text-baseline
   :draw-image m/draw-image
   :quadratic-curve-to m/quadratic-curve-to
   :bezier-curve-to m/bezier-curve-to
   :rounded-rect m/rounded-rect})

;; ---------------------------------------------------------------------
;; Turtle genesis
;; ---------------------------------------------------------------------

(declare t> tf)

(def turtle-base-tasks
  "turtle base tasks"
  {
   :self identity

   :swap
   (fn [t f & args] (apply f t args))

   :goto
   (fn [t [x y]] (assoc t :x x :y y))

   ;; steps --------------------

   :set-step
   (fn [t step] (assoc t :step step))

   :step
   (fn [{:keys [dir step x y extent visited-coords] :as turtle} & [step*]]
     (let [step (or step* step)
           {{minx :min maxx :max} :x
            {miny :min maxy :max} :y} extent
           radians (* dir js/Math.PI (/ 180))
           [dx dy] [(* step (js/Math.sin radians))
                    (* step (js/Math.cos radians))]]

       (assoc turtle
         :x (+ x dx)
         :y (+ y dy)

         :visited-coords
         (conj (or visited-coords ()) [x y])

         :extent
         {:x {:min (min minx x) :max (max maxx x)}
          :y {:min (min miny y) :max (max maxy y)}})))

   :step!
   (fn [t step] (t> t [:set-step! step] [:step]))

   ;; turns ---------------------

   :turn
   (fn [t angle] (update t :dir + angle))

   :turn-left
   (fn [t & [angle]] (t> t [:turn (* (:angle t) (or angle 1))]))

   :turn-right
   (fn [t & [angle]] (t> t [:turn (- (* (:angle t) (or angle 1)))]))

   :set-angle
   (fn [t angle] (assoc t :angle angle))

   :turn!
   (fn [t angle] (t> t [:set-angle angle] [:turn]))

   ;; backups --------------------

   :save
   (fn [t backup-key] (update t :backups assoc backup-key (t> t [:clear-backups])))

   :restore
   (fn [t backup-key] (assoc (get (:backups t) backup-key) :backups (:backups t)))

   :clear-backups
   (fn [t] (dissoc t :backups))

   ;; memory ----------------------

   :memorize
   (fn
     ([t mem-map] (update t :memory into mem-map))
     ([t mem-key impl] (assoc-in t [:memory mem-key] impl)))

   :forget
   (fn [t & mem-keys]
     (update t :memory #(apply dissoc % mem-keys)))

   ;; tasks -----------------------

   :learn
   (fn
     ([t task-map] (update t :tasks into task-map))
     ([t task-key impl] (assoc-in t [:tasks task-key] impl)))})

(defn- apologize!
  "when a turtle does not know how to do the task, she speaks"
  [task tasks]
  (throw
    (js/Error.
      (str "The turtle: I doesn't know how do that: " task
           "\n but I can do all of those things: " tasks))))

(declare flows)

(defn t>
  "take a cooperative turtle and any number of turtle-cmds
   and return the turtle when she have completed all the tasks
   ex: (t> turtle [:turn-left] [:step 10])"
  [turtle & cmds]
  (reduce
    (fn [t cmd]
      (let [[v & args] cmd
            task (or (turtle-base-tasks v) ((:tasks t) v))]
        (if task
          (apply task t args)
          (apologize!
            (cons v args)
            #{} #_(keys turtle-base-tasks)))))
    turtle
    cmds))

(defn tf
  "a turtle function
   take any number of turtle-cmds and
   return a function that wait for the turtle in order to ask her gently to do so"
  [& cmds]
  #(apply t> % cmds))

(defn turtle
  "something is moving under the sand"
  [& [opts]]
  (merge
    {:x 0
     :y 0
     :dir 0
     :angle 90
     :step 10
     :program nil
     :memory {}
     :tasks {}
     :backups {}}
    opts))

;; Flow
;;----------------------------------------------------------------------
;; a set of tasks to manage program execution flow

(def flows

  {
   ;; direct flow execute given cmds sequentially
   :>
   (fn [t & cmds]
     (reduce t> t cmds))

   ;; branch
   ;; the flow is paralelized,
   ;; then the resulting turtles are merged as specified via merge-fn.
   ;; merge-fn :: [initial-turtle resulting-turtles] => final-turtle
   :branch
   (fn [t merge-fn & cmds]
     (merge-fn t (mapv #(t> t %) cmds)))

   ;; if (pred turtle) is true
   ;; then do true-cmd
   ;; elsif false-cmd is given do it
   ;; else return turtle unchanged
   :if
   (fn [t pred true-cmd & [false-cmd]]
     (cond
       (pred t) (t> t true-cmd)
       false-cmd (t> t false-cmd)
       :else t))

   ;; given a collection of turtle-preds interleaved with cmds
   ;; if a pred succeed, do the assiociated cmd
   ;; else return turtle unchanged
   :cond
   (fn [t & pred-cmd-seq]
     (loop [t t [[p cmd] & xs] (partition 2 pred-cmd-seq)]
       (cond
         (p t) (t> t cmd)
         (seq xs) (recur t xs)
         :else t)))

   ;; execute a random cmd in the given cmds
   :rand-nth
   (fn [t cmds]
     (t> t (rand-nth cmds)))

   ;; given a {prob-weight cmd} map
   ;; execute the picked cmd
   :prob
   (fn [t prob-cmd-map]
     (let [sums (reductions + 0 (vals prob-cmd-map))
           parts (map #(hash-map :obj %1 :min (first %2) :max (second %2))
                      (keys prob-cmd-map)
                      (partition 2 1 sums))
           x (rand (last sums))
           l (first (filter #(<= (:min %) x (:max %)) parts))]
       (t> t (:obj l))))})

(defn cmd-rmap
  "map f recursively over cmd,
   for a simple cmd, it does nothing,
   but for flow type cmds, it maps f over nested cmds"
  [[v & args :as cmd] f]
  (let [mfm (fn [x] (map #(cmd-rmap % f) x))]
    (condp = v
      :>
      (into [:>] (mfm args))

      :branch
      (into [:branch (first args)] (mfm (next args)))

      :sym
      [:sym (first args) (cmd-rmap (second args) f)]

      :if
      (into [:if (first args)] (mfm (next args)))

      :cond
      (into [:cond]
            (mapcat #(vector %1 (cmd-rmap %2 f))
                    (partition 2 args)))

      :rand-nth
      [:rand-nth (mfm (first args))]

      :prob
      [:prob (into {} (map (fn [[k v]] [k (cmd-rmap v f)])
                           (first args)))]
      ;;else case
      (f cmd))))

;; Drawing
;;----------------------------------------------------------------------

(def drawing-tasks
  {:f (tf [:step])

   :F (fn [t & [step]]
        (let [{ox :x oy :y} t
              t (if step (t> t [:set-step step]) t)
              {:keys [x y] :as turtle} (t> t [:step])]
          (update-in
            turtle
            [:drawing :cmds]
            conj
            [:ctx
             [:begin-path]
             [:move-to [ox oy]]
             [:line-to [x y]]
             [:stroke]
             [:close-path]])))

   :sym
   (fn
     ([t n]
      (update t :program (fn [p] [:sym n p])))
     ([t n cmd]
      (let [angles (range 0 360 (/ 360 n))
            dc-count (-> t :drawing :cmds count)
            merge-fn
            (fn [turtle ts]
              (update-in
                turtle
                [:drawing :cmds]
                concat
                (mapcat #(->> % :drawing :cmds (drop dc-count)) ts)))]

        (t> t (into [:branch merge-fn]
                    (map
                      #(vector :> [:turn %] cmd)
                      angles))))))

   ;; aliases
   :- (tf [:turn-right])
   :+ (tf [:turn-left])

   ;; passthrough
   :ctx #(update-in % [:drawing :cmds] conj (into [:ctx] %&))
   :canvas #(update-in % [:drawing :cmds] conj (into [:ctx] %&))})

(defn get-center
  "get the center coords for a drawing turtle,
   depends on :canvas size and turtle :extent
   it means that the turtle has to execute her cmds before calling this"
  [t]
  (let [c (get-in t [:drawing :canvas])
        w (.-width c)
        h (.-height c)
        {minx :min maxx :max} (get-in t [:extent :x])
        {miny :min maxy :max} (get-in t [:extent :y])]
    [(middle w [minx maxx])
     (middle h [miny maxy])]))

(def drawing-module
  "where the dirty stuff occur..."
  (let [c ($1 "canvas")]
    {:canvas c
     :ctx (.getContext c "2d")
     :program nil
     :centerize? true
     :init
     (fn [{{:keys [canvas ctx]} :drawing}]
       (reset-transform ctx)
       (clear! canvas)
       (set! (.-strokeStyle ctx) "rgba(0,0,0,.4)")
       (set! (.-lineWidth ctx) 1))
     :draw!
     (fn [{{:keys [init]} :drawing :as t}]
       (init t)
       #_(println (js/Date.))
       (let [;b (.getTime (js/Date.))
             {{:keys [cmds ctx centerize?]} :drawing :as t} (t> t (:program t))
             ;_ (println "after program" (- (.getTime (js/Date.)) b))
             ctx-cmds (mapcat next (filter #(= :ctx (first %)) cmds))]
         #_(println "drawing-cmds: " cmds)
         #_(println "center " (get-center t))
         (when centerize?
           (apply m/translate ctx (get-center t)))
         #_((:begin-path ctx-actions) ctx)
         (let [;b (.getTime (js/Date.))
               ]
           (doseq [[v & args :as c] ctx-cmds]
             (apply (get ctx-actions v) ctx args))
           ;(println "after draw" (- (.getTime (js/Date.)) b))
           )
         nil
         #_((:stroke ctx-actions) ctx)))}))

(defn drawing-turtle
  "this turtle doesn't want to do real job"
  ([]
   (turtle
     {:tasks (merge flows drawing-tasks)
      :drawing drawing-module}))
  ([opts]
   (merge (drawing-turtle) opts)))

(defn draw!
  "call the draw! function of the drawing module:
   - compile cmds into drawing cmds,
   - handle centerization if specified
   - execute all ctx-cmds"
  [t]
  ((get-in t [:drawing :draw!]) t))

;; Lyndenmayer Systems
;;-------------------------------------------------------------------------

(def ls-tasks
  {
   ;;
   :ls/next
   (fn
     ([t]
      (let [{{:keys [rules before-next after-next]} :ls} t
            t (safe-call before-next t)
            rules (concretize rules t)
            t (-> t
                  (update-in [:ls :generation] inc)
                  (update :program
                          cmd-rmap
                          #(let [r (rules (first %))]
                            (cond
                              (fn? r) (r %)
                              r r
                              :else %))))]
        (safe-call after-next t)))
     ([t n]
      (t> t (into [:>] (repeat n [:ls/next])))))})

(defn ls-cmd
  "little shortcut for ls simple rules
   ex:
   (ls-cmd \"F-F-F\") -> [:> [:F][:-][:F][:-][:F]]
   (ls-cmd \"one two three\") -> [:> [:one][:two][:three]]"
  [s]
  (let [format (partial map (comp vector keyword))]
    (if (re-find #" " s)
      (into [:>] (format (clojure.string/split s #" ")))
      (into [:>] (format s)))))

;; tests -----------------------------------------------------------------

(comment
  (draw!
    (assoc (drawing-turtle)
      :program
      [:sym 5 [:> [:F] [:-] [:F] [:+] [:F]]])))

(def ls-turtle1
  (-> (drawing-turtle {:angle 80})
      (update :tasks into ls-tasks)
      (assoc-in [:ls :rules]
                {:F (ls-cmd "F-F+F+FF-F-F+F")})
      (assoc :program (ls-cmd "F-F-F-F"))))

(comment
  (draw! (t> ls-turtle1 [:ls/next 2] [:sym 4])))

#_(let [t (drawing-turtle {:angle 60 :step 10})]
    (time
      (dotimes [_ 10000]
        ((:step turtle-base-tasks) t))))

#_(let [t (drawing-turtle {:angle 60 :step 10})]
    (time
      (do (t> t (into [:>] (repeat 10000 [:step]))) nil)))


(comment
  (time
    (dotimes [_ 30]
      (draw! (t> (-> (drawing-turtle {:angle 60 :step 10})
                     (update :tasks into ls-tasks)
                     (assoc-in [:ls :rules]
                               #(let [angle (rand-nth (range 0 360 30))
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
                 [:ls/next 6]
                 [:swap (fn [t] (update t :program cmd-rmap #(if (#{[:a] [:b] [:c] [:d]} %) [:self] %)))]
                 [:sym 5])))))



