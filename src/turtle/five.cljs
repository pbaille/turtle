(ns turtle.five
  (:require [monet.canvas :as m]))

(enable-console-print!)

;; ---------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------

; dom ----

(defn $1 [sel]
  (js/document.querySelector sel))

(defn log [& xs]
  (doseq [x xs]
    (js/console.log x)))

; draw ---

(defn middle [width [left right]]
  (- (/ (- width (- right left)) 2) left))

(defn get-center [t]
  (let [c (get-in t [:drawing :canvas])
        w (.-width c)
        h (.-height c)
        {minx :min maxx :max} (get-in t [:extent :x])
        {miny :min maxy :max} (get-in t [:extent :y])]
    [(middle w [minx maxx])
     (middle h [miny maxy])]))

(defn clear! [canvas]
  (let [ctx (m/get-context canvas "2d")
        w (.-width canvas)
        h (.-height canvas)]
    (m/clear-rect ctx {:x 0 :y 0 :w w :h h})))

(defn reset-transform [ctx]
  (.setTransform ctx 1 0 0 1 0 0))

;; ---------------------------------------------------------------------
;; Turtle genesis
;; ---------------------------------------------------------------------

(declare t> tf)

(def turtle-base-tasks
  "turtle base tasks"
  {:set-step
   #(assoc %1 :step %2)

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
   #(t> %1 [:set-step! %2] [:step])

   :turn
   #(update %1 :dir + %2)

   :turn-left
   #(t> % [:turn (* (:angle %) (or %2 1))])

   :turn-right
   #(t> % [:turn (- (* (:angle %) (or %2 1)))])

   :set-angle
   #(assoc %1 :angle %2)

   :turn!
   #(t> %1 [:set-angle! %2] [:turn])

   :save
   #(update % :backups assoc %2 (t> % [:clear-backups]))

   :restore
   #(assoc (get (:backups %) %2) :backups (:backups %))

   :clear-backups
   #(dissoc % :backups)

   :memorize
   #(assoc %1 :memory %2 %3)

   :forget
   #(dissoc %1 :memory %2)

   :learn
   #(assoc %1 :tasks %2 %3)})

(defn- apologize!
  "when a turtle does not know how to do the task, she speaks"
  [task tasks]
  (throw
    (js/Error.
      (str "The turtle: I doesn't know how do that: " task
           "\n but I can do all of those things: " tasks))))

(defn t>
  "take a cooperative turtle and any number of turtle-tasks
   and return the turtle when she have completed all the tasks
   ex: (t> turtle [:turn-left] [:step 10])"
  [turtle & tasks]
  (reduce
    (fn [t [v & args]]
      (let [turtle-tasks (merge turtle-base-tasks (:tasks t))]
        (if-let [task (turtle-tasks v)]
          (apply task t args)
          (apologize!
            (cons v args)
            (keys turtle-tasks)))))
    turtle
    tasks))

(defn tf
  "a turtle function
   take any number of turtle-tasks and
   return a function that wait for the turtle in order to ask her gently to do so"
  [& tasks]
  #(apply t> % tasks))

;;----------------------------------------------------------------------
;; Drawing
;;----------------------------------------------------------------------

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

(defn create-drawing-turtle
  "create a drawing turtle from given options"
  [opts]
  (merge
    {:step 10
     :angle 90
     :cmds []

     ;; extra tasks for drawing
     :tasks
     {:f #(let [{:keys [x y] :as turtle} (t> % [:step])]
            (update-in
              turtle
              [:drawing :cmds]
              conj
              [:ctx [:stroke] [:move-to [x y]] [:begin-path]]))

      :F #(let [{:keys [x y] :as turtle} (t> % [:step])]
            (update-in
              turtle
              [:drawing :cmds]
              conj
              [:ctx [:line-to [x y]]]))

      ;; aliases
      :- (tf [:turn-right])
      :+ (tf [:turn-left])

      :ctx #(update-in % [:drawing :cmds] conj (into [:ctx] %&))
      :canvas #(update-in % [:drawing :cmds] conj (into [:ctx] %&))}

     :drawing {:canvas "canvas"
               :cmds []
               :centerize? true
               :init
               (fn [canvas ctx]
                 (reset-transform ctx)
                 (clear! canvas)
                 (set! (.-strokeStyle ctx) "rgba(0,0,0,.4)")
                 (set! (.-lineWidth ctx) 1))}}
    opts))

(defn init!
  "get the turtle associated canvas, extract ctx,
   apply initialize fn ([:drawing :init])"
  [{{:keys [canvas init]} :drawing :as turtle}]
  (let [c (if (string? canvas)
            ($1 canvas)
            canvas)
        ctx (.getContext c "2d")]
    (init c ctx)
    (update turtle
            :drawing
            assoc
            :canvas c
            :ctx ctx)))

(defn draw!
  "compile cmds into drawing cmds,
   handle centerization if specified
   execute all ctx actions"
  [t]
  (let [t (reduce t> t (:cmds t))
        ctx (get-in t [:drawing :ctx])
        cmds (get-in t [:drawing :cmds])
        centerize? (get-in t [:drawing :centerize?])
        ctx-cmds (mapcat next (filter #(= :ctx (first %)) cmds))]
    (when centerize?
      (apply m/translate ctx (get-center t)))
    (doseq [[v & args :as c] ctx-cmds]
      (apply (get ctx-actions v) ctx args))))

(defn t!
  "all in one helper to create a drawing turtle from an option map,
   initialize it then draw"
  [opts]
  (-> opts
      create-drawing-turtle
      init!
      draw!))

;; tests ------------------------------------------------------------------

(defn l-system [{:keys [format rules axiom iterations result] :as opts}]
  (if (zero? iterations)
    (format result)
    (recur (-> opts
               (assoc
                 :result
                 (mapcat #(rules % [%]) (or result axiom)))
               (update :iterations dec)))))

(t!
  {:cmds
   (vec (concat [[:ctx [:begin-path] [:move-to [0 0]]]]
                (l-system
                  {:iterations 2
                   :axiom [:F :- :F :- :F :- :F]
                   :rules {:F [:F :- :F :+ :F :+ :F :F :- :F :- :F :+ :F]}
                   :format (partial map vector)})
                [[:ctx [:stroke]]]))})

(comment

  ;; test
  (simple-draw
    (l-system
      {:iterations 2
       :axiom [:a]
       :rules {:a [:-- :d :F :++ :b :F :b :++ :F :d :--]
               :b [:+ :c :F :- :d :F :d :- :F :c :+]
               :c [:- :a :F :-- :c :F :c :-- :F :a :-]
               :d [:++ :b :F :+ :a :F :a :+ :F :b :++]}
       :format
       (fn [x])}))

  ;1.6
  (simple-draw
    (l-system
      {:iterations 2
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :- :F :+ :F :+ :F :F :- :F :- :F :+ :F]}
       :format simple-formatter}))

  ;1.7.a
  (simple-draw
    (l-system
      {:iterations 2
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :- :F :F :+ :F :F :+ :F :+ :F :- :F :- :F :F :+ :F :+ :F :- :F :- :F :F :- :F :F :+ :F]}
       :format simple-formatter})
    {:angle 95
     :step 8})

  ;1.7.b
  (simple-draw
    (l-system
      {:iterations 3
       :axiom [:- :F]
       :rules {:F [:F :- :F :+ :F :+ :F :- :F]}
       :format simple-formatter}))

  ;1.8
  (simple-draw
    (l-system
      {:iterations 2
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :- :f :+ :F :F :- :F :- :F :F :- :F :f :- :F :F :+ :f :- :F :F :+ :F :+ :F :F :+ :F :f :+ :F :F :F]
               :f [:f :f :f :f :f :f]}
       :format simple-formatter}))

  ;1.9.a
  (simple-draw
    (l-system
      {:iterations 3
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :F :- :F :- :F :- :F :- :F :- :F :+ :F]}
       :format simple-formatter}))

  ;1.9.b
  (simple-draw
    (l-system
      {:iterations 2
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :F :- :F :- :F :- :F :- :F :F]}
       :format simple-formatter}))

  ;1.9.c
  (simple-draw
    (l-system
      {:iterations 3
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :F :- :F :- :F :+ :F :- :F :F]}
       :format simple-formatter}))

  ;1.9.d
  (simple-draw
    (l-system
      {:iterations 4
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :F :- :F :- :- :F :- :F]}
       :format simple-formatter}))

  ;1.9.e
  (simple-draw
    (l-system
      {:iterations 4
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :- :F :F :- :- :F :- :F]}
       :format simple-formatter}))

  ;1.9.f
  (simple-draw
    (l-system
      {:iterations 4
       :axiom [:F :- :F :- :F :- :F]
       :rules {:F [:F :- :F :+ :F :- :F :- :F]}
       :format simple-formatter}))

  ;1.10.a
  (simple-draw
    (l-system
      {:iterations 10
       :axiom [:a]
       :rules {:a [:a :+ :b :+]
               :b [:- :a :- :b]}
       :format (assoc simple-formatter
                 :b [:turtle :F]
                 :a [:turtle :F])}))

  ;1.10.b
  (simple-draw
    (l-system
      {:iterations 5
       :axiom [:a]
       :rules {:b [:a :+ :b :+ :a]
               :a [:b :- :a :- :b]}
       :format
       {:a [:turtle :F]
        :b [:turtle :F]
        :+ [:turtle :+]
        :- [:turtle :-]}})
    {:angle 60 :step 10})

  ;since 1.11 -> 1.14 seems really boring to type it is left as an exercice for the reader :)

  ;1.15
  (simple-draw
    (l-system
      {:iterations 5
       :axiom [:a]
       :rules {:a [:+ :b :F :- :a :F :a :- :F :b :+]
               :b [:- :a :F :+ :b :F :b :+ :F :a :-]}
       :format
       simple-formatter})))
