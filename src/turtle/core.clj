(ns turtle.core)

(defmacro profile [expr]
  `(let [before (.getTime (js/Date.))
         ]
     expr
     (- (.getTime (js/Date.)) before)))
