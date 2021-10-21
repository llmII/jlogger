(defn nop
  "A function that does nothing, a no-op."
  [&])

(defn tnop
  "A function that returns true no matter the arguments presented."
  [&] true)

(defn pnop
  "A function that returns whatever is given to it."
  ;rest)

(defn partial-method
  "Create a method that's partially applied."
  [f self & applied-args]
  (if (zero? (length applied-args))
    (fn [self & args] (f self ;args))
    (fn [self & args] (f self ;applied-args ;args))))
