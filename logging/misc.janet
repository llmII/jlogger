(defn nop [&]
  ())

(defn tnop [&]
  true)

(defn dnop [& rest]
  ;rest)

(defn partial-method
  "Create a method that's partially applied."
  [f self & applied-args]
  (if (zero? (length applied-args))
    (fn [self & args] (f self ;args))
    (fn [self & args] (f self ;applied-args ;args))))

(defn datestr
  ```
  Turns the provided date `date` struct into a string like:
    YYYY/MM/DD HH:MM:SS
  ```
  [date]
  # YYYY/MM/DD HH:MM:SS
  (string/format "%.4d.%.2d.%.2d %.2d:%.2d:%.2d"
                 (date :year) (date :month) (date :month-day)
                 (date :hours) (date :minutes) (date :seconds)))
