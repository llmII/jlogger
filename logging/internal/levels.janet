(def levels
  {0 :fatal
   1 :alert
   2 :critical
   3 :error
   4 :warning
   5 :notice
   6 :info
   7 :debug})

(def blevels @{})
(eachp [numeric level] levels
  (put blevels level numeric))

(def blevels (table/to-struct blevels))
