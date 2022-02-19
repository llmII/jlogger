(use fugue)
(use logging/registry)

# logger
# ----------------------------------------------------------------------------
# Defines a logger, which is the primary user facing interface for writing to
# groups of logs (or a single log, a group of just 1). Keeps up with
# statistics per log.

# helpers ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# stat helpers
# Stats when returned look something like below:
# {:written {:lines :bytes :entries} :synced {:lines :bytes :entries}}
(defn- base-stat-entry? [x]
  (match x
    ({:lines lines :bytes bytes :entries entries}
      (and (number? lines) (number? bytes) (number? entries)))
    true))

(extend-multi
  stat/add [nil] [(x (dictionary? x))] [orig new]
  (eachp [k v] new
    (put orig k v))
  orig)

(extend-multi stat/add [_] [nil] [orig new]
  orig)

(extend-mult stat/add [nil] [nil] [orig new] nil)

(extend-multi
  stat/add [(o (base-stat-entry? o))] [(n (base-stat-entry? n))]
  [orig new]
  (add-entry orig new)
  (put orig :lines (+ (get orig :lines 0) (get new :lines 0)))
  (put orig :bytes (+ (get orig :bytes 0) (get new :bytes 0)))
  (put orig :entries (+ (get orig :entries 0) (get new :entries 0)))
  orig)

(defn- compare-stat [a b]
  (let [x              (if (dictionary? a) a {})
        y              (if (dictionary? b) b {})
        a-synced-bytes (get-in x [:synced :bytes] 0)
        b-synced-bytes (get-in y [:synced :bytes] 0)]
    (if (not= a-synced-bytes b-synced-bytes)
      (if (> a-synced-bytes b-synced-bytes)
        1
        -1)
      0)))

(extend-multi
  stat/comp [_] [(o (base-stat-entry? o))]
  [a b]
  (compare-stat a b))

(extend-multi
  stat/comp [(o (base-stat-entry? o))] [_]
  [a b]
  (compare-stat a b))

(extend-multi
  stat/comp [(o (base-stat-entry? o))] [(o (base-stat-entry? o))]
  [a b]
  (compare-stat a b))


(defn- book-keeping [self log data]
  (when (not (empty? data))
    (with-slots %logger self
      (when (not ((@ stats) (name log)))
        (put ((@ stats) (name log)) @{:written @{} :synced @{}}))
      (let [stat    ((@ stats) (name log))
            written (if (empty? (stat :written)) nil (stat :written))
            synced  (if (empty? (stat :synced)) nil (stat :synced))]
        (stat/add written (data :written))
        (stat/add synced (data :synced))))))

# logging helpers
#
# TODO: need a better word for "payload", just no idea if there is a better
# one and would hate to be stuck with it but calling it :rest is disingenuous
# as well.
#
# The idea for "payload" (internally `:rest`), is to provide extra data to
# entries, that a filter may filter or a formatter (custom) may mangle. It's
# meant to carry things like stack traces, but could be used to arguably
# implement everything presented in the base logger interface (assuming one
# rewrote and extracted all the base functionality into extension modules).
(defn- log-method-impl [self level rest]
  (with-slots %logger self
    (when-let [data @{:date    (os/date)
                      :level   level
                      :section (@ section)}
               len  (length rest)
               test (> 0 len)]
      (var kw-idx 0)
      # Only possible indexes for a keyword in the args are these, figure out
      # which is the last one and draw out format/args from the part after
      # them.
      (each idx [0 2 4]
        (let [kw? (get rest idx)]
          (when (and (= (type kw?) :keyword)
                     (or (= :tags kw?) (= :props kw?) (= :payload kw?)))
            (put data (if (= kw? :payload) :rest kw?) (get rest (+ 1 idx)))
            (set kw-idx idx))))
      (put data :format (get rest (+ 2 kw-idx)))
      (put data :args (when (> len (+ 3 kw-idx)) (slice rest (+ 3 kw-idx))))
      (when (data :format) # don't output if there was no format string
        (each log (@ logs)
          (book-keeping self log (write log data)))))))

# %logger ********************************************************************
(defproto %logger ()
  # init like
  # (:new %logger :logs [array-of-log-objects] :section [string])
  logs    {}
  section {}
  stats   {})

# Log methods, like error, are meant to be used like the below, where each key
# value pair is entirely optional, and format is mandatory, and args is
# optional:
# (error self :tags tags :props props :payload rest format & args)

(defmethod _init %logger [self]
  (set (stats self) @{}))

(defmethod fatal %logger [self & args]
  (log-method-impl self :fatal args))

(defmethod alert %logger [self & args]
  (log-method-impl self :alert args))

(defmethod critical %logger [self & args]
  (log-method-impl self :critical args))

(defmethod error %logger [self & args]
  (log-method-impl self :error args))

(defmethod warning %logger [self & args]
  (log-method-impl self :warning args))

(defmethod notice %logger [self & args]
  (log-method-impl self :notice args))

(defmethod info %logger [self & args]
  (log-method-impl self :info args))

(defmethod debug %logger [self & args]
  (log-method-impl self :debug args))

(defmethod flush %logger [self]
  (each log logs
    (flush log)))

# returns only the log stat with the lowest synced bytes
(defmethod stat %logger [self]
  (with-slots %logger self
    (reduce2
      |(if (= (stat/comp $2 $1) -1) $2 $1)
      (@ stats))))
