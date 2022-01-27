(use fugue)
(use logging/internal/sink)

# drains
# ----------------------------------------------------------------------------

# common +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(def- empty-stat {:lines 0 :bytes 0 :entries 0})

(defn limit-exceeded? [self]
  (var ret false)
  (eachp [setting limit] (settings self)
    (let [{:year-day yd :hours h}  (os/date)
          {:year-day ryd :hours rh} (runtime self)]
      (set ret
           (or ret
               (match setting
                 :interval           (> (- (os/time) (last-written self))
                                        limit)
                 [:boundary :daily]  (not= yd ryd)
                 [:boundary :hourly] (or (not= yd ryd) (not= h rh))
                 _                   (>= (self setting) limit))))))
  ret)

# %drain-base ****************************************************************
(defproto %drain-base ())
(defmethod drain %drain-base [self method entry])
(defmethod wrap-sink? %drain-base [self])

# %immediate-drain ***********************************************************
(defproto %immediate-drain %drain-base)

(defn- immediate-flush [&opt entry]
  (when-let [t (or entry (self :opened))
             s (if entry
                 {:lines (lines entry) :bytes (bytes enry) :entries 1}
                 empty-stat)
             r @[]]
    (when entry
      (array/push r (if (self :opened) [:write] [:open :write])
                  (put self :opened true)))
    (array/push r :flush [:stat {:written s :synced s}]]))

(defn- immediate-close []
  (when (self :opened)
      (put self :opened false)
      [:close]))

(defn- immediate-close []
  (put self :opened false)
  [:close [:stat {:written empty-stat :synced empty-stat}]])

(defmethod drain %immediate-drain [self method entry]
  (match method
    :write (immediate-flush entry)
    :flush (immediate-flush)
    :close (immediate-close)))

# %common-drain **************************************************************
# Drains should typically be based off of %drain-base, but %buffered-drain and
# %rotated-drain share basically all of the same functionality *except* when
# considering writes, so implmement the rest for them once.
(defproto %common-drain %drain-base
  # lines, entries, bytes, interval, boundary
  settings     {:init? true}
  bytes        {:default 0}
  lines        {:default 0}
  entries      {:default 0}
  opened       {:default false}
  action       {:default (fn [])}
  last-written {}
  runtime      {})

(defn common-drain-clear [self]
  (with-slots %common-drain-drain self
    (let [ret {:lines   (@ lines)
               :bytes   (@ bytes)
               :entries (@ entries)}]
      (set (@ bytes) 0)
      (set (@ lines) 0)
      (set (@ entries) 0)
      (set (@ last-written) (os/time))
      (set (@ runtime) (os/date)))
    ret))

(defn- common-drain-flush [self &opt stat]
  (default stat @{})
  (when (opened self)
    @[:flush [:stat (put stat :synced (common-drain-clear self))]]))

(def- actions {:rotate
               (fn [self op-stack stat]
                 (with-slots %common-drain self
                   (let [[flush stat] (common-drain-flush self stat)]
                     (array/push op-stack flush :close stat))
                   (set (@ opened) false)))

               :buffer
               (fn [self op-stack stat]
                 (array/concat op-stack (common-drain-flush self stat)))})

(defn- common-drain-write [self entry]
  (with-slots %common-drain self
    (let [ret  (if (@ opened) @[:write] @[:open :write])
          stat @{:written {:lines   (lines entry)
                           :bytes   (bytes entry)
                           :entries 1}}]
      (set (@ opened) true)
      (+= (@ lines) (lines entry))
      (+= (@ bytes) (bytes entry))
      (++ (@ entries))
      (if (limit-exceeded? self)
        ((actions ((@ settings) :action)) self ret stat)
        (array/push ret [:stat stat])))))

(defn- common-drain-close [self]
  (with-slots %common-drain self
    (when (@ opened)
      (let [[_ stat] (common-drain-flush self)]
        (set (@ opened) false)
        [:flush :close stat]))))

(defmethod _init %common-drain [self]
  (with-slots %common-drain self
    (set (@ last-written) (os/time))
    (set (@ runtime)      (os/date))))

(defmethod wrap-sink? %common-drain [self]
  (with-slots %common-drain self
    (when (= :buffer ((@ settings) :action))
      [[%common-drain-sink] []])))

(defmethod drain %common-drain [self method entry]
  (match method
    :write (common-drain-write entry)
    :flush (common-drain-flush)
    :close (common-drain-close)))

# %combo-drain ***************************************************************
(defproto %combo-drain %drains
  drains {})

(defmethod _init %combo-drain [self & drains]
  (if drains
    (with-slots %combo-drain self
      (set (@ drains) drains))
    (error (drain-errors :no-drains-combo-drain))))

(defmethod drain %combo-drain [self method entry]
  (let [ret @[]
        stat @{}]
    (map
      |(let [curr (drain $ method entry)]
         (match (last curr)
           [:stat v] (do
                       (array/concat ret (array/slice curr -2))
                       (merge (stat :written) (v :written))
                       (merge (stat :synced) (v :synced)))
           _ (array/concat ret curr)))
      drains)
    (array/push ret [:stat stat])))

(defmethod wrap-sink? %combo-drain [self]
  (map |(wrap-sink? $) (reverse drains)))
