(import logging/registry)
(use logging/misc)

# errors
# ----------------------------------------------------------------------------
(def- sink-errors
  @{:open-log-failed
    "Failed to open log <%s> for appending."
    :err-level-less-than-base
    "Error level must be greater than base log level."
    :wrong-console
    "Cannot open console of type <%s> please try :stderr or :stdout."})

# helpers
# ----------------------------------------------------------------------------

# destination types
# ============================================================================
# Destinations are just things that support :new, :write, and :close
#
# The base is buffered destination, which is used by all other destination
# types (rotating-destination, etc (where etc doesn't exist yet))

# buffered-destination #######################################################
# Buffers a destination according to how often it should be flushed, flushing
# the data to the backing store once a condition exists requiring the data to
# be flushed.
(defn- buffered-destination-clear
  [self]
  (let [@{:lines lines :bytes bytes :last-write write} self]
    (put self :lines 0)
    (put self :bytes 0)
    (put self :last-write (os/time))
    (buffer/clear (self :buffer))
    {:lines lines :bytes bytes :last-write write}))

(defn- buffered-destination-flush
  [self &opt force]
  (when
      (or
        force
        (>= (self :lines) ((self :flush-frequency) :lines))
        (>= (self :bytes) ((self :flush-frequency) :bytes))
        (>= (- (os/time) (self :last-write))
          ((self :flush-frequency) :interval)))
    (:write (self :file) (self :buffer))
    (:flush (self :file))
    (buffered-destination-clear self)))

(defn- buffered-destination-write
  [self data]
  (let [lines (length (string/find-all "\n" data))]
    (+= (self :bytes) (length data))
    (+= (self :lines) lines)
    (buffer/push-string (self :buffer) data)
    (buffered-destination-flush self)))

(defn- buffered-destination-force
  [self]
  (buffered-destination-flush self true))

(defn- buffered-destination-close
  [self]
  (:flush (self :file)) # always flush before closing
  (:close (self :file))
  (buffered-destination-clear self))

(defn- buffered-destination-open
  [self data]
  (when (not (self :file))
    (let [f (file/open (self :file-name) :w+n)]
      (when (not f)
        (errorf (sink-errors :open-log-failed) (self :file-name)))
      (put self :file (file/open (self :file-name) :w+n))
      (put self :close buffered-destination-close)
      (put self :flush buffered-destination-force)
      (put self :write buffered-destination-write)))
  (:write self data))

(def- *buffered-destination*
  @{:bytes           0
    :lines           0
    :file-name       ""
    :write           buffered-destination-open
    :flush           tnop
    :close           tnop})

(def- buffered-destination
  @{:new (fn [self name flush-frequency]
           (table/setproto @{:file-name name
                             :flush-frequency flush-frequency
                             :last-write (os/time)
                             :buffer @""}
                           *buffered-destination*))})

# end buffered-destination ###################################################

# console-destination ########################################################
# It's just a buffered destination with :close glued shut and no open, straight
# to write
(def- console-destination
  @{:new (fn [self which flush-frequency]
           (let [file (match which
                        :stdout stdout
                        :stderr stderr
                        _       (errorf (sink-errors :wrong-console) which))]
             (table/setproto @{:flush-frequency flush-frequency
                               :last-write      (os/time)
                               :buffer          @""
                               :file            file
                               :flush           buffered-destination-force
                               :write           buffered-destination-write}
               *buffered-destination*)))})

# end console-destination ####################################################

# rotating-destination #######################################################
# Rotates a buffered destination according to how often it should be rotated,
# changing the backing store once a condition exists requiring the backing
# store to be changed.

(defn- condition-handler
  [self cond val]
  (match [cond val]
    # special cases
    [:interval c]       (> (- (os/time) (self :last-write)) c)
    [:boundary :daily]  (not (= ((os/date) :year-day)
                                ((self :runtime) :year-day)))
    [:boundary :hourly] (not (= ((os/date) :hours)
                                ((self :runtime) :hours)))
    # :bytes, :lines
    [_ v]               (> (self cond) v)))

(defn- rotation-decide
  [self]
  (var ret false)
  (each x (map (fn [x] (condition-handler self (first x) (last x)))
            (filter (fn [x] (not (= (last x) :never)))
              (pairs (self :rotation))))
        (set ret (or ret x)))
  ret)

# forward decl
(var- rotated-destination-open nil)

(defn- rotated-destination-clear
  [self]
  (put self :last-rotate (os/time))
  (put self :runtime (os/date))
  (put self :lines 0)
  (put self :bytes 0)
  (put self :flush tnop)
  (put self :write rotated-destination-open))

(defn- rotated-destination-close
  [self]
  (rotated-destination-clear self)
  (when (self :destination)
    (:close (self :destination))))

(defn- rotated-destination-rotate
  [self]
  (when (rotation-decide self)
    (let [ret (:close (self :destination))]
      (put self :destination nil)
      (rotated-destination-clear self)
      ret)))

(defn- rotated-destination-inc
  [self stats]
  (each stat stats
        (when stat
          (put self :lines (+ (in self :lines 0) (in stat :lines 0)))
          (put self :bytes (+ (in self :bytes 0) (in stat :bytes 0)))
          (put self :last-write
            (max (in self :last-write 0) (in stat :last-write 0)))))
  {:lines (self :lines) :bytes (self :bytes) :last-write (self :last-write)})

(defn- rotated-destination-write
  [self data]
  (rotated-destination-inc
    self
    [(:write (self :destination) data) (rotated-destination-rotate self)]))

(defn- rotated-destination-force
  [self]
  (:flush (self :destination)))

(varfn rotated-destination-open
  [self data]
  (put self :destination
     (:new buffered-destination
           ((self :namer) (self :level) (self :name))
           (self :flush-frequency)))
  (put self :close rotated-destination-close)
  (put self :write rotated-destination-write)
  (put self :flush rotated-destination-force)
  (:write self data))

(def- *rotated-destination*
  @{:bytes 0
    :lines 0
    :write rotated-destination-open
    :flush tnop
    :close tnop})

(def- rotated-destination
  @{:new (fn [self rotation flush-frequency level name namer]
           (table/setproto @{:rotation        rotation
                             :flush-frequency flush-frequency
                             :runtime         (os/date)
                             :last-write     (os/time)
                             :level           level
                             :name            name
                             :namer           namer}
                           *rotated-destination*))})

# end rotating-destination ###################################################

(defn- elect-destination
  [self level]
  (in (self :destinations) level ((self :destinations) :default)))

(defn- elect-formatter
  [self level]
  (in (self :formatting) level ((self :formatting) :default)))

(defn- log-index-of
  "We want a nil, not an error"
  [indexable search]
  (if search
    (index-of search indexable)
    nil))

(defn- log-to-numeric
  [self level &opt max]
  (if-let [min (log-index-of ((self :parent) :levels) (self level))]
    min
    (if max (+ (length ((self :parent) :levels)) 1) 0)))

(defn- log-file-stats-inc
  [self stats]
  (if (dictionary? stats)
    (eachp [stat amount] stats
           (+= (self stat) amount))))

(defn- log-file-write
  [self destination formatter format & rest]
  (log-file-stats-inc self (:write destination (formatter format ;rest))))

(defn- log-file-flush
  [self]
  (each dest (self :destinations)
        (log-file-stats-inc self (:flush dest))))

(defn- generate-writer
  [self destination formatter]
  (partial-method log-file-write self destination formatter))

(defn- namer
  [self & args]
  (string (self :directory) "/" (((self :parent) :namer) ;args)))

(defn- generate-namer
  [self]
  (partial namer self))

(defn- self-or-parent
  [self key]
  (or (self key) ((self :parent) key)))

(defn- open-destination
  [self level]
  (:new rotated-destination
        (self-or-parent self :rotation)
        (self-or-parent self :flush-frequency)
        level
        (self :name)
        (self :namer)))

(defn- make-destinations
  [self]
  (let [min (log-to-numeric self :base-level)]
    (if (self :split)
      # open destination per level
      (eachp [numeric level] ((self :parent) :levels)
             (if (>= numeric min)
               # open destination
               (put (self :destinations) level (open-destination self level))
               # give it a nop
               nop))

      # open default destination only
      (put (self :destinations) :default (open-destination self :default)))))


(defn- make-level-methods
  [self]
  (let [min (log-to-numeric self :base-level)]
    (eachp [numeric level] ((self :parent) :levels)
           (put self level
                (if (>= numeric min)
                  # a destination writer
                  (generate-writer self (elect-destination self level)
                                   (elect-formatter self level))
                  # nop
                  nop)))))

# sinks
# ----------------------------------------------------------------------------

# file-sink ##################################################################
(def- *file-sink*
  @{:type       "file-sink"
    :bytes      0
    :lines      0
    :last-write 0
    :directory "."
    :formatting @{}
    :split      false

    :flush log-file-flush

    :write (fn [self level format & rest]
             ((self level) self format ;rest))

    :close (fn [self]
             (eachp [k dest] (self :destinations)
                    (:close dest)
                    (put self k nil))
             (table/clear self))

    :stats (fn [self]
             {:bytes      (self :bytes)
              :lines      (self :lines)
              :last-write (self :write)})})

(def- file-sink
  @{:new (fn [self parent init-tab]
           (let [ret (table/setproto (merge @{} init-tab) *file-sink*)]
             # set parent
             (put ret :parent parent)

             # init formatting
             (put ret :formatting
                ((parent :formatter) (ret :name) :file (parent :levels)))

             # set base-level
             (put ret :base-level (ret :levels))
             (put ret :levels nil) # remove the extraneous key

             # set up our namer
             (put ret :namer (generate-namer ret))

             # set up destinations
             (put ret :destinations @{})
             (make-destinations ret)

             # create our levels
             (make-level-methods ret)
             ret))})
# end file-sink ##############################################################

# console-sink ###############################################################
(def- *console-sink*
  (table/setproto
    @{ # override new, close, it's just a file, but using a console
       # destination, not a rotated-destination
      :close tnop} *file-sink*))

(def- console-sink
  @{:new (fn [self parent init-tab]
           (let [ret (table/setproto (merge @{} init-tab) *console-sink*)]
             # set parent
             (put ret :parent parent)

             # init formatting
             (put ret :formatting
               ((parent :formatter) (ret :name) :console (parent :levels)))

             # set base-level, error-levels
             (put ret :base-level (ret :levels))
             (put ret :levels nil)
             (put ret :error-level (ret :errors))
             (put ret :errors nil)
             (assert (> (log-to-numeric ret :error-level true)
                       (log-to-numeric ret :base-level))
               (sink-errors :err-level-less-than-base))

             # set up destinations
             (put ret :destinations @{})
             (let [out  (log-to-numeric ret :base-level)
                   err  (log-to-numeric ret :error-level true)
                   cout (:new console-destination
                              :stdout (parent :flush-frequency))
                   cerr (:new console-destination
                              :stderr (parent :flush-frequency))
                   ]
               (eachp [numeric level] (parent :levels)
                 (cond
                   (and (>= numeric min) (< numeric err))
                   (put (ret :destinations) level cout)
                   (>= numeric err)
                   (put (ret :destinations) level cerr)))
               (put (ret :destinations) :default cout))

             # create our levels
             (make-level-methods ret)
             ret))})
# end console-sink ###########################################################

(registry/register-sink
 "Documentation goes here."
 file-sink :file)

(registry/register-sink
 "Documentation goes here."
 console-sink :console)
