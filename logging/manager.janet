(use logging/misc)
(import logging/namers)
(import logging/formatters)
(import logging/registry)
(import logging/registry-internal :as reg)

# errors
# ----------------------------------------------------------------------------
(def manager-errors
  {:sink-not-named
     "Attempted to create a nameless sink."})

# log-manager
# ----------------------------------------------------------------------------
(def- *log-manager*
  @{:rotation {:bytes :never :lines :never :interval :never :boundary :daily}
    :flush-frequency {:bytes 0 :lines 0 :interval 0}
    :formatter formatters/default
    :namer     namers/default
    :levels    [:trace :debug :info :warn :error :fatal]

    :close (fn [self &opt name]
             (if name
               # don't close things we don't have, ignore user attempting to
               (when ((self :named-sinks) name)
                 (:close ((self :named-sinks) name)))

               # close 'em all!
               (do
                 (each [_ v] (self :named-sinks)
                   (:close v)))))

    :open (fn [self &keys {:type        type
                           :name        name
                           :initializer init}]
            (when (not name) (error (manager-errors :sink-not-named)))
            (when (not ((self :named-sinks) name))
              (put (self :named-sinks) name
                   (:new ((reg/reg :sinks) type)
                         self
                         (merge {:name name} (if init init @{})))))
            ((self :named-sinks) name))})

# Creates log managers, assigning them a proto of *log-manager*
# Each created log-manager proxies an actual log manager
(def log-manager
  :logging

  ```
  A log-manager is used to manage all log-sinks associated with it. To get a
  log manager you must `(:new)` one first. At the time of initialization it
  expects keywords for configuration. This configuration mostly applies to all
  log sinks managed by the log manager. For instance, rotation makes sense for
  log file type sinks, but not for console sinks, so the console sink simply
  ignores that.

  `:rotation` A struct that determines how often the log should be rotated
  Mostly applies to files or file type logs. Fields are `:bytes`, `:lines`,
  `:interval` and `:boundary`. `:bytes` determines how big a backing store may
  get before it's rotated. `:lines` determines how many lines a backing store
  can have before it's rotated. `:interval` determines how long the backing
  store may be written to before it's rotated. `:boundary` allows for
  something like intervals except glued to specific times (like `:hourly`
  `:daily`).

  `:flush-frequency` A struct that determines how often in lines or bites or
  at what interval a sink should be flushed to it's backing "store". This
  mostly again applies to log files. Can be used to prevent tiny disk writes
  to a degree. Could cause loss of logging data. The defaults are to always
  flush whenever a sink gets a new message as to what is to write.

  `:formatter` is the formatter the sinks will use to take the data logged and
  transform it into a log line. Most sinks will/should obey the formatter,
  albeit if one were to create a JSON formatter, perhaps the console sink
  would ignore it. This is a function that abides by a standard interface such
  that all sinks may use it.

  `:levels` is a list of levels in order of severity, from least severe to
  most.

  `:namer` is a function used for the naming of the log's backing store.
  Mostly applies to files.

  Almost all of these have "sane" defaults, such as `:rotation` `:boundary`
  being `:daily` and the others being `:never` (never rotate, except daily).
  `:flush-frequency` has all of it's fields zeroed out by default (flush with
  every write). There is a default `:formatter` and a default `:namer`. The
  default `:levels` are `[:trace :debug :info :warn :error :fatal]`.

  An instance of log-manager supports the `:open` and `:close` methods.
  TODO: Document these

  The `log-manager` that is recieved from `(import logging)` only supports
  creating new instances of `log-manager`'s.'

  An instance is acquired with the following:
        (:new logging/manager &keys {:rotation        rotation
                                     :flush-frequency flush-frequency
                                     :formatter       formatter
                                     :levels          levels
                                     :namer           namer})

  Changing a log manager's configuration after initialization is not
  supported.
  ```

  @{:new (fn [self &keys {:rotation        rotation
                          :flush-frequency flush-frequency
                          :formatter       formatter
                          :levels          levels
                          :namer           namer}]
           (let [ret (table/setproto @{} *log-manager*)]
             (put ret :named-sinks @{})
             (when rotation (put ret :rotation rotation))
             (when flush-frequency (put ret :flush-frequency flush-frequency))
             (when formatter (put ret :formatter formatter))
             (when levels (put ret :levels levels))
             (when namer (put ret :namer namer))
             ret))})

# combo-sink
# ----------------------------------------------------------------------------
# This is provided here because it relies on the internal method set of the
# *log-manager* to work.
(def- *combo-sink*
  (table/setproto
    @{:type "combo-sink"
      :flush (fn [self]
               (each sink (self :named-sinks)
                     (:flush sink)))

      :write (fn [self level format & rest]
               (each sink (self :named-sinks)
                 (:write sink level format ;rest)))

      :stats (fn [self]
               # TODO: implement average across all sinks?
               true)}
   *log-manager*))

(defn- make-methods
  [self]
  (each level ((self :parent) :levels)
        (put self level
          (fn [self format & rest]
            (:write self level format ;rest)))))

(def- combo-sink
  @{:new (fn [self parent init]
           (let [ret (table/setproto @{} *combo-sink*)]
             (put ret :parent parent)
             (put ret :name (init :name))
             (put ret :named-sinks @{})
             (let [{:rotation rot1 :flush-frequency ff1} init
                   {:rotation rot2 :flush-frequency ff2} parent]
               (when ff2 (put ret :flush-frequency ff2))
               (when ff1 (put ret :flush-frequency ff1))
               (when rot2 (put ret :rotation rot2))
               (when rot1 (put ret :rotation rot1)))
             (make-methods ret)
             ret))})

(registry/register-sink
 "Documentation goes here"
 combo-sink :combo)
