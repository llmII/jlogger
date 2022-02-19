(use fugue)
(use logging/internal/errors)

# registry
# ----------------------------------------------------------------------------
# Here we'll register sinks, filters, drains, formatters, and namers, and
# eventually provide macros like `define-sink` and `define-*` that will define
# such things and register them simultaneously.
#
# We're mapping types to keywords for a few reasons:
#
#   1. Symbol lookup and type checking could be done but would make usage of
#   fugue non-optional for new implementations of types. This could/would be
#   done by checking a type derives from a certain type. We want users to
#   retain flexibility in this regard.
#
#   2. If symbol lookup and type checking is to be avoided, we need a way to
#   know about types available when creating such at runtime based on the
#   configuration.

(def- registry
  {:sinks      @{}
   :filters    @{}
   :drains     @{}
   :formatters @{}
   :namers     @{}})

# allow duplicate insertions when the value is the exact same, but otherwise,
# error.
(defn- put-nonexistent [ds kw v section]
  (if-let [proto (get ds kw)
           res   (not= proto v)]
    (errorf (registry-errors :registry-insertion-error) kw section)
    (put ds kw v)))

(defn- has-method? [proto keyword]
  (when-let [method (get proto keyword)
             test   (= :function (type method))]
    true))

(defn- has-methods? [proto & keywords]
  (reduce2
    |(or $0 (has-method? proto $1))
    keywords))

(defn- register [section keyword proto initializer & methods]
  (when (has-methods? proto ;methods)
    (put-nonexistent (registry section) keyword [proto initializer] section)))

(defn- new-register [sect & methods]
  (fn [keyword proto initializer]
    (register sect keyword proto initializer ;methods)))

(def register-sink      (new-register :sinks      :open :write :close :flush))
(def register-filter    (new-register :filters    :filter))
(def register-drain     (new-register :drains     :drain))
(def register-formatter (new-register :formatters :format))
(def register-namer     (new-register :namers     :new-name))

(def- registry-get-impl
  (partial get-in registry))

(def registry-get [sect kw]
  (registry-get-impl sect kw))

# sink-methods ***************************************************************
(defgeneric open
  `Sink method: opens the sink according to the name passed to it for
  writing.`
  [self name])

(defgeneric write
  "Sink method: writes an entry to the sink."
  [self entry])

(defgeneric close
  "Sink method: closes the sink, invalidating further writes to it."
  [self])

(defgeneric flush
  "Sink method: flushes sink to the underlying data storage mechanism."
  [self])

# filter-methods *************************************************************
(defgeneric filter
  `Filter method: elects whether raw entry data should be passed along or
  dropped according to the raw entry data given it. Should return a truthy
  value if the entry should continue on to the formatting stage and not be
  dropped.`
  [self data])

# drain-methods **************************************************************
(defgeneric drain
  `Drain method: instructs a log as to what to do in accordance to the method
  invoked upon the log. This should return an array of actions, with the last
  item in the array being a stat entry to determine how much data has been
  written and how much flushed. The possible actions would be :open, :write,
  :close, or flush, as keywords, which determines what the log will do to it's
  sink. The actions are processed in order, and more than a single action can
  be specified within the actions array. For instance, if the method were
  :close, it might make sense for the drain, the IO scheduler basically, were
  to return [:flush :close stat-entry], so as to make sure all data is written
  before closing the sink. A stat entry is like
  [:stat {:written integer :synced integer}] which tells how much has been
  written according to IO scheduling for this one call to drain, and how much
  should be flushed by the end of actions processing.`
  [self method entry])

# formatter-methods **********************************************************
(defgeneric format
  `Format method: takes a table of data and emits a buffer of bytes to stick
  into a log entry.`
  [self data])

# namer-methods **************************************************************
(defgeneric new-name
  `Namer method: generates a name for the sink that when the sink's open
  method is called, the sink can open whatever is named.

  This method would be useful for things like saying what network connection
  to open, or database, etc. For files, according to the included files
  implementation, it returns the appropriate path to open.`
  [self])

# Register builtins **********************************************************
(import logging/internal/sink :as "sinks")
(import logging/internal/filter :as "filters")
(import logging/internal/drain :as "drains")
(import logging/internal/formatter :as "formatters")
(import logging/internal/namer :as "namers")

# Sinks ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(register-sink :file    %file-sink    |(:new %file-sink $&))
(register-sink :console %console-sink |(:new %console-sink $&))

# Filters ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(register-filter :filter %filter |(:new %filter))

# Drains +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(register-drain :immediate %filter |(:new %immediate-drain $&))
(register-drain :common    %filter |(:new %common-drain $&))
# aliases; TODO: make these merge the init settings table with defaults
# appropriate for rotation/buffering
(register-drain :rotate    %filter |(:new %common-drain $&))
(register-drain :buffer    %filter |(:new %common-drain $&))

# Formatters +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(register-formatter :text %text-formatter |(:new %text-formatter $&))

# Namers  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(register-namer :file    %file-namer |(:new %file-namer $&))
(register-namer :console %console    |(:new %console-namer $&))

# Statistics per write +++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This is an open multi-method because we may need to add native types as well
# as things that are matchable
# Compares a full stat (including both the written and synced)
(declare-open-multi stat/compare)
# Adds just the written or the synced part of a stat
(declare-open-multi stat/add)
