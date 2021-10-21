(use logging/misc)

# errors
# ----------------------------------------------------------------------------
(def- sink-errors
  @{:open-log-failed
    "Failed to open log <%s> for appending."
    :wrong-console
    "Cannot open console of type <%s> please try :stderr or :stdout."})

# sinks
# ----------------------------------------------------------------------------
# %sink ######################################################################
# output stream type that all streams derive from
(defproto %sink ()
  sink {}
  last-written {}
  runtime {})

(defmethod _init %sink [self]
  (with-slots %sink self
    (set (@ last-written) (os/time))
    (set (@ runtime) (os/date))))

(defmethod write %sink [self]
  (:write (sink self) data))

(defmethod flush %sink [self]
  (:flush (sink self)))

(defmethod close %sink [self]
  (:close (sink self)))

(defmethod open %sink [self data])
# end %sink ##################################################################

# %file-sink #################################################################
(defproto %file-sink %sink)

(defmethod open %file-sink [self file-name]
  (close self)
  (with-slots %file-sink self
    (if-let [f (file/open (@ file-name) :w+n)]
      (set (@ sink) f)
      (errorf (sink-errors :open-log-failed) (@ file-name)))))

(defmethod close %file-sink [self]
  (with-slots %file-sink self
    (when (= :core/file (type (sink self)))
      (:close (sink self))
      (set (@ sink) false))))

# end %file-sink #############################################################

# %console-sink ##############################################################
(defproto %console-sink %sink
  which {:init? true})

(defmethod open %console-sink [self which]
  (with-slots %console-sink [self]
    (match (@ which)
      :stderr (set (@ sink) stderr)
      :stdout (set (@ sink) stdout)
      _       (errorf (sink-errors :wrong-console) (@ which)))))

(defmethod close %console-sink [self])
# end %console-sink ##########################################################
