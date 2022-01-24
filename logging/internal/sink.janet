(use fugue)
(use logging/errors)

# sinks
# ----------------------------------------------------------------------------

# %sink **********************************************************************
(defproto %sink ()
  sink {})

(defmethod write %sink [self entry]
  (:write (sink self) (data entry)))

(defmethod flush %sink [self]
  (:flush (sink self)))

(defmethod close %sink [self]
  (:close (sink self)))

(defmethod open %sink [self data])

# %buffered-sink *************************************************************
# Wraps any sink type and makes it buffered until a flush is issued against
# it.
(defproto %buffered-sink %sink
  sink   {init? true}
  buffer {})

(defpmethod _init %buffered-sink [self]
  (put self :buffer (:new %buffer)))

(defmethod write %buffered-sink [self entry]
  (write (buffer self) entry))

(defmethod flush %buffered-sink [self]
  (flush (buffer self) (sink self)))

(defmethod close %buffered-sink [self]
  (close (sink self)))

(defmethod open %buffered-sink [self & rest]
  (open (sink self) ;rest))

# %file-sink *****************************************************************
(defproto %file-sink %sink)

(defmethod open %file-sink [self file-name]
  (with-slots %file-sink self
    (if-let [f (file/open (@ file-name) :w+n)]
      (set (@ sink) f)
      (errorf (sink-errors :open-log-failed) file-name))))

(defmethod close %file-sink [self]
  (with-slots %file-sink self
    # guard against multiple close
    (when (= :core/file (type (sink self)))
      (:close (sink self))
      (set (@ sink) nil))))

# %console-sink **************************************************************
(defproto %console-sink %sink
  which {})

(defmethod open %console-sink [self which]
  (with-slots %console-sink [self]
    (match which
      :stderr (set (@ sink) stderr)
      :stdout (set (@ sink) stdout)
      _       (errorf (sink-errors :wrong-console) (@ which)))))

(defmethod close %console-sink [self])
