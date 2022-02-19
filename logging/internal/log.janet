(use fugue)

# logs
# ----------------------------------------------------------------------------
# Defines a manageable logging object that besides writing into a sink may or
# may not have extra filtering mechanisms attached.

# %log-base ******************************************************************
(defproto %log-base ())
# minimum is going to be {:level :rest} for `rest` but could be more to it
(defmethod write %log-base [self level & rest])
(defmethod close %log-base [self])
(defmethod flush %log-base [self])

# %log ***********************************************************************
(defproto %log %log-base
  name      {:init? true} # used to identify each log for statistics purposes
  sink      {:init? true}
  filter    {:init? true}
  drain     {:init? true}
  formatter {:init? true}
  namer     {:init? true})

(defn- run-actions [self method &opt entry]
  (with-slots %log self
    (let [ret @{}]
      (each action (drain (@ drain) method entry)
        (match action
          :open   (open (@ sink) (new-name (@ namer)))
          :write  (write (@ sink) entry)
          :close  (close (@ sink))
          :flush  (flush (@ sink))

          [:stat {:written wrote :synced sunk}]
          (do
            (put (ret :written) wrote)
            (put (ret :synced) sunk))))
      ret)))

(defmethod _init %log [self]
  (with-slots %log self
    (each sink-spec (wrap-sink? drain)
      (let [[sink-type args] sink-spec]
        (set (@ sink) (:new sink-type ;[(@ sink) ;args]))))))

(defmethod write %log [self data]
  (with-slots %log self
    (when (not (filter (@ filter) data))
      (run-actions self :write (format (@ formatter) data)))))

(defmethod flush %log [self]
  (run-actions self :flush))

(defmethod close %log [self]
  (run-actions self :close))
