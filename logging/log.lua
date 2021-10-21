(use fugue)
(use logging/sinks)
(use logging/buffer)

# log ########################################################################
# A log is a sink, such that as they require a sink so as to drain input into,
# they may also be used as the sink of a another log, for composition
# purposes.
(defproto %log %sink
  buffer         {}
  sink           {:init? true}
  filter         {:init? true}
  drain          {:init? true}
  formatter      {:init? true}
  name-formatter {:init? true}
  settings       {:init? true}
  state          {})

(defn- run-actions [self method &opt data]
  (each action (drain self)
    (with-slots %log self
      (match action
        [:open true]     (open sink (name-formatter self))
        [:close true]    (close sink)
        [:write :buffer] (write x (formatter self data))
        [:write :sink]   (flush buffer sink)))))

(defmethod _init %log [self]
  (with-slots %log self
    (set (@ buffer) (:new buffer))
    (set (@ state) @{})))

(defmethod write %log [self data]
  (when (not (filter data))
    (let [{:level lvl :context ctx :data d} data]
      (run-actions self :write data))))

(defmethod flush %log [self]
  (run-actions self :flush))

(defmethod close %log [self]
  (run-actions self :close))
# end log ####################################################################
