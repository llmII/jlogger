(use logging/registry-internal)

(defn register-sink
  "Registers a sink `sink` with documentation `doc` assigned to a key `key`."
  [doc sink key]
  # TODO: use the documentation provided to tag onto the log-manager
  # documentation about sinks...
  (put (reg :sinks) key sink))
