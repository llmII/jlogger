(use logging/misc)

(defn- format-default
  [sink-type name level format & rest]
  (string/format (string/format "[%s/%-8s%s] %s\n"
                   name
                   (string/ascii-upper level)
                   (datestr (os/date))
                   format)
    ;rest))


(defn default
  ```
  Default format generator, used by a log manager, whose sinks use it to
  create a format function per level, customized to their sink type, with
  their log name already built into it.
  ```
  [log-name log-type levels]
  (let [ret @{}]
# Commented out section left for example purposes
    (each level levels
          (put ret level
            (partial format-default log-type log-name (string level))))
    (put ret :default (partial format-default log-type log-name "unknown"))
    ret))
