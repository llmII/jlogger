(use logging/misc)

(defn default
  [level name]
  (string/format "%s.%s.%s.log"
                 (string/replace " " "-" (datestr (os/date)))
                 name
                 (string level)))
