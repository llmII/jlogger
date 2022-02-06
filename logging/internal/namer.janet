(use fugue)

# namers
# ----------------------------------------------------------------------------

# %namer-base ****************************************************************
(defproto %namer-base ()
  settings {:init? true})

(defmethod new-name %namer-base [self])

# %console-namer *************************************************************
(defproto %console-namer %namer-base)

(defmethod new-name %console-namer [self]
  ((settings self) :which))

# %file-namer ****************************************************************
(defproto %file-namer %namer-base
  name          {}
  section       {}
  level         {}
  incrementing? {:default false}
  suffix        {:default ".log"}
  calls         {:default 0})

(defn- process-format [self settings field buf]
  (with-slots %file-name self
    (match field
      [:date    format] (buffer/push buf (date-string format (os/date)))
      [:name    format] (buffer/format buf format (@ name))
      [:section format] (buffer/format buf format (@ section))
      [:level   format] (buffer/format buf format (@ level))
      [:suffix  format] (buffer/format buf format (@ suffix))
      [:inc     format] (when (incrementing? self)
                          (buffer/format buf format (++ (@ calls)))))))

(defmethod new-name %file-namer [self]
  (with-slots %file-namer self
    (let [ret @""
          ord ((@ settings) :format)]
      (when ((@ settings) :dir)
        (buffer/push ret ((@ settings) :dir) "/"))
      (each fp ord
        (match (type fp)
          :string (buffer/push ret fp)
          :tuple  (buffer/push (process-format self (@ settings) fp ret))))
      ret)))

(def default-file-name-format
  [[:date    "%F"]
   [:name    "%s"]
   [:section "%s"]
   [:suffix  "%s"]])
