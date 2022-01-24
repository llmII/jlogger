(use fugue)
(use sys)
(use logging/entry)

(defproto %formatter ()
  state    {}
  settings {})

(defmethod format %formatter [self data]
  (string/format "%m" data))


# expects in settings:
#   format
#
# A format can specify any of the data fields referenced below in the format
# method along with a description of how to format them.
#
# In general it would be like:
# [:date {:format "strftime format here"}]
#
# Default order and format literals:
#   [section/level date] formatted [tags]\nrest\n
(defproto %text-formatter ()
  settings {:init? true})

(def default-text-format
  [                                        "["
   [:section   {:format "%s"}]             "/"
   [:level     {:format "%-10s"}]          " "
   [:date      {:format "%a %b %d %X %Y"}] "]"
   [:formatted {:format "%s"}]             " "
   [:tags      {:separator " "
                :prefix    "#"
                :lead      "["
                :tail      "]"}]           "\n"
   [:rest      {:format "%m"}]             "\n"])

(defn- process-format [entry field buf]
  (match field
    [:section   {:format format}] (buffer/format buf format (entry :section))
    [:level     {:format format}] (buffer/format buf format (entry :level))
    [:date      {:format format}] (buffer/push
                                    buf (date-string format (entry :date)))
    [:formatted {:format format}] (when (entry :format)
                                    (buffer/format
                                      buf format (string/format
                                                   (:entry format)
                                                   (:entry args))))
    [:rest      {:format format}] (when (entry :rest)
                                    (buffer/format buf format (entry :rest)))
    [:tags      format] (when (entry :tags)
                          (when (format :lead)
                            (buffer/push buf (format :lead)))
                          (each tag (entry :tags)
                            (buffer/push buf (format :prefix))
                            (buffer/format buf "%V" tag)
                            (buffer/push buf (format :separator)))
                          (buffer/popn buf (length (format :separator)))
                          (when (format :tail)
                            (buffer/push (format :tail))))
    _ ""))

# expects in data:
#   field   required?           default format
#   date    required            "%a %b %d %X %Y"
#   level   required            "%-10s"
#   section required            "%s" (normally won't want to change this)
#   format  optional (is nil)   none - is a format string to which args will
#                               be applied
#   args    optional (is nil)   none, is consumed by format
#   tags    optional (is nil)   {:separator " "
#                                :prefix    "#"}
#   rest    optional (is nil)   "%m"
(defmethod format %text-formatter [self data]
  (let [ret   @""]
    (each fp ((settings self) :format)
      (match (type fp)
        :string (buffer/push ret fp)
        :tuple  (buffer/push (process-format data fp ret))))
    (:new %text-entry ret)))
