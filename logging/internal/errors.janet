# errors
# ----------------------------------------------------------------------------

#  sink-errors ***************************************************************
(def sink-errors
  {:open-log-failed
   "Failed to open log <%s> for appending."
   :wrong-console
   "Cannot open console of type <%s> please try :stderr or :stdout."})

(def drain-errors
  {:no-drains-combo-drain
   "Attempt to initialize a combination drain without any drains."})

(def filter-errors
  {:disallowed-form
   (string
     "An attempt was made to create a filter with a disallowed form.\n\t"
     "Disallowed forms: %n\n\t"
     "Form:\n%s\n")})

(def registry-errors
  {:registry-insertion-error
   "Duplicated insertion of %s in registry %s"})
