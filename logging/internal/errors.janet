# errors
# ----------------------------------------------------------------------------

#  sink-errors ***************************************************************
(def- sink-errors
  @{:open-log-failed
    "Failed to open log <%s> for appending."
    :wrong-console
    "Cannot open console of type <%s> please try :stderr or :stdout."})
