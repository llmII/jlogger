(use fugue)
(use sys)

# entries
# ----------------------------------------------------------------------------

# %entry *********************************************************************
(defproto %log-entry ()
  bytes {}
  lines {}
  data  {})

# %text-entry ****************************************************************
(defproto %text-entry %log-entry
  data {:init? true})

(defmethod _init %text-entry [self]
  (with-slots %text-entry self
    (set (@ lines) (length (string/find-all "\n" (@ data))))
    (set (@ bytes) (length (@ data)))))
