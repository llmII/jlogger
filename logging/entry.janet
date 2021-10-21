(use fugue)

(defproto %log-entry ()
  bytes {:init? true}
  lines {:init? true}
  data  {:init? true})
