(use fugue)

(defproto %log-buffer ()
  bytes   {:default 0}
  lines   {:default 0}
  entries {:default 0}
  data    {})

(defmethod _init %log-buffer [self]
  (put self :data @[]))

(defmethod write %log-buffer [self entry]
  (with-slots %log-buffer self
    (+= (@ bytes) (bytes entry))
    (+= (@ lines) (lines entry))
    (++ (@ entries))
    (array/push (@ data) (data entry))))

(defmethod flush %log-buffer [self stream]
  (each entry (data self)
    (write stream entry))
  (flush stream)
  (clear self))

(defmethod clear %log-buffer [self]
  (with-slots %log-buffer self
    (set (@ lines) 0)
    (set (@ bytes) 0)
    (set (@ entries) 0)
    (array/clear (@ data))))
