(use fugue)

# buffers
# ----------------------------------------------------------------------------

# %buffer ********************************************************************
(defproto %buffer ()
  data {})

(defmethod _init %buffer [self]
  (put self :data @[]))

(defmethod write %buffer [self entry]
  (with-slots %buffer self
    (array/push (@ data) (data entry))))

(defmethod flush %buffer [self stream]
  (each entry (data self)
    (write stream entry))
  (flush stream)
  (array/clear (data self)))

(defmethod clear %buffer [self]
  (with-slots %buffer self
    (set (@ lines) 0)
    (set (@ bytes) 0)
    (set (@ entries) 0)
    (array/clear (@ data))))
