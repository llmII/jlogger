(use fugue)

# filters
# ----------------------------------------------------------------------------

# Allow these functions/macros to be referenced by filter bodies.
(def- filter-env
  (reduce
    |(put $0 $1 (dyn $1))
    @{}
    '(= < <= >= not=
        # allow polymorphic comparison:
        #   compare= compare< compare<= compare> compare>=
        compare= compare< compare<= compare> compare>=
        # allow deep comparison:
        #   deep= deep-not=
        deep= deep-not=
        # misc allow:
        #   nil? truthy? true? false? and or not
        nil? truthy? true? false? and or not)))

# unsafe-forms:
#   possibly block the special forms `while` and `fn` as it's possible to
#   infinite loop with these, also, `upscope`
(def- unsafe-forms
  (reduce
    |(put $0 $1 true)
    @{}
    #'(fn while upscope)
    '()))

(defn- tnop [&] true)

(defn- is-form? [list]
  (and (tuple? list)
       (= (tuple/type list) :parens)))

(defn- valid-needle? [needle]
  (or (function? needle) (dictionary? needle)))

(defn- occurs-impl [haystack needle interest?]
  (label res
    (each strand haystack
      (when (and (interest? strand)
                 (needle    strand))
        (return res strand))
      (when-let [ret (and (indexed? strand)
                          (occurs-impl strand needle interest?))]
        (return res ret)))))

(defn- occurs [haystack needle &opt interest?]
  (default interest? tnop)
  (and (valid-needle? needle)
       (occurs-impl form needle interest?)))

(defn- unsafe? [form]
  (occurs form unsafe-forms is-form?))

# %filter ********************************************************************
(defproto %filter ()
  settings  {:init? true}
  filter-fn {})

(defmethod _init %filter [self]
  (with-slots %filter self
    (let [body ((@ settings) :body)]
      (if (unsafe? body)
        (errorf (filter-errors :disallowed-form) blacklisted body)
        (set (@ filter-fn)
             ((compile
                ~(fn [date section level format args tags props rest]
                   ,;body)
                filter-env)))))))

(defmethod filter %filter [self data]
  (let [{:section section
         :level   level
         :date    date
         :format  format
         :args    args
         :tags    tags
         :props   props
         :rest    rest} data]
    ((filter-fn self) date section level format args tags props rest)))

# default-filter *************************************************************
(def default-filter (:new %filter '(true)))
