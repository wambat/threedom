(ns threedom.core)

(defn construct [klass & args]
  (clojure.pprint/pprint "Constructing")
  (clojure.pprint/pprint klass)
  (clojure.pprint/pprint args)
  (clojure.lang.Reflector/invokeConstructor klass (into-array Object args)))

(defn find-method [klass name pklass]
  (try
    (if-let [m (.getMethod klass name (into-array [pklass]))]
      m)
    (catch Exception e
      (do
        (if-let [spklass (.getSuperclass pklass)]
          (if (not= klass spklass)
            (find-method klass name spklass)))))))

(defprotocol IRender
  (render [this]))

(defn make-node! [owner klass konstructor & [setters children]]
  (clojure.pprint/pprint "Make node:")
  (clojure.pprint/pprint klass)
  (clojure.pprint/pprint "Konstructor:")
  (clojure.pprint/pprint konstructor)
  (let [node (apply construct klass konstructor)]
    (doseq [[meth val] (into [] setters)]
      (let [s (str "set" (name meth))
            v (apply construct (first val) (second val))
            met (find-method (.getClass node) s (.getClass v))]
        (clojure.pprint/pprint "Invoke")
        (clojure.pprint/pprint s)
        (clojure.pprint/pprint met)
        (clojure.pprint/pprint "params")
        (clojure.pprint/pprint v)
        (clojure.pprint/pprint "on")
        (clojure.pprint/pprint node)
        (.invoke met node (to-array [v]))
        (clojure.pprint/pprint "S")
        ))
    (clojure.pprint/pprint "attach")
    (clojure.pprint/pprint owner)
    (clojure.pprint/pprint node)
    (.attachChild owner node)

    (doseq [c children]
      (apply make-node! node c)
      )))

(defn materialize-diffs [cm owner old-state new-state options]
  (clojure.pprint/pprint "State from:")
  (clojure.pprint/pprint old-state)
  (clojure.pprint/pprint "State to:")
  (clojure.pprint/pprint new-state)
  (doseq [i new-state]
    (apply make-node! owner i)))

(defn handle-diffs
  ;; ([f target state]
  ;;  (materialize-diffs target nil (f state)))
  ([f owner old-state new-state options]
   (let [cm-old (f old-state owner options)
         cm-new (f new-state owner options)
         r-old-state (if old-state
                      (render cm-old)
                      nil)
         r-new-state (render cm-new)]
     (materialize-diffs cm-new owner r-old-state r-new-state options))))

(defn root
  "Mount rendering loop on node"
  [f value {:keys [target] :as options}]

  (add-watch value :watcher
             (fn [key atm old-state new-state]
               (handle-diffs f target old-state new-state options)))
  (handle-diffs f target nil @value options)
  ) 
