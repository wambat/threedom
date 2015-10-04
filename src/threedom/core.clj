(ns threedom.core
  (:require [clojure.data :as cd]
            [clojure.set :as cs]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :as sp])
  (:import [com.jme3 app.SimpleApplication
            material.Material
            material.RenderState
            material.RenderState$BlendMode
            light.DirectionalLight
            scene.Geometry
            scene.Spatial
            system.AppSettings
            system.JmeSystem
            util.SkyFactory
            renderer.queue.RenderQueue$Bucket
            scene.shape.Box
            scene.Node
            math.Vector3f
            math.ColorRGBA]))

(defn construct [klass & args]
  ;; (clojure.pprint/pprint "Constructing")
  ;; (clojure.pprint/pprint klass)
  ;; (clojure.pprint/pprint args)
  (clojure.lang.Reflector/invokeConstructor klass (into-array Object args)))

(defn find-method [klass name pklasses]
  ;; (clojure.pprint/pprint "Find method")
  ;; (clojure.pprint/pprint klass)
  ;; (clojure.pprint/pprint name)
  ;; (clojure.pprint/pprint pklasses)
  (try
    (if-let [m (if (> (count pklasses) 0)
                 (.getMethod klass name (into-array pklasses))
                 (.getMethod klass name nil))]
      m)
    (catch Exception e
      (do
        (if-let [spklasses (concat (butlast pklasses) [(.getSuperclass (last pklasses))])
                 ;;(map #(.getSuperclass %) pklasses)
                 ]
          (if (not= pklasses spklasses)
            (find-method klass name spklasses)))))))

(defprotocol IRender
  (render [this]))

(defn make-node [klass konstructor & [setters children]]
  ;; (clojure.pprint/pprint "Make node:")
  ;; (clojure.pprint/pprint klass)
  ;; (clojure.pprint/pprint "Konstructor:")
  ;; (clojure.pprint/pprint konstructor)
  (let [node (apply construct klass konstructor)]
    (doseq [[meth vals] (into [] setters)]
      (let [s (name meth)
            vs (map #(if (and
                          (sequential? %)
                          (>= (count %) 2))
                       (apply make-node %)
                       %) vals)
            met (find-method (.getClass node) s (map #(.getClass %) vs))]
        ;; (clojure.pprint/pprint "Invoke")
        ;; (clojure.pprint/pprint s)
        ;; (clojure.pprint/pprint met)
        ;; (clojure.pprint/pprint "params")
        ;; (clojure.pprint/pprint vals)
        ;; (clojure.pprint/pprint vs)
        ;; (clojure.pprint/pprint "on")
        ;; (clojure.pprint/pprint node)
        (.invoke met node (to-array vs))
        ;; (clojure.pprint/pprint "S")
        ))
    (doseq [c children]
      (let [cnode (apply make-node c)]
        (if (instance? Spatial cnode)
          (.attachChild node cnode)
          (.addLight node cnode))))
    node))

(defn materialize-diffs [cm owner old-state new-state options]
  ;; (clojure.pprint/pprint "State from:")
  ;; (clojure.pprint/pprint old-state)
  ;; (clojure.pprint/pprint "State to:")
  ;; (clojure.pprint/pprint new-state)
  (doseq [i new-state]
    (let [node (apply make-node i)]
      (if (instance? Spatial node)
        (.attachChild owner node)
        (.addLight owner node)))))

(defn materialize-node-diffs! [node old-state new-state]
  (let [s-old (into #{} (map #(take 2 %) old-state))
        s-new (into #{} (map #(take 2 %) new-state))
        s-to-delete (cs/difference s-old s-new)
        s-to-add (cs/difference s-new s-old)
        ;; to-delete (sp/select [sp/ALL (sp/collect-one) (sp/srange 0 2) #(not (contains? s-new %))]
        ;;            old-state)
        to-create (sp/select [sp/ALL #(not (contains? s-old (take 2 %)))]
                                new-state)
        to-update-old (sp/select [sp/ALL #(contains? s-new (take 2 %))]
                                    old-state)
        to-update-new (sp/select [sp/ALL #(contains? s-old (take 2 %))]
                                    new-state)]
    (pprint "ZCREATE")
    (pprint (sp/select [sp/ALL #(not (contains? s-old (take 2 %)))]
                       new-state))
    ;; (pprint to-update-old)
    ;; (pprint to-update-new)
    {:s-old s-old
     :s-new s-new
     :to-update-old to-update-old
     :to-update-new to-update-new
     :to-delete s-to-delete
     :to-create to-create
     :zchildren (mapv #(materialize-node-diffs! node
                                               (try
                                                 (nth %1 3)
                                                 (catch java.lang.IndexOutOfBoundsException _
                                                   []))
                                               (try
                                                 (nth %2 3)
                                                 (catch java.lang.IndexOutOfBoundsException _
                                                   [])))
                     to-update-old
                     to-update-new)}))

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

     (clojure.pprint/pprint "Move")
     (if r-old-state
       (do
         (clojure.pprint/pprint "Transition")
         (let [d (cd/diff r-old-state r-new-state)]
           (clojure.pprint/pprint "Old")
           (clojure.pprint/pprint r-old-state)
           (clojure.pprint/pprint "New")
           (clojure.pprint/pprint r-new-state)
           ))
       (materialize-diffs cm-new owner r-old-state r-new-state options)))))

(defn root
  "Mount rendering loop on node"
  [f value {:keys [target] :as options}]

  (add-watch value :watcher
             (fn [key atm old-state new-state]
               (handle-diffs f target old-state new-state options)
               ))
  (handle-diffs f target nil @value options)
  )
