(ns threedom.core
  (:require [clojure.data :as cd]
            [clojure.set :as cs]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :refer [mult poll! go tap untap chan alt! >!! <!! filter> onto-chan pipe close!] :as async]
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

(declare make-node)
(declare materialize-node-diffs!)

(defn dbg [v]
  (clojure.pprint/pprint v)
  )
(def update-chan (chan 1))

(defn construct [klass & args]
  ;; (dbg "Constructing")
  ;; (dbg klass)
  ;; (dbg args)
  (clojure.lang.Reflector/invokeConstructor klass (into-array Object args)))

(defn slow-find-method [klass name pklasses]
  (dbg "Slow Find method")
  (dbg klass)
  (dbg name)
  (dbg pklasses)
  (let [methods (.getMethods klass)
        fmethods (filter #(= (.getName %) name) methods)]
    (if (= (count fmethods) 1)
      (first fmethods)
      nil;;(throw (ex-info "No method found" {:name name :klass klass}))
      )))

(defn find-method [klass name pklasses]
  (dbg "Find method")
  (dbg klass)
  (dbg name)
  (dbg pklasses)
  (if-let [s (slow-find-method klass name pklasses)]
    s
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
              (find-method klass name spklasses))))))))

(defprotocol IRender
  (render [this]))

(defn get-child-by-name [node name]
  (dbg "GetChild")
  (dbg name)
  (.getChild node (first name)))

(defn set-node-props! [node setters]
  (doseq [[meth vals] (into [] setters)]
    (dbg "Meth")
    (dbg meth)
    (dbg "Vals")
    (dbg vals)
    (let [s (name meth)
          vs (map #(if (and
                        (sequential? %)
                        (>= (count %) 2))
                     (apply make-node %)
                     %) vals)
          met (find-method (.getClass node) s (map #(.getClass %) vs))]
      (.invoke met node (to-array vs)))))

(defn make-component! [owner cm data options]
  (let [c (cm data owner options)]
    (materialize-node-diffs! cm owner [] (render c))))

(defn make-node [klass konstructor & [setters children]]
  (dbg "Make node:")
  (dbg klass)
  (dbg "Konstructor:")
  (dbg konstructor)
  (let [node (apply construct klass konstructor)]
    (set-node-props! node setters)
    (doseq [c children]
      (if (get (meta c) :component)
        (apply make-component! node c)
        (let [cnode (apply make-node c)]
          (if (instance? Spatial cnode)
            (.attachChild node cnode)
            (.addLight node cnode))))) node))

(defn detach-component! [node old-cm]
  (dbg "To delete CMP>>>")
  (dbg old-cm)
  (let [[f data options ] old-cm
        rf (f data node options)]
    (materialize-node-diffs! rf
                             node
                             (render rf) [])))

(defn detach-node! [node klass name]
  (.detachChildNamed node (first name)))


(defn make-nodes! [cm owner state]
  ;; (dbg "State from:")
  ;; (dbg old-state)
  ;; (dbg "State to:")
  ;; (dbg new-state)
  (doseq [i state]
    (if (get (meta i) :component)
      (apply make-component! owner i)
      (let [node (apply make-node i)]
        (if (instance? Spatial node)
          (.attachChild owner node)
          (.addLight owner node))))))

(defn update-props! [node old-state new-state]
  (dbg "State from:")
  (dbg old-state)
  (dbg "State to:")
  (dbg new-state)
  (let [[_ props-to-update _] (cd/diff old-state new-state)
        pkeys (keys props-to-update)]
    (set-node-props! node (select-keys new-state pkeys))))


(defn update-component! [node old-cm new-cm]
  (let [[f data options ] old-cm
        [f' data' options'] new-cm
        rf (f data node options)
        rf' (f' data' node options')]
    (materialize-node-diffs! rf'
                             node
                             (render rf)
                             (render rf'))))

(defn materialize-node-diffs! [cm node old-state new-state]
  (let [s-old (into #{} (map #(take 2 %) old-state))
        s-new (into #{} (map #(take 2 %) new-state))
        s-to-delete (cs/difference s-old s-new)
        s-to-add (cs/difference s-new s-old)
        to-delete (sp/select [sp/ALL (sp/collect-one) (sp/srange 0 2) #(not (contains? s-new %))]
                   old-state)
        to-create (sp/select [sp/ALL #(not (contains? s-old (take 2 %)))]
                                new-state)
        to-update-old-children (sp/select [sp/ALL #(contains? s-new (take 2 %))]
                                    old-state)
        to-update-new-children (sp/select [sp/ALL #(contains? s-old (take 2 %))]
                                    new-state)]
    (doseq [d s-to-delete]
      (if (get (meta d) :component)
        (detach-component! node d)
        (apply detach-node! node d)))

    (doall (mapv (fn [o n] (if (get (meta n) :component)
                            (do
                              (dbg "I'm compomnent")
                              (dbg n)
                              (update-component! node o n))
                            (do
                              (dbg "I'm node")
                              (dbg n)
                              (update-props! (get-child-by-name node (get o 1 "NOT_FOUND"))
                                              (get o 2 {})
                                              (get n 2 {})))))
                 to-update-old-children
                 to-update-new-children))
    (make-nodes! cm node to-create)
    (doall (mapv #(materialize-node-diffs!
                   cm
                   (get-child-by-name node (get %1 1 "NOT_FOUND"))
                   (get %1 3 [])
                   (get %2 3 []))
                 to-update-old-children
                 to-update-new-children))))

(defn build [cm data options]
  (with-meta [cm data options] {:component true}))

(defn handle-diffs
  ;; ([f target state]
  ;;  (materialize-diffs target nil (f state)))
  ([f owner old-state new-state options]
   (let [cm-old (f old-state owner options)
         cm-new (f new-state owner options)
         r-old-state (if old-state
                      (render cm-old)
                      [])
         r-new-state (render cm-new)]
     (dbg "Rebuild")
     (materialize-node-diffs! cm-new owner r-old-state r-new-state))))

(defn root
  "Mount rendering loop on node"
  [f value {:keys [target] :as options}]
  (remove-watch value :watcher)
  (add-watch value :watcher
             (fn [key atm old-state new-state]
               (>!! update-chan [f target old-state new-state options])
               ;;(handle-diffs f target old-state new-state options)
               ))
  (>!! update-chan [f target nil @value options])
  ;;(handle-diffs f target nil @value options)
  )

(defn process-state-updates [app tpf]
  (if-let [vals (poll! update-chan)]
    (apply handle-diffs vals)))
