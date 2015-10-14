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
(def last-obj-tree (atom []))
(declare make-node)
(declare materialize-node-diffs!)
(declare materialize-component-diffs!)
(declare materialize-diffs!)

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
  ;; (dbg "Slow Find method")
  ;; (dbg klass)
  ;; (dbg name)
  ;; (dbg pklasses)
  (let [methods (.getMethods klass)
        fmethods (filter #(= (.getName %) name) methods)]
    (if (= (count fmethods) 1)
      (first fmethods)
      nil;;(throw (ex-info "No method found" {:name name :klass klass}))
      )))

(defn find-method [klass name pklasses]
  ;; (dbg "Find method")
  ;; (dbg klass)
  ;; (dbg name)
  ;; (dbg pklasses)
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
  ;; (dbg "GetChild")
  ;; (dbg name)
  (.getChild node (first name)))

(defn set-node-props! [node setters]
  (doseq [[meth vals] (into [] setters)]
    (try
      (let [s (name meth)
            vs (map #(if (and
                          (sequential? %)
                          (>= (count %) 2))
                       (:node (apply make-node %))
                       %) vals)
            met (find-method (.getClass node) s (map #(.getClass %) vs))]
        (.invoke met node (to-array vs)))
      (catch Exception e
        (throw
         (ex-info "Set Node Props err" {:node node
                                        :setters setters}))))))

(defn make-component! [owner cm data options]
  (let [c (cm data owner options)]
    (materialize-node-diffs! cm owner [] (render c))))

(defn make-node [klass konstructor & [setters children]]
  ;; (dbg "Make node:")
  ;; (dbg klass)
  ;; (dbg "Konstructor:")
  ;; (dbg konstructor)
  (let [node (apply construct klass konstructor)]
    (set-node-props! node setters)
    {:obj-tree [klass konstructor setters (if-not (empty? children)
                                            (materialize-diffs! node [] children))]
     :node node}))

(defn detach-component! [node old-cm]
  ;; (dbg "To delete CMP>>>")
  ;; (dbg old-cm)
  (let [[f data options ] old-cm
        rf (f data node options)]
    (materialize-node-diffs! rf
                             node
                             (render rf) [])))

(defn detach-node! [node klass name]
  (.detachChildNamed node (first name)))


(defn make-nodes! [owner state]
  ;; (dbg "State from:")
  ;; (dbg old-state)
  ;; (dbg "State to:")
  ;; (dbg new-state)
  (mapv (fn [i]
          (let [{:keys [node obj-tree]} (apply make-node i)]
            (if (instance? Spatial node)
              (.attachChild owner node)
              (.addLight owner node))

            (dbg "State to:")
            (dbg obj-tree)
            obj-tree))
        state))

(defn update-props! [node old-state new-state]
  ;; (dbg "State from:")
  ;; (dbg old-state)
  ;; (dbg "State to:")
  ;; (dbg new-state)
  (let [[_ props-to-update _] (cd/diff old-state new-state)
        pkeys (keys props-to-update)]
    (set-node-props! node (select-keys new-state pkeys))))

(defn materialize-diffs! [owner old-obj-tree obj-tree]
  ;; (dbg "Mat.Diff")
  ;; (dbg obj-tree)
  (concat
   (materialize-node-diffs!
    owner
    (remove #(get (meta %) :component)
            old-obj-tree)
    (remove #(get (meta %) :component)
            obj-tree))
   (materialize-component-diffs!
    owner
    (filter #(get (meta %) :component) old-obj-tree)
    (filter #(get (meta %) :component) obj-tree))))

(defn materialize-component-diffs! [owner old-obj-tree comps]
  ;; (dbg "Mat.Diff")
  ;; (dbg comps)
  (letfn [(inflate [component data options]
            ;; (dbg "Inflate")
            ;; (dbg component)
            ;; (dbg data)
            ;; (dbg options)
            (let [c (component data owner options)]
               ;; (dbg "CMP")
               ;; (dbg component)
               (with-meta (render c) {:component component})))]
    (materialize-node-diffs! owner old-obj-tree (map (fn [[component data options]]
                                                        (inflate
                                                         component
                                                         data
                                                         options)) comps))))

(defn materialize-node-diffs! [owner old-obj-tree obj-tree]
  ;; (dbg "TREE")
  ;; (dbg obj-tree)
  (let [s-old (into #{} (map #(take 2 %) old-obj-tree))
        s-new (into #{} (map #(take 2 %) obj-tree))
        s-to-delete (cs/difference s-old s-new)
        s-to-add (cs/difference s-new s-old)
        to-delete (sp/select [sp/ALL (sp/collect-one) (sp/srange 0 2) #(not (contains? s-new %))]
                   old-obj-tree)
        to-create (sp/select [sp/ALL #(not (contains? s-old (take 2 %)))]
                                obj-tree)
        ;;TODO: This should be sorted out by some key
        to-update-old (sp/select [sp/ALL #(contains? s-new (take 2 %))]
                                    old-obj-tree)
        to-update-new (sp/select [sp/ALL #(contains? s-old (take 2 %))]
                                    obj-tree)]
    (doseq [d s-to-delete]
      (apply detach-node! owner d))
    (let [created-obj-tree (concat (make-nodes! owner to-create)
                                   (map (fn [o n]
                                          (update-props! (get-child-by-name owner (get o 1 "NOT_FOUND"))
                                                         (get o 2 {})
                                                         (get n 2 {}))
                                          (materialize-diffs!
                                           (get-child-by-name owner (get o 1 "NOT_FOUND"))
                                           (get o 3 [])
                                           (get n 3 [])))
                                        to-update-old
                                        to-update-new))]
      ;; (dbg "Created")
      ;; (dbg created-obj-tree)
      created-obj-tree)))

(defn build [cm data options]
  (let [r (with-meta [cm data options] {:component true})]
    ;; (dbg "built")
    ;; (dbg r)
    r)) 

(defn build-root-component!
  ;; ([f target state]
  ;;  (materialize-diffs target nil (f state)))
  ([component owner old-obj-tree data options]
   ;; (dbg "Rebuild")
   (materialize-diffs! owner old-obj-tree [(build component data options)])))

(defn root
  "Mount rendering loop on node"
  [component state-atom {:keys [target] :as options}]
  (remove-watch state-atom :watcher)
  (comment add-watch state-atom :watcher
             (fn [key atm old-state new-state]
               (>!! update-chan [component target @last-obj-tree new-state options])
               ;;(handle-diffs f target old-state new-state options)
               ))
  (>!! update-chan [component target [] @state-atom options])
  ;;(handle-diffs f target nil @value options)
  )

(defn process-state-updates [app tpf]
  (if-let [vals (poll! update-chan)]
    (reset! last-obj-tree (apply build-root-component! vals))))
