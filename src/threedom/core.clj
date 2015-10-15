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

(defn dbgm [obj]
  (let [orig-dispatch clojure.pprint/*print-pprint-dispatch*]
    (clojure.pprint/with-pprint-dispatch 
      (fn [o]
        (when (meta o)
          (print "^")
          (orig-dispatch (meta o))
          (clojure.pprint/pprint-newline :fill))
        (orig-dispatch o))
      (clojure.pprint/pprint obj))))

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
            vs (mapv #(if (and
                          (sequential? %)
                          (>= (count %) 2))
                       (:node (make-node %))
                       %) vals)
            met (find-method (.getClass node) s (mapv #(.getClass %) vs))]
        (.invoke met node (to-array vs)))
      (catch NullPointerException e
        (throw
         (ex-info "Set Node Props err" {:node node
                                        :setters setters}))))))

(defn make-node [[klass konstructor setters children :as data]]

  ;; (dbg "Make node:")
  ;; (dbg klass)
  ;; (dbg "Konstructor:")
  ;; (dbg konstructor)

  (let [node (apply construct klass konstructor)
        m (meta data)]
    (set-node-props! node setters)
    {:obj-tree (with-meta [klass konstructor setters (if-not (empty? children)
                                                      (materialize-diffs! node [] children))]
                 m)
     :node node}))

(defn detach-node! [node descr]

  ;; (dbg "Detaching")
  ;; (dbg node)
  ;; (dbg "Descr")
  ;; (dbgm descr)
  (.detachChildNamed node (get-in descr [1 0])))


(defn make-nodes! [owner state]
  ;; (dbg "Make Nodes")
  ;; (dbg "State from:")
  ;; (dbg old-state)
  ;; (dbg "State to:")
  ;; (dbg new-state)
  (mapv (fn [i]
          (let [{:keys [node obj-tree]} (make-node i)]
            (if (instance? Spatial node)
              (.attachChild owner node)
              (.addLight owner node))
            ;; (dbg "Make Node!")
            ;; (dbgm obj-tree)
            ;; (dbg "From")
            ;; (dbgm i)
            obj-tree))
        state))

(defn update-props! [node old-state new-state]
  ;;(dbg "Update props")
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
  ;; (dbg old-obj-tree)
  ;; (dbg "New")
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
    (materialize-node-diffs! owner old-obj-tree (mapv (fn [[component data options]]
                                                        (inflate
                                                         component
                                                         data
                                                         options)) comps))))

(defn materialize-node-diffs! [owner old-obj-tree obj-tree]
  (let [s-old (into #{} (mapv #(take 2 %) old-obj-tree))
        s-new (into #{} (mapv #(take 2 %) obj-tree))
        s-to-delete (cs/difference s-old s-new)
        s-to-add (cs/difference s-new s-old)
        to-delete (sp/select [sp/ALL #(not (contains? s-new (take 2 %)))]
                   old-obj-tree)
        to-create (sp/select [sp/ALL #(not (contains? s-old (take 2 %)))]
                                obj-tree)
        ;;TODO: This should be sorted out by some key
        to-update-old (sp/select [sp/ALL #(contains? s-new (take 2 %))]
                                    old-obj-tree)
        to-update-new (sp/select [sp/ALL #(contains? s-old (take 2 %))]
                                    obj-tree)]

    ;; (dbg "old-obj-tree")
    ;; (dbg old-obj-tree)
    ;; (dbg "obj-tree")
    ;; (dbg obj-tree)
    ;; (dbg "s-old")
    ;; (dbg s-old)
    ;; (dbg "s-new")
    ;; (dbg s-new)
    ;; (dbg "S To delete")
    ;; (dbgm s-to-delete)
    ;; (dbg "To delete")
    ;; (dbgm to-delete)
    (doseq [d to-delete]
      (detach-node! owner d))
    (let [new-nodes-obj-tree (make-nodes! owner to-create)
          updated-nodes-obj-tree
          (mapv (fn [o n]
                  (update-props! (get-child-by-name owner (get o 1 "NOT_FOUND"))
                                 (get o 2 {})
                                 (get n 2 {}))
                  (let [old-c (get o 3 [])
                        new-c (get n 3 [])]
                    (if (or (not (empty? old-c))
                            (not (empty? new-c)))
                      (conj (into [] (take 3 n)) (materialize-diffs!
                                        (get-child-by-name owner (get o 1 "NOT_FOUND"))
                                        old-c
                                        new-c))
                      n)))
                to-update-old
                to-update-new)
          new-obj-tree (concat new-nodes-obj-tree updated-nodes-obj-tree)]
      ;; (dbg "Created")
      ;; (dbgm new-nodes-obj-tree)
      ;; (dbg "Updated")
      ;; (dbgm updated-nodes-obj-tree)
      new-obj-tree)))

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
  (add-watch state-atom :watcher
             (fn [key atm old-state new-state]
               (>!! update-chan [component target @last-obj-tree new-state options])
               ;;(handle-diffs f target old-state new-state options)
               ))
  (>!! update-chan [component target [] @state-atom options])
  ;;(handle-diffs f target nil @value options)
  )

(defn process-state-updates [app tpf]
  (if-let [vals (poll! update-chan)]
    (do
      (dbg ">>>>>>>>>>>>>")
      ;;(dbgm (with-meta {:yo "oy"} {:mmmet "aaa!"}))
      (dbgm (reset! last-obj-tree (apply build-root-component! vals))))))
