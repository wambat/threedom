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
(def cnt (atom 0))
(def last-obj-tree (atom []))
(declare make-node)
(declare materialize-diffs!)

(defn deebm [obj]
  (let [orig-dispatch clojure.pprint/*print-pprint-dispatch*]
    (clojure.pprint/with-pprint-dispatch 
      (fn [o]
        (when (meta o)
          (print "^")
          (orig-dispatch (meta o))
          (clojure.pprint/pprint-newline :fill))
        (orig-dispatch o))
      (clojure.pprint/pprint obj))))

(defn deeb [v]
  (clojure.pprint/pprint v)
  )
(def update-chan (chan 1))

(defn construct [klass & args]
  ;; (deeb "Constructing")
  ;; (deeb klass)
  ;; (deeb args)
  (clojure.lang.Reflector/invokeConstructor klass (into-array Object args)))

(defn- args-satisfies [args types]
  (and (= (count args)
          (count types))
       (every? (fn [[arg tp]]
                 (let [tp (if (= (.getName tp) "float")
                            java.lang.Float
                            tp)]
                   (instance? tp arg)))
               (map (fn [a t]
                      [a t]) args types))))

(defn find-method [klass name args]
  ;; (deeb "Slow Find method")
  ;; (deeb klass)
  ;; (deeb name)
  ;; (deeb "args")
  ;; (deeb args)
  (let [methods (.getMethods klass)
        fmethods (filter #(= (.getName %) name) methods)]
    ;; (deeb fmethods)
    (some #(if (do
                 ;; (deeb (.getParameterTypes %))
                 ;; (deeb (mapv (fn [a] (.getClass a)) args))
                 (args-satisfies args (.getParameterTypes %)))
             %
             nil) fmethods)
    ))

#_(defn find-method [klass name pklasses]
  ;; (deeb "Find method")
  ;; (deeb klass)
  ;; (deeb name)
  ;; (deeb pklasses)
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
  ;; (deeb "GetChild")
  ;; (deeb node)
  ;; (deeb name)
  (let [c (.getChild node (first name))]
    (if-not c
      (do
        (deeb ["No node" node (first name)])
        (deeb "BUT children:")
        (deeb (.getChildren node))))
    c))

(defn set-node-props! [node setters]
  ;; (throw (ex-info "Set Node Props NONODE" {:node node
  ;;                                          :setters setters}))
  (if (not (or (nil? node)
               (empty? setters)))
    (doseq [[meth vals] (into [] setters)]
      (let [s (name meth)
            vs (mapv #(if (and
                           (sequential? %)
                           (>= (count %) 2))
                        (:node (make-node %))
                        %) vals)
            met (find-method (.getClass node) s vs)]
        (try
          (.invoke met node (to-array vs))
          (catch NullPointerException e
            (throw
             (ex-info "Set Node Props NPE" {:node node
                                            :met met
                                            :params vs})))
          (catch IllegalArgumentException e
            (throw
             (ex-info "Set Node Props ARG" {:node node
                                            :met met
                                            :params vs}))))))
    [ "Set Node Props NONODE" {:node node
                                            :setters setters}]))

(defn make-node [[klass konstructor setters children :as data]]

  ;; (deeb "Make node:")
  ;; (deeb klass)
  ;; (deeb "Konstructor:")
  ;; (deeb konstructor)

  (let [node (apply construct klass konstructor)
        m (meta data)]
    (set-node-props! node setters)
    {:obj-tree (with-meta [klass konstructor setters (if-not (empty? children)
                                                      (materialize-diffs! node [] children))]
                 m)
     :node node}))

(defn detach-node! [node descr]

  ;; (deeb "Detaching")
  ;; (deeb node)
  ;; (deeb "Name")
  ;; (deeb (get-in descr [1 0]))
  ;; (deeb "Descr")
  ;; (deebm descr)
  (.detachChildNamed node (get-in descr [1 0])))


(defn make-nodes! [owner state]
  ;; (deeb "Make Nodes")
  ;; (deeb "State from:")
  ;; (deeb state)
  ;; (deeb "State to:")
  ;; (deeb new-state)
  (mapv (fn [i]
          (let [{:keys [node obj-tree]} (make-node i)]
            (if (instance? Spatial node)
              (.attachChild owner node)
              (.addLight owner node))
            ;; (deeb "Make Node!")
            ;; (deebm obj-tree)
            ;; (deeb "From")
            ;; (deebm i)
            obj-tree))
        state))

(defn update-props! [node old-state new-state]
  ;; (deeb "Update props")
  ;; (deeb "State from:")
  ;; (deeb old-state)
  ;; (deeb "State to:")
  ;; (deeb new-state)
  (let [[_ props-to-update _] (cd/diff old-state new-state)
        pkeys (keys props-to-update)]

    ;; (deeb "Update props")
    ;; (deeb node)
    ;; (deeb "setters")
    ;; (deeb (select-keys new-state pkeys))
    (set-node-props! node (select-keys new-state pkeys))))

(defn render-components [owner comps]
  ;; (deeb "Mat.Diff")
  ;; (deeb old-obj-tree)
  ;; (deeb "New")
  ;; (deeb comps)
  (letfn [(inflate [component data options]
            ;; (deeb "Inflate")
            ;; (deeb component)
            ;; (deeb data)
            ;; (deeb options)
            (let [c (component data owner options)]
               ;; (deeb "CMP")
               ;; (deeb component)
               (with-meta (render c) {:component component})))]
    (mapv (fn [[component data options :as q]]
            (if (:component (meta q))
              (inflate
               component
               data
               options)
              q)) comps)))

(defn materialize-diffs! [owner old-obj-tree unexpanded-obj-tree]
  (let [obj-tree (render-components owner unexpanded-obj-tree)
        s-old (into #{} (mapv #(take 2 %) old-obj-tree))
        s-new (into #{} (mapv #(take 2 %) obj-tree))
        s-to-delete (cs/difference s-old s-new)
        s-to-add (cs/difference s-new s-old)
        to-delete (sp/select [sp/ALL #(not (contains? s-new (take 2 %)))]
                   old-obj-tree)
        to-create (sp/select [sp/ALL #(not (contains? s-old (take 2 %)))]
                                obj-tree)
        ;;TODO: This should be sorted out by some key
        to-update-old (sort-by second (sp/select [sp/ALL #(contains? s-new (take 2 %))]
                                                 old-obj-tree))
        to-update-new (sort-by second (sp/select [sp/ALL #(contains? s-old (take 2 %))]
                                                 obj-tree))]

    ;; (deeb "old-obj-tree")
    ;; (deebm old-obj-tree)
    ;; (deeb "obj-tree")
    ;; (deebm obj-tree)
    ;; (deeb "s-old")
    ;; (deeb s-old)
    ;; (deeb "s-new")
    ;; (deeb s-new)
    ;; (deeb "S To delete")
    ;; (deebm s-to-delete)
    ;; (deeb "S To add")
    ;; (deebm s-to-add)
    ;; (deeb "To delete")
    ;; (deebm to-delete)
    (doseq [d to-delete]
      (detach-node! owner d))
    (let [new-nodes-obj-tree (make-nodes! owner to-create)
          updated-nodes-obj-tree
          (mapv (fn [o n]
                  ;; (deeb "Upd. Props.")
                  ;; (deeb o)
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
      ;; (deeb "Created")
      ;; (deebm new-nodes-obj-tree)
      ;; (deeb "Updated")
      ;; (deebm updated-nodes-obj-tree)
      new-obj-tree)))

(defn build [cm data options]
  (let [r (with-meta [cm data options] {:component true})]
    ;; (deeb "built")
    ;; (deeb r)
    r)) 

(defn build-root-component!
  ;; ([f target state]
  ;;  (materialize-diffs target nil (f state)))
  ([component owner old-obj-tree data options]
   ;; (deeb "Rebuild")
   (materialize-diffs! owner old-obj-tree [(build component data options)])))

(defn root
  "Mount rendering loop on node"
  [component state-atom {:keys [target] :as options}]
  (remove-watch state-atom :watcher)
  (add-watch state-atom :watcher
             (fn [key atm old-state new-state]
               ;; (deeb "<<<<<<<<<<CH")
               (>!! update-chan [component target @last-obj-tree new-state options])
               ;;(handle-diffs f target old-state new-state options)
               ))
  (>!! update-chan [component target [] @state-atom options])
  ;;(handle-diffs f target nil @value options)
  )

(defn process-state-updates [app tpf]
  (if-let [vals (poll! update-chan)]
    (let [new-obj-tree (apply build-root-component! vals)]
      (swap! cnt inc)
     ;; (deeb (str @cnt ">>>>>>>>>>>>>"))
      (clojure.pprint/pprint tpf)
      ;;(deeb Thread/currentThread))
      ;;(deebm (with-meta {:yo "oy"} {:mmmet "aaa!"}))
      (reset! last-obj-tree new-obj-tree)
      ;;(deebm @last-obj-tree)
      )))
