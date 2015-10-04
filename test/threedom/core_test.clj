(ns threedom.core-test
  (:require [clojure.test :refer :all]
            [threedom.core :refer :all])
  (:import [com.jme3 app.SimpleApplication
            material.Material
            material.RenderState
            material.RenderState$BlendMode
            light.DirectionalLight
            scene.Geometry
            system.AppSettings
            system.JmeSystem
            util.SkyFactory
            renderer.queue.RenderQueue$Bucket
            scene.shape.Box
            scene.Node
            texture.Texture
            texture.Texture2D
            math.Vector3f
            math.ColorRGBA]))
(def old-scene [[com.jme3.scene.Node
  ["pivot"]
  {}
  [[com.jme3.scene.Geometry
    ["Box_-1_-1_-2"]
    {:setMesh [[com.jme3.scene.shape.Box [0.2 0.2 0.2]]],
     :setLocalTranslation [[com.jme3.math.Vector3f [-1 -1 -2]]],
     :setMaterial
     [[com.jme3.material.Material
       ["DESCTOP_ASSET_MANAGER"
        "Common/MatDefs/Misc/Unshaded.j3md"]
       {:setColor ["Color" ColorRGBA/White],
        :setTexture
        ["ColorMap"
         (Texture2D.)]}]]}]
   [com.jme3.scene.Geometry
    ["Box_0_0_0"]
    {:setMesh [[com.jme3.scene.shape.Box [0.2 0.2 0.2]]],
     :setLocalTranslation [[com.jme3.math.Vector3f [0 0 0]]],
     :setMaterial
     [[com.jme3.material.Material
       ["DESCTOP_ASSET_MANAGER"
        "Common/MatDefs/Misc/Unshaded.j3md"]
       {:setColor ["Color" ColorRGBA/Red],
        :setTexture
        ["ColorMap"
         (Texture2D.)]}]]}]]]
 [com.jme3.light.DirectionalLight
  []
  {:setColor [ColorRGBA/White],
   :setDirection
   [[com.jme3.math.Vector3f [1 0 -2] {:normalizeLocal []}]]}]])

(def new-scene [[com.jme3.scene.Node
  ["pivot"]
  {}
  [[com.jme3.scene.Geometry
    ["Box_0_0_0"]
    {:setMesh [[com.jme3.scene.shape.Box [0.2 0.2 0.2]]],
     :setLocalTranslation [[com.jme3.math.Vector3f [0 0 0]]],
     :setMaterial
     [[com.jme3.material.Material
       ["DESCTOP_ASSET_MANAGER"
        "Common/MatDefs/Misc/Unshaded.j3md"]
       {:setColor ["Color" ColorRGBA/White],
        :setTexture
        ["ColorMap"
         (Texture2D.)]}]]}]
   [com.jme3.scene.Geometry
    ["Box_1_1_1"]
    {:setMesh [[com.jme3.scene.shape.Box [0.2 0.2 0.2]]],
     :setLocalTranslation [[com.jme3.math.Vector3f [1 1 1]]],
     :setMaterial
     [[com.jme3.material.Material
       ["DESCTOP_ASSET_MANAGER"
        "Common/MatDefs/Misc/Unshaded.j3md"]
       {:setColor ["Color" ColorRGBA/White],
        :setTexture
        ["ColorMap"
         (Texture2D.)]}]]}]]]
 [com.jme3.light.DirectionalLight
  []
  {:setColor [ColorRGBA/White],
   :setDirection
   [[com.jme3.math.Vector3f [1 0 -2] {:normalizeLocal []}]]}]])

#_ (deftest make-diff-test
  (testing "Finding out diffs."
    (is (= {} (materialize-node-diffs! nil old-scene new-scene)))))
