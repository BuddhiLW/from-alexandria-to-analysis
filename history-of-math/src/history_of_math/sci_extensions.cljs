(ns history-of-math.sci-extensions
  "The series' browser bundle: Clerk's viewer JS plus emmy-viewers (Leva,
   Mafs, MathBox, JSXGraph, MathLive and the Kernel contract of PR #83) and
   alexandria's proof-player namespaces, compiled ahead of time by
   history-of-math.build (shadow-cljs release) instead of loaded as source
   into SCI on each page.

   The player namespaces copied here are listed, for the JVM side, in
   history-of-math.widgets/bundled-cljs: Clerk then ships only each
   notebook's scene namespace as source (its :require-cljs render-fn), and
   that scene's defmethods extend the compiled alexandria.medium.scene/draw."
  (:require [alexandria.medium.anim]
            [alexandria.medium.figure]
            [alexandria.medium.html-labels]
            [alexandria.medium.math]
            [alexandria.medium.plane]
            [alexandria.medium.player]
            [alexandria.medium.scene]
            [alexandria.medium.svg]
            [alexandria.medium.timeline]
            [alexandria.palette]
            [emmy.viewer.sci]
            [sci.core :as sci]
            [sci.ctx-store]))

(emmy.viewer.sci/install!)

(def alexandria-namespaces
  {'alexandria.medium.anim     (sci/copy-ns alexandria.medium.anim (sci/create-ns 'alexandria.medium.anim))
   'alexandria.medium.figure   (sci/copy-ns alexandria.medium.figure (sci/create-ns 'alexandria.medium.figure))
   'alexandria.medium.html-labels (sci/copy-ns alexandria.medium.html-labels (sci/create-ns 'alexandria.medium.html-labels))
   'alexandria.medium.math     (sci/copy-ns alexandria.medium.math (sci/create-ns 'alexandria.medium.math))
   'alexandria.medium.plane    (sci/copy-ns alexandria.medium.plane (sci/create-ns 'alexandria.medium.plane))
   'alexandria.medium.player   (sci/copy-ns alexandria.medium.player (sci/create-ns 'alexandria.medium.player))
   'alexandria.medium.scene    (sci/copy-ns alexandria.medium.scene (sci/create-ns 'alexandria.medium.scene))
   'alexandria.medium.svg      (sci/copy-ns alexandria.medium.svg (sci/create-ns 'alexandria.medium.svg))
   'alexandria.medium.timeline (sci/copy-ns alexandria.medium.timeline (sci/create-ns 'alexandria.medium.timeline))
   'alexandria.palette         (sci/copy-ns alexandria.palette (sci/create-ns 'alexandria.palette))})

(sci.ctx-store/swap-ctx!
 sci/merge-opts
 {:classes    {'Math js/Math}
  :namespaces alexandria-namespaces})
