(ns alexandria.medium.math
  "The host's floating-point functions under one name on the JVM and in the
   browser (Clerk's interpreter resolves neither Math/... nor js/Math.x
   interop on both).")

(def pi #?(:clj Math/PI :cljs js/Math.PI))

(defn sin [x] #?(:clj (Math/sin x) :cljs (js/Math.sin x)))
(defn cos [x] #?(:clj (Math/cos x) :cljs (js/Math.cos x)))
(defn tan [x] #?(:clj (Math/tan x) :cljs (js/Math.tan x)))
(defn sqrt [x] #?(:clj (Math/sqrt x) :cljs (js/Math.sqrt x)))
(defn abs [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn floor [x] #?(:clj (Math/floor x) :cljs (js/Math.floor x)))
(defn pow [x y] #?(:clj (Math/pow x y) :cljs (js/Math.pow x y)))
(defn hypot [x y] #?(:clj (Math/hypot x y) :cljs (js/Math.hypot x y)))

(defn exp [x] #?(:clj (Math/exp x) :cljs (js/Math.exp x)))

(defn atan2 [y x] #?(:clj (Math/atan2 y x) :cljs (js/Math.atan2 y x)))

(defn acos [x] #?(:clj (Math/acos x) :cljs (js/Math.acos x)))
