(ns avl-tree)

(defrecord AVLTreeVertex [key value height left right])

(defn create-node [key value]
  (AVLTreeVertex. key value 0 nil nil))

(defn count-new-height [height, rotation-type]
  (cond
    (= rotation-type "slight")
    (max (- height 2) 1)
    (= rotation-type "big")
    (max (dec height) 1)))

(defn slight-right-rotation [node, rotation-type]
  (cond
    (= rotation-type "slight")
    (let [new-root (:left node)
          new-leaf-left (:right new-root)
          new-root-right (assoc node :left new-leaf-left :height
                                     (count-new-height (:height node) "slight"))]
      (assoc new-root :right new-root-right))
    (= rotation-type "big")
    (let [new-root-template (:left node)
          new-root (assoc new-root-template :height (inc (:height new-root-template)))
          new-leaf-left (:right new-root)
          new-root-right (assoc node :left new-leaf-left :height
                                     (count-new-height (:height node) "big"))]
      (assoc new-root :right new-root-right))))

(defn slight-left-rotation [node, rotation-type]
  (cond
    (= rotation-type "slight")
    (let [new-root (:right node)
          new-leaf-right (:left new-root)
          new-root-left (assoc node :right new-leaf-right :height
                                    (count-new-height (:height node) "slight"))]
      (assoc new-root :left new-root-left))
    (= rotation-type "big")
    (let [new-root-template (:right node)
          new-root (assoc new-root-template :height (inc (:height new-root-template)))
          new-leaf-right (:left new-root-template)
          new-root-left (assoc node :right new-leaf-right :height
                                    (count-new-height (:height node) "big"))]
      (assoc new-root :left new-root-left))))

(defn big-right-rotation [node]
  (let [new-left (slight-left-rotation (:left node) "big")
        new-tree (assoc node :left new-left)]
    (slight-right-rotation new-tree "slight")))

(defn big-left-rotation [node]
  (let [new-right (slight-right-rotation (:right node) "big")
        new-tree (assoc node :right new-right)]
    (slight-left-rotation new-tree "slight")))

(defn height [node]
  (if node (:height node) 0))

(defn update-height [node]
  (let [hl (height (:left node))
        hr (height (:right node))
        new-height (inc (max hl hr))]
    (assoc node :height new-height)))

(defn balance-factor [node]
  (let [hl (height (:left node))
        hr (height (:right node))]
    (- hl hr)))

(defn balance [node]
  (let [bf (balance-factor node)]
    (cond
      (== bf -2)
      (if (<= (balance-factor (:right node)) 0)
        (slight-left-rotation node "slight")
        (big-left-rotation node))

      (== bf 2)
      (if (>= (balance-factor (:left node)) 0)
        (slight-right-rotation node "slight")
        (big-right-rotation node))

      :else (update-height node))))

(defn type-checking [n1 n2]
  (= (type (:key n1)) (type (:key n2))))

(defn insert [root node]
  (if (type-checking root node)
    (let [cmp (compare (:key root) (:key node))]
      (if (or (zero? cmp) (neg? cmp))
        (if (nil? (:right root))
          (assoc root :right node :height 2)
          (let [new-node (balance (insert (:right root) node))]
            (balance (assoc root :right new-node))))
        (if (nil? (:left root))
          (assoc root :left node :height 2)
          (let [new-node (balance (insert (:left root) node))]
            (balance (assoc root :left new-node))))))
    (do
      (println "Key Error: Invalid key type")
      root)))

(defn search-node [node key]
  (let [cmp (compare (:key node) key)]
    (cond
      (nil? node) nil
      (zero? cmp) node
      (neg? cmp) (search-node (:right node) key)
      (pos? cmp) (search-node (:left node) key))))

(defn search-min-node [node]
  (if (:left node)
    (recur (:left node))
    node))

(defn delete [node key]
  (if node
    (let [cmp (compare (:key node) key)]
      (cond
        (nil? node) nil
        (neg? cmp) (balance (assoc node :right (delete (:right node) key)))
        (pos? cmp) (balance (assoc node :left (delete (:left node) key)))
        (zero? cmp)
        (if (nil? (:right node))
          (:left node)
          (let [nr (:right node)
                min-node (search-min-node nr)
                new-root (assoc node :key (:key min-node) :value (:value min-node))]
            (balance (assoc new-root :right (delete nr (:key min-node))))))))))

;slight-right-rotation
;(def o (AVLTreeVertex. "o" 1 1 nil nil))
;(def n (AVLTreeVertex. "n" 1 2 o nil))
;(def m (AVLTreeVertex. "m" 1 1 nil nil))
;(def f (AVLTreeVertex. "f" 1 3 n m))
;(def h (AVLTreeVertex. "h" 1 1 nil nil))
;(def e (AVLTreeVertex. "e" 1 2 nil h))
;(def d (AVLTreeVertex. "d" 1 4 f e))
;(def j (AVLTreeVertex. "j" 1 1 nil nil))
;(def c (AVLTreeVertex. "c" 1 1 nil nil))
;(def b (AVLTreeVertex. "b" 1 2 j c))
;(def a (AVLTreeVertex. "a" 1 5 d b))

;slight-left-rotation
;(def o (AVLTreeVertex. "o" 1 1 nil nil))
;(def n (AVLTreeVertex. "n" 1 2 nil o))
;(def m (AVLTreeVertex. "m" 1 1 nil nil))
;(def f (AVLTreeVertex. "f" 1 3 m n))
;(def j (AVLTreeVertex. "j" 1 1 nil nil))
;(def h (AVLTreeVertex. "h" 1 1 nil nil))
;(def e (AVLTreeVertex. "e" 1 2 h j))
;(def d (AVLTreeVertex. "d" 1 4 e f))
;(def g (AVLTreeVertex. "g" 1 1 nil nil))
;(def c (AVLTreeVertex. "c" 1 1 nil nil))
;(def b (AVLTreeVertex. "b" 1 2 c g))
;(def a (AVLTreeVertex. "a" 1 5 b d))

;big-right-rotation
;(def m (AVLTreeVertex. "m" 1 1 nil nil))
;(def l (AVLTreeVertex. "l" 1 2 nil m))
;(def k (AVLTreeVertex. "k" 1 1 nil nil))
;(def j (AVLTreeVertex. "j" 1 1 nil nil))
;(def h (AVLTreeVertex. "h" 1 1 nil nil))
;(def d (AVLTreeVertex. "d" 1 2 h j))
;(def e (AVLTreeVertex. "e" 1 3 k l))
;(def b (AVLTreeVertex. "b" 1 4 d e))
;(def g (AVLTreeVertex. "g" 1 1 nil nil))
;(def f (AVLTreeVertex. "f" 1 1 nil nil))
;(def c (AVLTreeVertex. "c" 1 2 f g))
;(def a (AVLTreeVertex. "a" 1 5 b c))

;big-left-rotation
;(def m (AVLTreeVertex. "m" 1 1 nil nil))
;(def l (AVLTreeVertex. "l" 1 1 nil nil))
;(def k (AVLTreeVertex. "k" 1 1 nil nil))
;(def j (AVLTreeVertex. "j" 1 2 nil m))
;(def h (AVLTreeVertex. "h" 1 1 nil nil))
;(def g (AVLTreeVertex. "g" 1 2 k l))
;(def f (AVLTreeVertex. "f" 1 3 h j))
;(def c (AVLTreeVertex. "c" 1 4 f g))
;(def e (AVLTreeVertex. "e" 1 1 nil nil))
;(def d (AVLTreeVertex. "d" 1 1 nil nil))
;(def b (AVLTreeVertex. "b" 1 2 d e))
;(def a (AVLTreeVertex. "a" 1 5 b c))