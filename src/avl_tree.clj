(ns avl-tree)

(defrecord AVLTreeVertex [key value height left right])

(defn create-node [key value]
  (AVLTreeVertex. key value 1 nil nil))

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

(defn map-avl [f node]
  (when node
    (let [l (map-avl f (:left node))
          r (map-avl f (:right node))
          new-value (f [(:key node) (:value node)])]
      (into () (concat l [new-value] r)))))

(defn filter-avl [f node]
  (when node
    (let [l (filter-avl f (:left node))
          r (filter-avl f (:right node))
          entry [(:key node) (:value node)]
          new-value (if (f entry) [entry])]
      (into () (concat l new-value r)))))

(defn- opposite [side] (if (= side :left) :right :left))
(defn reduce-avl [f acc node side]
  (if (nil? node)
    acc
    (let [acc (reduce-avl f acc (side node) side)
          acc (f acc [(:key node) (:value node)])
          acc (reduce-avl f acc ((opposite side) node) side)]
      acc)))

(defn visualize [node]
  (letfn [(build-tree [node prefix is-left]
            (when node
              (str
                (build-tree (:right node) (str prefix (if is-left "│   " "    ")) false)
                prefix
                (if is-left "└── " "┌── ")
                (str (:key node) ":" (:value node) " (h:" (:height node) ")")
                "\n"
                (build-tree (:left node) (str prefix (if is-left "    " "│   ")) true))))]
    (println (build-tree node "" false))))

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

;delete
;1
;(def h (AVLTreeVertex. 1 1 1 nil nil))
;(def g (AVLTreeVertex. 3 3 1 nil nil))
;(def f (AVLTreeVertex. 9 9 1 nil nil))
;(def e (AVLTreeVertex. 8 8 2 nil f))
;(def d (AVLTreeVertex. 11 11 1 nil nil))
;(def c (AVLTreeVertex. 2 2 2 h g))
;(def b (AVLTreeVertex. 10 10 3 e d))
;(def a (AVLTreeVertex. 7 7 4 c b))
;2
;(def c (AVLTreeVertex. "c" 2 1 nil nil))
;(def a (AVLTreeVertex. "a" 3 1 nil nil))
;(def b (AVLTreeVertex. "b" 1 1 a c))

;map
;(map #(- % 2) (map-avl second b))

;filter
;(filter-avl #(= (second %) 2) b)

;reduce
;(def c (AVLTreeVertex. "c" "c" 1 nil nil))
;(def a (AVLTreeVertex. "a" "a" 1 nil nil))
;(def b (AVLTreeVertex. "b" "b" 2 a c))
;(reduce-avl #(str %1 (second %2)) 0 a :left)
;(reduce-avl #(str %1 (second %2)) 0 a :right)