(ns avl-tree
  (:refer-clojure :exclude [merge]))

(defrecord AVLTreeVertex [key value height left right])

(defn create-node [key value]
  (AVLTreeVertex. key value 1 nil nil))

(defn- count-new-height [height, rotation-type]
  (cond
    (= rotation-type "slight")
    (max (- height 2) 1)
    (= rotation-type "big")
    (max (dec height) 1)))

(defn- slight-right-rotation [node, rotation-type]
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

(defn- slight-left-rotation [node, rotation-type]
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

(defn- big-right-rotation [node]
  (let [new-left (slight-left-rotation (:left node) "big")
        new-tree (assoc node :left new-left)]
    (slight-right-rotation new-tree "slight")))

(defn- big-left-rotation [node]
  (let [new-right (slight-right-rotation (:right node) "big")
        new-tree (assoc node :right new-right)]
    (slight-left-rotation new-tree "slight")))

(defn- height [node]
  (if node (:height node) 0))

(defn- update-height [node]
  (let [hl (height (:left node))
        hr (height (:right node))
        new-height (inc (max hl hr))]
    (assoc node :height new-height)))

(defn- balance-factor [node]
  (let [hl (height (:left node))
        hr (height (:right node))]
    (- hl hr)))

(defn- balance [node]
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

(defn- type-checking [n1 n2]
  (= (type (:key n1)) (type (:key n2))))

(defn insert [root node]
  (if (nil? root)
    node
    (if (type-checking root node)
      (let [cmp (compare (:key root) (:key node))]
        (cond
          (zero? cmp)
          (assoc root :value (:value node))
          (neg? cmp)
          (if (nil? (:right root))
            (assoc root :right node :height 2)
            (let [new-node (balance (insert (:right root) node))]
              (balance (assoc root :right new-node))))
          (pos? cmp)
          (if (nil? (:left root))
            (assoc root :left node :height 2)
            (let [new-node (balance (insert (:left root) node))]
              (balance (assoc root :left new-node))))))
      (throw (Exception. "Key Error: Invalid key type")))))

(defn search-node [node key]
  (let [cmp (compare (:key node) key)]
    (cond
      (nil? node) nil
      (zero? cmp) node
      (neg? cmp) (search-node (:right node) key)
      (pos? cmp) (search-node (:left node) key))))

(defn- search-min-node [node]
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
            (balance (assoc new-root :right (delete nr (:key min-node))))))))
    node))

(defn map-avl [f node]
  (when node
    (let [l (map-avl f (:left node))
          r (map-avl f (:right node))
          new-value (f [(:key node) (:value node)])]
      (into () (reverse (concat l [new-value] r))))))

(defn filter-avl [f node]
  (when node
    (let [l (filter-avl f (:left node))
          r (filter-avl f (:right node))
          entry [(:key node) (:value node)]
          new-value (if (f entry) [entry] [])]
      (into () (concat l new-value r)))))

(defn- opposite [side] (if (= side :left) :right :left))
(defn reduce-avl [f acc node side]
  (if (nil? node)
    acc
    (cond
      (= side :right)
      (let [acc (reduce-avl f acc (side node) side)
            acc (f [(:key node) (:value node)] acc)
            acc (reduce-avl f acc ((opposite side) node) side)]
        acc)
      (= side :left)
      (let [acc (reduce-avl f acc (side node) side)
            acc (f acc [(:key node) (:value node)])
            acc (reduce-avl f acc ((opposite side) node) side)]
        acc))))

(defn- merge-type [v1 v2]
  (if (= (type v1) (type v2))
    (cond
      (number? v1)
      (+ v1 v2)
      (string? v1)
      (str v1 v2)
      (instance? clojure.lang.IPersistentCollection v1)
      (into v1 v2))
    (throw (Exception. "Key Error: Invalid key type"))))
(defn merge-insert [root node]
  (if (nil? root)
    node
    (if (type-checking root node)
      (let [cmp (compare (:key root) (:key node))]
        (cond
          (zero? cmp)
          (let [new-value (merge-type (:value root) (:value node))
                new-node (create-node (:key node) new-value)
                new-tree (delete root (:key node))]
            (insert new-tree new-node))
          (neg? cmp)
          (if (nil? (:right root))
            (assoc root :right node :height 2)
            (let [new-node (balance (merge-insert (:right root) node))]
              (balance (assoc root :right new-node))))
          (pos? cmp)
          (if (nil? (:left root))
            (assoc root :left node :height 2)
            (let [new-node (balance (merge-insert (:left root) node))]
              (balance (assoc root :left new-node))))))
      (throw (Exception. "Key Error: Invalid key type!!!!!!!!!!")))))

(defn merge [root1 root2]
  (let [keys (map-avl first root2)
        values (map-avl second root2)
        length (count keys)]
    (loop [i 0
           new-tree root1]
      (if (> i (- length 1))
        new-tree
        (recur (inc i) (merge-insert new-tree (create-node (nth keys i) (nth values i))))))))