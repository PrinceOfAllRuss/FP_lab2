# Лабораторная работа №2

```
Выполнил: Филатов Фёдор P3322

Вариант: avl-dict
```

## Требования к разрабатываемой структуре данных

1. Структура данных должна быть моноидом
2. Структура данных должна быть неизменяемой
3. Структура должна быть полиморфной
4. Должны быть реализованы функции:
    1. Добавление \ Удаление элементов
    2. Фильтрация
    3. Отображение
    4. Левая и правая свертки
5. Должен быть реализован набор unit-тестов
6. Должен быть реализован набор property-based тестов
   (не менее 3х тестов, в том числе описывающих спецификацию структуры как моноида)

## Реализация

### Реализация AVL-дерева
``` ./src/avl-tree.clj ```

AVL-дерево — это самосбалансированное бинарное дерево поиска, в котором для каждой вершины разница высот 
ее левого и правого поддеревьев (называемая балансировочным фактором) может принимать значения -1, 0 или +1. 
Это свойство обеспечивает высокую эффективность операций поиска, вставки и удаления, позволяя им 
выполняться за O(log n) в среднем и в худшем случаях.

Ограничения, накладываемые на AVL-дерево:
1. Балансировка: Для каждой вершины дерева разница высот левого и правого поддеревьев не превышает 1. Это гарантирует, 
что дерево остается сбалансированным, что, в свою очередь, обеспечивает логарифмическую сложность операций.
2. Бинарное дерево поиска: Как и любое бинарное дерево поиска, AVL-дерево поддерживает порядок 
элементов, что позволяет эффективно выполнять операции поиска. Для каждой вершины все ключи в 
левом поддереве меньше ее ключа, а все ключи в равом поддереве больше.
3. Автоматическая балансировка: При вставке или удалении узлов AVL-дерево автоматически корректирует свое 
состояние с помощью вращений (левого или правого), чтобы поддерживать балансировку. Эти вращения позволяют 
избежать деградации структуры данных до линейного времени выполнения.

### AVL-дерево

```clojure
(ns avl-tree
  (:refer-clojure :exclude [merge]))

; Структура данных
(defrecord AVLTreeVertex [key value height left right])

; Создание вершины
(defn create-node [key value]
  (AVLTreeVertex. key value 1 nil nil))

; Вычисление новой высоты (необходимо для добавления, удаления, балансировки)
(defn- count-new-height [height, rotation-type]
  (cond
    (= rotation-type "slight")
    (max (- height 2) 1)
    (= rotation-type "big")
    (max (dec height) 1)))

; Малый правый поворот (необходимо для балансировки)
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

; Малый левый поворот (необходимо для балансировки)
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

; Большой правый поворот (необходимо для балансировки)
(defn- big-right-rotation [node]
  (let [new-left (slight-left-rotation (:left node) "big")
        new-tree (assoc node :left new-left)]
    (slight-right-rotation new-tree "slight")))

; Большой левый поворот (необходимо для балансировки)
(defn- big-left-rotation [node]
  (let [new-right (slight-right-rotation (:right node) "big")
        new-tree (assoc node :right new-right)]
    (slight-left-rotation new-tree "slight")))

; Вспомогательная функция получения высоты
(defn- height [node]
  (if node (:height node) 0))

; Обновить высоту вершины
(defn- update-height [node]
  (let [hl (height (:left node))
        hr (height (:right node))
        new-height (inc (max hl hr))]
    (assoc node :height new-height)))

; Вычисление фактора балансировки
(defn- balance-factor [node]
  (let [hl (height (:left node))
        hr (height (:right node))]
    (- hl hr)))

; Балансировка дерева
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

; Сравнение типов ключей (полиморфизм)
(defn- type-checking [n1 n2]
  (= (type (:key n1)) (type (:key n2))))

; Добавление вершины
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

; Поиск вершины по ключу
(defn search-node [node key]
  (let [cmp (compare (:key node) key)]
    (cond
      (nil? node) nil
      (zero? cmp) node
      (neg? cmp) (search-node (:right node) key)
      (pos? cmp) (search-node (:left node) key))))

; Поиск вершины с минимальным ключем (необходимо для удаления)
(defn- search-min-node [node]
  (if (:left node)
    (recur (:left node))
    node))

; Удаление вершины по ключу
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

; Функция map для дерева
(defn map-avl [f node]
  (when node
    (let [l (map-avl f (:left node))
          r (map-avl f (:right node))
          new-value (f [(:key node) (:value node)])]
      (into () (reverse (concat l [new-value] r))))))

; Функция filter для дерева
(defn filter-avl [f node]
  (when node
    (let [l (filter-avl f (:left node))
          r (filter-avl f (:right node))
          entry [(:key node) (:value node)]
          new-value (if (f entry) [entry] [])]
      (into () (concat l new-value r)))))

; Вспомогательная функция для reduce (позволяет реализовать левую и правую свертку)
(defn- opposite [side] (if (= side :left) :right :left))

; Функция reduce для дерева
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

; Проверка типа ключа для определения функции объединения
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

; Добавление вершины при объединении деревеьев
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

; Объединение деревьев
(defn merge [root1 root2]
  (let [keys (map-avl first root2)
        values (map-avl second root2)
        length (count keys)]
    (loop [i 0
           new-tree root1]
      (if (> i (- length 1))
        new-tree
        (recur (inc i) (merge-insert new-tree (create-node (nth keys i) (nth values i))))))))
```

### Реализация интерфейса
``` ./src/dictionary.clj ```

Для описания интерфейса был объявлен протокол IDict

```clojure

(defprotocol IDict
  (get [this key])
  (add [this key value])
  (delete [this key])
  (conj [this ^IDict dict])
  (into [this sequence])
  (reduce-right [this start f])
  (reduce-left [this start f])
  (map [this f])
  (filter [this f])
  (values [this])
  (keys [this])
  (get-tree [this]))

```

А также тип реализующий данный протокол, а также ряд других для поддержки необходимых операций

```clojure

(deftype AVLDictionary [avl-tree]
  IDict
  (get [_ key] (:value (avl-tree/search-node avl-tree key)))
  (add [_ key value] (AVLDictionary. (avl-tree/insert avl-tree (avl-tree/create-node key value))))
  (delete [_ key] (AVLDictionary. (avl-tree/delete avl-tree key)))
  (conj [_ dict] (AVLDictionary. (avl-tree/merge avl-tree (get-tree dict))))
  (into [this sequence] (reduce (fn [acc [k v]] (add acc k v)) this sequence))
  (reduce-right [_ f start] (avl-tree/reduce-avl f start avl-tree :right))
  (reduce-left [_ f start] (avl-tree/reduce-avl f start avl-tree :left))
  (map [_ f] (avl-tree/map-avl f avl-tree))
  (filter [_ f] (avl-tree/filter-avl f avl-tree))
  (values [_] (avl-tree/map-avl second avl-tree))
  (keys [_] (avl-tree/map-avl first avl-tree))
  (get-tree [_] avl-tree)
  IPersistentMap
  (equiv [this obj]
    (= (seq this) (seq obj)))
  (seq [_]
    (letfn [(tree->seq [node]
              (when node
                (concat (tree->seq (:left node))
                        (list [(:key node)
                               (if (instance? AVLDictionary (:value node))
                                 (clojure.core/into {} (seq (:value node)))
                                 (:value node))])
                        (tree->seq (:right node)))))]
      (tree->seq avl-tree))))

```

### Реализация unit-тестирования
``` ./test/tree_unit_test.clj ```

Были написаны unit тесты для каждой из реализованых в интерфейсе функций.

```clojure
(ns tree_unit_test
  (:require [clojure.test :refer [deftest is testing]])
  (:refer-clojure :exclude [merge map filter keys into get conj abs])
  (:require [dictionary :refer [add conj delete filter get into keys map
                                avl-dict reduce-left reduce-right values]]))

(deftest test-avl-creation
         (testing "Test dict creation"
                  (is (thrown? Exception (avl-dict 1)) "Test for invalid arguments number")
                  (is (= (avl-dict :a 1 :b 2 :c 3) {:a 1 :b 2 :c 3})) "Test success creation"
                  (is (thrown? Exception (avl-dict 1 2 "c" 3)) "Test incomparable keys")
                  (is (= (avl-dict :a "c" :b 2) {:a "c" :b 2}) "Test different values types")
                  (is (= (avl-dict :a "c" :b 2) (avl-dict :b 2 :a "c")) "Test commutative")))


(deftest test-avl-add-element
         (testing "Test new elements insertion"
                  (is (= (-> (avl-dict) (add 1 2) (add 3 4))  {1 2, 3 4})) "Test success insertion"
                  (is (thrown? Exception (-> (avl-dict) (add 1 2) (add "string-key" 3))) "Test incomparable keys")
                  (is (= (-> (avl-dict) (add 1 2) (add 2 3))
                         (-> (avl-dict) (add 2 3) (add 1 2))) "Test commutative")
                  (is (= (-> (avl-dict) (add 3 (-> (avl-dict) (add 1 2)))) {3 {1 2}}) "Test nested dict insertion")))

(deftest test-avl-remove-element
         (testing "Test removing elements"
                  (is (= (-> (avl-dict 1 2 2 3 3 4) (delete 1)) (avl-dict 2 3 3 4)))
                  (is (= (-> (avl-dict 1 2) (delete 2)) (-> (avl-dict 1 2))))
                  (is (= (-> (avl-dict 1 2) (delete 1)) (avl-dict)))))

(deftest test-avl-get-element
         (testing "Test get elements"
                  (is (= (get (avl-dict 1 2 3 4) 1) 2))
                  (is (= (get (avl-dict) 1) nil))
                  (is (= (get (avl-dict 1 2) 2) nil))))

(deftest test-avl-conjunction
         (testing "Test dicts conjunction"
                  (is (= (conj (avl-dict 1 2) (avl-dict 2 3)) (avl-dict 1 2 2 3)))
                  (is (thrown? Exception (conj (avl-dict 1 2) (avl-dict "2" 3))))))

(deftest test-avl-reducing
         (testing "Test reducing elements of dict"
                  (is (= "abcd"
                         (reduce-left (avl-dict 1 "a" 2 "b" 3 "c" 4 "d") #(str %1 (second %2)) "")))
                  (is (= "dcba"
                         (reduce-right (avl-dict 1 "a" 2 "b" 3 "c" 4 "d") #(str %2 (second %1)) "")))))

(deftest test-avl-mapping
         (testing "Test mapping through elements of dict"
                  (is (= '(4 6 8)
                         (map (avl-dict 1 2 2 3 3 4) #(* (second %) 2))))
                  (is (= [[2 4] [4 6] [6 8]]
                         (map (avl-dict 1 2 2 3 3 4) (fn [[k v]] [(* k 2) (* v 2)]))))
                  (is (= (avl-dict 2 4 4 6 6 8)
                         (into (avl-dict) (map (avl-dict 1 2 2 3 3 4) (fn [[k v]] [(* k 2) (* v 2)])))))
                  (is (= nil
                         (map (avl-dict) (constantly nil))))))

(deftest test-avl-filtering
         (testing "Test filtering elements of dict"
                  (is (= '([2 4])
                         (filter (avl-dict 1 2 3 4 2 4) #(and (even? (first %)) (even? (second %))))))
                  (is (= nil
                         (filter (avl-dict) (constantly true))))
                  (is (= ()
                         (filter (avl-dict 1 2) #(even? (first %)))))
                  (is (= ()
                         (filter (avl-dict 2 3) (constantly false))))))

(deftest test-values-getter
         (testing "Test dict values getter"
                  (is (= nil
                         (values (avl-dict))))
                  (is (= '(1 2 3)
                         (values (avl-dict 1 1 2 2 3 3))))))

(deftest test-keys-getter
         (testing "Test dict keys getter"
                  (is (= nil
                         (keys (avl-dict))))
                  (is (= '(1 2 3)
                         (keys (avl-dict 1 1 2 2 3 3))))))
```

### Реализация property-based тестирования
``` ./src/tree_prop_test.clj ```

Для написания property-based тестирования были использваны генераторы и раннер из пакета test.check

##### Свойство моноида - коммутативность операции слияния

```clojure
(deftest test-commutative 
         (testing "Test that conjunction operations is commutative"
                  (is (check/quick-check
                        100
                        (prop/for-all [dict gen-dict
                                       second-dict gen-dict]
                                      (= (conj dict second-dict) (conj second-dict dict)))))))
```

##### Свойство моноида - операция слияния с нейтральным элементом не меняет структуру

```clojure
(deftest test-neutral-element
         (testing "Test operations with neutral element"
                  (is (check/quick-check
                        100
                        (prop/for-all [dict gen-dict]
                                      (= (conj dict (avl-dict)) dict))))
                  (is (check/quick-check
                        100
                        (prop/for-all [dict gen-dict]
                                      (= (conj (avl-dict) dict) dict))))))
```

##### Свойство AVL-дерева

```clojure
; Проверка сбалансированности дерева
(defn avl-balanced? [avl]
  (if (nil? avl) true
                 (let [lh (:height (:left avl))
                       rh (:height (:right avl))]
                   (and (<= (abs (- lh rh)) 1)
                        (avl-balanced? (:left avl))
                        (avl-balanced? (:right avl))))))

; После создания дерево сбалансировано
(deftest test-balance-after-creation
         (testing "Test that tree balance after creation"
                  (is (check/quick-check
                        100
                        (prop/for-all [dict gen-dict]
                                      (avl-balanced? (get-tree dict)))))))
; После добавления вершин дерево сбалансировано
(deftest test-balance-after-insert
         (testing "Test that tree balance after inserting keys and values"
                  (is (check/quick-check
                        100
                        (prop/for-all [dict gen-dict
                                       keys (gen-numbers 20)
                                       values (gen-numbers 20)]
                                      (let [updated-dict (reduce (fn [d [k v]] (add d k v))
                                                                 dict
                                                                 (map vector keys values))]
                                        (avl-balanced? (get-tree updated-dict))))))))
; После удаления вершин дерево сбалансировано
(deftest test-balance-after-delete
         (testing "Test that tree balance after deleting random keys"
                  (is (check/quick-check
                        100
                        (prop/for-all [dict gen-dict]
                                      (let [keys (keys dict)
                                            keys-to-remove (take 20 (shuffle keys))
                                            updated-dict (reduce (fn [d k] (delete d k)) dict keys-to-remove)]
                                        (avl-balanced? (get-tree updated-dict))))))))
```

## Вывод

Выполняя данную лабораторную работу, я освоил несколько важных приемов функционального программирования и работы с пользовательскими типами данных:

1. Реализация АВЛ-дерева: Я создал полиморфную структуру данных АВЛ-дерево, которая обеспечивает эффективное хранение и поиск ключей. Это позволило мне глубже понять принципы работы сбалансированных деревьев и их реализацию в функциональном стиле.

2. Неизменяемость данных: Все операции с АВЛ-деревом реализованы с соблюдением принципа неизменяемости, что является ключевым аспектом функционального программирования.

3. Функции высшего порядка: Использование функций `map-avl`, `filter-avl`, `reduce-avl` демонстрирует мощь функций высшего порядка в обработке данных.

4. Добавил unit-тестирование и property-based тестирование.

5. Полиморфизм: АВЛ-дерево реализовано как полиморфная структура, способная хранить значения различных типов.

Эта лабораторная работа позволила мне глубже погрузиться в функциональное программирование, работу со сложными структурами данных и различные методы тестирования. 