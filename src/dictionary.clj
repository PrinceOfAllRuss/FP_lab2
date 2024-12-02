(ns dictionary
  (:refer-clojure :exclude [merge map filter keys into get conj])
  (:require [avl-tree])
  (:import (clojure.lang IPersistentMap)))

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

(defn avl-dict [& values]
  (if (not (even? (count values)))
    (throw (Exception. "Bad values count (must be even)"))
    (into (AVLDictionary. nil) (partition 2 values))))

(def dict (avl-dict 1 (String. "a")
                   2 {:f "b" :g (avl-dict 3 4)}
                   3 "c"
                   7 '(1 23)
                   5 [1 2 3]
                   4 "d"))
