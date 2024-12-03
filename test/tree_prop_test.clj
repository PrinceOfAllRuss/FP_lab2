(ns tree_prop_test
  (:refer-clojure :exclude [into conj keys merge])
  (:require [clojure.test :refer :all]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [dictionary :refer [conj into add get-tree keys avl-dict]]
            [avl-tree :refer :all]))

(def gen-dict
  (gen/fmap (fn [list] (into (avl-dict) (partition 2 list))) (gen/list gen/small-integer)))

(defn gen-numbers [n]
  (gen/vector gen/small-integer n))

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

(deftest test-commutative
  (testing "Test that conjunction operations is commutative"
    (is (check/quick-check
          100
          (prop/for-all [dict gen-dict
                         second-dict gen-dict]
                        (= (conj dict second-dict) (conj second-dict dict)))))))

(defn avl-balanced? [avl]
  (if (nil? avl) true
                 (let [lh (:height (:left avl))
                       rh (:height (:right avl))]
                   (and (<= (abs (- lh rh)) 1)
                        (avl-balanced? (:left avl))
                        (avl-balanced? (:right avl))))))

(deftest test-balance-after-creation
  (testing "Test that tree balance after creation"
    (is (check/quick-check
          100
          (prop/for-all [dict gen-dict]
            (avl-balanced? (get-tree dict)))))))

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

(deftest test-balance-after-delete
  (testing "Test that tree balance after deleting random keys"
    (is (check/quick-check
          100
          (prop/for-all [dict gen-dict]
            (let [keys (keys dict)
                  keys-to-remove (take 20 (shuffle keys))
                  updated-dict (reduce (fn [d k] (delete d k)) dict keys-to-remove)]
              (avl-balanced? (get-tree updated-dict))))))))