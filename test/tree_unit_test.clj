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