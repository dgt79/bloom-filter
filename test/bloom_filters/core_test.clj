(ns bloom-filters.core-test
  (:require [clojure.test :refer :all]
            [bloom-filters.core :refer :all]))

(deftest put-test
  (testing "put sets the correct bits in the vector"
    (let [expected [1,1,1,0,0,0]
          hashes [(fn [x] 0) (fn [x] 1) (fn [x] 2)]
          empty [0,0,0,0,0,0]
          filter (bloom-filter hashes empty)]
      (is (= expected (:vector (put filter 2))))
      )
    ))

(deftest in?-test
  (testing "in? correct checks if we have bit in filter"
    (let [filter {:hashes [identity] :vector [1,0]}]
        (is (= true (in? filter 0)))
        (is (= false (in? filter 1)))
      )
    ))

(deftest bloom-test
  (testing "bloom filter with real hash functions"
    (let [ dictionary #{"dog" "cat" "mushroom" "blah" "boo" "fancy" "more" "words"}
					 words ["dog" "cat" "NOT-THERE"]
           hash-functions [sdbm-hash djb2-hash hash]
					 empty-vector (vec (take 100 (repeatedly (fn [] 0))))
           empty-filter (bloom-filter hash-functions empty-vector)
           filter (put-all empty-filter dictionary)
           _ (println filter)
           ]
      (doseq [w words]
				(if (and (in? filter w) (contains? dictionary w))
					(println (str "Word " w " exists in the filter"))
					(println (str "Word " w " doesn't exist in the filter")))
				))))
