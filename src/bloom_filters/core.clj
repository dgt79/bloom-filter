(ns bloom-filters.core
  (:require [clojure.core :refer :all]))


(defn bloom-filter
  [hashes vector]
  {:hashes hashes :vector vector})

(defn- bit-index [h value vector]
  "takes a hashing function, an input to the hash function and a vector; returns the index of the bucket in the vector."
  (mod (h value) (count vector)))

(defn put
  "adds value to the bloom filter"
  [filter value]
  (bloom-filter (:hashes filter) (reduce
     (fn [vector h] (assoc vector (bit-index h value vector)  1))
     (:vector filter)
     (:hashes filter))))

(defn put-all
  [filter values]
  (reduce put filter values))

(defn in?
  "validates is the value exists in the filter"
  [filter value]
  (every? (fn [h]
            (= 1
               (get (:vector filter) (bit-index h value (:vector filter)))))
          (:hashes filter)))


(defn- char-to-int [c]
  (Character/codePointAt (str c) 0))

(def magic-constant 5381)

(defn djb2-hash [str]
  (reduce (fn [h c]
            (+ (*' h 33) (char-to-int c)))
          magic-constant          ;;djb magic constant
          str))

(defn sdbm-hash [str]
  (reduce (fn [h c]
            (+ (*' h magic-constant) (char-to-int c)))
          magic-constant
          str))
