;;; clj-relative-time.core_test.clj -- A relative time library for clojure built on clj-time

;; by Arnold Matyasi and loganisorb http://www.loganis.com
;; June 20, 2014

;; Copyright (c) iWebMa Ltd. 2014. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns clj-relative-time.core-test
  (:use clojure.test
        clj-relative-time.core)
  (:require [clojure.math.combinatorics :as combo]))

(defn run-rel-tests []
  (let [rel-strings (combo/cartesian-product  ["this" "last" "prev"] 
                                              [1 2 3 4]  
                                              ["day" "week" "4week" "month" "calmonth" "quarter" "half" "year"])
        rel-res   (into [] (map (fn [x] 
                                  (let [rel-string (apply str (interpose "_" x))]
                                    {(keyword rel-string) (period2begend rel-string)})
                                  ) rel-strings))
        ]
   (apply str (interpose "\n" rel-res))
  ))

(deftest a-test
  (testing "Relative time to absolute time test"
    (is (nil? (print (str (run-rel-tests)))))))
