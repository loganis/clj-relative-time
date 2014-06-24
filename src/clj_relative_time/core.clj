;;; clj-relative-time.core.clj -- A relative time library for clojure built on clj-time

;; by Arnold Matyasi and Loganis Orb, http://www.loganis.com
;; June 20, 2014

;; Copyright (c) iWebMa Ltd. 2014. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clj-relative-time.core
  (:require [clj-time.core :as tm]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relative time helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def date-formatter (tf/formatter "yyyy-MM-dd"))

(def date-unparser (partial tf/unparse date-formatter))

(defn joda->str
  "JodaTime obj to str."
  [j]
  (date-unparser j))

(defn per-numberp
  "Search for _digits_ number in string s."
  [s]
  (re-find #"_(\d.*?)_" s))

(defn modquarter
  "Modulus for quarter. To calc the first month of a quarter"
  [m]
  (let [mq (- (mod m 3) 1)
        mq (if (< mq 0) 2 mq)]
    mq))

(defn beghalf
  "First month of a half year."
  [m]
  (if (>= (/ m 7) 1)
    7
    1))
  
(defn dnow
  "Date now"
  []
  (tm/now))

(defn ddays
  "Period of n days."
  [n]
  (tm/days n))

(defn dweeks
  "Period of n weeks."
  [n]
  (tm/weeks n))

(defn dmonths
  "Period of n months."
  [n]
  (tm/months n))

(defn dminus
  "Calucalte minus for a date and a period."
  ([dt p]
     (tm/minus dt p))
  ([dt p & ps]
     (tm/minus dt p ps)))

(defn dformat
  "Format date to string."
  [j]
  (-> j (joda->str)))

(defn period-normalize
  "Normalize s input string to explicitly include number 1.
   Ex. 'last week' string to last_1_week ."
  [s]
  (let [minor (-> #"this|last|prev" (re-find s))
        major (-> #"day|4week|week|calmonth|month|quarter|half|year" (re-find s))]
    (str minor "_1_" major)))


(defn period-number-missing
  "Check if number missing in s string, normalize if needed."
  [s]
  
  (let [numberp (per-numberp s)]
    (if-not (nil? numberp)
      s
      (period-normalize s))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; period2begend main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; this function is responsible for transforming a period (e.g. "last_2_weeks") into 
;; a pair of dates that will enter into the value for the :beg and :end keys

(defn period2begend
  "Calculate absolute :beg :end time from relative time string definition.
   Input q: query map containing period string definition
   Output: :beg :end absolute date string map"
  [q]
  (let [;; get string definition, replace space to _
        ;; Check if period definition is in standard preop_n_postop format case and plural insensitive
        ;; Examples: last_1_day, Last_2_Weeks
        per (-> q (clojure.string/replace  #" " "_") (clojure.string/lower-case) (period-number-missing))
        log (prn per)
        numberp (per-numberp per) ; regex find pattern _(1 or more number digits)_ in period string
        opts (if-not (nil? numberp) ; if pattern match calculate a 3 term based map of period definition
               {:number (read-string (last numberp)) ; middle term, a number 1 or more 
                :preop (-> #"(.*?)_"     ; minor term, regex find pattern (1 or more char)_ 
                           (re-find per) 
                           last)
                :postop (-> #"\d.*?_(.*)" ; major term, regex find pattern: after 1 or more number digits_(1 or more chars)
                            (re-find per)
                            last)
                })
        minor (-> opts :preop) ; Minor term of  period definition
        middle (-> opts :number) ; Middle term of period definiiton
        major (-> opts :postop) ; Major term of period definition
        now (dnow)
        dow (-> now (tm/day-of-week)) ; joda day of week now, values: 1-7, 1 monday - 7 sunday
        dom (-> now (tm/day)) ; day of month now, values :1-31
        moy (-> now (tm/month)) ; month of year now, values 1-12
        yea (-> now (tm/year)) ; year value of date now
        beyesterday {:beg (-> now (dminus (ddays 1)) (dformat)) ; default begin and end both yesterday
                     :end (-> now (dminus (ddays 1)) (dformat))}
        ]
    
    (when numberp 
      (cond
       ;;;;;;;;;;;;;;;;;
       ;; DAY
       ;;;;;;;;;;;;;;;;;
       (= "day" (re-find #"day" major))
       (cond
        ;; THIS | LAST
        (contains? #{ "this" "last"} (re-find #"this|last" minor)) 
        {:beg (-> now (dminus (-> middle ddays)) (dformat))
         :end (-> now (dminus (-> 1 ddays)) (dformat))}
        ;; PREV
        (contains? #{ "prev" } (re-find #"prev" minor)) 
        {:beg (-> now (dminus (-> (* 2 middle) ddays)) (dformat))
         :end (-> now (dminus (-> middle ddays)) (dminus (-> 1 ddays)) (dformat))}
        ;; ELSE
        :else beyesterday) 

       ;;;;;;;;;;;;;;;;;
       ;; WEEK
       ;;;;;;;;;;;;;;;;;
       (= "week" (re-find #"^week" major))
       (cond 
        ;; LAST
        (contains? #{ "last" } (re-find #"last" minor))
        {:beg (-> now (dminus (-> 7 (* middle) (ddays))) (dformat))
         :end (-> now (dminus (-> 1 (ddays))) (dformat))}
        ;; PREV
        (contains? #{ "prev" } (re-find #"prev" minor))
        {:beg (-> now (dminus (-> 14 (* middle) (ddays))) (dformat))
         :end (-> now (dminus (-> (+ (* 7 middle) 1) (ddays))) (dformat))}
        ;; THIS
        (contains? #{ "this" } (re-find #"this" minor))
          {:beg (-> now (dminus (-> dow (- 1) (ddays)))
                    (dminus (-> middle (- 1) (dweeks))) (dformat))
           :end (-> now (dminus (-> 1 (ddays))) (dformat))}
          
          :else beyesterday)

       ;;;;;;;;;;;;;;;;;
       ;; 4WEEK | MONTH
       ;;;;;;;;;;;;;;;;;
       (contains? #{ "4week" "month"} (re-find #"^4week|^month" major))
       (cond 
        ;; LAST | THIS
        (contains? #{ "last" "this" } (re-find #"last|this" minor))
        {:beg (-> now (dminus (-> 28 (* middle) (ddays))) (dformat))
         :end (-> now (dminus (-> 1 (ddays))) (dformat))}
        ;; PREV
        (contains? #{ "prev" } (re-find #"prev" minor))
        {:beg (-> now (dminus (-> 56 (* middle) (ddays))) (dformat))
         :end (-> now (dminus (-> (+ 1 (* 28 middle)) (ddays))) (dformat))}
        ;; ELSE
        :else beyesterday)
       
       ;; CALMONTH
       (= "calmonth" (re-find #"^calmonth" major))
       (cond 
        ;; LAST
        (contains? #{ "last" } (re-find #"last" minor))
        {:beg (-> now (dminus (-> middle (dmonths)))
                  (dminus (-> (- dom 1) (ddays))) (dformat))
         :end (-> now (dminus (-> dom (ddays))) (dformat))}
        ;; PREV
        (contains? #{ "prev" } (re-find #"prev" minor))
          {:beg (-> now (dminus (-> (* 2 middle) (dmonths))) 
                    (dminus (-> (- dom 1) (ddays))) (dformat))
           :end (-> now (dminus (-> middle (dmonths))) (dminus (-> dom (ddays))) (dformat))}

        ;; this CALMONTH
        (contains? #{ "this" } (re-find #"this" minor))
          {:beg (-> now 
                    (dminus (-> dom (- 1) (ddays)))
                    (dminus (-> middle (- 1) (dmonths)))
                    (dformat))
           :end (-> now (dminus (-> 1 (ddays)))
                    (dformat))}
               
        :else nil)

       ;; QUARTER
       (= "quarter" (re-find #"^quarter" major))
       (cond 

        ;; last QUARTER
        (contains? #{ "last" } (re-find #"last" minor))
        {:beg (-> now 
                  (dminus (-> dom (- 1) (ddays)))
                  (dminus (-> moy (modquarter) (dmonths)))
                  (dminus (-> 3 (* middle) (dmonths)))
                  (dformat))
         :end (-> now 
                  (dminus (-> dom (- 1) (ddays)))
                  (dminus (-> moy (modquarter) (dmonths)))
                  (dminus (-> 1 (ddays)))
                  (dformat))}

        ;; prev QUARTER
        (contains? #{ "prev" } (re-find #"prev" minor))
          {:beg (-> now 
                    (dminus (-> dom (- 1) (ddays)))
                    (dminus (-> moy (modquarter) (dmonths)))
                    (dminus (-> 6 (* middle) (dmonths)))
                    (dformat))
           :end (-> now 
                    (dminus (-> dom (- 1) (ddays)))
                    (dminus (-> moy (modquarter) (dmonths)))
                    (dminus (-> 3 (* middle) (dmonths)))
                    (dminus (-> 1 (ddays)))
                    (dformat))}
          
        ;; this QUARTER
        (contains? #{ "this" } (re-find #"this" minor))
        {:beg (-> now 
                  (dminus (-> dom (- 1) (ddays)))
                  (dminus (-> moy (modquarter) (dmonths)))
                  (dminus (-> 3 (* (- middle 1)) (dmonths)))
                  (dformat))
         :end (-> now
                  (dminus (-> 1 (ddays)))
                  (dformat))}

        :else nil)

       ;; HALF

       (= "half" (re-find #"^half" major))
       (cond 

        ;; last HALF
        (contains? #{ "last" } (re-find #"last" minor))
        {:beg (-> now 
                  (dminus (-> dom (- 1) (ddays)))
                  (dminus (-> moy (- (beghalf moy)) (dmonths)))
                  (dminus (-> 6 (* middle) (dmonths)))
                  (dformat))
         :end (-> now 
                  (dminus (-> dom (- 1) (ddays)))
                  (dminus (-> moy (- (beghalf moy)) (dmonths)))
                  (dminus (-> 1 (ddays)))
                  (dformat))}

        ;; prev HALF
        (contains? #{ "prev" } (re-find #"prev" minor))
          {:beg (-> now
                    (dminus (-> dom (- 1) (ddays)))
                    (dminus (-> moy (- (beghalf moy)) (dmonths)))
                    (dminus (-> 12 (* middle) (dmonths)))
                    (dformat))
           :end (-> now
                    (dminus (-> dom (- 1) (ddays)))
                    (dminus (-> moy (- (beghalf moy)) (dmonths)))
                    (dminus (-> 6 (* middle) (dmonths)))
                    (dminus (-> 1 (ddays)))
                    (dformat))}

        ;; this HALF
        (contains? #{ "this" } (re-find #"this" minor))
        {:beg (-> now 
                  (dminus (-> dom (- 1) (ddays)))
                  (dminus (-> moy (- (beghalf moy)) (dmonths)))
                  (dminus (-> 6 (* (- middle 1)) (dmonths)))
                  (dformat))
         :end (-> now
                  (dminus (-> 1 (ddays)))
                  (dformat))}
        :else beyesterday)

       ;; YEAR

       (= "year" (re-find #"^year" major))
       (cond 

        ;; last YEAR
        (contains? #{ "last" } (re-find #"last" minor))
        {:beg (-> (str (- yea middle) "-01-01")
                  (tc/from-string)
                  (dformat))
         :end (-> (str (- yea 1) "-12-31")
                  (tc/from-string)
                  (dformat))}

        ;; prev YEAR
        (contains? #{ "prev" } (re-find #"prev" minor))
          {:beg (-> (str (- yea (* 2 middle)) "-01-01")
                    (tc/from-string)
                    (dformat))
           :end (-> (str (- yea (+ 1 middle)) "-12-31")
                    (tc/from-string)
                    (dformat))}


        ;; this YEAR
        (contains? #{ "this" } (re-find #"this" minor))
        {:beg (-> (str (- yea (- middle 1)) "-01-01")
                  (tc/from-string)
                  (dformat))
         :end (-> now
                  (dminus (-> 1 (ddays)))
                  (dformat))}
        :else beyesterday)


       :else beyesterday)
    )))
