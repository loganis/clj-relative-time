clj-relative-time
=================

A relative time library for clojure built on clj-time.

## Usage

``` bash
git clone https://github.com/loganis/clj-relative-time.git
cd clj-relative-time
lein test
```


### Vocabulary used in tests

``` clojure
["this" "last" "prev"] 
[1 2 3 4]  
["day" "week" "4week" "month" "calmonth" "quarter" "half" "year"]
```


### Sample test output

``` bash
...
{:this_1_day {:beg "2014-06-23", :end "2014-06-23"}}
{:this_1_week {:beg "2014-06-23", :end "2014-06-23"}}
{:this_1_4week {:beg "2014-05-27", :end "2014-06-23"}}
{:this_1_month {:beg "2014-05-27", :end "2014-06-23"}}
{:this_1_calmonth {:beg "2014-06-01", :end "2014-06-23"}}
{:this_1_quarter {:beg "2014-04-01", :end "2014-06-23"}}
{:this_1_half {:beg "2014-01-01", :end "2014-06-23"}}
{:this_1_year {:beg "2014-01-01", :end "2014-06-23"}}
...
```


### period2begend function

``` clojure
 (:use clj-relative-time.core)
 (period2begend "last_2_weeks")
```

## License

Copyright © 2014 Loganis - iWebMa Ltd.

Distributed under the Eclipse Public License, the same as Clojure.
