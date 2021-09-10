(defn- mk-boolean [v]
  #(when (dyn :debug) (eprintf "DEBUG: mk-boolean '%q'" v))
  (cond
    (= v "false") false
    (= v "true") true
    # should never happen...
    (errorf "invalid boolean: '%q'" v)))

(defn- mk-array [lst]
  #(when (dyn :debug) (eprintf "DEBUG: mk-array '%q'" lst))
  lst)

(defn- mk-number [str]
  (var n (scan-number (string/trim str)))
  #(when (dyn :debug) (eprintf "DEBUG: mk-number '%q' --> '%q'" str n))
  (when (nil? n)
    (errorf ("Failed to convert parsed number '%q'" str)))
  n)

(defn- mk-keyword [& str]
  #(when (dyn :debug) (eprintf "DEBUG: mk-keyword '%q'" str))
  (map keyword str))

(defn- mk-table [lst]
  (def accum @{})
  (var idx 0)
  (while (< idx (length lst))
    (def k (lst idx))
    (def v (lst (++ idx)))
    (put-in accum k v)
    (++ idx))
  accum)

(defn- mk-string [str]
  #(when (dyn :debug) (eprintf "DEBUG: mk-string '%q'" str))
  str)

(def json-grammar
  `PEG for converting a string containing JSON into a Janet datastructure.
   Relies on private helper functions for the conversions.`
  (peg/compile
   ~{:json (sequence :ws* :value :ws*)
     :value (choice
               (/ (% :string) ,mk-string)
               (/ (<- :number) ,mk-number)
               (/ (group :object) ,mk-table)
               (/ (group :array) ,mk-array)
               (/ (sequence :ws* (<- :boolean) :ws*) ,mk-boolean)
               (/ (sequence :ws* :null :ws*) :null))

     :object (sequence :begin-object (any :members) :end-object)
     :array (sequence :begin-array (any :elements) :end-array)

     :member (sequence :ws* (/ :string ,mk-keyword) :name-separator :element)
     :members (sequence :member (any (sequence :value-separator :member)))
     :element (sequence :ws* :value :ws*)
     :elements (sequence :element (any (sequence :value-separator :element)))
     :string (sequence
                :ws*
                :double-quote
                (<- (any (choice (sequence :backslash :escaped) :ascii-printable :ascii-extended :space)))
                :double-quote
                :ws*)

     :number (sequence :ws* (not :illegal-numbers) (any "-") :integer (any :fraction) (any :exponent) :ws*)
     :illegal-numbers (sequence (any "-") "0" :d)
     :integer (choice (sequence :onenine :d*) "0")
     :fraction (sequence "." (choice :onenine "0") :d*)
     :exponent (sequence (set "eE") (any :sign) :onenine :d*)
     :onenine (range "19")
     :sign (set "+-")

     :begin-array (sequence :ws* "[" :ws*)
     :begin-object (sequence :ws* "{" :ws*)
     :end-array (sequence :ws* "]" :ws*)
     :end-object (sequence :ws* "}" :ws*)
     :name-separator (sequence :ws* ":" :ws*)
     :value-separator (sequence :ws* "," :ws*)

     :space "\x20"
     :htab "\x09"
     :nl "\x0A"
     :cr "\x0D"
     :ws (choice :space :nl :cr :htab)
     :ws* (any :ws)
     :ws+ (some :ws)
     :double-quote "\""
     :backslash "\\"
     :slash "/"
     :escaped (choice :double-quote :backslash :slash (set  "bfnrt") (sequence "u" (repeat 4 :h)))

     :ascii-printable (range "\x20\x21" "\x23\x7F")   # no double-quote
     :ascii-extended (range "\xA0\xFF")

     :boolean (choice :true :false)
     :true "true"
     :false "false"
     :null "null"

     :main (sequence :json -1)}))

(defn parse [v]
  (try (let [p (peg/match json-grammar v)]
         (cond
           (nil? p) (eprint "Nothing parsed from supplied string")
           (empty? p) (eprint "Empty parse from supplied string")
           (p 0)))
       ([err] (eprintf "Error- Parsing failed: %q" err))))
