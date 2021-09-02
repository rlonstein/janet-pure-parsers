(defn- mk-boolean [v]
  (cond
    (= v "false") false
    (= v "true") true
    # should never happen...
    (errorf "invalid boolean: '%q'" v)))

(defn- mk-array [lst]
  lst)

(defn- mk-number [str]
  (scan-number str))

(defn- mk-keyword [& str]
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

# FIXME only used for tracing
(defn- mk-string [str]
  str)

(def json-grammar
  `PEG for converting a string containing JSON into a Janet datastructure.
   Relies on private helper functions for the conversions.

   TODO: Unicode support

   TODO: Tests
`
  (peg/compile
   ~{:json (sequence :ws* :value :ws*)
     :value (choice
               (/ (% :string) ,mk-string)
               (/ (<- :number) ,mk-number)
               (/ (group :object) ,mk-table)
               (/ (group :array) ,mk-array)
               (/ (sequence :ws* (<- :boolean) :ws*) ,mk-boolean)
               (/ (sequence :ws* :null :ws*) :null))

     :object (sequence :begin-object :members :end-object)
     :array (sequence :begin-array :elements :end-array)

     :member (sequence :ws* (/ :string ,mk-keyword) :name-separator :element)
     :members (sequence :member (any (sequence :value-separator :member)))
     :element (sequence :ws* :value :ws*)
     :elements (sequence :element (any (sequence :value-separator :element)))

     :string (sequence
                :ws*
                :double-quote
                # FIXME- :w doesn't capture all printables & these ranges don't work as expected
                (<- (any (choice :w* (any :ascii-printable) (any :ascii-extended))))
                :double-quote
                :ws*)
     :ascii-printable (range "\x20\x7E")
     :ascii-extended (range "\xA0\xFF")

     :number (sequence :ws* :integer :fraction :exponent :ws*)
     :integer (choice
                (sequence :dash :onenine :d*)
                (sequence :dash :d)
                (sequence :onenine :d*)
                :d)
     :fraction (choice (sequence "." :d+) "")
     :exponent (choice (sequence (set "eE") :sign :d+) "")
     :onenine (range "19")
     :dash "-"
     :sign (choice (set "+-") "")

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
     :escaped (choice :double-quote :backslash :slash "b" "f" "n" "r" "t" (sequence "u" (repeat 4 :h)))

     :boolean (choice :true :false)     
     :true "true"
     :false "false"
     :null "null"

     :main (sequence :json -1)}))

(defn parse-json [v]
  (var p (peg/match json-grammar v))
  # FIXME- handle errors
  (get p 0))

(def js1 `{"foo":1,"bar":{"x":"y", "z": null},"baz":["abc","def","ghi"],"quxx":true,"blarg":false,"nums":[-1,0,1.23,-4.32,6.02e23]}`)
(def js2 `{
  "items": {
    "item": [
      {
        "id": "0001",
        "type": "donut",
        "name": "Cake",
        "ppu": 0.55,
        "batters": {
          "batter": [
            {
              "id": "1001",
              "type": "Regular"
            },
            {
              "id": "1002",
              "type": "Chocolate"
            },
            {
              "id": "1003",
              "type": "Blueberry"
            },
            {
              "id": "1004",
              "type": "Devil's Food"
            }
          ]
        },
        "topping": [
          {
            "id": "5001",
            "type": "None"
          },
          {
            "id": "5002",
            "type": "Glazed"
          },
          {
            "id": "5005",
            "type": "Sugar"
          },
          {
            "id": "5007",
            "type": "Powdered Sugar"
          },
          {
            "id": "5006",
            "type": "Chocolate with Sprinkles"
          },
          {
            "id": "5003",
            "type": "Chocolate"
          },
          {
            "id": "5004",
            "type": "Maple"
          }
        ]
      }
    ]
  }
}`)


(def js3 `
{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 27,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ],
  "children": [],
  "spouse": null
}
`)

(def js4 `[
      {
         "precision": "zip",
         "Latitude":  37.7668,
         "Longitude": -122.3959,
         "Address":   "",
         "City":      "SAN FRANCISCO",
         "State":     "CA",
         "Zip":       "94107",
         "Country":   "US"
      },
      {
         "precision": "zip",
         "Latitude":  37.371991,
         "Longitude": -122.026020,
         "Address":   "",
         "City":      "SUNNYVALE",
         "State":     "CA",
         "Zip":       "94085",
         "Country":   "US"
      }
   ]`)

(def js5 `{
      "Image": {
          "Width":  800,
          "Height": 600,
          "Title": "View from 15th Floor",
          "Thumbnail": {
              "Url":    "http://www.example.com/image/481989943",
              "Height": 125,
              "Width":  "100"
          },
          "IDs": [116, 943, 234, 38793]}}`)
