(def- csv-record-grammar
  `PEG grammar for parsing a line of CSV, collects fields into single record`
  '{:main (group :record)

    :double-quote "\""
    :escape "\\"
    :nl (choice "\r\n" "\n")
    :field-separator ","
    :chars (range "\x20\x21" "\x23\x2B" "\x2D\x7E")

    :record (sequence :field (any (sequence :field-separator :field)))
    :field (choice :quoted :unquoted)
    :quoted (<- (sequence :double-quote :quoted-content :double-quote))
    :unquoted (<- (any :unquoted-content))
    :unquoted-content (any (choice :chars (not (choice :double-quote :nl :field-separator)) ))
    :quoted-content (any (choice :escaped-chars :unquoted-content))
    :escaped-chars (sequence :escape (choice :field-separator :double-quote))
   })

(defn iter
  `Create a fiber that iterates over the supplied open file handle
parsing the lines as CSV records.

Optionally specify the field separator pattern (default \",\").`
  [filehandle &opt separator]
  (def grammar @{})
  (merge-into grammar csv-record-grammar)
  (if separator (set (grammar :field-separator) separator))
  (def parser (peg/compile csv-record-grammar))
  (fiber/new (fn []
               (loop [line :iterate (file/read filehandle :line)]
                 (yield (first (peg/match parser line)))))))

(defn parse
  `Parse the contents of the specified filename into an array of records.

Specify the field separator with :separator (default \",\").

Setting :has-header true will use the first row of the file as column
names, converting them to keywords, and produce table records. Extra
values that exceed the column names in the header will use their index
position as a column name.`
  [filename &keys{:separator separator :has-header has-header}]
  (def valid-states {:new true :alive true :pending true})
  (def records @[])
  (defn mktable
    [keys values]
    (var tbl @{})
    (def numkeys (length keys))
    (for idx 0 (length values)
      (if (< idx numkeys)
        (put tbl (keys idx) (values idx))
        (put tbl (keyword idx) (values idx))))
    tbl)

  (with [fh (file/open filename)]
        (def iter (csv-iter fh separator))
        (var rec @[])
        (var header nil)
        (while (in valid-states (fiber/status iter))
          (set rec (resume iter))
          (if rec
            (cond
              (and has-header (nil? header)) (set header (map keyword rec))
              (and has-header (not (nil? header))) (array/insert records -1 (mktable header rec))
              (array/insert records -1 rec)))))
  records)
