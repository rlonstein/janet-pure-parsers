### Run the supplied file name through the parser,
### suitable for execution against the JSON Test Suite
###
### ex. janet -n -q -s ./test-json.janet <fn>
###
(import ../../json)

(def args (dyn :args))
(var s (slurp (args 1)))
(try
  (do (var result (json/parse s))
      (if (nil? result)
        (os/exit 1)
        (do
          (printf "%q" result)
          (os/exit 0))))
  ([err] (do (eprintf "%q" err) (os/exit 1))))

