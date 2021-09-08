#!/bin/bash -xe

SUITE="./json-test-suite"
if [ ! -e "${SUITE}" ]; then
    git clone --depth 1 https://github.com/nst/JSONTestSuite "${SUITE}"
fi

SCRIPT_PATH="${SUITE}/parsers/test_janet_pp.sh"
if [ ! -e "${SCRIPT_PATH}" ]; then
cat <<- SCRIPT_END > "${SCRIPT_PATH}"
#/usr/bin/env sh
exec janet -s -n -q "${PWD}/test-json.janet" \$@
SCRIPT_END

chmod +x "${SCRIPT_PATH}"
fi

if ! grep Janet "${SUITE}/run_tests.py" >/dev/null; then
patch -d "${SUITE}" << "PATCH_END"
index 3312d8c..2d90f98 100755
--- i/run_tests.py
+++ w/run_tests.py
@@ -488,6 +488,11 @@ programs = {
        {
            "url":"https://github.com/nlohmann/json",
            "commands":[os.path.join(PARSERS_DIR, "test_nlohmann_json_20190718/bin/test_nlohmann_json")]
+       },
+    "Janet Pure Parsers":
+       {
+           "url":"https://sr.ht/~rlonstein/janet-pure-parsers",
+           "commands":[os.path.join(PARSERS_DIR, "test_janet_pp.sh")]
        }
 }
PATCH_END
fi

