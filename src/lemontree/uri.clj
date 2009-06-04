(in-ns 'lemontree)

(defn common-subpath
  "Find a sequence of tokens that is common to both paths (lists of
  tokens), starting at the beginning. Return the longest matching
  sequence."
  [path1 path2]
  (remove nil? (map (fn [a b] (when (and a b (= a b)) a)) path1 path2)))

(deftest common-subpath-tests
  (is (= (common-subpath '("abc") '("abc" "cde")) '("abc")))
  (is (= (common-subpath '("abc" "cde") '("abc" "cde")) '("abc" "cde")))
  (is (= (common-subpath '("abc" "cde" "fgh") '("abc" "cde")) '("abc" "cde")))
  (is (= (common-subpath () '("abc" "cde")) ()))
  (is (= (common-subpath '("") '("" "cde")) '("")))
  (is (= (common-subpath '("abc") ()) ()))
  (is (= (common-subpath '("abc") '("cde")) ()))
  (is (= (common-subpath () '("abc" "cde")) ())))

(defn match-tokens
  "Tries to match supplied uri-tokens against the widget's :path using
  common-subpath. Returns a vector consisting of three values: whether
  the widget should be rendered, a list of consumed tokens and a list of
  remaining tokens."
  ([widget uri-tokens]
     (if (contains? widget :path)
       (let [matched-tokens (common-subpath (:path widget) uri-tokens)]
	 (if (seq matched-tokens)
	   [true matched-tokens (drop (count matched-tokens) uri-tokens)]
	   [false () uri-tokens]))
       [true () uri-tokens]))
  ([widget uri-tokens consumed-tokens]
     (let [[match matched remaining] (match-tokens widget uri-tokens)]
       [match (if (seq matched) (concat matched consumed-tokens) consumed-tokens) remaining])))

(deftest match-tokens-tests
  (let [w (assoc (make-widget) :dom-id "users-widget" :path '("users"))]

    (is (= (match-tokens w '("abc")) [false () '("abc")]))
    (is (= (match-tokens w '("users")) [true '("users") ()]))
    (is (= (match-tokens w '("users" "all")) [true '("users") '("all")]))
    (is (= (match-tokens w '("users" "")) [true '("users") '("")]))

    (is (= (match-tokens w '("abc") '("already-consumed")) [false '("already-consumed") '("abc")]))
    (is (= (match-tokens w '("users") '("already-consumed")) [true '("users" "already-consumed") ()]))
    (is (= (match-tokens w '("users" "all") '("already-consumed")) [true '("users" "already-consumed") '("all")]))
    (is (= (match-tokens w '("users" "") '("already-consumed")) [true '("users" "already-consumed") '("")]))

    (is (= (match-tokens w '("users" "") nil) [true '("users") '("")]))))

(defn tokenize-uri
  "Split an URI (without parameters) into a list of string tokens,
  making sure there is an empty string at the end."
  [uri]
  (concat (rest (str-utils/re-split #"/" uri)) '("")))

(deftest tokenize-uri-tests
  (is (= (tokenize-uri "") '("")))
  (is (= (tokenize-uri "/") '("")))
  (is (= (tokenize-uri "/abc") '("abc" "")))
  (is (= (tokenize-uri "/abc/cde") '("abc" "cde" ""))))
