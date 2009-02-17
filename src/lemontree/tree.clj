(in-ns 'lemontree)

(defn make-widget-tree [root]
  (zip/zipper #(contains? ^% :children)
	      #(seq (:children ^%))
	      replace-children
	      root))

(defmacro with-test-tree [tree-var & body]
  `(let [~tree-var
	 (make-widget-tree
	  (replace-children (assoc (make-widget) :dom-id "home-widget")
			    (list (assoc (make-widget) :dom-id "empty-widget" :path '(""))
				  (replace-children (assoc (make-widget) :dom-id "users-widget" :path '("users"))
						    (list (assoc (make-widget) :dom-id "all-widget" :path '("all"))))
				  (assoc (make-widget) :dom-id "misc-widget" :path '("misc")))))]
     ~@body))

(defn lift-loc-op
  "Change a function loc -> string into a function seed -> seed"
  [f]
  (fn [seed & more]
    (let [result (apply f seed more)]
      (assoc seed :render-list (cons result (:render-list seed))))))

(defn lift-modifying-loc-op
  "Change a function loc -> loc into a function seed -> seed, replacing the loc in the seed with the one returned by the function."
  [f]
  (fn [seed & more]
    (let [result (apply f seed more)]
      (assoc seed :tree-loc result))))

(defn unit-seed
  "Build an empty seed from loc and uri-tokens."
  [loc uri-tokens]
  (hash-map :tree-loc loc
	    :render-list nil
	    :consumed-tokens nil
	    :remaining-tokens uri-tokens))

;;; tokens we have consumed so far
(def *consumed-tokens* nil)

;;; remaining tokens in the URI
(def *remaining-tokens* nil)

(defn lift-tree-op
  "Change a zipper operation into an operation on seeds."
  [op-fn]
  (fn [seed] (assoc seed :tree-loc (op-fn (:tree-loc seed)))))

(defmacro with-token-match
  "Tries to match remaining URI-tokens agains the supplied seed. If
try-matching is false, just executes body. Otherwise, calls
match-tokens, binds *remaining-tokens* and *consumed-tokens* to new
values, executes body only if there is a match, otherwise simply returns
the seed."
  [try-matching seed & body]
  `(if (not ~try-matching)
     ~@body
     (let [[match# consumed# remaining#]
	   (match-tokens (zip/node (:tree-loc ~seed)) *remaining-tokens* *consumed-tokens*)]
       (if (not match#)
	 ~seed
	 (binding [*consumed-tokens* consumed#
		   *remaining-tokens* remaining#]
	   ~@body)))))

(let [[down up right] (map lift-tree-op [zip/down zip/up zip/right])]
  (defn foldts
    "Perform a fold on a tree calling functions fdown fup and fhere at appropriate times."
    ([fdown fup fhere seed try-matching lift-operation]
       (let [[fd fu fh] (map lift-operation [fdown fup fhere])]
	 (with-token-match try-matching seed
	   (if (not (zip/branch? (:tree-loc seed)))
	     (fh seed)			; call fhere on leaf nodes only
	     (loop [kid-seed (down (fd seed))]
	       (if (not (zip/right (:tree-loc kid-seed)))
		 (up (fu (foldts fdown fup fhere kid-seed try-matching) seed))
		 (recur (right (foldts fdown fup fhere kid-seed try-matching)))))))))
    ([fdown fup fhere seed try-matching] (foldts fdown fup fhere seed try-matching lift-loc-op))
    ([fdown fup fhere seed] (foldts fdown fup fhere seed true lift-loc-op))))


(defn render-widget-tree [widget-tree uri-tokens]
  (reverse
   (:render-list
    (binding [*remaining-tokens* uri-tokens]
      (foldts
       (fn [seed]
	 (str "<div>" (render (zip/node (:tree-loc seed)))))
       (fn [seed old-seed]
	 (println "fup -- consumed: " *consumed-tokens* "remaining: " *remaining-tokens*)
	 "</div>")
       (fn [seed]
	 (str "<div>" (render (zip/node (:tree-loc seed))) "</div>"))
       (unit-seed widget-tree uri-tokens))))))

