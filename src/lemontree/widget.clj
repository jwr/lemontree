(in-ns 'lemontree)

(defstruct widget :type :dom-id)

(defn replace-children [widget children]
  (if children
    (with-meta widget (assoc ^widget :children children))
    (with-meta widget (dissoc ^widget :children))))

(defn widget-children [widget]
  (:children ^widget))

(defn make-widget
  ([] (make-widget :widget))
  ([type]
     (struct-map widget
       :type type
       :dom-id (str (gensym "id"))))
  ([type children]
     (replace-children (make-widget type) children)))


(defmulti render :type)

(defmethod render :widget [widget]
  (html [:h1 (:dom-id widget)]))

