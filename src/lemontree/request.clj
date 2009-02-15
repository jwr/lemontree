(in-ns 'lemontree)


(def widget-tree (with-test-tree w w))

(defn render-tree [uri]
  (apply str (render-widget-tree widget-tree (tokenize-uri uri))))

(defn process-request [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (render-tree (:uri req))})

(defn app [req]
  (process-request req))

;; (ring.jetty/run {:port 8081} app)
