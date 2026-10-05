(ns replicant.dom-test
  (:require [clojure.test :refer [deftest is testing]]
            [replicant.dom :as d]))

;; A minimal document: just enough of the DOM for replicant.dom to render into,
;; so the tests can run in node. Nodes created through it directly, and not by
;; Replicant, play the part of nodes inserted by browser extensions.

(defn- detach [^js node]
  (when-let [^js parent (.-parentNode node)]
    (let [siblings (.-childNodes parent)]
      (.splice siblings (.indexOf siblings node) 1))
    (set! (.-parentNode node) nil)))

(defn- clear [^js node]
  (doseq [child (vec (.-childNodes node))]
    (detach child)))

(defn- define-property [obj prop descriptor]
  (js/Object.defineProperty obj prop descriptor))

(defn- create-node [tag-name text]
  (let [attrs (atom {})
        node #js {:tagName tag-name
                  :nodeValue text
                  :parentNode nil
                  :childNodes #js []
                  :isConnected true
                  :classList #js {:add (fn [_]) :remove (fn [_])}
                  :style #js {:setProperty (fn [_ _]) :removeProperty (fn [_])}
                  :addEventListener (fn [_ _ _])
                  :removeEventListener (fn [_ _ _])
                  :setAttribute (fn [k v] (swap! attrs assoc k v))
                  :removeAttribute (fn [k] (swap! attrs dissoc k))
                  :getAttribute (fn [k] (get @attrs k))}]
    (define-property node "firstChild"
      #js {:get (fn [] (or (aget (.-childNodes node) 0) nil))})
    (define-property node "nextSibling"
      #js {:get (fn []
                  (when-let [^js parent (.-parentNode node)]
                    (let [siblings (.-childNodes parent)]
                      (or (aget siblings (inc (.indexOf siblings node))) nil))))})
    ;; Setting markup replaces the children with nodes that Replicant did not create
    (define-property node "innerHTML"
      #js {:set (fn [markup]
                  (clear node)
                  (when (seq markup)
                    (.appendChild node (create-node "parsed-markup" nil))))})
    (define-property node "textContent"
      #js {:set (fn [_] (clear node))})
    (set! (.-insertBefore node)
          (fn [^js child reference]
            (detach child)
            (let [siblings (.-childNodes node)]
              (.splice siblings (if reference (.indexOf siblings reference) (.-length siblings)) 0 child))
            (set! (.-parentNode child) node)
            child))
    (set! (.-appendChild node)
          (fn [child]
            (.insertBefore node child nil)))
    (set! (.-removeChild node)
          (fn [^js child]
            (when-not (identical? node (.-parentNode child))
              (throw (js/Error. "The node to be removed is not a child of this node")))
            (detach child)
            child))
    (set! (.-replaceChild node)
          (fn [new-child old-child]
            (.insertBefore node new-child old-child)
            (.removeChild node old-child)))
    node))

(def ^:private document
  #js {:createElement (fn [tag-name] (create-node tag-name nil))
       :createElementNS (fn [_ tag-name] (create-node tag-name nil))
       :createTextNode (fn [text] (create-node nil text))})

(defn- with-document [f]
  (let [original (.-document js/globalThis)]
    (set! (.-document js/globalThis) document)
    (try
      (f)
      (finally
        (set! (.-document js/globalThis) original)))))

(defn- ->hiccup [^js node]
  (if-let [tag-name (.-tagName node)]
    (into [(keyword tag-name)] (map ->hiccup (.-childNodes node)))
    (.-nodeValue node)))

(defn- child [^js node & path]
  (reduce (fn [^js node idx] (aget (.-childNodes node) idx)) node path))

(defn- insert-foreign-after [^js node tag-name]
  (.insertBefore (.-parentNode node) (.createElement document tag-name) (.-nextSibling node)))

(defn- form [{:keys [error]}]
  [:form
   [:input {:type "email"}]
   (when error
     [:p error])
   [:button {:disabled (boolean error)} "Send"]])

(deftest render-test
  (testing "Inserts, updates and removes nodes"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el (form {:error "Not an email address"}))
          (is (= (->hiccup el)
                 [:div [:form [:input] [:p "Not an email address"] [:button "Send"]]]))
          (is (= (.getAttribute (child el 0 2) "disabled") true))

          (d/render el (form {}))
          (is (= (->hiccup el)
                 [:div [:form [:input] [:button "Send"]]]))
          (is (nil? (.getAttribute (child el 0 1) "disabled"))))))))

(deftest foreign-nodes-test
  (testing "Updates the node after an element inserted by other code"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el (form {:error "Not an email address"}))
          ;; What a password manager does to an input field
          (insert-foreign-after (child el 0 0) "password-manager")

          (d/render el (form {}))
          (is (= (->hiccup el)
                 [:div [:form [:input] [:password-manager] [:button "Send"]]]))
          (is (nil? (.getAttribute (child el 0 2) "disabled")))))))

  (testing "Inserts a node at its position among the nodes Replicant created"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el (form {}))
          (insert-foreign-after (child el 0 0) "password-manager")

          (d/render el (form {:error "Not an email address"}))
          (is (= (->hiccup el)
                 [:div [:form [:input] [:password-manager] [:p "Not an email address"] [:button "Send"]]]))
          (is (= (.getAttribute (child el 0 3) "disabled") true))))))

  (testing "Replaces the text after an element inserted by other code"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el [:p [:strong "Hello"] " world"])
          (insert-foreign-after (child el 0 0) "translation")

          (d/render el [:p [:strong "Hello"] " there"])
          (is (= (->hiccup el)
                 [:div [:p [:strong "Hello"] [:translation] " there"]]))))))

  (testing "Ignores foreign nodes among the top-level nodes"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el (list [:h1 "Title"] [:p "One"]))
          (.insertBefore el (.createElement document "extension") (child el 0))

          (d/render el (list [:h1 "Title"] [:p "Two"]))
          (is (= (->hiccup el)
                 [:div [:extension] [:h1 "Title"] [:p "Two"]]))))))

  (testing "Moves keyed nodes past an element inserted by other code"
    (with-document
      (fn []
        (let [el (.createElement document "div")
              item (fn [k & [text]] [:li {:replicant/key k} (or text (str "Item " k))])]
          (d/render el [:ul (item 1) (item 2) (item 3)])
          (insert-foreign-after (child el 0 0) "extension")

          (d/render el [:ul (item 3 "Item 3 (moved)") (item 1) (item 2)])
          (is (= (remove #{[:extension]} (rest (->hiccup (child el 0))))
                 [[:li "Item 3 (moved)"] [:li "Item 1"] [:li "Item 2"]]))

          (d/render el [:ul (item 3) (item 2)])
          (is (= (remove #{[:extension]} (rest (->hiccup (child el 0))))
                 [[:li "Item 3"] [:li "Item 2"]]))))))

  (testing "Keeps finding nodes after the foreign node is gone"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el (form {}))
          (let [foreign (insert-foreign-after (child el 0 0) "password-manager")]
            (d/render el (form {:error "Not an email address"}))
            (.removeChild (child el 0) foreign))

          (d/render el (form {}))
          (is (= (->hiccup el)
                 [:div [:form [:input] [:button "Send"]]]))
          (is (nil? (.getAttribute (child el 0 1) "disabled")))))))

  (testing "Renders children in place of markup set with innerHTML"
    (with-document
      (fn []
        (let [el (.createElement document "div")]
          (d/render el [:div {:innerHTML "<em>Hello</em>"}])
          (is (= (->hiccup el) [:div [:div [:parsed-markup]]]))

          (d/render el [:div [:em "Hello"] [:em "there"]])
          (d/render el [:div [:em "Hello"] [:em "you"]])
          (is (= (->hiccup el) [:div [:div [:em "Hello"] [:em "you"]]])))))))
