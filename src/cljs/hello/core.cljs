;; All code in this file comes from David Nolen's excellent Om Basic Tutorial:
;;   https://github.com/swannodette/om/wiki/Basic-Tutorial
;;
;; Live code reloading is provided by Immo Heikkinen's lively library:
;;   https://github.com/immoh/lively

(ns hello.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require lively
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.data :as data]
            [clojure.string :as string]))

(lively/start "/js/hello.js")

(enable-console-print!)

(def app-state
  (atom
   {:contacts
    [{:first "Ben"                         :last "Bitdiddle" :email "benb@mit.edu"}
     {:first "Alyssa" :middle-initial "P"  :last "Hacker"    :email "aphacker@mit.edu"}
     {:first "Eva"    :middle         "Lu" :last "Ator"      :email "eval@mit.edu"}
     {:first "Louis"                       :last "Reasoner"  :email "prolog@mit.edu"}
     {:first "Cy"     :middle-initial "D"  :last "Effect"    :email "bugs@mit.edu"}
     {:first "Lem"    :middle-initial "E"  :last "Tweakit"   :email "morebugs@mit.edu"}]
    :people
    [{:type :student :first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
     {:type :student :first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
     {:type :professor :first "Gerald" :middle "Jay" :last "Sussman" :email "metacirc@mit.edu" :classes [:6001 :6946]}
     {:type :student :first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
     {:type :student :first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
     {:type :professor :first "Hal" :last "Abelson" :email "evalapply@mit.edu" :classes [:6001]}]
    :classes
    {:6001 "The Structure and Interpretation of Computer Programs"
     :6946 "The Structure and Interpretation of Classical Mechanics"
     :1806 "Linear Algebra"}}))

(defn parse-contact [contact-str]
  (let [[first middle last :as parts] (string/split contact-str #"\s+")
        [first last middle] (if (nil? last) [first middle] [first last middle])
        middle (when middle (string/replace middle "." ""))
        c (if middle (count middle) 0)]
    (when (>= (count parts) 2)
      (cond-> {:first first :last last}
              (== c 1) (assoc :middle-initial middle)
              (>= c 2) (assoc :middle middle)))))

(defn add-contact [app owner]
  (let [new-contact (-> (om/get-node owner "new-contact")
                        .-value
                        parse-contact)]
    (when new-contact
      (om/transact! app :contacts #(conj % new-contact))
      (om/set-state! owner :text ""))))

(defn middle-name [{:keys [middle middle-initial]}]
  (cond
   middle (str " " middle)
   middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
              (dom/span nil (display-name contact))
              (dom/button #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))

(defn handle-input-change [e owner {:keys [text]}]
  (let [value (.. e -target -value)]
    (if-not (re-find #"[0-9]" value)
      (om/set-state! owner :text value)
      (om/set-state! owner :text text))))

(defn contacts-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)
       :text ""})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go (loop []
              (let [contact (<! delete)]
                (om/transact! app :contacts
                              (fn [xs] (vec (remove #(= contact %) xs))))
                (recur))))))
    om/IRenderState
    (render-state [this state]
      (dom/div nil
               (dom/h2 nil "Contact list")
               (apply dom/ul nil
                      (om/build-all contact-view
                                    (:contacts app)
                                    {:init-state state}))
               (dom/div nil
                        (dom/input #js {:type "text" :ref "new-contact" :value (:text state)
                                        :onChange #(handle-input-change % owner state)})
                        (dom/button #js {:onClick #(add-contact app owner)} "Add contact"))))))

(om/root contacts-view
         app-state
         {:target (. js/document (getElementById "contacts"))})

(defn student-view [student owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil (display-name student)))))

(defn professor-view [professor owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil
              (dom/div nil (display-name professor))
              (dom/label nil "Classes")
              (apply dom/ul nil
                     (map #(dom/li nil (om/value %)) (:classes professor)))))))

(defmulti entry-view (fn [person _] (:type person)))

(defmethod entry-view :student [person owner]
  (student-view person owner))

(defmethod entry-view :professor [person owner]
  (professor-view person owner))

(defn people [app]
  (->> (:people app)
       (mapv (fn [x]
               (if (:classes x)
                 (update-in x [:classes]
                            (fn [cs] (mapv (:classes app) cs)))
                 x)))))

(defn registry-view [app owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (dom/div #js {:id "registry"}
               (dom/h2 nil "Registry")
               (apply dom/ul nil
                      (om/build-all entry-view (people app)))))))

(om/root registry-view
         app-state
         {:target (. js/document (getElementById "registry"))})

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))

(defn handle-change [e text owner]
  (om/transact! text (fn [_] (.. e -target -value))))

(defn commit-change [text owner]
  (om/set-state! owner :editing false))

(extend-type string
  ICloneable
  (-clone [s] (js/String. s)))

(extend-type js/String
  ICloneable
  (-clone [s] (js/String. s))
  om/IValue
  (-value [s] (str s)))

(defn editable [text owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      (dom/li nil
              (dom/span #js {:style (display (not editing))} (om/value text))
              (dom/input
               #js {:style (display editing)
                    :value (om/value text)
                    :onChange #(handle-change % text owner)
                    :onKeyDown #(when (= (.-key %) "Enter")
                                  (commit-change text owner))
                    :onBlur (fn [e] (commit-change text owner))})
              (dom/button
               #js {:style (display (not editing))
                    :onClick #(om/set-state! owner :editing true)}
               "Edit")))))

(defn classes-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "classes"}
               (dom/h2 nil "Classes")
               (apply dom/ul nil
                      (om/build-all editable (vals (:classes app))))))))

(om/root classes-view
         app-state
         {:target (. js/document (getElementById "classes"))})
