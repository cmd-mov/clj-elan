(ns com.github.cmd-mov.clj-elan.core
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]
            [tech.v3.dataset.print :as print]
            [com.rpl.specter :as s]))

;;;;; =====================================================================
;;;;; # Section 1: Reading and writing ELAN files
;;;;; =====================================================================
(defn read-eaf
  "Parses an ELAN .eaf XML file from the given filename into a standard Clojure structure."
  [filename]
  (-> 
   (slurp filename)
   xml/parse-str))

(defn write-eaf
  "Writes to an ELAN .eaf XML file"
  [doc filename]
  (with-open [out-file (io/writer filename)]
  (xml/emit doc out-file)))

;;; --- REPL Playground & Examples ---
(comment
  (read-eaf (io/resource "segmentation-annotator-1.eaf")))

;;;;; =====================================================================
;;;;; # Section 2: Accessing and modifying components of ELAN file
;;;;; =====================================================================
;;;;; ELAN-document
;;;;; - header
;;;;; -- video rel path
;;;;; -- video path
;;;;; - controlled vocabulary
;;;;; -- controlled vocabulary entry 
;;;;; - linguistic type
;;;;; -- controlled vocabulary ref 
;;;;; - tier
;;;;; -- linguistic type ref
;;;;; -- annotations
;;;;; --- annotation
;;;;; - time-order
;;;;; -- time-slots
;;;;; --- time-slot

;;;;; ## Header operations
(def ^:private header-path [:content s/ALL #(= (:tag %) :HEADER)])
(def ^:private media-descriptor-path [header-path :content s/ALL #(= (:tag %) :MEDIA_DESCRIPTOR)])
(def ^:private video-url-path  [media-descriptor-path :attrs :MEDIA_URL])
(def ^:private video-rel-url-path [media-descriptor-path :attrs :RELATIVE_MEDIA_URL])

(defn header
  "Returns the ELAN header"
  [doc]
  (s/select-first header-path doc))

(defn media-descriptor
  "Returns the ELAN media descriptor"
  [doc]
   (s/select-first media-descriptor-path doc))

(defn video-url
  "Returns the absolute path to the video"
  [doc]
  (s/select-first video-url-path doc))

(defn video-rel-url
  "Returns the relative path to the video"
  [doc]
  (s/select-first video-rel-url-path doc))

(defn replace-video-url
  "Replaces the absolute path to the video"
  [doc new-url]
  (s/setval video-url-path new-url doc))
 
(defn replace-video-rel-url
  "Replaces the relative path to the video"
  [doc new-rel-url]
  (s/setval video-rel-url-path new-rel-url doc))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (header doc)
    (media-descriptor doc)
    (video-url doc)
    (video-rel-url doc)
    (video-url (replace-video-url doc "new/path/to/video.mp4"))
    (video-rel-url (replace-video-rel-url doc "new/rel/path/to/video.mp4"))))

;;;;; ## Controlled vocabulary operations
(def ^:private cvs-path (s/walker #(= (:tag %) :CONTROLLED_VOCABULARY)))
(defn- cv-path [cv-id]
  [cvs-path (s/selected? :attrs :CV_ID #(= % cv-id))])

(defn cvs
  "Returns all controlled vocabularies"
  [doc]
  (s/select cvs-path doc))

(defn cv-ids
  "Returns all controlled vocabulary ids"
  [doc]
  (s/select [cvs-path :attrs :CV_ID] doc))

(defn validate-cv-id! [doc cv-id]
  (when-not (some #{cv-id} (cv-ids doc))
    (throw (ex-info "Controlled vocabulary ID not present in doc."
                    {:error-type :invalid-input
                     :bad-value cv-id
                     :context :cv-validation}))))

(defn cv
  "Returns a controlled vocabulary by ID"
  [doc cv-id]
  (validate-cv-id! doc cv-id)
  (s/select-first (cv-path cv-id) doc))

(defn remove-cv
  "Removes a controlled vocabulary by ID"
  [doc cv-id]
  (s/setval (cv-path cv-id) s/NONE doc))

(defn add-cv
  "Adds a new controlled vocabulary"
  [doc cv]
  (s/setval [:content s/AFTER-ELEM] cv doc))

(defn copy-cv
  "Copies a controlled vocabulary with a given ID from one document to another"
  [doc-from doc-to cv-id]
  (add-cv doc-to (cv doc-from cv-id)))

(defn cv-vocab-with-ids
  "Returns a list of all controlled vocabulary entries and their associated unique identifiers"
  [doc cv-id]
  (letfn [(cve-id-and-content [cv-entry]
            {:cve-id (-> cv-entry :attrs :CVE_ID)
             :content (->> cv-entry :content (filter map?) first :content first)})]
    (->> (cv doc cv-id)
         (s/select (s/walker #(= (:tag %) :CV_ENTRY_ML)))
         (map cve-id-and-content))))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (cvs doc)
    (cv-ids doc)
    (cv doc (first (cv-ids doc)))
    (remove-cv doc (first (cv-ids doc)))
    (cv-vocab-with-ids doc (first (cv-ids doc)))))

;;;;; ## Linguistic types operations 
(def ^:private linguistic-types-path (s/walker #(= (:tag %) :LINGUISTIC_TYPE)))
(defn- linguistic-type-path [linguistic-type-id]
  [linguistic-types-path (s/selected? :attrs :LINGUISTIC_TYPE_ID #(= % linguistic-type-id))])

(defn linguistic-types
  "Returns all linguistic types"
  [doc]
  (s/select linguistic-types-path doc))

(defn linguistic-type-ids
  "Returns all linguistic type IDs"
  [doc]
  (s/select [linguistic-types-path :attrs :LINGUISTIC_TYPE_ID] doc))

(defn validate-linguistic-type-id! [doc linguistic-type-id]
  (when-not (some #{linguistic-type-id} (linguistic-type-ids doc))
    (throw (ex-info "Linguistic type ID not present in doc."
                    {:error-type :invalid-input
                     :bad-value linguistic-type-id
                     :context :linguistic-type-validation}))))
  
(defn linguistic-type
  "Returns a linguistic type with a given ID"
  [doc linguistic-type-id]
  (validate-linguistic-type-id! doc linguistic-type-id)
  (s/select-first (linguistic-type-path linguistic-type-id) doc))

(defn linguistic-type-cv
  "Returns the controlled vocabulary id of a linguistic type with a given ID"
  [doc linguistic-type-id]
  (validate-linguistic-type-id! doc linguistic-type-id)
  (s/select-first [:attrs :CONTROLLED_VOCABULARY_REF] (linguistic-type doc linguistic-type-id)))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (linguistic-types doc)
    (linguistic-type-ids doc)
    (linguistic-type doc (first (linguistic-type-ids doc)))
    (linguistic-type doc (second (linguistic-type-ids doc)))
    (linguistic-type doc "wrong-type")
    (linguistic-type-cv doc (first (linguistic-type-ids doc)))
    (linguistic-type-cv doc (second (linguistic-type-ids doc)))))

;;;;; ## Tier operations
(def ^:private tiers-path [:content s/ALL #(= (:tag %) :TIER)])
(defn- tier-path [tier-id]
  [tiers-path #(= (-> % :attrs :TIER_ID) tier-id)])

(defn tiers
  "Returns all tiers in a document"
  [doc]
  (s/select tiers-path doc))

(defn tier-ids
  "Returns all tier IDs in a document"
  [doc]
  (s/select [tiers-path :attrs :TIER_ID] doc))

(defn validate-tier-id! [doc tier-id]
  (when-not (some #{tier-id} (tier-ids doc))
    (throw (ex-info "Tier ID not present in doc."
                    {:error-type :invalid-input
                     :bad-value tier-id
                     :context :tier-validation}))))

(defn tier
  "Returns a tier with a given tier ID"
  [doc tier-id]
  (validate-tier-id! doc tier-id)
  (s/select-first (tier-path tier-id) doc))

(defn replace-tier
  "Replaces a tier with a given id with a new tier"
  [doc tier-id new-tier]
  (validate-tier-id! doc tier-id)
  (s/setval (tier-path tier-id) new-tier doc))

(defn add-tier
  "Adds a new tier to document"
  [doc new-tier]
  (s/setval [:content s/AFTER-ELEM] new-tier doc))

(defn remove-tier
  "Removes tier from document"
  [doc tier-id]
  (validate-tier-id! doc tier-id)
  (s/setval (tier-path tier-id) s/NONE doc))

(defn rename-tier
  "Changes tier ID of a tier with a given tier ID"
  [doc tier-id new-tier-id]
  (validate-tier-id! doc tier-id)
  (s/setval [(tier-path tier-id) :attrs :TIER_ID] new-tier-id doc))

(defn copy-tier-between-files
  "Copy a tier with a given ID from one document to another"
  [doc1 doc2 tier-id]
  (add-tier doc2 (tier doc1 tier-id)))

(defn tier-type
  "Returns the linguistic type ref of the tier with the target tier ID"
  [doc tier-id]
  (validate-tier-id! doc tier-id)
  (s/select-first [(tier-path tier-id) :attrs :LINGUISTIC_TYPE_REF] doc))

(defn change-tier-type
  "Changes the linguistic type ref of tier"
  [doc tier-id new-type]
  (validate-tier-id! doc tier-id)
  (s/setval [(tier-path tier-id) :attrs :LINGUISTIC_TYPE_REF] new-type doc))

(defn tier-cv
  "Returns the controlled vocabulary associated with a tier"
  [doc tier-id]
  (->> (tier-type doc tier-id)
       (linguistic-type-cv doc)
       (cv-vocab-with-ids doc)))

(defn tier-cv-id
  "Returns the controlled vocabulary ID of a tier with a given ID"
  [doc tier-id]
  (-> (linguistic-type doc (tier-type doc tier-id)) :attrs :CONTROLLED_VOCABULARY_REF))

(defn tier-cve-id
  "Returns the controlled vocabulary entry ID associated with a particular content of a particular tier"
  [doc tier-id content]
  (->> (tier-cv doc tier-id)
       (filter #(= (:content %) content))
       first
       :cve-id))
  
;;; --- REPL Playground & Examples --- 
(comment
  (let [doc1 (read-eaf (io/resource "segmentation-annotator-1.eaf"))
        doc2 (read-eaf (io/resource "segmentation-annotator-2.eaf"))]
    (tiers doc1)
    (tier-ids doc1)
    (tier doc1 (first (tier-ids doc1)))
    (tier doc1 "wrong-id") ;; error
    (replace-tier doc1
                  (first (tier-ids doc1))
                  (tier doc1 (second (tier-ids doc1))))
    (tier-ids (replace-tier doc1
                            (first (tier-ids doc1))
                            (tier doc1 (second (tier-ids doc1)))))
    (tier-ids (rename-tier doc1 (second (tier-ids doc1)) "New-tier-name"))
    (tier-ids (copy-tier-between-files doc1 doc2 (first (tier-ids doc1))))
    (tier-type doc1 "Funct-seg-p1")
    (tier-type (change-tier-type doc1 "Funct-seg-p1" "new-type"))
    (tier-cv doc1 (first (tier-ids doc1)))
    (tier-cve-id doc1 (second (tier-ids doc1)) "Clear")
    (tier-cve-id doc1 (first (tier-ids doc1)) "Not present")))

;;;;; ## Tier annotations operations
(defn- tier-annotations-path [tier-id] [(tier-path tier-id) :content])

(defn tier-annotations
  "Returns a list of all annotations in a tier"
  [doc tier-id]
  (validate-tier-id! doc tier-id)
  (s/select (tier-annotations-path tier-id) doc))

(defn replace-tier-annotations
  "Replaces all annotations with a new list of annotations of a tier with a given ID"
  [doc tier-id new-annotations]
  (validate-tier-id! doc tier-id)
  (s/setval (tier-annotations-path tier-id) new-annotations doc))

(defn remove-tier-annotations
  "Removes all annotations of a tier"
  [doc tier-id]
  (validate-tier-id! doc tier-id)
  (s/setval (tier-annotations-path tier-id) s/NONE doc))

(defn copy-tier-annotations
  "Copies all annotations of a tier with an ID tier-id-from from doc-from to tier in ID tier-id-to in doc-to. "
  [doc-from tier-id-from doc-to tier-id-to]
  (validate-tier-id! doc-from tier-id-from)
  (validate-tier-id! doc-to tier-id-to)
  (replace-tier-annotations doc-to tier-id-to (tier-annotations doc-from tier-id-from)))

(defn add-annotation-to-tier
  "Adds a new annotation to list of annotations of tier with given ID."
  [doc tier-id new-annotation]
  (validate-tier-id! doc tier-id)
  (s/setval [(tier-annotations-path tier-id) s/AFTER-ELEM] new-annotation doc))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc1 (read-eaf (io/resource "segmentation-annotator-1.eaf"))
        doc2 (read-eaf (io/resource "segmentation-annotator-2.eaf"))]
    (tier-annotations doc1 (first (tier-ids doc1)))
    (let [tier-id (first (tier-ids doc1))]
      (tier-annotations 
       (replace-tier-annotations doc1 tier-id
                                 (tier-annotations doc2 (second (tier-ids doc2))))
       tier-id))
    (let [tier-id (first (tier-ids doc1))]
      (tier-annotations 
       (remove-tier-annotations doc1 tier-id) tier-id))
    (let [tier-id1 (first (tier-ids doc1))
          tier-id2 (second (tier-ids doc2))]
      (tier-annotations (copy-tier-annotations doc2 tier-id2 doc1 tier-id1)
                        tier-id1))
  (let [tier-id (first (tier-ids doc1))
            new-annotation (first (tier-annotations doc2 (second (tier-ids doc2))))]
        (tier-annotations (add-annotation-to-tier doc1 tier-id new-annotation) tier-id))))

;;;;; ## Annotation operations
(def ^:private all-annotations-path [tiers-path :content s/ALL map?])
(defn- annotation-path [annotation-id]
  [all-annotations-path (s/selected? :content s/ALL :attrs :ANNOTATION_ID #(= % annotation-id))])
(def ^:private annotation-content-path [:content s/ALL map?])
(def ^:private annotation-text-path [annotation-content-path :content s/ALL map? :content s/FIRST])
(def ^:private annotation-attrs-path [annotation-content-path :attrs])
(def ^:private annotation-id-path [annotation-attrs-path :ANNOTATION_ID])
(def ^:private annotation-start-time-ref-path [annotation-attrs-path :TIME_SLOT_REF1])
(def ^:private annotation-end-time-ref-path [annotation-attrs-path :TIME_SLOT_REF2])

(defn annotation-ids
  "Returns a vector of all annotation IDs in documents"
  [doc]
  (s/select [all-annotations-path :content s/ALL map? :attrs :ANNOTATION_ID] doc))

(defn validate-annotation-id! [doc annotation-id]
  (when-not (some #{annotation-id} (annotation-ids doc))
    (throw (ex-info "Annotation ID not present in doc."
                    {:error-type :invalid-input
                     :bad-value annotation-id
                     :context :tier-validation}))))

(defn annotation
  "Returns annotation with target ID."
  [doc annotation-id]
  (validate-annotation-id! doc annotation-id)
  (s/select-first (annotation-path annotation-id) doc))

(defn remove-annotation
  "Removes annotation with target ID from document."
  [doc annotation-id]
  (validate-annotation-id! doc annotation-id)
  (s/setval (annotation-path annotation-id) s/NONE doc))

(defn replace-annotation-id
  "Replaces annotation ID with a new ID"
  [doc annotation-id new-id]
  (validate-annotation-id! doc annotation-id)
  (s/setval [(annotation-path annotation-id) annotation-id-path] new-id doc))

(defn annotation-text
  "Returns text of annotation with target ID."
  [doc annotation-id]
  (validate-annotation-id! doc annotation-id)
  (s/select-first [(annotation-path annotation-id) annotation-text-path] doc))

(defn replace-annotation-text
  "Replaces the text of annotation with a target ID."
  [doc annotation-id new-text]
  (validate-annotation-id! doc annotation-id)
  (s/setval [(annotation-path annotation-id) annotation-text-path] new-text doc))

(defn annotation-start-time-ref
  "Returns the start time reference of annotation with a target ID."
  [doc annotation-id]
  (validate-annotation-id! doc annotation-id)
  (s/select-first [(annotation-path annotation-id) annotation-start-time-ref-path] doc))

(defn annotation-end-time-ref
  "Returns the end time reference of annotation with a target ID."
  [doc annotation-id]
  (validate-annotation-id! doc annotation-id)
  (s/select-first [(annotation-path annotation-id) annotation-end-time-ref-path] doc))

(defn replace-annotation-start-time-ref
  "Replaces the start time reference of annotation with a target ID."
  [doc annotation-id new-ref]
  (validate-annotation-id! doc annotation-id)
  (s/setval [(annotation-path annotation-id) annotation-start-time-ref-path] new-ref doc))

(defn replace-annotation-end-time-ref
  "Replaces the end time reference of annotation with a target ID."
  [doc annotation-id new-ref]
  (validate-annotation-id! doc annotation-id)
  (s/setval [(annotation-path annotation-id) annotation-end-time-ref-path] new-ref doc))

(defn- id-to-int
  "Returns the number of different types of ID (annotation, time stamp, even, and functional segment)"
  [id]
  (-> id
      (string/split #"a|ts|e|fs")
      second
      (Integer/parseInt)))

(defn last-annotation-id
  "Returns the last annotation ID in the document"
  [doc]
  (let [aid
        (->>
         (annotation-ids doc)
         (map id-to-int)
         sort
         last
         (str "a"))]
    (if (= aid "a") "a0" aid)))

(defn assert-is-annotation-id! [annotation-id]
  (when-not (string/starts-with? annotation-id "a")
    (throw (ex-info "Input not proper annotation ID."
                    {:error-type :invalid-input
                     :bad-value annotation-id
                     :context :annotation-id-validation}))))

(defn update-annotation-id
  "Updates an annotation ID with a given function applied to the number part of the ID. "
  [annotation-id update-fn]
  (assert-is-annotation-id! annotation-id)
  (->> annotation-id
       id-to-int
       update-fn
       (str "a")))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (annotation-ids doc)
    (annotation doc "a128")
    (annotation doc "a1111")
    (annotation (remove-annotation doc "a128") "a128")
    (annotation (replace-annotation-id doc "a128" "a1111") "a1111")
    (annotation-text doc "a128")
    (annotation (replace-annotation-text doc "a128" "some new text") "a128")
    (annotation-start-time-ref doc "a128")
    (annotation-end-time-ref doc "a128")
    (annotation (replace-annotation-start-time-ref doc "a128" "ts1111") "a128")
    (annotation (replace-annotation-end-time-ref doc "a128" "ts1111") "a128")
    (id-to-int "a123")
    (id-to-int "ts123")
    (last-annotation-id doc)
    (update-annotation-id (last-annotation-id doc) #(+ 10 %))))

;;;;; ## Time order operations
(def ^:private time-order-path [:content s/ALL #(= (:tag %) :TIME_ORDER)])
(def ^:private time-slots-path [time-order-path :content s/ALL map?])

(defn time-order
  "Returns time order of document."
  [doc]
  (s/select-first time-order-path doc))

(defn replace-time-order
  "Replaces time order of document with a new time order."
  [doc new-time-order]
  (s/setval time-order-path new-time-order doc))

(defn time-slots
  "Returns a vector of all time slots in document."
  [doc]
  (s/select time-slots-path doc))

(defn replace-time-slots
  "Replaces time slots in document with new time slots."
  [doc new-time-slots]
  (s/setval [time-order-path :content] new-time-slots doc))

(defn add-time-slot
  "Adds a time slot to document."
  [doc new-time-slot]
  (s/setval [time-order-path :content s/AFTER-ELEM] new-time-slot doc))

(defn time-slot-ids
  "Returns all time slot IDs in document."
  [doc]
  (s/select [time-slots-path :attrs :TIME_SLOT_ID] doc))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc1 (read-eaf (io/resource "segmentation-annotator-1.eaf"))
        doc2 (read-eaf (io/resource "segmentation-annotator-2.eaf"))]
    (time-order doc1)
    (time-order (replace-time-order doc1 (time-order doc2)))
    (time-slots doc1)
    (time-slots (replace-time-slots doc1 (time-slots doc2)))
    (time-slot-ids doc1)))

;;;;; ## Time slot operations 
(defn- time-slot-path [time-slot-id] [time-slots-path  (s/selected? :attrs :TIME_SLOT_ID #(= % time-slot-id))])
(defn- selected-time-slots-path [time-slot-ids] [time-slots-path  (s/selected? :attrs :TIME_SLOT_ID #(some #{%} time-slot-ids))])

(defn validate-time-slot-id! [doc time-slot-id]
  (when-not (some #{time-slot-id} (time-slot-ids doc))
    (throw (ex-info "Time slot ID not present in doc."
                    {:error-type :invalid-input
                     :bad-value time-slot-id
                     :context :tier-validation}))))

(defn time-slot
  "Returns a time slot with target ID."
  [doc time-slot-id]
  (validate-time-slot-id! doc time-slot-id)
  (s/select-first (time-slot-path time-slot-id) doc))

(defn selected-time-slots
  "Returns all time slots that match a given vector of time slot IDs."
  [doc time-slot-ids]
  (s/select (selected-time-slots-path time-slot-ids) doc))

(defn remove-time-slot
  "Removes a time slot with a given ID."
  [doc time-slot-id]
  (validate-time-slot-id! doc time-slot-id)
  (s/setval (time-slot-path time-slot-id) s/NONE doc))

(defn last-time-slot-id
  "Returns last time slot ID in document."
  [doc]
  (let [index
        (->> (time-slot-ids doc)
             (map id-to-int)
             sort
             last
             (str "ts"))]
    (if (= index "ts") "ts0" index)))

(defn assert-is-time-slot-id!
  "Checks if input is formatted as a time slot."
  [time-slot-id]
  (when-not (string/starts-with? time-slot-id "ts")
    (throw (ex-info "Input not proper time slot ID."
                    {:error-type :invalid-input
                     :bad-value time-slot-id
                     :context :time-slot-id-validation}))))

(defn update-time-slot-id
  "Updates the numeric portion of a given time slot ID with an update-fn."
  [time-slot-id update-fn]
  (assert-is-time-slot-id! time-slot-id)
  (str "ts"
       (-> time-slot-id
           (string/split #"ts")
           second
           (Integer/parseInt)
           update-fn)))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc1 (read-eaf (io/resource "segmentation-annotator-1.eaf"))
        doc2 (read-eaf (io/resource "segmentation-annotator-2.eaf"))]
    (time-slot doc1 "ts244")
    (time-slot doc2 "ts245")
    (time-slots (add-time-slot doc1 (time-slot doc2 "ts245")))
    (time-slots (remove-time-slot doc1 "ts244"))
    (last-time-slot-id doc1)
    (last-time-slot-id (remove-time-slot doc1 "ts244"))
    (update-time-slot-id "ts123" #(* 10 %))
    (update-time-slot-id "s123" inc)))

;;;;; =====================================================================
;;;;; # Section 3: Summary functions (useful in REPL) 
;;;;; =====================================================================

(defn doc-summary
  "Provides a summary of the document, including video file, controlled vocabularies, tiers, and number of annotations."
  [doc]
  {:video-file (video-rel-url doc)
   :controlled-vocabularies (cv-ids doc)
   :tiers (tier-ids doc)
   :annotation-count (count (annotation-ids doc))
   :time-slots (count (time-slots doc))})

(defn cv-summary
  "Summarizes control vocabulary, including associated tiers and number of entries"
  [doc cv-id]
  {:control-vocabulary-name cv-id
   :associated-tiers (->> (tier-ids doc)
                          (filter #(= (tier-cv-id doc %) cv-id)))
   :entries-count (count (cv-vocab-with-ids doc cv-id))})

(defn tier-summary
  "Provides a summary of tier, including number of annotations and span of timings"
  [doc tier-id]
    (let [start-tsIDs (s/select [(tier-annotations-path "Funct-seg-p1") s/ALL map? annotation-start-time-ref-path] doc)
          end-tsIDs (s/select [(tier-annotations-path "Funct-seg-p1") s/ALL map? annotation-end-time-ref-path] doc)]
      {:tier-name "Funct-seg-p1"
       :controlled-vocabulary (tier-cv doc tier-id)
       :annotation-count (-> doc (tier-annotations "Funct-seg-p1") first count)
       :first-annotation-start
       (->>
        (s/select [(selected-time-slots-path start-tsIDs) :attrs :TIME_VALUE] doc)
        (map Integer/parseInt)
        (reduce min))
       :last-annotation-end
       (->>
        (s/select [(selected-time-slots-path end-tsIDs) :attrs :TIME_VALUE] doc)
        (map Integer/parseInt)
        (reduce max))}))

(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (doc-summary doc)
    (cv-summary doc "Clear")
    (tier-summary doc (first (tier-ids doc)))))

;;;;; =====================================================================
;;;;; # Section 4: Converting to and from a dataset 
;;;;; =====================================================================
(defn xml-node
  "A generic function to generate an xml node."
  [tag attrs & content]
  (let [clean-content (or content '())]
    {:tag tag :attrs attrs :content clean-content}))

;;;;; ## Converting tier from xml to dataset and back 
(defn flatten-annotation
  "Changes the XML representation of an annotation to a flat map."
  [annotation]
  (let [content (->> annotation
                     :content (filter map?) first
                     :content (filter map?) first
                     :content first)
        attrs (->> annotation
                   :content (filter map?) first
                   :attrs)]
    (assoc attrs :content content)))

(defn unflatten-annotation
  "Converts a flat representation of annotation to a nested XML representation."
  ([{aID :ANNOTATION_ID ts1 :TIME_SLOT_REF1 ts2 :TIME_SLOT_REF2 cve-ref :CVE_REF c :content}]
  (xml-node :ANNOTATION {}
            (xml-node :ALIGNABLE_ANNOTATION
                      (if cve-ref
                        {:ANNOTATION_ID aID
                         :CVE_REF cve-ref 
                         :TIME_SLOT_REF1 ts1
                         :TIME_SLOT_REF2 ts2}
                        {:ANNOTATION_ID aID
                         :TIME_SLOT_REF1 ts1
                         :TIME_SLOT_REF2 ts2})
                      (xml-node :ANNOTATION_VALUE {} c))))
  ([aID ts1 ts2 content]
   (unflatten-annotation {:ANNOTATION_ID aID
                          :TIME_SLOT_REF1 ts1
                          :TIME_SLOT_REF2 ts2
                          :content content}))
  ([aID ts1 ts2 cve-ref content]
   (unflatten-annotation {:ANNOTATION_ID aID
                          :TIME_SLOT_REF1 ts1
                          :TIME_SLOT_REF2 ts2
                          :CVE_REF cve-ref
                          :content content})))

(defn tier-as-dataset
  "Converts tier from XML to a dataset representation."
  [doc tier-id]
  (->> (tier doc tier-id)
       (s/select (s/walker #(= (:tag %) :ANNOTATION)))
       (map flatten-annotation)
       (tc/dataset)))
  
(defn tier-dataset->xml
  "Converts a tier from a dataset to XML representation."
  [dataset tier-id type target-doc]
  (apply xml-node :TIER {:LINGUISTIC_TYPE_REF type
                         :TIER_ID tier-id}
         (map unflatten-annotation
              (-> dataset
                  (tc/map-columns :CVE_REF [:content] #(tier-cve-id target-doc tier-id %))
                  (tc/rows :as-maps)))))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (xml-node :ANNOTATION {} "content")
    (xml-node :ANNOTATION {}
              (xml-node :ALIGNABLE_ANNOTATION
                        {:ANNOTATION_ID "a122"
                         :TIME_SLOT_REF1 "ts233"
                         :TIME_SLOT_REF2 "ts234"}
                        (xml-node :ANNOTATION_VALUE {} "oh")))
    (flatten-annotation (annotation doc "a122"))
    (unflatten-annotation (flatten-annotation (annotation doc "a122")))
    (unflatten-annotation "a122" "ts233" "ts234" (xml-node :ANNOTATION_VALUE {} "oh"))
    (unflatten-annotation "a122" "ts233" "ts234" "cveid_dbceba0b-0bb1-4c9f-a1e3-ab40bb2deee6" (xml-node :ANNOTATION_VALUE {} "oh"))
    (tier-as-dataset doc (first (tier-ids doc)))
    (let [target-tier (first (tier-ids doc))]
      (tier-dataset->xml
       (tier-as-dataset doc target-tier)
       target-tier
       (tier-type doc target-tier)
       doc))))

;;;;; ## Converting time order from xml to dataset and back 
(defn time-order-as-dataset
  "Returns time order of document as dataset."
  [doc]
  (tc/convert-types
   (tc/dataset (s/select [:content s/ALL map? :attrs] (time-order doc)))
   :TIME_VALUE :int64))

(defn unflatten-time-slot
  "Converts time slot from a flat map to an XML representation."
  ([{tsID :TIME_SLOT_ID time :TIME_VALUE}]
   (xml-node :TIME_SLOT {:TIME_SLOT_ID tsID :TIME_VALUE (str time)}))
  ([tsID time]
  (unflatten-time-slot {:TIME_SLOT_ID tsID :TIME_VALUE time})))

(defn unflatten-and-add-time-slot
  "Adds a time slot with a particular ID to document"
  [doc tsID time]
  (add-time-slot doc (unflatten-time-slot tsID time)))

(defn time-order-dataset->xml [dataset]
  {:tag :TIME_ORDER
   :attrs {},
   :content (map unflatten-time-slot (tc/rows dataset :as-maps))})

(defn tier-with-time-values-as-dataset
  "Gets tier with associated time values as as a dataset."
  [doc tier-id]
  (let [tr (tier-as-dataset doc tier-id)
        tss (time-order-as-dataset doc)]
    (-> tr
        (tc/left-join (tc/rename-columns tss {:TIME_SLOT_ID :TIME_SLOT_REF1 :TIME_VALUE :TIME_VALUE1}) :TIME_SLOT_REF1)
        (tc/left-join (tc/rename-columns tss {:TIME_SLOT_ID :TIME_SLOT_REF2 :TIME_VALUE :TIME_VALUE2}) :TIME_SLOT_REF2)
        (tc/drop-columns [:right.TIME_SLOT_REF1 :right.TIME_SLOT_REF2]))))

;; update ELAN doc with datasets 
(defn- add-dataset-row-to-doc
  "Adds an row from a dataset as an annotation in an ELAN document."
  [doc row]
  (let [aID_last (last-annotation-id doc)
        aID_new (update-annotation-id aID_last inc)
        tsID_last (last-time-slot-id doc)
        tsID_start (update-time-slot-id tsID_last inc)
        tsID_end (update-time-slot-id tsID_start inc)]
    (->
     doc
     (unflatten-and-add-time-slot tsID_start (-> row :TIME_VALUE1 first))
     (unflatten-and-add-time-slot tsID_end (-> row :TIME_VALUE2 first))
     (add-annotation-to-tier
      (-> row :tier first)
      (unflatten-annotation aID_new tsID_start tsID_end (-> row :content first))))))

(defn add-dataset-to-doc
  "Adds a dataset as annotations in an ELAN document."
  [doc ds]
  (if (= (tc/row-count ds) 0)
    doc
    (let [next-row (tc/select-rows ds 0)
          next-tier-id (-> next-row :tier first)]
      (add-dataset-to-doc
       (add-dataset-row-to-doc
        (if (some #{next-tier-id} (tier-ids doc))
          doc
          (add-tier doc (xml-node :TIER {:TIER_ID next-tier-id})))
        next-row)
       (tc/drop-rows ds 0)))))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc (read-eaf (io/resource "segmentation-annotator-1.eaf"))]
    (time-order-as-dataset doc)
    (unflatten-time-slot (first (tc/rows (time-order-as-dataset doc) :as-maps)))
    (unflatten-time-slot-2 "ts1" 1880)
    (time-slots
     (unflatten-and-add-time-slot
      doc
      (update-time-slot-id (last-time-slot-id doc) inc)
      180000))
    (time-order-dataset->xml (time-order-as-dataset doc))
    (tier-with-time-values-as-dataset doc (first (tier-ids doc)))))

;;;;; =====================================================================
;;;;; # Section 5: Utility functions 
;;;;; =====================================================================
(defn truncate-time
  "Sets all time values above an upper limit equal to upper limit."
  [time upper-limit]
  (if (> time upper-limit) upper-limit time))

(defn same-intervals?
  "Checks if two intervals are equal within a tolerance tau."
  [t-start-1 t-end-1 t-start-2 t-end-2 tau]
  (and 
   (< (abs (- t-start-1 t-start-2)) tau)
   (< (abs (- t-end-1 t-end-2)) tau)))

(defn dataset-annotations-equal?
  "Checks if two annotations are equal, that is, with the same content and time intervals."
  [annotation-1 annotation-2 tau]
  (let [text1 (-> annotation-1 :content first)
        text2 (-> annotation-2 :content first)
        t-start-1 (-> annotation-1 :TIME_VALUE1 first)
        t-end-1 (-> annotation-1 :TIME_VALUE2 first)
        t-start-2 (-> annotation-2 :TIME_VALUE1 first)
        t-end-2 (-> annotation-2 :TIME_VALUE2 first)] 
    (and (= text1 text2)
         (same-intervals? t-start-1 t-end-1 t-start-2 t-end-2 tau))))

(defn assign-agreement
  "Assigns agreement between two annotators by comparing subsequent entries in a dataset.
  The dataset should be the concatenation of the two annotator's datasets, ordered by time."
  [all agree disagree select-annotation-when-agree tau]
  (case (tc/row-count all)
    0 (tc/concat (tc/add-column agree :category "agree")
                 (tc/add-column disagree :category "disagree"))
    1 (tc/concat (tc/add-column agree :category "agree")
                 (tc/add-column disagree :category "disagree")
                 (tc/add-column all :category "disagree"))
    (if (dataset-annotations-equal? (tc/select-rows all 0) (tc/select-rows all 1) tau)
      (assign-agreement
       (tc/drop-rows all [0 1])
       (tc/concat agree
                  (-> all
                      (tc/select-rows [0 1])
                      select-annotation-when-agree))
       disagree
       select-annotation-when-agree
       tau)
      (assign-agreement
       (tc/drop-rows all 0)
       agree
       (tc/concat disagree (tc/select-rows all 0))
       select-annotation-when-agree
       tau))))

(defn tier-agreement
  "Compares the annotations of two annotators."
  [doc1 doc2 tier-id tau]
  (let [ds1 (tier-with-time-values-as-dataset doc1 tier-id)
        ds2 (tier-with-time-values-as-dataset doc2 tier-id)
        empty-ds (tc/dataset {})
        select-annotation-when-agree
        (fn [agreed-upon-rows] ;; select annotation of second annotator
          (tc/select-rows agreed-upon-rows #(= (:annotator %) 2)))]
    (->
     (tc/concat (tc/add-column ds1 :annotator 1)
                (tc/add-column ds2 :annotator 2))
     (tc/select-columns (complement #{:ANNOTATION_ID :right.TIME_SLOT_REF1 :right.TIME_SLOT_REF2}))
     (tc/order-by :TIME_VALUE1)
     (assign-agreement empty-ds empty-ds select-annotation-when-agree tau))))

(defn replace-prefix
  "Replaces all annotations with a give prefix with a new prefix."
  [doc old-prefix new-prefix]
  (let [prefix-pattern (re-pattern old-prefix)]
    (s/transform
     [all-annotations-path (s/selected? :content s/ALL map? :content s/ALL map? :content s/ALL #(string/starts-with? % old-prefix)) :content s/ALL map? :content s/ALL map? :content s/ALL]
     #(string/replace % prefix-pattern new-prefix)
     doc)))

;;; --- REPL Playground & Examples --- 
(comment
  (let [doc1 (read-eaf (io/resource "segmentation-annotator-1.eaf"))
        doc2 (read-eaf (io/resource "segmentation-annotator-2.eaf"))]
    (truncate-time 100)
    (truncate-time 1000000)
    (same-intervals? 100 1000 200 900 200)
    (same-intervals? 100 1000 200 900 50)
    (let [ds (tier-with-time-values-as-dataset doc1 (first (tier-ids doc1)))]
      (dataset-annotations-equal? (tc/select-rows ds 1) (tc/select-rows ds 1) 200))
    (let [ds (tier-with-time-values-as-dataset doc1 (first (tier-ids doc1)))]
      (dataset-annotations-equal? (tc/select-rows ds 1) (tc/select-rows ds 0) 200))
    (tier-agreement doc1 doc2 (first (tier-ids doc1)))))

  ;; Note:
  ;; - the function that copies a tier should look up the controlled vocabulary in the new document; if it is not associated, it should copy the controlled vocabulary as well; if it exists, it should update the annotations based on the controlled vocabulary in the new document
  ;; - there should be a function that reorders tier IDs by time stamps, which also updates annotation ids accordingly
  ;; - there should be a function that orders annotation IDs (maybe; if they are indeed ordered in ELAN) in order of increasing time stamps, per tier
  ;; - add a function to map over annotations
  ;; - set dataset name to tier name
  ;; - when converting to dataset, set name to tier ID or cv id, etc
  ;; - there might be a built-in function for creating an xml node; 
  
