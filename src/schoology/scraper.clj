(ns schoology.scraper
  (:require
    [clojure.string :as str]
    [hickory.core :as h]
    [hickory.select :as s]
    [clojure.pprint :as pp])
  (:import (java.time LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(defn assignment-rows
  [hickory]
  (s/select (s/class "item-row") hickory))

(defn first-content-first
  [n]
  (-> n first :content first))

(defn first-content-second
  [n]
  (-> n first :content second))

(defn assignment-row->title
  [ar]
  (first-content-first
    (s/select
      (s/descendant (s/and (s/tag "span") (s/class "title"))
                    (s/tag "a"))
      ar)))

(defn assignment-row->due-date
  [ar]
  (when-let [dd-str (first-content-second
                      (s/select
                        (s/descendant (s/and (s/tag "span") (s/class "due-date")))
                        ar))]
    (ZonedDateTime/parse (str/upper-case dd-str)
                         (.withZone
                           (DateTimeFormatter/ofPattern "M/dd/uu hh:mma")
                           (ZoneId/of "America/Denver")))))

(defn assignment-row->grade
  [ar]
  (let [gc            (first
                        (s/select
                          (s/descendant (s/and (s/tag "td")
                                               (s/class "grade-column")))
                          ar))
        max-grade     (first-content-first
                        (s/select
                          (s/descendant
                            (s/and (s/tag "span") (s/class "max-grade")))
                          gc))
        awarded-grade (first-content-first
                        (s/select
                          (s/descendant
                            (s/and (s/tag "span") (s/class "awarded-grade"))
                            (s/and (s/tag "span") (s/class "rounded-grade")))
                          gc))
        no-grade      (first-content-first
                        (s/select
                          (s/descendant
                            (s/and (s/tag "span") (s/class "no-grade")))
                          gc))
        exception     (first-content-first
                        (s/select
                          (s/descendant
                            (s/and (s/tag "span") (s/class "exception-text")))
                          gc))
        comments      (first-content-first
                        (s/select
                          (s/descendant
                            (s/and (s/tag "div") (s/class "grade-wrapper"))
                            (s/and (s/tag "span") (s/class "visually-hidden")))
                          gc))
        grade         (cond
                        awarded-grade (if max-grade
                                        (str awarded-grade max-grade)
                                        awarded-grade)
                        no-grade no-grade
                        :else "???")
        context       (cond
                        (and exception comments) (str exception ": " comments)
                        comments comments
                        exception exception)]
    {:grade grade, :context context}))

(defn assignment-row->comments
  [ar]
  (first-content-second
    (s/select
      (s/descendant
        (s/and (s/tag "td") (s/class "comment-column"))
        (s/and (s/tag "span") (s/class "comment")))
      ar)))

(defn max-len-of-values
  [k as]
  (apply max (map #(-> % k count) as)))

(defn format-len-for-key
  [k as]
  (+ 2 (max-len-of-values k as)))

(defn len->str-format
  [len]
  (str "%-" len "s"))

(defn assignment-rows->assignments
  [ars]
  (map (fn [a]
         {:title    (assignment-row->title a)
          :due-date (assignment-row->due-date a)
          :grade    (assignment-row->grade a)
          :comments (assignment-row->comments a)})
       ars))

(defn generate-table
  [assignments]
  (let [title-len       (format-len-for-key :title assignments)
        title-format    (len->str-format title-len)
        due-date-len    19
        due-date-format (fn [day]
                          (let [year-len (if (< day 10) 7 6)]
                            (str "%1$ta, %1$tb %1$te, %1$-" year-len "tY")))
        due-header-fmt  (len->str-format due-date-len)
        grade-len       (format-len-for-key (comp :grade :grade) assignments)
        grade-format    (len->str-format grade-len)
        context-len     (format-len-for-key (comp :context :grade) assignments)
        context-format  (len->str-format context-len)
        comments-len    (format-len-for-key :comments assignments)
        comments-format (len->str-format comments-len)]
    (println (format title-format "TITLE")
             (format due-header-fmt "DUE")
             (format grade-format "GRADE")
             (format context-format "CONTEXT")
             (format comments-format "COMMENTS"))
    (doseq [a (sort-by :due-date assignments)]
      (println (format title-format (:title a))
               (if-let [dd (:due-date a)]
                 (format (due-date-format (.getDayOfMonth dd)) dd)
                 (format due-header-fmt ""))
               (format grade-format (get-in a [:grade :grade]))
               (if-let [gc (get-in a [:grade :context])]
                 (format context-format gc)
                 (format context-format ""))
               (if-let [c (:comments a)]
                 (format comments-format c)
                 (format comments-format ""))))))

(defn -main
  [& args]
  (let [html-file     (first args)
        html          (-> html-file slurp h/parse h/as-hickory)
        assignments   (-> html assignment-rows assignment-rows->assignments)
        output-format (if-let [of (second args)]
                        (keyword of)
                        :table)]
    (case output-format
      :table (-> assignments generate-table println)
      :edn (pp/pprint assignments))))



