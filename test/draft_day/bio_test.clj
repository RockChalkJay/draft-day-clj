(ns draft-day.bio-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.bio :as bio]))

(def ^:private nacua
  {:sleeper/years-exp 3
   :bio {:birth-year 2001 :draft-year 2023 :draft-round 5 :draft-overall 177}})

;; ---- age ----

(deftest age-is-measured-against-the-season
  ;; Only the birth year is known, so an age against today would be spuriously
  ;; precise, and it would tick over mid-season with no new data behind it.
  (is (= 25 (bio/age (:bio nacua) 2026)))
  (is (= 24 (bio/age (:bio nacua) 2025))))

(deftest no-birth-year-and-no-season-are-both-nothing
  (is (nil? (bio/age {} 2026)))
  (is (nil? (bio/age (:bio nacua) nil))))

(deftest a-birth-year-after-the-season-is-not-a-negative-age
  ;; A corrupt row must drop the segment rather than print "Age -3".
  (is (nil? (bio/age {:birth-year 2030} 2026))))

;; ---- experience ----

(deftest a-rookie-is-named-rather-than-counted
  (is (= "Rookie" (bio/experience-label 0)))
  (is (= "1 yr" (bio/experience-label 1)))
  (is (= "5 yrs" (bio/experience-label 5)))
  (is (nil? (bio/experience-label nil))))

;; ---- draft capital ----

(deftest draft-capital-reads-as-a-sentence
  (is (= "2023 Rd 5, #177" (bio/draft-label (:bio nacua)))))

(deftest the-round-and-the-pick-drop-out-on-their-own
  ;; An old row can carry a year and nothing else.
  (is (= "2023" (bio/draft-label {:draft-year 2023})))
  (is (= "2023 Rd 5" (bio/draft-label {:draft-year 2023 :draft-round 5})))
  (is (= "2023, #177" (bio/draft-label {:draft-year 2023 :draft-overall 177}))))

(deftest an-absent-draft-is-never-called-undrafted
  ;; `snapshot-row` strips nils, so undrafted and no-row-at-all are the same
  ;; shape here. Inventing a verdict from an absent field is BLANK IS NOT ZERO.
  (is (nil? (bio/draft-label {})))
  (is (nil? (bio/draft-label {:birth-year 2001}))))

;; ---- the line ----

(deftest the-line-reads-oldest-fact-first
  (is (= "Age 25 · 3 yrs · 2023 Rd 5, #177" (bio/line nacua 2026))))

(deftest each-part-survives-the-others-being-absent
  (is (= "Rookie" (bio/line {:sleeper/years-exp 0} 2026)))
  (is (= "Age 25" (bio/line {:bio {:birth-year 2001}} 2026)))
  (is (= "2023 Rd 1, #5"
         (bio/line {:bio {:draft-year 2023 :draft-round 1
                          :draft-overall 5}} 2026))))

(deftest a-player-nobody-knows-anything-about-gets-no-line
  ;; nil rather than an empty string, so the caller drops the element instead of
  ;; rendering a blank paragraph under the name.
  (is (nil? (bio/line {} 2026)))
  (is (nil? (bio/line {:bio {}} 2026))))
