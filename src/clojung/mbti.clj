(ns

  ^{:doc "Contains a derived model for psychological preferences based on the
    work of Carl Gustav Jung known as the Meyers-Briggs Type Indicator (MBTI).
    It predicts to a certain degree in how people perceive the world and make
    decisions based on those preferences."}

  clojung.mbti

  (:require [clojure.math.combinatorics :refer [cartesian-product]]
            [plumbing.core :refer :all]
            [plumbing.graph :as graph]))

(defn list-all-types
  "Returns a sorted list of these psychological differences into four opposite
  pairs, or dichotomies, with a resulting 16 possible psychological types."
  []
  (map #(apply str %)
       (cartesian-product [\I \E] [\S \N] [\F \T] [\P \J])))

;; A bipartition, two parts (subsets) that are jointly exhaustive (everything
;; must belong to one part or the other) and mutually exclusive (nothing can
;; belong simultaneously to both parts).

;; NOTE: While in logic, the partitions are opposites if there exists a proposition
;; such that it holds over one and not the other. In MBTI this indicates a preference.

(def dichotomies
  {
   ;; Extraversion (E)	–	(I) Introversion
   ;;
   ;; Each of the cognitive functions can operate in the external world of
   ;; behavior, action, people, and things ("extraverted attitude") or the
   ;; internal world of ideas and reflection ("introverted attitude").
   \E \I \I \E

   ;; The four (4) principal psychological functions (S-N and T-F):

   ;; Sensing  (S)	–	(N) Intuition (the "irrational" (perceiving) functions)
   \S \N \N \S

   ;; Thinking (T)	–	(F) Feeling (the "rational" (judging) functions)
   \T \F \F \T

   ;; Judging (J) - (P) Perception (preference using either J or P extraverted)
   \J \P \P \J})

;; (defn toggle
;;   "Takes any key and a value supposedly in the dichotomies and
;;   returns a map with that key and its complement. You can also
;;   just use e.g. (dichotomies \T) and it will return `\F`"
;;   [k v] (update-in {k v} [k] dichotomies))


;;;
;;; J/P Rational/Irrational functions
;;;

(defnk perceiving-fn [abbr] (nth abbr 1)) ; irrational S-N
(defnk judging-fn [abbr] (nth abbr 2))    ; rational   T-F

;;;
;;; Preferences
;;;

(defnk attitude
  "The attitude preference (E-I) determines whether the extraverted function is
  dominant or auxiliary."
  [abbr] (first abbr))

(defnk lifestyle
  "The overall lifestyle preference (J-P) determines whether the judging (T-F) or
  perceiving (S-N) preference is most evident in the outside world; i.e., which
  function has an extraverted attitude."
  [abbr] (last abbr))

;;?
(defnk prefered-fn
  "Flipped value to determine preference based on lifestyle."
  [abbr lifestyle] (:p (toggle :p lifestyle)))

(defnk extraverted-fn
  "Function paired with the extraverted attitude: dominant for those who prefer
  extraverted attitude, auxiliary for those that preference introversion."
  [abbr p?] (if p? (second abbr) (nth abbr 2)))

(defnk introverted-fn
  "Function paired the introverted attitude: dominant for those with preference
  for extraverted attitude, auxiliary for those who prefer extraversion."
  [extraverted perceiving-fn judging-fn]
  (if (= extraverted perceiving-fn) judging-fn perceiving-fn))

;;;
;;; Cognitive functions
;;;

(defnk dominant-fn
  "For those with an overall preference for extraversion, the function with the
  extraverted attitude will be the dominant function. For those with an overall
  preference for introversion, the function with the extraverted attitude is the
  auxiliary; the dominant is the other function in the main four letter preference."
  [i? e? j? p? abbr]
  (->>
   (cond
    (and p? i?) (->> abbr (take-nth 2))
    (and j? e?) (->> abbr (take-nth 2))
    (and p? e?) (->> abbr (take 2))
    (and j? i?) (->> abbr (take 2)))
    reverse
   (into [])))

(defnk auxiliary-fn
  "The Auxiliary function for extraverts is the secondary preference of the
  judging or perceiving functions, and it is experienced with an introverted
  attitude. For those with an overall preference for introversion, the function
  with the extraverted attitude is the auxiliary; the dominant is the other
  function in the main four letter preference."
  [dominant perceiving-fn judging-fn]
  (cond
   (= (dominant 0) judging-fn) [perceiving-fn (dichotomies (second dominant))]
   (= (dominant 0) perceiving-fn) [judging-fn (dichotomies (second dominant))]))

(defnk tertiary-fn
  "The Tertiary function is the opposite preference from the Auxiliary.
  For example, if the Auxiliary is thinking then the Tertiary would be feeling.
  The attitude of the Tertiary is the subject of some debate and therefore is
  not normally indicated; i.e. if the Auxiliary was Te then the Tertiary would
  be F (not Fe or Fi)"
  [attitude auxiliary]
  [(dichotomies (first auxiliary)) attitude])

(defnk inferior-fn
  "The Inferior function is the opposite preference and attitude from the
  Dominant, so for an ESTJ with dominant Te the Inferior would be Fi."
  [dominant attitude]
  [(dichotomies (first dominant)) (dichotomies attitude)])

(def mbti-graph
  {:attitude attitude
   :perceiving-fn perceiving-fn ; how information is taken in
   :judging-fn judging-fn ; how decissions are made
   :lifestyle lifestyle ; external (world) orientation
   :prefered-fn prefered-fn
   :extraverted extraverted-fn
   :introverted introverted-fn

   ;; Predicates inner fn due to breivity

   ;; Predicts how types will focus their attention or get their energy.
   :i? (fnk [attitude] (= attitude \I))
   :e? (fnk [attitude] (= attitude \E))

   ;; Predicts how a type will perceive or take in information.
   :s? (fnk [perceiving-fn] (= perceiving-fn \S))
   :n? (fnk [perceiving-fn] (= perceiving-fn \N))

   ;; Predicts how a type will prefer to make decisions.
   :p? (fnk [lifestyle] (= lifestyle \P))
   :j? (fnk [lifestyle] (= lifestyle \J))

   ;; Predicts how a type will orient themselves to the external world.
   :t? (fnk [judging-fn] (= judging-fn \T))
   :f? (fnk [judging-fn] (= judging-fn \F))

   :rational? (fnk [prefered-fn] (= prefered-fn \J))
   :irrational? (fnk [prefered-fn] (= prefered-fn \P))

   ;; Keyword emitting functions for clarity mostly
   :ratio (fnk [rational?] (if rational? :rational :irrational))

   ;; The interaction of 2, 3, or 4 preferences is
   ;; known as type dynamics.

   ;; Is likely to be evident earliest in life.
   :dominant dominant-fn

   ;; Becomes more evident (differentiated) during teenage years
   ;; (provides balance to the dominant).
   :auxiliary auxiliary-fn

   ;; In normal development, individuals tend to become more fluent with a
   ;; third, tertiary function during mid life.
   :tertiary tertiary-fn
   :inferior inferior-fn

   })

(def eager-mbti (graph/eager-compile mbti-graph))

(eager-mbti {:abbr "INTP"})

(->> (eager-mbti {:abbr "ENFP"})
     ((juxt :dominant :auxiliary :tertiary :inferior :ratio)))

(defn keirsey-temperaments
  [[first second third fourth :as abbr]]
  (cond
   (and (= second \S)
        (= fourth \J)) "Guardian"
   (and (= second \S)
        (= fourth \P)) "Artisan"
   (and (= second \N)
        (= third  \F)) "Idealist"
   (and (= second \N)
        (= third  \T)) "Rational"))

(keirsey-temperaments "INTP")



;; * Note that the terms used for each dichotomy have specific technical meanings relating to the MBTI which differ from their everyday usage. For example, people who prefer judgment over perception are not necessarily more judgmental or less perceptive. Nor does the MBTI instrument measure aptitude; it simply indicates for one preference over another.[7]:3 Someone reporting a high score for extraversion over introversion cannot be correctly described as more extraverted: they simply have a clear preference.
