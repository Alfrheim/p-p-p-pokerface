(ns p-p-p-pokerface)

(def replacements {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})


(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

;; Exercice 1
(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn same-of-a-kind [suit_rank_f hand number]
  (let [ranks (map suit_rank_f hand)
        frequen (vals (frequencies ranks))]
    (= number (apply max frequen))))

;; Exercice 3
(defn pair? [hand]
  (same-of-a-kind rank hand 2)) ;; if its bigger is something else, but not pair

;; Exercice 4
(defn three-of-a-kind? [hand]
  (same-of-a-kind rank hand 3))

;; Exercice 5
(defn four-of-a-kind? [hand]
  (same-of-a-kind rank hand 4))

;; Exercice 6
(defn flush? [hand]
  (same-of-a-kind suit hand 5))

(defn hand->vals [hand suit_rank_f]
  (vals (frequencies (map suit_rank_f hand))))

;; Exercice 7
(defn full-house? [hand]
  (= (range 2 4) (sort (hand->vals hand rank))))

(defn two-pairs? [hand]
  (= '(1 2 2) (sort (hand->vals hand rank))))

(defn straight? [hand]
  (let [ sorted (sort (map rank hand)) smallest (first sorted)
        alt (sort (replace {14 1} sorted)) alt-smallest (first alt) ]
    (or
     (= sorted (range smallest (+ smallest 5)))
     (= alt (range alt-smallest (+ alt-smallest 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

