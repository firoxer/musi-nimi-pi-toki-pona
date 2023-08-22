(local words-path "words.tsv")
(local answers-path "answers.tsv")

(fn load-words []
  (collect [line (io.lines words-path)]
    (let [(word definition) (string.match line "(.*)\t(.*)")]
      (values word definition))))

(fn load-answers []
  (let [load! (fn []
                (icollect [line (io.lines answers-path)]
                  (let [(time word correct?) (string.match line "(.*)\t(.*)\t(.*)")]
                    {:time (tonumber time)
                     :word word
                     :correct? (= correct? "yes")})))]
    (case (pcall load!)
      (true answers) answers
      (false _) []))) ; Likely just a missing file

; Ported from https://en.wikipedia.org/wiki/SuperMemo
(fn supermemo2 [grade {: interval : repetitions : easiness}]
  {:interval (if (< grade 3)
               1
               (case repetitions
                 0 1
                 1 6
                 :else (* interval easiness)))
   :repetitions (if (< grade 3)
                  1
                  (+ repetitions 1))
   :easiness (math.max 1.3
                       (+ easiness (- 0.1 (* (- 5 grade)
                                             (+ 0.08 (* 0.02 (- 5 grade)))))))})

(fn sort-by-review-order [words answers]
  (let [stats (accumulate [stats {}
                           _ {: time : word : correct?} (ipairs answers)]
                (let [word-stats (or (. stats word)
                                     {:repetitions 0 :easiness 2.5 :interval 0})
                      grade (if correct? 4 1)
                      new-word-stats (supermemo2 grade word-stats)]
                  (tset new-word-stats :last-reviewed time)
                  (tset stats word new-word-stats)
                  stats))
        to-sort (icollect [word {: last-reviewed : interval} (pairs stats)]
                  {: word
                   :definition (. words word)
                   :review-time (+ (or last-reviewed 0)
                                   (* interval 86400))})
        by-review-time #(< $1.review-time $2.review-time)]
      (doto to-sort
       (table.sort by-review-time))))

(fn empty-string? [s]
  (= "" (string.gsub s "%s" "")))

(fn record-answer [word correct?]
  (with-open [file (io.open answers-path :a)]
    (file:write (os.time) "\t" word "\t" (if correct? "yes" "no") "\n")))

(fn play [words-due]
  (case words-due
    [{: word : definition} & remaining-words-due]
    (do (io.write "\n" word "\n> ")
        (let [(input? input) (pcall #(io.read :*l))
              correct? (and (not (empty-string? input))
                            (string.match definition input))]
          (when input?
            (print (string.format (if correct?              "pona! ni li \"%s\""
                                      (empty-string? input) "\"%s\""
                                      :else                 "ike. ni li \"%s\"")
                                  definition))
            (record-answer word correct?)
            (play remaining-words-due))))))

(fn take [n tbl]
  (local new-tbl [])
  (each [_ e (ipairs tbl) &until (= n (length new-tbl))]
    (table.insert new-tbl e))
  new-tbl)

(fn init []
  (print "o musi nimi pi toki pona!")
  (let [max-words (or (tonumber (. arg 1)) 10)
        all-words (load-words)
        prior-answers (load-answers)
        words-in-review-order (sort-by-review-order all-words prior-answers)
        words-to-review (take max-words words-in-review-order)]
    (play words-to-review)
    (print "\ntawa pona!")))

(init)
