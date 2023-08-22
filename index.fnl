(local words-path "words.tsv")
(local answers-path "answers.tsv")

(fn load-words []
  (collect [line (io.lines words-path)]
    (let [(word definition) (string.match line "(.*)\t(.*)")]
      (values word definition))))

(fn load-answers []
  (let [load! (fn []
                (icollect [line (io.lines answers-path)]
                  (let [(time word grade) (string.match line "(.*)\t(.*)\t(.*)")]
                    {:time (tonumber time)
                     :word word
                     :grade (tonumber grade)})))]
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
  (let [initial-stats (collect [word (pairs words)]
                        (values word {:repetitions 0 :easiness 2.5 :interval 0}))
        stats (accumulate [stats initial-stats
                           _ {: time : word : grade} (ipairs answers)]
                (let [word-stats (supermemo2 grade (. stats word))]
                  (tset word-stats :last-reviewed time)
                  (tset stats word word-stats)
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

(fn record-answer [word grade]
  (with-open [file (io.open answers-path :a)]
    (file:write (os.time) "\t" word "\t" grade "\n")))

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
            (record-answer word (if correct? 4 1)) ; TODO: More granular grades
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
