(local words-path "words.tsv")
(local answers-path "answers.tsv")

(fn empty-string? [s]
  (= "" (string.gsub s "%s" "")))

(fn shuffle! [tbl]
  (for [i (length tbl) 2 -1]
    (let [j (math.random i)
          tmp (. tbl i)]
      (tset tbl i (. tbl j))
      (tset tbl j tmp))))

(fn take-first [n tbl]
  (local new-tbl {})
  (each [_ e (ipairs tbl) &until (= n (length new-tbl))]
    (table.insert new-tbl e))
  new-tbl)

(fn file-exists? [path]
  (let [file (io.open path :r)
        exists? (~= nil file)]
    (when exists?
      (io.close file))
    exists?))

(fn load-words []
  (collect [line (io.lines words-path)]
    (let [(word definition) (string.match line "(.*)\t(.*)")]
      (values word definition))))

(fn load-answers []
  (if (not (file-exists? answers-path))
    []
    (icollect [line (io.lines answers-path)]
      (let [(raw-time word raw-correct?) (string.match line "(.*)\t(.*)\t(.*)")]
        {:time (tonumber raw-time)
         :word word
         :correct? (= raw-correct? "yes")}))))

; Ported from https://en.wikipedia.org/wiki/SuperMemo
(fn supermemo2 [q n ef i]
  (let [i* (if (< q 3)
             1
             (case n
               0 1
               1 6
               :else (* i ef)))
        n* (if (< q 3)
             1
             (+ n 1))
        ef* (math.max 1.3
                      (+ ef (- 0.1 (* (- 5 q)
                                      (+ 0.08 (* 0.02 (- 5 q)))))))]
    (values n* ef* i*)))

(fn process-words-and-answers [words answers]
  (let [stats (collect [word (pairs words)]
                (values word {:last-reviewed nil
                              :repetitions 0
                              :easiness 2.5
                              :interval 0}))]
    (each [_ {: time : word : correct?} (ipairs answers)]
      (let [{: repetitions : easiness : interval} (. stats word)
            user-grade (if correct? 4 1)
            (new-repetitions
             new-easiness
             new-interval) (supermemo2 user-grade repetitions easiness interval)
            new-stats {:last-reviewed time
                       :repetitions new-repetitions
                       :easiness new-easiness
                       :interval new-interval}]
        (tset stats word new-stats)))
    stats))

(fn record-answer [word correct?]
  (with-open [file (io.open answers-path :a)]
    (file:write (os.time) "\t" word "\t" (if correct? "yes" "no") "\n")))

(fn play [words-due]
  (when (next words-due)
    (let [{: word : definition} (table.remove words-due 1)
          correct? #(and (not (empty-string? $))
                         (string.match definition $))]
      (print "")
      (print word)
      (io.write "> ")
      (let [guess (case (pcall #(io.read :*l))
                    (false _) :quit
                    (where (_ input) (and (not (empty-string? input))
                                          (string.match definition input))) :correct
                    (where (_ input) (empty-string? input)) :empty
                    _ :incorrect)]
        (case guess
          :correct (do (print (string.format "pona! ni li \"%s\"" definition))
                       (record-answer word true)
                       (play words-due))

          :incorrect (do (print (string.format "ike. ni li \"%s\"" definition))
                         (record-answer word false)
                         (play words-due))

          :empty (do (print (string.format "\"%s\"" definition))
                     (record-answer word false)
                     (play words-due)))))))

(fn due? [stats word]
  (let [time-now (os.time)
        {: last-reviewed : interval} (. stats word)
        one-day 86400
        review-time (+ (or last-reviewed 0)
                       (* interval one-day))]
    (< review-time time-now)))

(fn init [words-per-session]
  (print "musi sitelen pi toki pona!")
  (let [all-words (load-words)
        prior-answers (load-answers)
        stats (process-words-and-answers all-words prior-answers)
        words-due (icollect [word definition (pairs all-words)]
                    (when (due? stats word)
                      {: word : definition}))]
    (shuffle! words-due)
    (play (take-first words-per-session words-due))
    (print "\ntawa pona!")))

(let [words-per-session (or (tonumber (. arg 1)) 10)]
  (init words-per-session))
