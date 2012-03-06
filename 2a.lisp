;***********Tests****************;
(deftest !generate3 ()
  (reset-seed)
  (test nil (generates 10 'schedule '*grammar3*)))

(deftest !raindi ()
  (reset-seed)
  (test 9 (randi 10)))

(deftest !reset-seed ()
  (test 10013 (reset-seed)))

(deftest !shuffle ()
  (reset-seed)
  (test '(adam bro dude brah) (shuffle '(bro dude adam brah))))

(deftest !random-elt ()
  (reset-seed)
  (test 'brah (random-elt '(bro dude adam brah))))

(deftest !one-of ()
  (reset-seed)
  (test '(brah) (one-of '(bro dude adam brah))))

(deftest !mappend ()
  (test '(2 3 5 6) (mappend #'cdr '((1 2 3) (4 5 6)))))