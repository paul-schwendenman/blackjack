(setf suit '((1 . "Spades")(2 . "Hearts")(3 . "Spades")(4 . "Diamonds")))
(setf rank '((1 . "Ace")(2 . "2")(3 . "3")(4 . "4")
	(5 . "5")(6 . "6")(7 . "7")(8 . "8")(9 . "9")
	(10 . "10")(11 . "Jack")(12 . "Queen")(13 . "King")))


;(setf players (make-array '(5 2) :initial-element nil))
;(setf (aref players 0 0) 'Name)
;(setf (aref players 0 1) 'Cards)


;Card Struct
(defun print-card (p stream depth)
	(format stream "~A of ~A," (cdr (assoc (card-rank p) rank)) (cdr (assoc (card-suit p) suit))))

(defstruct (card (:print-function print-card))
	suit
	rank)
	
(make-card :suit 'S :rank 'Q)

(defun get-value (card)
	(case (card-rank card)
		(1 11)
		(11 10)
		(12 10)
		(13 10)
		(otherwise (card-rank card))))

(defun get-value1 (card)
	(case (card-rank card)
		(11 10)
		(12 10)
		(13 10)
		(otherwise (card-rank card))))

;Player
(defun print-player (p stream depth)
	(format stream "~A: ~A" (player-name p) (player-cards p)))

(defstruct (player (:print-function print-player))
	name
	cards)
	
(defun new-player (name)
	(make-player :name name :cards ()))
	
(defun add-card (player card)
	(if (null card)
	nil ; In case the deck is empty.
	(setf (player-cards player) (cons card (player-cards player)))))
	

(defun create-deck ()
	())

;Game Play
;(defun get-sum (x)
;		(if (null y)
;			0
;			(+ (get-value (car y)) (get-sum (cdr y)))
;		)(player-cards x))


(setf f (new-player 'phil))
(add-card f (make-card :suit 1 :rank 4))
(add-card f (make-card :suit 4 :rank 13))


;Server

(defun hit ()
	(add-card this-player (draw-card)))

(defun stay ()
	(player-cards this-player))

(defun again ()
	(sandboxed-eval last-command))

(defun draw-card ()
	(setf card (pick-card deck))
	(setf deck (remove card deck))
	(values card))

(defun pick-card (deck)
	(if (null deck)
	nil
	(nth (random (length deck)) deck)))

(setf known-commands '(hit stay again))

(defun sandboxed-eval (cmd)
	(if (sandbox-check cmd)
		(eval cmd)
		'(I do not know that command)))

(defun sandbox-check (cmd)
	(if (null cmd)
	nil
	(or (member (car cmd) known-commands)
		(and (eq 'if (first cmd)) (sandbox-check (third cmd)) (sandbox-check (fourth cmd)))))) ; We ignore the condition or 'second'
(progn
	(setf deck '(a b c d))
	(print "Name: ")	
	(setf this-player (new-player (read)))
	(print "Welcome")
	(loop
	(print "> ")
	;(setf read-stuff (list (car (concatenate 'string "(" (read-line) ")")) 'd))
	(setf read-stuff (read))
	(print (sandboxed-eval read-stuff))
	)
	(print 'done)
	)
	
	