; * * * * * * * * * * * * * * * * * * * * *
; * Card Structure			  *
; * * * * * * * * * * * * * * * * * * * * *

; p is the card defstruct here
(defun print-card (p stream depth)
  (format stream "~A of ~A," (cdr (assoc (card-rank p) rank)) (cdr (assoc (card-suit p) suit))))

(defstruct (card (:print-function print-card))
  suit
  rank)
  
(defun get-value (card)
  (case (card-rank card)
    (1 11)
    (11 10)
    (12 10)
    (13 10)
    (otherwise (card-rank card))))

; Set up the suits for 'pretty' printing of cards
(setf suit '((1 . "Spades")(2 . "Hearts")(3 . "Spades")(4 . "Diamonds")))

; Set up the ranks for 'pretty' printing of cards
(setf rank '((1 . "Ace")(2 . "2")(3 . "3")(4 . "4")
  (5 . "5")(6 . "6")(7 . "7")(8 . "8")(9 . "9")
  (10 . "10")(11 . "Jack")(12 . "Queen")(13 . "King")))


; * * * * * * * * * * * * * * * * * * * * *
; * Player Structure			  *
; * * * * * * * * * * * * * * * * * * * * *

(defun print-player (p stream depth)
  (format stream "~A: ~A" (player-name p) (player-cards p)))

; prints all but the dealer's first card
(defun print-dealer (dealer stream)
    (format stream "Dealer: *** of ***, ~A" (cdr (player-cards dealer))))

(defstruct (player (:print-function print-player))
  name
  cards)
  
(defun new-player (name)
  (make-player :name name :cards ()))
  
(defun add-card (player card)
  (if (null card)
  nil
  (setf (player-cards player) (cons card (player-cards player)))))
  

; * * * * * * * * * * * * * * * * * * * * *
; * Hash Table				  *
; * * * * * * * * * * * * * * * * * * * * *

(defun print-player-state (p stream depth)
  (format stream "~A of ~A games played."  (player-state-wins p) (player-state-games-played p)))

(defstruct (player-state (:print-function print-player-state))
  wins
  games-played
  password
  bank)
  
(defun load-player (name)
  (gethash name hash-table))
  
(defun save-player (name info)
  (setf (gethash name hash-table) info))

; * * * * * * * * * * * * * * * * * * * * *
; * Card Helpers			  *
; * * * * * * * * * * * * * * * * * * * * *


(defun create-deck ()
  (setf deck (new-player "deck"))
  (loop for i in suit do 
    (loop for j in rank do
      (add-card deck (make-card :rank (car j) :suit (car i)))))
  (player-cards deck))

(defun create-shoe ()
  (append (create-deck) (create-deck)  (create-deck) (create-deck) (create-deck) (create-deck)))

(defun shuffle (x)
  (setf len (length x))
  (dotimes (n len)
    (rotatef (nth n x) (nth (random len) x)))
  x)

(defun card-sum (cards)
  (setf sum (reduce (lambda (a b) (+ (get-value b) a)) cards :initial-value 0))
  (if (> sum 21)
    (correct-sum sum (length (remove-if-not (lambda (a) (= 1 (card-rank a))) cards)))
    sum))

(defun correct-sum (sum aces)
  (if (and (> sum 21) (> aces 0))
    (correct-sum (- sum 10) (- aces 1))
    sum))

;(defun get-sum (x)
;    (if (null x)
;      0
;      (+ (get-value (car x)) (get-sum (cdr x)))))

(defun draw-card ()
  (setf card (car deck))
  (setf deck (cdr deck))
  (values card))

; * * * * * * * * * * * * * * * * * * * * *
; * Macros				  *
; * * * * * * * * * * * * * * * * * * * * *

(defmacro iff (b a c question then colon else) `(if (,a ,b ,c) ,then ,else))

; * * * * * * * * * * * * * * * * * * * * *
; * Sandbox Evaluate			  *
; * * * * * * * * * * * * * * * * * * * * *

; Don't want the user to be able to run anything they want on the server.

; With ifs
;(setf known-commands '(hit stay again))
;(setf known-variables '(nil))
;
;(defun sandboxed-eval (cmd)
;  (if (sandbox-check cmd)
;    (eval cmd)
;    (format t "I do not know that command")))
;
;(defun sandbox-check (cmd)
;  (if (null cmd)
;  nil
;  (or (member (car cmd) known-commands)
;    (and (eq 'if (first cmd)) (sandbox-check-cond (second cmd)
;    (sandbox-check (third cmd)) (sandbox-check (fourth cmd)))))))
;    
;(defun sandbox-check-cond (cond)
;  (and (member (first cond) '(> < = eq eql and or))
;    (or (member (second cond) known-variables) (numberp (second cond)) (sandbox-check-cond (second cond)))
;    (or (member (third cond) known-variables) (numberp (third cond)) (sandbox-check-cond (third cond)))))

;; With out if's
(setf known-commands '(hit stay))

(defun sandboxed-eval (cmd)
  (if (member (car cmd) known-commands)
    (eval cmd)
    '(I do not know that command)))


; * * * * * * * * * * * * * * * * * * * * *
; * Functions the player can run	  *
; * * * * * * * * * * * * * * * * * * * * *

(defun dealer-play ()
  (add-card dealer (draw-card))
  (if (< (card-sum (player-cards dealer)) 17)
  (dealer-play)))
      
; * * * * * * * * * * * * * * * * * * * * *
; * Functions the player can run	  *
; * * * * * * * * * * * * * * * * * * * * *

(defun hit ()
  (add-card player (draw-card)))

(defun stay ()
  (player-cards player))

(defun again ()
  (sandboxed-eval last-command))

(defun echo ()
  (last-command))


; * * * * * * * * * * * * * * * * * * * * *
; * Main Function			  *
; * * * * * * * * * * * * * * * * * * * * *


(progn
  ; Open Socket
  (setf hash-table (make-hash-table))
  (defparameter game-socket (socket-server 4321))

  (loop

    ; Accept Connection
    (format t "Waiting for player...")
    (defparameter one-stream (socket-accept game-socket))
    (defparameter string-stream (make-string-output-stream))
        
        ; seed the random number generator
        (defparameter *random-state* (make-random-state t))
    ; Make deck and Shuffle
    (setf deck (shuffle (shuffle (create-shoe))))
    
    ; Get Name and make player
    (setf player-name (read one-stream))
    (setf player (new-player player-name))
    (setf dealer (new-player "Dealer"))
    (format one-stream "Welcome~A!" (player-name player))
    (format t "~A has joined~%" (player-name player))
    
    ; Load Dealer State
    (setf dealer-info (load-player 'dealer))
    (if (null dealer-info) (progn (setf dealer-info (make-player-state))
      (setf (player-state-wins dealer-info) 0)
      (setf (player-state-games-played dealer-info) 0)))
      
    ; Load Player State
    (setf player-info (load-player player-name))
    (if (null player-info) (progn (setf player-info (make-player-state))
      (setf (player-state-wins player-info) 0)
      (setf (player-state-games-played player-info) 0)))
      
    
    ; Deal Two Cards
    (add-card player (draw-card))
    (add-card dealer (draw-card))
    (add-card player (draw-card))
    (add-card dealer (draw-card))

    (print player string-stream)
        (format string-stream "~%")     ; prints a new line
        (print-dealer dealer string-stream)
    (print (get-output-stream-string string-stream) one-stream)
    
                ; Get Bet <------ Maybe                                                                                                                                                      
                ; (setf input (read one-stream))                                                                                                                                             
                
    (loop
      ; Hit or stay?
      (setf input (read one-stream))
      (if (or (> (length (player-cards player)) 4) (eq (car input) 'close) (eq (car input) 'ragequit)) (return))

                        ; Eval 
                        (sandboxed-eval input)
                        ;(print (read) one-stream)              
                        
                        ; Print New Cards
                        (if (or (> (card-sum (player-cards player)) 21) (eq (car input) 'stay))
                                (progn (print 'done one-stream) (return))
                                (progn (print player string-stream)
                                (print (get-output-stream-string string-stream) one-stream))))
                                                                                   
    (if (eq (car input) 'ragequit) (return))
                        
                        ; Save last-command
                        ;(if (not (eq (car input) 'again))
                        ;       (setf last-command input))))
    
    (if (not (eq (car input) 'close))
    (progn
    ; Handle Dealer
    (if (and (> 17 (card-sum (player-cards dealer))) (> 22 (card-sum (player-cards player))))
      (dealer-play))

    (print (list player dealer) string-stream)
    
    ; Winner
    (setf dealer-score (card-sum (player-cards dealer)))
    (setf player-score (card-sum (player-cards player)))
    (if (and (or (> dealer-score 21) (> player-score dealer-score) (> (length (player-cards player)) 4)) (< player-score 22))
      (progn (print (list 'Winner player-score 'to dealer-score) string-stream)
        (setf (player-state-wins player-info) (+ 1 (player-state-wins player-info)))
        (setf (player-state-games-played player-info) (+ 1 (player-state-games-played player-info)))
        (setf (player-state-games-played dealer-info) (+ 1 (player-state-games-played dealer-info))))
      (progn (print (list 'Loser player-score 'to dealer-score) string-stream)
        (setf (player-state-wins dealer-info) (+ 1 (player-state-wins dealer-info)))
        (setf (player-state-games-played player-info) (+ 1 (player-state-games-played player-info)))
        (setf (player-state-games-played dealer-info) (+ 1 (player-state-games-played dealer-info)))))
                (if (> (length (player-cards player)) 4) (print "Five Card Charlie" string-stream))
    (print player-info string-stream)
    
    ; Send Result
    (print (get-output-stream-string string-stream) one-stream)
    
    ; Save Player State
    (save-player (player-name player) player-info)

    ; Save Dealer State
    (save-player 'dealer dealer-info)))

    ; Close Connection
    (close one-stream)
    
    (format t "~A Done ~%" (player-name player))
    (format t "The dealer is ~A~%" (load-player 'dealer)))
  
  ; Close Socket
  (socket-server-close game-socket))
  
(defun get-clients ()
    (if (socket-wait game-socket 0 500)
      (cons (socket-accept game-socket :timeout '(0 500)) (get-clients))
      nil))

;  (defparameter game-socket (socket-server 4321))
;  (socket-server-close game-socket)
;  (defparameter server (socket-connect 4321 "127.0.0.1"))

;(defun get-clients ()
;  (setf clients '())
;  (loop
;    (setf user (handler-case (socket-accept game-socket :timeout '(0 500)) (error (se) 'nothin)))
;    (cons clients user)
;    (if (null user) (return clients))))
;