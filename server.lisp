; * * * * * * * * * * * * * * * * * * * * *
; * Settings for quitting and closing	  *
; * * * * * * * * * * * * * * * * * * * * *

(setf close 'close)
(setf quit 'quit)

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
  socket-stream
  cards
  player-info
  string-stream)
  
(defun new-player (name ss)
  (make-player :name name :cards () :socket-stream ss))
  
(defun add-card (player card)
  (if (null card)
  nil
  (setf (player-cards player) (cons card (player-cards player)))))
  
(player-send (player)
  (print (get-output-stream-string (player-string-stream player)) (player-socket-stream player)))


; * * * * * * * * * * * * * * * * * * * * *
; * Hash Table				  *
; * * * * * * * * * * * * * * * * * * * * *

(defun print-player-state (p stream depth)
  (format stream "~A of ~A games played."  (player-state-wins p) (player-state-games-played p)))

(defstruct (player-state (:print-function print-player-state))
  (wins 0)
  (games-played 0)
  password
  (bank 100))
  
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


  
(defun get-clients ()
  (if (socket-wait game-socket 0 500)
      (cons (socket-accept game-socket :timeout '(0 500)) (get-clients))
      nil))

(defun close-clients (lst-clients)
  (if (null lst-clients)
      nil
      (progn (close (car lst-clients))
        ;(format t "~A Done ~%" (player-name ))
        (close-clients (cdr lst-clients)))))

(defun get-client-names (lst-clients)
      (mapcar #'(lambda (one-stream) 
        (let* ((player-name (read one-stream))(player (new-player player-name one-stream)))
          (format one-stream "Welcome~A!" (player-name player))
          (format t "~A has joined~%" (player-name player))
          player)) lst-clients))

(defun deal (lst-clients)
  (if (null lst-clients)
      nil
      (progn (add-card (car lst-clients) (draw-card))
        (deal (cdr lst-clients)))))

        
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
(progn
  ; Make hash-table for memory of players
  (setf hash-table (make-hash-table))

  ; Seed the random number generator
  (defparameter *random-state* (make-random-state t))

  ; Open Socket
  (defparameter game-socket (socket-server 4321))

  ; Make dealer
  (setf dealer (new-player "Dealer"))

  ; Load Dealer State
  (setf dealer-info (load-player 'dealer))
      
  ; Make deck and Shuffle
  (setf deck (shuffle (shuffle (create-shoe))))

  ; Accept Connection
  (format t "Waiting for player...")
  (setf clients (let ((client-connections (cons (socket-accept game-socket) (get-client))))
  
  ; Get Name and make player
  (get-client-names client-connections)))
  
  ; Deal Two Cards
  (deal clients)
  (add-card dealer (draw-card))
  (deal clients)
  (add-card dealer (draw-card))


(one-hand)

      (if (and (> 17 (card-sum (player-cards dealer))) (> 22 (card-sum (player-cards player))))
        (dealer-play))

(handle-game-end

    ; Save Dealer State
    (save-player 'dealer dealer-info)))

    ; Close Connection
    (close one-stream)
    
    (format t "~A Done ~%" (player-name player))
    (format t "The dealer is ~A~%" (load-player 'dealer)))
  
  ; Close Socket
  (socket-server-close game-socket))

(defun one-hand (player)
  (let* ((player-info (load-player (player-name player)))			; Load Player State
        (string-stream (make-string-output-stream))   				; Start String Stream for smarter printing
        (one-stream (player-socket-stream player)))

    ; Print Cards to player
    (print player string-stream)
    (format string-stream "~%")     						; prints a new line
    (print-dealer dealer string-stream)
    (print (get-output-stream-string string-stream) one-stream)
    
    ; Get Bet <------ Maybe                                                                                                                                                      
    ; (setf input (read one-stream))                                                                                                                                             
                
    (loop
      ; Hit or stay?
      (setf input (read one-stream))
      (if (or (> (length (player-cards player)) 4) (eq (car input) close) (eq (car input) quit)) (return))
        ; Eval 
        (sandboxed-eval input)
                        
        ; Print New Cards
        (if (or (> (card-sum (player-cards player)) 21) (eq (car input) 'stay))
          (progn (print 'done one-stream) (return))
          (progn (print player string-stream)
            (print (get-output-stream-string string-stream) one-stream))))))
                                                                                   
      ; Handle Dealer


(defun handle-game-end (player)
  (let* ((player-info (load-player (player-name player)))
        (string-stream (make-string-output-stream))
        (one-stream (player-socket-stream player)))
  
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

    ; Five Card Charlie
    (if (> (length (player-cards player)) 4) (print "Five Card Charlie" string-stream))
      (print player-info string-stream)
    
    ; Send Result
    (print (get-output-stream-string string-stream) one-stream)
    
    ; Save Player State
    (save-player (player-name player) player-info)))
