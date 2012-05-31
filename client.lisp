;(defmacro iff (b a c question then colon else) `(if (,a ,b ,c) ,then ,else))

(defun repl ()
	(setf reply (read server))
	(format t "~A~%" reply)
	(setf command (read1))
	(print command server)
	(unless (eq (car command) 'quit)
		(setf reply (read server))
		(print (read server))
		(repl)))
;(defun repl ()
;	(setf reply (read server))
;	(if (eq `,reply 'done)
;		(print (read server))
;		(format t "~A~%" (read server)))
;	(setf command (read1))
;	(print command server)
;	(unless (eq (car command) 'quit)
;		(setf reply (read server))
;			(print (read server))
;			(progn
;				(format t "~A~%" reply)
;				(repl)))))


(defun read1 ()
	(format t "~%> ")
	(read-from-string (concatenate 'string "(" (read-line)  ")")))

(progn
	; Get Name For Session
	(format t "Name: ")
	(setf name (read))
	
	(loop
		;Make connection
		(defparameter server (socket-connect 4321 "127.0.0.1"))
		
		;Send Name
		(print name server)
		(format t "Connected~%")
		(format t "~A~%" (read server))
		
		; Print Cards
		(format t "~A~%" (read server))
		
		; Get Command
		(loop
			(setf command (read1))
			(print command server)
			(if (eq (car command) 'quit) (return))
			(if (eq (car command) 'close) (return))
		
			; Get Response
			(setf response (read server))
			(if (not (eq response 'done))
			(format t "~A~%" response)
			(return)))

		(if (eq (car command) 'quit) (return))
		(if (eq (car command) 'close) (return))

		; Get Result		
		(format t "~A~%" (read server))
		
		; Close Connection
		(format t "~%~%............~%~%")
		(close server)))
