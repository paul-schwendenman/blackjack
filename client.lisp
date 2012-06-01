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
    (format t "Connected... Waiting for new game~%")
    (format t "~A~%" (read server))
    
    ; Print Cards
    (format t "~A~%" (read server))
    
    ; Get Command
    (loop
      (setf command (read1))
      (print command server)
      (if (eq (car command) 'ragequit) (return))
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

 