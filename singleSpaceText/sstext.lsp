;##########################################################################
;# File Name:       sstext.lsp
;# Author:          Andrew Nisbet
;# Date:            Copyright (c) June 7, 2006, Andrew Nisbet
;# Version:         1.0
;# Version History: June x, 2006 Initial release
;# Dependancies:    None
;# To do:
;##########################################################################

; Algorithm:
; The use case begins when the command 'sstext.lsp' is loaded into autocad and
; 'SST' is entered at the command line.
; The user is prompted to select seed text.
; The user hits <enter>.
; The user is then prompted to select a group (1 or more) lines of text.
; The user hits <enter>.
; The selected text is aligned and single spaced.
; The use case ends.
;
; The use case begins after the application is loaded and running and
; the user has selected a seed string and hit <enter>.
; The application finds the x value of the seed line and stores it.
; The application finds the y value of the seed line and stores it.
; The use case ends.
;
; The use case begins when the user has finished selecting a seed string.
; Loop:
; 	The application takes next string from the selection.
; 	The application changes its x-value to that of seedX.
; 	The application changes its y-value to that of seedY + 0.125197 * loopNum
; end
; The use case ends when the string list has been exhausted.

; =================== Main entry point of App. ============================
(defun C:sst()
  (setvar "CMDECHO" 0)
  
  ; Get the seed text
  ; This gets the location and name of the entity selected.
  (textpage)
  (prompt "\nSelect seed text. ")
  (setq seedText (ssget))
  (if (null seedText) 
    (progn
      (princ "\nERROR: Nothing selected.")
      (setvar "CMDECHO" 1)
      (exit)
    )
  )
  (setq seedlen (sslength seedText))
  (if (> seedlen 1)
    (progn
      (princ "\nERROR: Please select only one piece of text.")
      (setvar "CMDECHO" 1)
      (exit)
    )
  )
  ; get the x and y values for the insertion point.
  ;(setq text (entget (ssname 0 seedText)))
  (setq ent (ssname seedText 0))
  ;(print ent)
  (setq ent1 (entget ent))
  
  ;(textpage)
  ;(princ "\nResults from the selected entity: ")
  ;(setq ct 0)
  ;(repeat (length ent1)
  ;  (print (nth ct ent1))
  ;  (setq ct (1+ ct))
  ;)
  (setq seedX (nth 1 (assoc 10 ent1)))
  (setq seedY (nth 2 (assoc 10 ent1)))
  ;(print seedX)
  ;(print seedY)
  ;(princ)
  ;(exit)
  ; The user is then prompted to select a group (1 or more) lines of text.
  ; The user hits <enter>.
  (prompt "\nSelect all text (including seed) to move, in order. ")
  (setq ssText (ssget))
  (if (null ssText) 
    (progn
      (princ "\nERROR: Nothing selected.")
      (setvar "CMDECHO" 1)
      (exit)
    )
  )
  ; Set a counter; this will be used to calculate offset.
  (setq ct 0)
  (repeat (sslength ssText)
    ; get the next entity name
    (setq sent (ssname ssText ct))
    ; get the next entity off the selection set.
    (setq sent1 (entget sent))
    ; display which one it is.
    ;(print sent1)
    ; Modify each of the text element's locations
    ; seedY + 0.125197 * loopNum
    (setq newlocation (list 10 seedX (- seedY (* 0.125197 ct)) 0.0))
    ; substitute the location of the text with the new location
    (setq sent1 (subst newlocation (assoc 10 sent1) sent1))
    ; Modify the entity in the drawing database.
    (entmod sent1)
    ; Increment the counter.
    (setq ct (1+ ct))
  )

			
  (setvar "CMDECHO" 1)
  (princ)
)
