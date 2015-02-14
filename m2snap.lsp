;======================================================================
;
;	File:		m2snap.lsp
;	Purpose:	Move text to snap
;	Method:		Make a selection set of all text on selected layer.
;			Iterate over the entire selection set and...
;			1) Find current justification position
;			2) Determine where we are in relation to next snap point
;			3) Change x and y coordinates of group code 11 to the 
;			next snap point.
;			4) Update the drawing database to reflect change.
;			M2SNAP WILL NOT MOVE TEXT THAT IS ON SNAP ALREADY.
;			5) If text has no justification offer to draw a box
;			around the text.
;
;	**Warning**:THIS SCRIPT WILL ONLY WORK WITH TEXT ENTITIES. If you
;			select a layer that contains both text and drawing objects
;			move the text to a unique layer before running or the
;			script will crash.
;
;	Author:		Andrew Nisbet
;	Date:		Copyright (c) March 29, 2002
;	Version:	1.0
;			1.1 April 1, 2002 Move the text	to snap even if it
;			is not justified. (For Don and the CC-130 who use
;			default text all the time.) **FIXED**
;			- show_unjust is set incorrectly comparison should be to ""
;			not nil. **FIXED**
;			- show_unjust is unsatifactory just move all text to snap
;			regardless. Note: you can move text by group code 10 unless
;			group code 11 is set; in that case you can only modify
;			the entity's group 11 code not either 10 or 11.
;
;=======================================================================








;;; TAKES EACH TEXT ELEMENT ON USER LAYER AND PERFORMS CALCULATIONS TO
;;; DETERMINE IF AND WHERE TEXT IS TO MOVE TO.
(defun move_text(index)
	; Pull a entity name from the selectionset by index.
	(setq one_ent (ssname ss1 index))

	; let this_ent = the actual entity from the data base.
	(setq this_ent (entget one_ent))
	
	; group code 11 stores the target of where we want the snap point to be.
	; Don't confuse this with group code 10 which is just the insertion point
	; of the text. If the text carries any justification other than default
	; group code 11 will be set to some value other than 0.0, 0.0, 0.0.
	(setq eleven (assoc 11 this_ent))
	
	(if (equal eleven (list 11 0.0 0.0 0.0))
		(progn ; progna - use this for moving text by insertion.
			(setq ten (assoc 10 this_ent))
			; get the x and y coordinates for group code 10 - insertion
			(prompt "\nOld location: ")
			(print ten)
			(setq origin (cdr ten))
			(setq byhandle 10)
		) ; end progna - else user wants to move text by justification
		(progn ; prognb
			; group code 11 stores the target of where we want the snap point to be.
			; Don't confuse this with group code 10 which is just the insertion point
			; of the text. If the text carries any justification other than default
			; group code 11 will be set to some value other than 0.0, 0.0, 0.0.
			; get the x and y coordinates for group code 11 - justification
			(prompt "\nOld location: ")
			(print eleven)
			(setq origin (cdr eleven))
			(setq byhandle 11)
		) ; end of prognb
	)
	
	; Here we isolate the x, y values for location analysis.
	(setq x (car origin))
	(setq y (nth 1 origin))
	
	;======================== x coordinate handled here =================
	; Now that we have the current decimal location of the x value
	; we now have to find out how far that is from the next snap
	; dxx is the temp value to compare to diffx.
	(setq nextsnapx 0.0)
	(while (> x nextsnapx)
		(setq nextsnapx (+ nextsnapx snapdist))
	)
	
	
	
	;======================== y coordinate handled here =================
	(setq nextsnapy 0.0)
	(while (> y nextsnapy)
		(setq nextsnapy (+ nextsnapy snapdist))
	)
	
	
	
	
	;======================== Change group code ======================
	
	(setq newlocation (list byhandle nextsnapx nextsnapy 0.0))
	(prompt "\nNew location: ")	
	(print newlocation)
	; This line tells AutoCAD to substitute group code 10 with the 
	; newly calculated group code 10 'newlocation'
	(setq this_ent (subst newlocation (assoc byhandle this_ent) this_ent))
	; and update the drawing database.
	(entmod this_ent)
	

) ;;; END OF DEFUN move_text(index)





;==================== Function main =============================
;;; MAIN ENTRY POINT FOR THIS APPLICATION.
(defun C:m2snap()
	
	(setvar "CMDECHO" 0)
	(initget (+ 2 4))
	(setq snapdist (getreal "\nEnter snap <use current>: "))
	(if (= snapdist nil)
		(setq snapdist (car (getvar "SNAPUNIT")))
	)
	
	; Set this var to the name of user's choice.
	(setq lay_name (getstring "\nEnter text layer \"layer name\" <ETEXT>: "))
	(if (= lay_name "")
		(setq lay_name "ETEXT")
	)
	; now test to see if there is a layer by that name.
	(if (= (tblsearch "layer" lay_name) nil)
		(progn
			(prompt "\nNo layer by that name: ")
			(print lay_name)
			(setvar "CMDECHO" 1)
			(quit)
		)
	)
	
	
	
	
	; Set ss1 (selection set 1) to all values in data base that conform
	; to the selection set filter of being on layer named ETEXT
	(setq ss1
		; X is all in database, cons is construct list first element
		(ssget "X" (list (cons 8 lay_name)))
	)


	; Find the length of the list so we can iterate over the entire pickset.
	(setq ilast (sslength ss1))



	; Iterate over the list calling move_ent() with the argument of i
	(setq i 0)
	(while (< i ilast)
	; this next line will just do the first two entities.
	;(while (< i 2)
		(move_text i)
		(setq i (+ i 1))
	)

	; print this message and exit
	(princ "\ntotal number of items on list: ")
	(princ ilast)
	(setvar "CMDECHO" 1)
	(print)
)
