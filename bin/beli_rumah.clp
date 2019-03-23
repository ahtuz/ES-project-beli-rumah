(deftemplate houseWithGarage
    ;template house with garage
    (slot type)
    (slot room)
    (slot price)
    (slot location)
    (slot garage)
)

(deftemplate houseNoGarage
    ;template house without garage
    (slot type)
    (slot room)
    (slot price)
    (slot location)
)

(deffunction menu()
    ;menu
	(printout t "================" crlf)
	(printout t "|| Beli Rumah ||" crlf)
	(printout t "================" crlf)
	(printout t "1. View House" crlf)
	(printout t "2. Add a New House" crlf)
	(printout t "3. Update House Detail" crlf)
	(printout t "4. Delete House" crlf)
	(printout t "5. Search Match" crlf)
	(printout t "6. Exit" crlf)
)

(deffunction clearScreen()
    ;clear screen
    (for (bind ?x 0)(< ?x 10)(++ ?x)
        (printout t crlf)
    )
)

(deffunction viewHouse()
    (printout t "List of house to be viewed" crlf)
    (printout t "============================================" crlf)
    (printout t "1. House with Garage" crlf)
    (printout t "2. House without Garage" crlf)
    
    (printout t "Choose[1/2 | 0 to back to main menu]: ")
    
)

(reset)

;inisialisasi choice menu
(bind ?choice 0)

(while (neq ?choice 6)
    (menu)

    ;inisialisasi flag validasi choice
    (bind ?flagChoice FALSE)
    
    (while(eq ?flagChoice FALSE)
        
        (printout t ">> Input [1-6]: ")
		(bind ?choice (read))
        
        ;validasi choice tipe adalah angka
        (if(eq (numberp ?choice) TRUE) then
            
            ;validasi choice harus 1...6
        	(if(or (< ?choice 1) (> ?choice 6)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    (if (eq ?choice 1) then
			(viewHouse)
        
        elif (eq ?choice 2) then
        	;(addHouse)
        
        elif (eq ?choice 3) then
        	;(updateHouse)
        
        elif (eq ?choice 4) then
        	;(deleteHouse)
        
        elif (eq ?choice 5) then
        	;(searchHouse)
    )
    
    (clearScreen)
    
)