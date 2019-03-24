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

(deffunction viewHouse() ;not finish
    (printout t "List of house to be viewed" crlf)
    (printout t "============================================" crlf)
    (printout t "1. House with Garage" crlf)
    (printout t "2. House without Garage" crlf)
    
    ;inisialisasi viewHouse choice
    (bind ?choice -1)
    
    (while (neq ?choice 0)
        ;inisialisasi flag validasi viewHouse choice
	    (bind ?flagChoice FALSE)
	    
	    (while(eq ?flagChoice FALSE)
	        
	        (printout t "Choose [ 1/2 | 0 to back to main menu ]: ")
        	(bind ?choice (read))
	        
	        ;validasi tipe data choice adalah angka
	        (if(eq (numberp ?choice) TRUE) then
	            
	            ;validasi choice harus 1...2
	        	(if(or (< ?choice 0) (> ?choice 2)) then
	            	(bind ?flagChoice FALSE)
	        	else
	        		(bind ?flagChoice TRUE)
	        	)
	            
	        else
	        	(bind ?flagChoice FALSE)
	    	)
	    )
        
        (if (eq ?choice 1) then
            	(printout t "House With Garage" crlf)
            	(printout t "============================================" crlf)
            	(printout t "| No. | Type				| Room		| Price				| Location			| Garage	|" crlf)
            	;(printout t "|" ?no "|" ?type "				| " Room		| Price				| Location			| Garage	|" crlf)
            	(printout t "============================================" crlf)
            
            	;agar bisa keluar dari looping awal
            	(bind ?choice 0)
            	(printout t "Press Enter to Continue...")
            	(readline)
            
	        elif (eq ?choice 2) then
	        	(printout t "House Without Garage")
	            
	        
            	(printout t "============================================" crlf)
            
	        	;agar bisa keluar dari looping awal
	            (bind ?choice 0)
                (printout t "Press Enter to Continue...")
            	(readline)
        )
    )
)

(deffunction addHouseWithGarage()
    
    ;insert house type
    (bind ?type "")
    
    (while (and (neq ?type "Cottage")
            (neq ?type "Light House")
            (neq ?type "Skyscraper"))
		(printout t "Input house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?type (readline))
    )
    
    ;insert room number
    (bind ?room 0)
    
    (while (or (< ?room 1)
            (> ?room 5))
        (printout t "Input room number [1 - 5]: ")
        (bind ?room (read))
    )
    
    ;insert house price
    (bind ?price 0)
    
    ;inisialisasi flag validasi house price
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input house price [1000 - 5000000] (dollars): ")
		(bind ?price (read))
        
        ;validasi price tipe adalah angka
        (if(eq (numberp ?price) TRUE) then
            
            ;validasi price harus 1000...5000000 dollars
        	(if(or (< ?price 1000) (> ?price 5000000)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    ;insert house location
    (bind ?type "")
    
    (while (and (neq ?type "West Jakarta")
            (neq ?type "North Jakarta")
            (neq ?type "South Jakarta"))
		(printout t "Input house type [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
        (bind ?type (readline))
    )
    
    
    ;insert house garage
    (bind ?garage 0)
    
    ;inisialisasi flag validasi house garage
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input garage number [1 - 5]: ")
		(bind ?garage (read))
        
        ;validasi garage tipe adalah angka
        (if(eq (numberp ?garage) TRUE) then
            
            ;validasi garage harus 1...5
        	(if(or (< ?garage 1) (> ?garage 5)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    (assert(houseWithGarage(type ?type)(room ?room)(price ?price)(location ?location)(garage ?garage)))
    
)

(deffunction addHouseNoGarage()
    
    ;insert house type
    (bind ?type "")
    
    (while (and (neq ?type "Cottage")
            (neq ?type "Light House")
            (neq ?type "Skyscraper"))
		(printout t "Input house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?type (readline))
    )
    
    ;insert room number
    (bind ?room 0)
    
    (while (or (< ?room 1)
            (> ?room 5))
        (printout t "Input room number [1 - 5]: ")
        (bind ?room (read))
    )
    
    ;insert house price
    (bind ?price 0)
    
    ;inisialisasi flag validasi house price
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input house price [1000 - 5000000] (dollars): ")
		(bind ?price (read))
        
        ;validasi price tipe adalah angka
        (if(eq (numberp ?price) TRUE) then
            
            ;validasi price harus 1000...5000000 dollars
        	(if(or (< ?price 1000) (> ?price 5000000)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    ;insert house location
    (bind ?type "")
    
    (while (and (neq ?type "West Jakarta")
            (neq ?type "North Jakarta")
            (neq ?type "South Jakarta"))
		(printout t "Input house type [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
        (bind ?type (readline))
    )
    
    (assert(houseNoGarage(type ?type)(room ?room)(price ?price)(location ?location)))
    
)

(deffunction addHouse() ;not finish
	(printout t "Type of house to be addded" crlf)
    (printout t "============================================" crlf)
    (printout t "1. House with Garage" crlf)
    (printout t "2. House without Garage" crlf)
    
    ;inisialisasi addHouse choice
    (bind ?choice -1)
    
    (while (neq ?choice 0)
        ;inisialisasi flag validasi addHouse choice
	    (bind ?flagChoice FALSE)
	    
	    (while(eq ?flagChoice FALSE)
	        
	        (printout t "Choose [ 1/2 | 0 to back to main menu ]: ")
        	(bind ?choice (read))
	        
	        ;validasi tipe data choice adalah angka
	        (if(eq (numberp ?choice) TRUE) then
	            
	            ;validasi choice harus 1...2
	        	(if(or (< ?choice 0) (> ?choice 2)) then
	            	(bind ?flagChoice FALSE)
	        	else
	        		(bind ?flagChoice TRUE)
	        	)
	            
	        else
	        	(bind ?flagChoice FALSE)
	    	)
	    )
        
        (if (eq ?choice 1) then
            (addHouseWithGarage)
         elif (eq ?choice 2) then
            (addHouseNoGarage)
        )
    )   
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
        	(addHouse)
        
        elif (eq ?choice 3) then
        	;(updateHouse)
        
        elif (eq ?choice 4) then
        	;(deleteHouse)
        
        elif (eq ?choice 5) then
        	;(searchHouse)
    )
    
    (clearScreen)
    
)