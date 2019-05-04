(defglobal 
    ?*withGarageCount* = 0
    ?*noGarageCount* = 0
)

;not yet make global idx for add
;not yet make idx for differentiate with garage & no garage

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

(defrule viewHouseWithGarage
    (houseWithGarage (type ?type)(room ?room)(price ?price)(location ?location)(garage ?garage))
    =>
	(bind ?*withGarageCount* (+ ?*withGarageCount* 1))
    (printout t ?*withGarageCount* ". " ?type " |" ?room " |" ?price " |" ?location " |" ?garage " " crlf)
)

(defrule viewHouseNoGarage
    (houseNoGarage (type ?type)(room ?room)(price ?price)(location ?location))
    =>
	(bind ?*noGarageCount* (+ ?*noGarageCount* 1))
    (printout t ?*noGarageCount* ". " ?type " |" ?room " |" ?price " |" ?location crlf)
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
            	(run)
            	(printout t "============================================" crlf)
            
            	;agar bisa keluar dari looping awal
            	(bind ?choice 0)
            	(printout t "Press Enter to Continue...")
            	(readline)
            
	        elif (eq ?choice 2) then
	        	(printout t "House Without Garage")
	            (printout t "============================================" crlf)
            	(printout t "| No. | Type				| Room		| Price				| Location			|" crlf)
            	(run)
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
    (bind ?location "")
    
    (while (and (neq ?location "West Jakarta")
            (neq ?location "North Jakarta")
            (neq ?location "South Jakarta"))
		(printout t "Input house type [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
        (bind ?location (readline))
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
    (bind ?location "")
    
    (while (and (neq ?location "West Jakarta")
            (neq ?location "North Jakarta")
            (neq ?location "South Jakarta"))
		(printout t "Input house type [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
        (bind ?location (readline))
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
            (bind ?choice 0)
         elif (eq ?choice 2) then
            (addHouseNoGarage)
            (bind ?choice 0)
        )
    )   
)

(deffunction updateHouseWithGarage(); Not perfect
    (viewHouse)
    (run)
        
    (bind ?idxFlag FALSE)
    (bind ?idx -1)

    (while(eq ?idxFlag FALSE)

        (printout t "Which house to be updated [ 1.. "?*withGarageCount" | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*withGarageCount* )) then 
                (bind ?flagChoice FALSE)
            else
                (bind ?flagChoice TRUE)
            )
                    
        else
            (bind ?flagChoice FALSE)

        )    
    )

    (bind ?utype "")
    (while (and (neq ?utype "Cottage") (neq ?utype "Light House") (neq ?utype "Skyscraper"))
        (printout t "Input New House Type [Cottage | Light House | Skyscraper] (Case Sensitive): ")
        (bind ?utype (readline))
    )

    (bind ?uroom 0)
    (while ( or (eq (numberp ?uroom) FALSE) (or (< ?uroom 1) (> ?uroom 5) ) )
        (printout t "Input Room Number [1..5]: ")
        (bind ?uroom (read))
    )

    (bind ?uprice 0)
    (while ( or (eq (numberp ?uprice) FALSE) (or (< ?uprice 1000) (> ?uprice 500000) ) )
        (printout t "Input New House Price [1000..500000]: ")
        (bind ?uprice (read))
    )

    (bind ?uloc "")
    (while (and (neq ?uloc "West Jakarta") (neq ?uloc "North Jakarta") (neq ?uloc "South Jakarta"))
        (printout t "Input Location [West Jakarta | North Jakarta | South Jakarta] (Case Sensitive): ")
        (bind ?uloc (readline))
    )

    (bind ?ugarage 0)
    (while ( or (eq (numberp ?ugarage) FALSE) (or (< ?ugarage 1) (> ?ugarage 5) ) )
        (printout t "Input Garage Number [1..5]: ")
        (bind ?ugarage (read))
    )

    (modify ?idx (type ?utype)(room ?uroom)(price ?uprice)(location ?uloc)(garage ?ugarage))
)

(deffunction updateHouseNoGarage(); Also Not perfect
    (viewHouse)
    (run)
        
    (bind ?idxFlag FALSE)
    (bind ?idx -1)

    (while(eq ?idxFlag FALSE)

        (printout t "Which house to be updated [ 1.. "?*noGarageCount" | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*noGarageCount* )) then 
                (bind ?flagChoice FALSE)
            else
                (bind ?flagChoice TRUE)
            )
                    
        else
            (bind ?flagChoice FALSE)

        )    
    )

    (bind ?utype "")
    (while (and (neq ?utype "Cottage") (neq ?utype "Light House") (neq ?utype "Skyscraper"))
        (printout t "Input New House Type [Cottage | Light House | Skyscraper] (Case Sensitive): ")
        (bind ?utype (readline))
    )

    (bind ?uroom 0)
    (while ( or (eq (numberp ?uroom) FALSE) (or (< ?uroom 1) (> ?uroom 5) ) )
        (printout t "Input Room Number [1..5]: ")
        (bind ?uroom (read))
    )

    (bind ?uprice 0)
    (while ( or (eq (numberp ?uprice) FALSE) (or (< ?uprice 1000) (> ?uprice 500000) ) )
        (printout t "Input New House Price [1000..500000]: ")
        (bind ?uprice (read))
    )

    (bind ?uloc "")
    (while (and (neq ?uloc "West Jakarta") (neq ?uloc "North Jakarta") (neq ?uloc "South Jakarta"))
        (printout t "Input Location [West Jakarta | North Jakarta | South Jakarta] (Case Sensitive): ")
        (bind ?uloc (readline))
    )

    (modify ?idx (type ?utype)(room ?uroom)(price ?uprice)(location ?uloc))
    
)

(deffunction deleteHouseWithGarage() ; definitely needs fixing
    (viewHouse)
    (run)
        
    (bind ?idxFlag FALSE)
    (bind ?idx -1)

    (while(eq ?idxFlag FALSE)

        (printout t "Which house to be deleted [ 1.. "?*withGarageCount" | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*withGarageCount* )) then 
                (bind ?flagChoice FALSE)
            else
                (bind ?flagChoice TRUE)
            )
                    
        else
            (bind ?flagChoice FALSE)

        )    
    )

    (retract ?idx)

)

(deffunction deleteHouseNoGarage() ; definitely needs fixing
    (viewHouse)
    (run)
        
    (bind ?idxFlag FALSE)
    (bind ?idx -1)

    (while(eq ?idxFlag FALSE)

        (printout t "Which house to be deleted [ 1.. "?*noGarageCount" | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*noGarageCount* )) then 
                (bind ?flagChoice FALSE)
            else
                (bind ?flagChoice TRUE)
            )
                    
        else
            (bind ?flagChoice FALSE)

        )    
    )

    (retract ?idx)

)

(deffunction updateHouse() ; Please Check Again
    (printout t "Type of house to be updated" crlf)
    (printout t "=========================" crlf)
    (printout t "1. House with garage" crlf)
    (printout t "2. House without garage" crlf)

    (bind ?choice -1)
    
    (while (neq ?choice 0)
        
	    (bind ?flagChoice FALSE)

	    (while(eq ?flagChoice FALSE)
	        
	        (printout t "Choose [ 1..2 | 0 to back to main menu ]: ")
        	(bind ?choice (read))
	        
	        (if(eq (numberp ?choice) TRUE) then
	            
	        	(if (and (neq ?choice 0) (neq ?choice 1) (neq ?choice 2)) then
	            	(bind ?flagChoice FALSE)
	        	else
	        		(bind ?flagChoice TRUE)
	        	)
	            
	        else
	        	(bind ?flagChoice FALSE)
	    	)
	    )

        (bind ?idx -1)
        (if (eq ?choice 1) then
            (updateHouseWithGarage)

        elif (eq ?choice 2) then 
            (updateHouseNoGarage)


        )

        (printout t "============================================" crlf)
                
        (bind ?choice 0)
        (printout t "Press Enter to Continue...")
        (readline)
    )
)

(deffunction deleteHouse() ; Please Check Again
    (printout t "Type of house to be deleted" crlf)
    (printout t "=========================" crlf)
    (printout t "1. House with garage" crlf)
    (printout t "2. House without garage" crlf)

    (bind ?choice -1)
    
    (while (neq ?choice 0)
        
	    (bind ?flagChoice FALSE)

	    (while(eq ?flagChoice FALSE)
	        
	        (printout t "Choose [ 1..2 | 0 to back to main menu ]: ")
        	(bind ?choice (read))
	        
	        (if(eq (numberp ?choice) TRUE) then
	            
	        	(if (and (neq ?choice 0) (neq ?choice 1) (neq ?choice 2)) then
	            	(bind ?flagChoice FALSE)
	        	else
	        		(bind ?flagChoice TRUE)
	        	)
	            
	        else
	        	(bind ?flagChoice FALSE)
	    	)
	    )

        (bind ?idx -1)
        (if (eq ?choice 1) then
            (deleteHouseWithGarage)

        elif (eq ?choice 2) then 
            (deleteHouseNoGarage)


        )

        (printout t "============================================" crlf)
                
        (bind ?choice 0)
        (printout t "Press Enter to Continue...")
        (readline)
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
        	(updateHouse)
        
        elif (eq ?choice 4) then
        	(deleteHouse)
        
        elif (eq ?choice 5) then
        	;(searchHouse)
    )
    
    (clearScreen)
    
)