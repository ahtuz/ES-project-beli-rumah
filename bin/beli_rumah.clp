(defglobal 
    ?*withGarageCount* = 0
    ?*noGarageCount* = 0
    ?*withGarageID* = 5
    ?*noGarageID* = 5
)

"withGarageCount & noGarageCount is for the view house rule"
"withGarageID & noGarageID starts from 5 because there are 5 initial facts"

;not yet make global idx for add
;not yet make idx for differentiate with garage & no garage

(deftemplate houseWithGarage
    ;template house with garage
    (slot id)
    (slot type)
    (slot room)
    (slot price)
    (slot location)
    (slot garage)
)

(deftemplate houseNoGarage
    ;template house without garage
    (slot id)
    (slot type)
    (slot room)
    (slot price)
    (slot location)
)

(deftemplate houseWithGarageValidSearch
    (slot id)
    (slot type)
    (slot room)
    (slot price)
    (slot location)
)

(defrule viewHouseWithGarage
    (view 1)
    (houseWithGarage (type ?type)(room ?room)(price ?price)(location ?loc)(garage ?garage))
    =>
    (bind ?*withGarageCount* (++ ?*withGarageCount*))
    (printout t ?*withGarageCount* ". | " ?type " | " ?room " | " ?price " | " ?loc " | " ?garage " |" crlf)
)

(defrule viewHouseNoGarage
    (view 0)
    (houseNoGarage (type ?type)(room ?room)(price ?price)(location ?loc))
    =>
    (bind ?*noGarageCount* (++ ?*noGarageCount*))
    (printout t ?*noGarageCount* ". | " ?type " | " ?room " | " ?price " | " ?loc " |" crlf)
)

(defrule retractView
    "untuk delete fact VIEW"
    ?view <- (view ?i)
    =>
    (retract ?view)
)

(defrule updateWithGarage
	?modify <- (modify ?idx 1)
    ?data-fact <- (houseWithGarage (id ?idx))
    =>
    (modify ?data-fact (id ?idx)(type ?utype)(room ?uroom)(price ?uprice)(location ?uloc)(garage ?ugarage))
    (retract ?modify)
)

(defrule updateNoGarage
	?modify <- (modify ?idx)
    ?data-fact <- (houseNoGarage (id ?idx))
    =>
    (modify ?data-fact (id ?idx)(type ?utype)(room ?uroom)(price ?uprice)(location ?uloc))
    (retract ?modify)
)

(defrule swapID
    ?swap <- (swap ?i ?idx)
    ?current-fact <- (houseWithGarage (id ?i))
    ?next-fact <- (houseWithGarage (id ?idx))
    =>
    (modify ?current-fact (id ?idx))
    (modify ?next-fact (id ?i))
    (retract ?swap)
)

(defrule deleteWithGarage
	?delete <- (delete ?idx ?withGarageIDToken 1)
    ?data-fact <- (houseWithGarage (id ?idx))
    =>
    (printout t "huehue")
    (bind ?idx (- ?withGarageIDToken ?idx))
    
    (while (neq ?idx ?*withGarageID*)
        (bind ?i (++ ?idx))
    	(assert (swap ?i ?idx))
        (run)
        (bind ?i (++ ?i))
        (bind ?idx (++ ?idx))
    )
    
    (retract ?data-fact)
    (retract ?delete)
)

(defrule deleteNoGarage
	?delete <- (delete ?idx)
    ?data-fact <- (houseNoGarage (id ?idx))
    =>
    (retract ?data-fact)
    (bind ?*noGarageID* (-- ?*noGarageID*))
    (bind ?noGarageIDFlag ?*noGarageID*)
    (while (neq ?noGarageIDFlag 0)
        (assert(houseWithGarage (id ?noGarageIDFlag)(type ?type)(room ?room)(price ?price)(location ?location)(garage ?garage)))
        (bind ?noGarageIDFlag (-- ?noGarageIDFlag))
    )
    (retract ?delete)
)

(defrule searchWithGarage
	?search <- (search "With Garage" ?sIncome ?sLocation ?sType)
    ;?data-fact <- (houseWithGarage (price ?price&:(<= ?price ?sIncome)) (location ?sLocation) (type ?sType))
    ?data-fact <- (houseWithGarage (price ?price) (location ?location) (type ?type))
    =>
    (bind ?flagValidity 3)
    
    (bind ?withGarageIDFlag (bind ?*withGarageID* (-- ?*withGarageID*)))
    (while (eq ?withGarageIDFlag 0)
        (if(< ?sIncome ?price) then
            (bind ?flagValidity (-- ?flagValidity))
        )
        (if(neq ?location ?sLocation) then
            (bind ?flagValidity (-- ?flagValidity))
        )
        (if(neq ?type ?sType) then
            (bind ?flagValidity (-- ?flagValidity))
        )
        (bind ?withGarageIDFlag (-- ?withGarageIDFlag))
    )
    
    (if (>= ?flagValidity 2) then
        (bind ?countValidity (++ ?countValidity))
        (assert houseWithGarageValidSearch (id ?countValidity) )
    )

    ;(printout t ?data-fact ?price ?sLocation ?sType)
    (retract ?search)
)

(defrule searchNoGarage
	?search <- (search ?sPreferences ?sIncome ?sLocation ?sType)
    ?data-fact <- (houseNoGarage (price ?price&:(< ?price ?sIncome)) (location ?location&:(= ?location ?sLocation)) (type ?type&:(= ?type ?sType)))
    =>
    (printout t ?data-fact)
    (retract ?search)
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
            	"view 1 berarti run defrule viewHouseWithGarage"
            	(bind ?*withGarageCount* 0)
                (assert (view 1))
            	(run)
            	(printout t "============================================" crlf)
            	(bind ?choice 0)
            	(printout t "Press Enter to Continue...")
            	(readline)
            
	        elif (eq ?choice 2) then
	        	(printout t "House Without Garage" crlf)
            	"view 0 berarti run defrule viewHouseNoGarage"
            	(bind ?*noGarageCount* 0)
	            (assert (view 0))
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
    
    (bind ?id (++ ?*withGarageID*))
    (assert(houseWithGarage (id ?id)(type ?type)(room ?room)(price ?price)(location ?location)(garage ?garage)))
    
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
    
    (bind ?id (++ ?*noGarageID*))
    (assert(houseNoGarage (id ?id)(type ?type)(room ?room)(price ?price)(location ?location)))
    
)

(deffunction addHouse()
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

(deffunction updateHouseWithGarage()
    (bind ?*withGarageCount* 0)
    (assert (view 1))
    (run)
        
    (bind ?flagChoice FALSE)
    (bind ?idx -1)
    
    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be updated [ 1.." ?*withGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (or (< ?idx 0) (> ?idx ?*withGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (eq ?idx 0) then
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
                
                "updateWithGarage"
                
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
                
                (bind ?*withGarageID* (+ ?*withGarageID* 1))
                (bind ?idx (- ?*withGarageID* ?idx))
			    (assert (modify ?idx 1))
                (run)
                (bind ?*withGarageID* (- ?*withGarageID* 1))
            )
                    
       	else
            (bind ?flagChoice FALSE)
        )
    )
)

(deffunction updateHouseNoGarage()
    (bind ?*noGarageCount* 0)
	(assert (view 0))
    (run)
        
    (bind ?flagChoice FALSE)
    (bind ?idx -1)

    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be updated [ 1.." ?*noGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*noGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (eq ?idx 0) then
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
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
			
			    (bind ?*noGarageID* (+ ?*noGarageID* 1))
                (bind ?idx (- ?*noGarageID* ?idx))
			    (assert (modify ?idx))
                (run)
                (bind ?*noGarageID* (- ?*noGarageID* 1))
            )
                    
        else
            (bind ?flagChoice FALSE)
        )    
    )
)

(deffunction deleteHouseWithGarage()
    (bind ?*withGarageCount* 0)
	(assert (view 1))
    (run)
        
    (bind ?flagChoice FALSE)
    (bind ?idx -1)

    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be deleted [ 1.. " ?*withGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*withGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (= ?idx 0) then
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
                (bind ?withGarageIDToken (+ ?*withGarageID* 1))
                (bind ?idx (- ?withGarageIDToken ?idx))
			    (assert (delete ?idx ?withGarageIDToken 1))
                (run)
            )   
        else
            (bind ?flagChoice FALSE)

        )    
    )
)

(deffunction deleteHouseNoGarage()
    (bind ?*noGarageCount* 0)
	(assert (view 0))
    (run)
        
    (bind ?flagChoice FALSE)
    (bind ?idx -1)

    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be deleted [ 1.. "?*noGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*noGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (= ?idx 0) then
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
    			(bind ?*noGarageID* (+ ?*noGarageID* 1))
                (bind ?idx (- ?*noGarageID* ?idx))
			    (assert (delete ?idx))
                (run)
                (bind ?*noGarageID* (- ?*noGarageID* 1))
            )      
        else
            (bind ?flagChoice FALSE)

        )    
    )
)

(deffunction updateHouse()
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

(deffunction deleteHouse()
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

(deffunction searchHouse()
    "not yet finished"
    (bind ?sName "")
    (bind ?sGender "")
    (bind ?sPreferences "")
    (bind ?sIncome -1)
    (bind ?sLocation "")
    (bind ?sType "")
    (bind ?sCar -1)
    
    "input name"
    (bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        (printout t "Input your name [3..20]: ")
        (bind ?sName (readline))
	    (if(or (< (str-length ?sName) 3) (> (str-length ?sName) 20)) then
	       (bind ?flagChoice FALSE)
	    else
	       (bind ?flagChoice TRUE)
	    )
    )
    
    "input gender"
    (while (and (neq ?sGender "Male")
            (neq ?sGender "Female"))
		(printout t "Input your gender [Male | Female] (CASE-SENSITIVE): ")
        (bind ?sGender (readline))
    )
    
    "input search preferences"
    (while (and (neq ?sPreferences "With Garage")
            (neq ?sPreferences "Without Garage"))
		(printout t "Input house preferences [With Garage | Without Garage] (CASE-SENSITIVE): ")
        (bind ?sPreferences (readline))
    )
    
    "input user income"
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input your income [10000 - 500000] (dollars): ")
		(bind ?sIncome (read))
        
        ;validasi income tipe adalah angka
        (if(eq (numberp ?sIncome) TRUE) then
            
            ;validasi income harus 10000...500000 dollars
        	(if(or (< ?sIncome 10000) (> ?sIncome 500000)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    "input search house location"
    (bind ?sLocation "")
    
    (while (and (neq ?sLocation "West Jakarta")
            (neq ?sLocation "North Jakarta")
            (neq ?sLocation "South Jakarta"))
		(printout t "Input your work location [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
        (bind ?sLocation (readline))
    )
    
    "input search house type"
    (while (and (neq ?sType "Cottage")
            (neq ?sType "Light House")
            (neq ?sType "Skyscraper"))
		(printout t "Input your preffered house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?sType (readline))
    )
    
    "input car numbers"
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input number of car you own [1 - 5]: ")
		(bind ?sCar (read))
        
        "validasi car numbers tipe adalah angka"
        (if(eq (numberp ?sCar) TRUE) then
            
            "validasi car numbers harus 1...5"
        	(if(or (< ?sCar 1) (> ?sCar 5)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    (assert (search ?sPreferences ?sIncome ?sLocation ?sType))
    (run)
)

(deffacts house
    "intentionally shuffled"
	(houseNoGarage (id 1)(type "Cottage") (room 3) (price 7500) (location "South Jakarta"))
	(houseNoGarage (id 2)(type "Light House") (room 3) (price 25000) (location "South Jakarta"))
    (houseWithGarage (id 1)(type "Cottage") (room 3) (price 4500) (location "North Jakarta") (garage 1)) 
	(houseWithGarage (id 2)(type "Skyscraper") (room 5) (price 175000) (location "South Jakarta") (garage 3))
	(houseNoGarage (id 3)(type "Skyscraper") (room 4) (price 100000) (location "West Jakarta")) 
	(houseNoGarage (id 4)(type "Cottage") (room 2) (price 5000) (location "North Jakarta")) 
	(houseWithGarage (id 3)(type "Cottage") (room 3) (price 30000) (location "West Jakarta") (garage 2)) 
    (houseNoGarage (id 5)(type "Light House") (room 3) (price 10000) (location "West Jakarta"))  
	(houseWithGarage (id 4)(type "Light House") (room 2) (price 7500) (location "South Jakarta") (garage 2)) 
	(houseWithGarage (id 5)(type "Light House") (room 4) (price 7500) (location "West Jakarta") (garage 1))    
)

(reset)

;inisialisasi choice menu
(bind ?choice 0)

(while (neq ?choice 6)
    (menu)
    (facts)

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
        	(searchHouse)
    )
    
    (clearScreen)
    
)