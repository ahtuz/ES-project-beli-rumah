(defglobal 
    ?*withGarageCount* = 0
    ?*noGarageCount* = 0
    ?*withGarageID* = 5
    ?*noGarageID* = 5
)

; withGarageCount & noGarageCount is for the view house rule
; withGarageID & noGarageID starts from 5 because there are 5 initial facts

; NOTE:
; soalnya agak ambigu pada bagian menu SEARCH
; pada contoh diberikan 2T & 1F -> TRUE
; bagaimana dengan 1T & 2F? apakah dianggap TRUE juga?
; oleh karena itu, kami anggap SETIDAKNYA ada 1 KRITERIA saja yang cocok,
; maka house akan dimasukkan ke hasil search house yang valid dan akan dihitung match rate nya

; ada sedikit error dari jess:
; bila modify (update) suatu data, maka data tersebut otomatis menjadi facts terakhir
; sehingga bila di-view setelah modify akan muncul jadi data yang paling pertama
; hal tersebut bisa mempengaruhi saat kita ingin delete/update data untuk selanjutnya

(deftemplate houseWithGarage
    "template house with garage"
    (slot id)
    (slot type)
    (slot room)
    (slot price)
    (slot location)
    (slot garage)
)

(deftemplate houseNoGarage
    "template house without garage"
    (slot id)
    (slot type)
    (slot room)
    (slot price)
    (slot location)
)

(deftemplate houseValidSearch
    "template for containing searched house with garage"
    (slot type)
    (slot roomNumber)
    (slot price)
    (slot location)
    (slot number)
    (slot match-rate)
)

(deftemplate userSearchInfo
    "template for containing user preferred with garage house provided information"
    (slot name)
    (slot gender)
    (slot interest)
    (slot type)
    (slot income)
    (slot location)
    (slot carCount)
)

(defquery getValidHouse
    "untuk query house info"
    (houseValidSearch (type ?type) (roomNumber ?roomNumber) (price ?price) (location ?location) (number ?number) (match-rate ?match-rate))
)

(defquery getUserInfo
    "untuk query user inputted info"
	(userSearchInfo (name ?name)(gender ?gender)(interest ?interest)(location ?location)(income ?income)(type ?type)(carCount ?carCount))
)

(defrule viewHouseWithGarage
    "rule for view house with garage"
    (view 1)
    (houseWithGarage (type ?type)(room ?room)(price ?price)(location ?loc)(garage ?garage))
    =>
    (printout t "| ")
    (bind ?*withGarageCount* (++ ?*withGarageCount*))
    (printout t ?*withGarageCount* " . | ")
    (format t %-14s (str-cat ?type))
    (printout t " |  " ?room "   | ")
    (format t %-10s (str-cat ?price " $ "))
    (printout t "| ")
    (format t %-15s (str-cat ?loc))
    (printout t "|    " ?garage "   |" crlf)
)

(defrule viewHouseNoGarage
    "rule for view house without garage"
    (view 0)
    (houseNoGarage (type ?type)(room ?room)(price ?price)(location ?loc))
    =>
    (printout t "| ")
    (bind ?*noGarageCount* (++ ?*noGarageCount*))
    (printout t ?*noGarageCount* " . | ")
    (format t %-14s (str-cat ?type))
    (printout t " |  " ?room "   | ")
    (format t %-10s (str-cat ?price " $ "))
    (printout t "| ")
    (format t %-15s (str-cat ?loc))
    (printout t "|" crlf)
)

(defrule retractView
    "rule used for delete fact view"
    ?view <- (view ?i)
    =>
    (retract ?view)
)

(defrule updateWithGarage
    "rule to update house with garage"
	?modify <- (modify ?idx 1)
    ?data-fact <- (houseWithGarage (id ?idx))
    =>
    (modify ?data-fact (id ?idx)(type ?utype)(room ?uroom)(price ?uprice)(location ?uloc)(garage ?ugarage))
    (retract ?modify)
)

(defrule updateNoGarage
    "rule to update house without garage"
	?modify <- (modify ?idx)
    ?data-fact <- (houseNoGarage (id ?idx))
    =>
    (modify ?data-fact (id ?idx)(type ?utype)(room ?uroom)(price ?uprice)(location ?uloc))
    (retract ?modify)
)

(defrule reassignHouseWithGarage
    "rule to assign new id value for other house (with garage) after delete"
    ?reassign <- (reassign ?nextIdx 1)
    ?next-fact <- (houseWithGarage (id ?nextIdx))
    =>
    (bind ?newIdx (- ?nextIdx 1))
    (modify ?next-fact (id ?newIdx))
    (retract ?reassign)
)

(defrule reassignHouseNoGarage
    "rule to assign new id value for other house (without garage) after delete"
    ?reassign <- (reassign ?nextIdx)
    ?next-fact <- (houseNoGarage (id ?nextIdx))
    =>
    ; assign idx dari idx setelah house yang didelete dikurangi 1.
    ; e.g. deleted idx = 3, next idx from delete idx = 4, assign next idx menjadi 3.
    (bind ?newIdx (- ?nextIdx 1))
    (modify ?next-fact (id ?newIdx))
    (retract ?reassign)
)

(defrule deleteWithGarage
    "rule to delete desired house with garage from the index"
	?delete <- (delete ?delIdx ?nextIdx 1)
    ?del-fact <- (houseWithGarage (id ?delIdx))
    =>
    (retract ?del-fact)
    (retract ?delete)

    (while (<= ?nextIdx ?*withGarageID*)
        ; reassign akan dilakukan sampai nextIdx = jumlah data house with garage
        (assert (reassign ?nextIdx 1))
        (run)
		(bind ?nextIdx (+ ?nextIdx 1))
    )
    
    (bind ?*withGarageID* (- ?*withGarageID* 1))
)

(defrule deleteNoGarage
    "rule to delete desired house without garage from the index"
	?delete <- (delete ?delIdx ?nextIdx)
    ?del-fact <- (houseNoGarage (id ?delIdx))
    =>
    (retract ?del-fact)
    (retract ?delete)
    
    (while (<= ?nextIdx ?*noGarageID*)
        (assert (reassign ?nextIdx))
        (run)
		(bind ?nextIdx (+ ?nextIdx 1))
    )
    
    (bind ?*noGarageID* (- ?*noGarageID* 1))
)

(defrule retractSearch
	?search <- (search ?sPreferences ?sIncome ?sLocation ?sType)
    =>
    (retract ?search)    
)

(defrule searchWithGarage
    "rule to search house with garage"
	?search <- (search "With Garage" ?sIncome ?sLocation ?sType ?sCar)
    ?data-fact <- (houseWithGarage (price ?price) (location ?location) (type ?type)(room ?roomNumber)(garage ?garage))
    =>
    ; untuk mengecek validitas setiap house dibandingkan dengan user input
    ; validity 3 artinya masih TRUE semua
    ; validity akan dikurangi 1 setiap ada 1 FALSE
    ; tempRate untuk counter match rate
    
    (bind ?flagValidity 3)
    (bind ?tempRate 100)
    
    ; cek dahulu apakah masuk kriteria
    (if(< ?sIncome ?price) then
        (bind ?flagValidity (- ?flagValidity 1))
    )
    (if(neq ?location ?sLocation) then
        (bind ?flagValidity (-- ?flagValidity))
    )
    (if(neq ?type ?sType) then
        (bind ?flagValidity (-- ?flagValidity))
    )
    
    ; menghitung match rate untuk data search yang valid (validity >= 1)
    (if (>= ?flagValidity 1) then
        (if(< ?sIncome ?price) then
            (bind ?tempRate (- ?tempRate 10))
        )
        (if(neq ?location ?sLocation) then
            (bind ?tempRate (- ?tempRate 10))
        )
        (if(neq ?type ?sType) then
            (bind ?tempRate (- ?tempRate 5))
        )
        (if(< ?garage ?sCar) then
            (bind ?tempRate (- ?tempRate 10))
        )
        
        (assert (houseValidSearch(price ?price) (location ?location) (type ?type) (roomNumber ?roomNumber)(number ?garage)(match-rate ?tempRate)))
    )
)

(defrule searchNoGarage
    "rule to search house with garage"
	?search <- (search "Without Garage" ?sIncome ?sLocation ?sType ?sCar)
    ?data-fact <- (houseNoGarage (price ?price) (location ?location) (type ?type) (room ?roomNumber))
    =>
    ; untuk mengecek validitas setiap house dibandingkan dengan user input
    ; validity 3 artinya masih TRUE semua
    ; validity akan dikurangi 1 setiap ada 1 FALSE
    ; tempRate untuk counter match rate
    
    (bind ?flagValidity 3)
    (bind ?tempRate 100)
    
    ; cek dahulu apakah masuk kriteria
    (if(< ?sIncome ?price) then
        (bind ?flagValidity (- ?flagValidity 1))
    )
    (if(neq ?location ?sLocation) then
        (bind ?flagValidity (-- ?flagValidity))
    )
    (if(neq ?type ?sType) then
        (bind ?flagValidity (-- ?flagValidity))
    )
    
    ; menghitung match rate untuk data search yang valid (validity >= 1)
    (if (>= ?flagValidity 1) then
        (if(< ?sIncome ?price) then
            (bind ?tempRate (- ?tempRate 10))
        )
        (if(neq ?location ?sLocation) then
            (bind ?tempRate (- ?tempRate 10))
        )
        (if(neq ?type ?sType) then
            (bind ?tempRate (- ?tempRate 5))
        )
        
        (assert (houseValidSearch(price ?price) (location ?location) (type ?type)(roomNumber ?roomNumber)(number 0)(match-rate ?tempRate)))
    )
)

(deffunction menu()
    "display menu"
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
    "function untuk clear screen"
    (for (bind ?x 0)(< ?x 10)(++ ?x)
        (printout t crlf)
    )
)

(deffunction viewHouse()
    "view house menu"
    (printout t "List of house to be viewed" crlf)
    (printout t "============================================" crlf)
    (printout t "1. House with Garage" crlf)
    (printout t "2. House without Garage" crlf)
    
    "inisialisasi viewHouse choice"
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
            	(printout t crlf "House With Garage" crlf)
            	; view 1 berarti run defrule viewHouseWithGarage
                (printout t "=====================================================================" crlf)
            	(printout t "| No. |   House Type   | Room |  Price    |    Location    | Garage |" crlf)
            	(printout t "=====================================================================" crlf)
            	(bind ?*withGarageCount* 0)
                (assert (view 1))
            	(run)
            	(printout t "=====================================================================" crlf)
            	(bind ?choice 0)
            	(printout t "Press Enter to Continue...")
            	(readline)
            
	        elif (eq ?choice 2) then
	        	(printout t crlf "House Without Garage" crlf)
            	; view 0 berarti run defrule viewHouseNoGarage
            	(printout t "============================================================" crlf)
            	(printout t "| No. |   House Type   | Room |  Price    |    Location    |" crlf)
            	(printout t "============================================================" crlf)
            	(bind ?*noGarageCount* 0)
	            (assert (view 0))
                (run)
            	(printout t "============================================================" crlf)
	        	; agar bisa keluar dari looping awal
	            (bind ?choice 0)
                (printout t "Press Enter to Continue...")
            	(readline)
        )
    )
)

(deffunction addHouseWithGarage()
    
    ; insert house type
    (bind ?type "")
    
    (while (and (neq ?type "Cottage")
            (neq ?type "Light House")
            (neq ?type "Skyscraper"))
		(printout t "Input house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?type (readline))
    )
    
    ; insert room number
    (bind ?room 0)
    
    (while (or (< ?room 1)
            (> ?room 5))
        (printout t "Input room number [1 - 5]: ")
        (bind ?room (read))
    )
    
    ; insert house price
    (bind ?price 0)
    
    ; inisialisasi flag validasi house price
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input house price [1000 - 500000] (dollars): ")
		(bind ?price (read))
        
        ; validasi price tipe adalah angka
        (if(eq (numberp ?price) TRUE) then
            
            ; validasi price harus 1000...500000 dollars
        	(if(or (< ?price 1000) (> ?price 500000)) then
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
    
    ; add jumlah house untuk house with garage
    ; assign id house baru dengan id terakhir
    (bind ?*withGarageID* (+ ?*withGarageID* 1))
    (bind ?id ?*withGarageID*)
    (assert(houseWithGarage (id ?id)(type ?type)(room ?room)(price ?price)(location ?location)(garage ?garage)))
    
)

(deffunction addHouseNoGarage()
    
    ; insert house type
    (bind ?type "")
    
    (while (and (neq ?type "Cottage")
            (neq ?type "Light House")
            (neq ?type "Skyscraper"))
		(printout t "Input house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?type (readline))
    )
    
    ; insert room number
    (bind ?room 0)
    
    (while (or (< ?room 1)
            (> ?room 5))
        (printout t "Input room number [1 - 5]: ")
        (bind ?room (read))
    )
    
    ; insert house price
    (bind ?price 0)
    
    ; inisialisasi flag validasi house price
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input house price [1000 - 500000] (dollars): ")
		(bind ?price (read))
        
        ; validasi price tipe adalah angka
        (if(eq (numberp ?price) TRUE) then
            
            ; validasi price harus 1000...500000 dollars
        	(if(or (< ?price 1000) (> ?price 500000)) then
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
    
    ; add jumlah house untuk house without garage
    ; assign id house baru dengan id terakhir
    (bind ?*noGarageID* (+ ?*noGarageID* 1))
    (bind ?id ?*noGarageID*)
    (assert(houseNoGarage (id ?id)(type ?type)(room ?room)(price ?price)(location ?location)))
    
)

(deffunction addHouse()
    "menu utama untuk add house"
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
	        
	        ; validasi tipe data choice adalah angka
	        (if(eq (numberp ?choice) TRUE) then
	            
	            ; validasi choice harus 1...2
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
    "function update house with garage"
    (printout t "=====================================================================" crlf)
    (printout t "| No. |   House Type   | Room |  Price    |    Location    | Garage |" crlf)
    (printout t "=====================================================================" crlf)
    (bind ?*withGarageCount* 0)
    (assert (view 1))
    (run)
    (printout t "=====================================================================" crlf)
        
    (bind ?flagChoice FALSE)
    (bind ?idx -1)
    
    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be updated [ 1.." ?*withGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
        
        ; validasi index yang di-input user
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (or (< ?idx 0) (> ?idx ?*withGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (eq ?idx 0) then
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
                
                ; tanya user setiap variabel house dari index terpilih ingin diganti menjadi apa
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
                
                ; mengkonversi idx yang di-input menjadi id house sebenarnya
                ; hal ini diperlukan karena id house dengan idx tidak selaras / saling bertebalikan
                ; disebabkan function view menampilkan dari facts terakhir (bottom-up)  
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
    "function update house without garage"
    (printout t "============================================================" crlf)
    (printout t "| No. |   House Type   | Room |  Price    |    Location    |" crlf)
	(printout t "============================================================" crlf)
    (bind ?*noGarageCount* 0)
	(assert (view 0))
    (run)
    (printout t "============================================================" crlf)
        
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
    "function delete house with garage"
    (printout t "=====================================================================" crlf)
	(printout t "| No. |   House Type   | Room |  Price    |    Location    | Garage |" crlf)
	(printout t "=====================================================================" crlf)
    (bind ?*withGarageCount* 0)
	(assert (view 1))
    (run)
    (printout t "=====================================================================" crlf)
    
    ; inisialisasi flag untuk validasi index input
    (bind ?flagChoice FALSE)
    (bind ?idx -1)

    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be deleted [ 1.." ?*withGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*withGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (= ?idx 0) then
                ; index 0, langsung kembali ke main menu
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
                
                ; jika index benar, langsung delete indexnya
                ; karena nomor pada view dan index berbeda, jadi idx input user harus dikonversi dulu
                ; nextIdx diperlukan untuk rule reassign index lain setelah index input di-delete
                (bind ?withGarageIDToken (+ ?*withGarageID* 1))
                (bind ?delIdx (- ?withGarageIDToken ?idx))
                (bind ?nextIdx (+ ?delIdx 1))
			    (assert (delete ?delIdx ?nextIdx 1))
                (run)
            )   
        else
            (bind ?flagChoice FALSE)

        )    
    )
)

(deffunction deleteHouseNoGarage()
    (printout t "============================================================" crlf)
    (printout t "| No. |   House Type   | Room |  Price    |    Location    |" crlf)
	(printout t "============================================================" crlf)
    (bind ?*noGarageCount* 0)
	(assert (view 0))
    (run)
    (printout t "============================================================" crlf)
        
    (bind ?flagChoice FALSE)
    (bind ?idx -1)

    (while(eq ?flagChoice FALSE)

        (printout t "Which house to be deleted [ 1.."?*noGarageCount* " | 0 to back to main menu ]: ") 
        (bind ?idx (read))
                
        (if(eq (numberp ?idx) TRUE) then
                    
            (if (and (< ?idx 0) (> ?idx ?*noGarageCount* )) then 
                (bind ?flagChoice FALSE)
            elif (= ?idx 0) then
                (bind ?flagChoice TRUE)
            else
                (bind ?flagChoice TRUE)
                (bind ?noGarageIDToken (+ ?*noGarageID* 1))
                (bind ?delIdx (- ?noGarageIDToken ?idx))
                (bind ?nextIdx (+ ?delIdx 1))
			    (assert (delete ?delIdx ?nextIdx))
                (run)
            )      
        else
            (bind ?flagChoice FALSE)

        )    
    )
)

(deffunction updateHouse()
    "function utama update house"
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
    "function utama delete house"
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
    "function utama search house"
    (bind ?sName "")
    (bind ?sGender "")
    (bind ?sPreferences "")
    (bind ?sIncome -1)
    (bind ?sLocation "")
    (bind ?sType "")
    (bind ?sCar -1)
    
    ; input name
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
    
    ; input gender
    (while (and (neq ?sGender "Male")
            (neq ?sGender "Female"))
		(printout t "Input your gender [Male | Female] (CASE-SENSITIVE): ")
        (bind ?sGender (readline))
    )
    
    ; input search preferences
    (while (and (neq ?sPreferences "With Garage")
            (neq ?sPreferences "Without Garage"))
		(printout t "Input house preferences [With Garage | Without Garage] (CASE-SENSITIVE): ")
        (bind ?sPreferences (readline))
    )
    
    ; input user income
	(bind ?flagChoice FALSE)

    (while(eq ?flagChoice FALSE)
        
        (printout t "Input your income [10000 - 500000] (dollars): ")
		(bind ?sIncome (read))
        
        ; validasi income tipe adalah angka
        (if(eq (numberp ?sIncome) TRUE) then
            
            ; validasi income harus 10000...500000 dollars
        	(if(or (< ?sIncome 10000) (> ?sIncome 500000)) then
            	(bind ?flagChoice FALSE)
        	else
        		(bind ?flagChoice TRUE)
        	)
            
        else
        	(bind ?flagChoice FALSE)
    	)
    )
    
    ; input search house location
    (bind ?sLocation "")
    
    (while (and (neq ?sLocation "West Jakarta")
            (neq ?sLocation "North Jakarta")
            (neq ?sLocation "South Jakarta"))
		(printout t "Input your work location [West Jakarta | North Jakarta | South Jakarta] (CASE-SENSITIVE): ")
        (bind ?sLocation (readline))
    )
    
    ; input search house type
    (while (and (neq ?sType "Cottage")
            (neq ?sType "Light House")
            (neq ?sType "Skyscraper"))
		(printout t "Input your preffered house type [Cottage | Light House | Skyscraper] (CASE-SENSITIVE): ")
        (bind ?sType (readline))
    )
    
    ; menanyakan input jumlah mobil jika preferensi user adalah with garage
    (if (eq ?sPreferences "With Garage") then
	; input car numbers
		(bind ?flagChoice FALSE)
	
	    (while(eq ?flagChoice FALSE)
	        
	        (printout t "Input number of car you own [1 - 5]: ")
			(bind ?sCar (read))
	        
	        ; validasi car numbers adalah angka
	        (if(eq (numberp ?sCar) TRUE) then
	            
	            ; validasi car numbers harus 1...5
	        	(if(or (< ?sCar 1) (> ?sCar 5)) then
	            	(bind ?flagChoice FALSE)
	        	else
	        		(bind ?flagChoice TRUE)
	        	)
	            
	        else
	        	(bind ?flagChoice FALSE)
	    	)
	    )
        
        ; insert data with garage yang ada
        ; mereset countValidity jadi 0 kembali
	    (bind ?*countValidity* 0)
	    
	    ; masukkan ke dalam template user info with garage
	    (assert (userSearchInfo(name ?sName)(gender ?sGender)(interest ?sPreferences)(income ?sIncome)(location ?sLocation)(type ?sType)(carCount ?sCar)))
        
	    ; masukkan ke dalam defrule search house sesuai dengan preferensi (interest) with garage
	    (assert (search ?sPreferences ?sIncome ?sLocation ?sType ?sCar))
    	(run)
        
        else (
            ; insert data without garage yang ada
            ; mereset countValidity jadi 0 kembali
		    (bind ?*countValidity* 0)
		    
		    ; masukkan ke dalam template user info without garage (artinya car = 0)
		    (assert (userSearchInfo (name ?sName)(gender ?sGender)(interest ?sPreferences)(income ?sIncome)(location ?sLocation)(type ?sType)(carCount 0)))
            
		    ; masukkan ke dalam defrule search house sesuai dengan preferensi (interest) without garage
		    (assert (search ?sPreferences ?sIncome ?sLocation ?sType))
    		(run)
        )
    )
)

(deffacts house
    "initial data"
    (houseWithGarage (id 1)(type "Cottage") (room 3) (price 4500) (location "North Jakarta") (garage 1)) 
	(houseWithGarage (id 2)(type "Skyscraper") (room 5) (price 175000) (location "South Jakarta") (garage 3))
    (houseWithGarage (id 3)(type "Cottage") (room 3) (price 30000) (location "West Jakarta") (garage 2)) 
	(houseWithGarage (id 4)(type "Light House") (room 2) (price 7500) (location "South Jakarta") (garage 2)) 
	(houseWithGarage (id 5)(type "Light House") (room 4) (price 7500) (location "West Jakarta") (garage 1)) 
    (houseNoGarage (id 1)(type "Cottage") (room 3) (price 7500) (location "South Jakarta"))
	(houseNoGarage (id 2)(type "Light House") (room 3) (price 25000) (location "South Jakarta"))
	(houseNoGarage (id 3)(type "Skyscraper") (room 4) (price 100000) (location "West Jakarta")) 
	(houseNoGarage (id 4)(type "Cottage") (room 2) (price 5000) (location "North Jakarta")) 
    (houseNoGarage (id 5)(type "Light House") (room 3) (price 10000) (location "West Jakarta")) 
)

(reset)

;inisialisasi choice menu
(bind ?choice 0)

(while (neq ?choice 6)
    (menu)

    ; inisialisasi flag validasi choice
    (bind ?flagChoice FALSE)
    
    (while(eq ?flagChoice FALSE)
        
        (printout t ">> Input [1-6]: ")
		(bind ?choice (read))
        
        ; validasi choice tipe adalah angka
        (if(eq (numberp ?choice) TRUE) then
            
            ; validasi choice harus 1...6
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
        	(new Template)
    )
    
    (clearScreen)
    
)