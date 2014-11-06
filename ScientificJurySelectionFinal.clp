;**************************************************
; file: ScientificJurySelection.clp
; description: An expert system that uses social science techniques  
; and expertise to choose 
; favorable jurors during a criminal or civil trial.
;********************************************

;*******************
;DEFTEMPLATES
;*******************

;TEMPLATE: The characteristics fact specifies the options you can input
;we left possible ones commented out incase we want to add them in at a later date
(deftemplate characteristics
   	(slot gender)    ; gender
   	(slot income)     ; income category
	(slot education) ; education level
	(slot religion) ; religion practiced
	(slot sexual_orientation) ; which side they like
	(slot occupation) ; working society class
	(slot lifestyle) ; how they live
	(slot age) ; which age category
	(slot marital_status) ; state recognized marital status
	(slot living_situation) ; housing situation
	;(slot siblings) ; brothers, sisters, & how many
	;(slot smokers) ; smoke or not
	;(slot transportation) ; vehicle you own
	;(slot clothing) ;category 
	;(slot size_of_family) ; size of your family
	;(slot pets) ; pets & how many 
	;(slot criminal_record) ; history of past charges
	;(slot car_type) ; type of car you drive
	;(slot body) ; body type
)

;TEMPLATE: How many potential jurors
(deftemplate option
    (slot number)	; How many potential jurors
)

;****************************
;DEFGLOBALS
;****************************

;GLOBAL VARIABLE: help track the inputed data throughout for the weighted value
(defglobal ?*gray-code* = 0)
(defglobal ?*tally-code* = 0)
(defglobal ?*presentage-code* = 0)

;*******************
;DEFFACTS
;*******************

;FACT: init_facts will be the first fact always
(deffacts init_facts
	(task user_prompt)	; this will trigger menu to be printed
)

;*********************
;RULES
;********************

;RULE: jurors is the first prompt you will see asking how many potential jurors
(defrule jurors
	?f1 <- (task user_prompt)
	
=>
	(printout t "What characteristics do you want:" crlf)
	(printout t "#1 Gender:" crlf)
	(printout t "#2 Income:" crlf)
	(printout t "#3 Education:" crlf)
	(printout t "#4 Religion:" crlf)
	(printout t "#5 Sexual Orientation:" crlf)
	(printout t "#6 Occupation:" crlf)
	(printout t "#7 Lifestyle:" crlf)
	(printout t "#8 Age:" crlf)
	(printout t "#9 Material Status:" crlf)
	(printout t "#10 Living Situation:" crlf)
	(printout t "#11 No more characteristics:" crlf)
	(assert (option (number (read))))	; stores the user's selection
	(assert (task need_to_check))     ;asserts the validity check
	(retract ?f1)
)

;RULE: check to see if inputed juror option makes sense
(defrule check_juror_option_validity
	?f1 <- (task need_to_check)
	?f2 <- (option (number ?input))
	(test (or (not (integerp ?input)) ;default in clips to check if integer or not: integerp
		(< ?input 1)
		(> ?input 11)
	))
=>
	(printout t crlf  crlf crlf "Invalid ammount of potential Jurors. Please try again."  crlf crlf)
	(retract ?f1)
	(retract ?f2)
	(assert (task user_prompt)); brings you back to user prompt if not a valid option
)

;RULE: end-program
(defrule end-program ;rule to end program from the prompt
	(option (number 11))
=>
(bind ?percentage (/ ?*tally-code* ?*gray-code*)) ; does the statistical straight-line percentage
	 
	
	
	(printout t "The likelyhood of this person voting guilty (closer to 0) or innocent(closer to 100) " ?percentage crlf);prints out the likelyhood of the verdict to the user

	(printout t "Ending" crlf)
	(halt) ;ends the program again
)

;RULE: ask-prompt-again
(defrule ask-prompt-again ;allows for loop of the characteristics
	?f1 <- (task ask-again) ; sets the task as a fact
=>
	(printout t "Are there more characteristics you need to list \(y for yes, n for no\)?" crlf) ;asks if there are more transactions requires y for yes n for no
	(bind ?answer (read)) ; sets the value to answer
	(assert (ask-again ?answer)) ; sets the answer as a fact
	(retract ?f1)
)

;RULE: end-program
(defrule end-program1 ;rule to end program from ask again
	(ask-again n)
=>
	(bind ?percentage (/ ?*tally-code* ?*gray-code*)) ; does the statistical analysis
	 
	
	
	(printout t "The likelyhood of this person voting guilty (closer to 0) or innocent(closer to 100) " ?percentage crlf); prints out to the user the likelyhood of the verdict
	(halt); ends the program
)

;RULE: redo-program
(defrule redo-program ;rule to ask the main menu again
	?f80<- (ask-again y); if ask again is yes then this fires

=>

	(retract ?f80) ;retracts the rule
	(assert (task user_prompt)) ; fires the menu
)

;RULE: gender
(defrule gender ;rule to add statistical data for gender
	(option (number 1)) ; fires if the user selects option 1
=>
	(printout t "Please enter the gender:" crlf); prints out to the user the gender menu
	(printout t "#1 Male:" crlf)
	(printout t "#2 Female:" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (gender ?answer)))	; stores the user's selection
)

;RULE: male
(defrule male ;rule to to add the statistical analysis for the male classification
	(characteristics (gender 1))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 80))
	(assert (task ask-again))
)

;RULE: female
(defrule female ;rule to to add the statistical analysis for the female classification
	(characteristics (gender 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 40))
	(assert (task ask-again))
)

;RULE: income
(defrule income ;prints out to the user the income level menu
	(option (number 2))
=>
	(printout t "Please enter the income:" crlf); prints out to the user the income level menu
	(printout t "#1 lower:" crlf)
	(printout t "#2 middle:" crlf)
	(printout t "#3 upper:" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (income ?answer)))	; stores the user's selection
	 
)

;RULE: lower-income
(defrule lower-income ;rule to to add the statistical analysis for the low income classification
	(characteristics (income 1))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 95))
	(assert (task ask-again))
)

;RULE: middle-income
(defrule middle-income ;rule to to add the statistical analysis for the middle income classification
	(characteristics (income 2))
=>
	 
	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 40))
	 
	(assert (task ask-again))
)

;RULE: upper-income
(defrule upper-income ;rule to to add the statistical analysis for the upper income classification
	(characteristics (income 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 5))
	(assert (task ask-again))
)

;RULE: education
(defrule education ; prints out to the user the education level menu
	(option (number 3))
=>
	(printout t "Please enter the education level:" crlf)
	(printout t "#1 some high school:" crlf)
	(printout t "#2 high school or equivalant:" crlf)
	(printout t "#3 some college:" crlf)
	(printout t "#4 associates degree:" crlf)
	(printout t "#5 bachlors degree:" crlf)
	(printout t "#6 masters degree:" crlf)
	(printout t "#7 certification of advanced study:" crlf)
	(printout t "#8 doctorate:" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (education ?answer)))	; stores the user's selection
)

;RULE: some-high-school
(defrule some-high-school ;rule to to add the statistical analysis for the some high school classification
	(characteristics (education 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 100))
	(assert (task ask-again))
)

;RULE: high-school
(defrule high-school ;rule to to add the statistical analysis for the high school classification
	(characteristics (education 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 87.6))
	(assert (task ask-again))
)

;RULE: some-college
(defrule some-college ;rule to to add the statistical analysis for the some college classification
	(characteristics (education 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 75.2))
	(assert (task ask-again))
)

;RULE: associates-degree
(defrule associates-degree ;rule to to add the statistical analysis for the associates classification
	(characteristics (education 4))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 62.8))
	(assert (task ask-again))
)

;RULE: bachlors-degree
(defrule bachlors-degree ;rule to to add the statistical analysis for the bacholors degree classification
	(characteristics (education 5))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50.4))
	(assert (task ask-again))
)

;RULE: masters-degree
(defrule masters-degree ;rule to to add the statistical analysis for the masters degree classification
	(characteristics (education 6))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 38))
	(assert (task ask-again))
)

;RULE: advanced-study
(defrule advanced-study ;rule to to add the statistical analysis for the advanced study classification
	(characteristics (education 7))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 25.6))
	(assert (task ask-again))
)

;RULE: doctorate
(defrule doctorate ;rule to to add the statistical analysis for the doctorate classification
	(characteristics (education 8))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 13.2))
	(assert (task ask-again))
)

;RULE: religion
(defrule religion ; prints out to the user the religious affiliation menu
	(option (number 4))
=>
	(printout t "Please enter the religious affiliation:" crlf)
	(printout t "#1 Christianity:" crlf)
	(printout t "#2 Islam:" crlf)
	(printout t "#3 Unaffiliated:" crlf)
	(printout t "#4 Hinduism:" crlf)
	(printout t "#5 Buddhism:" crlf)
	(printout t "#6 Folk religions:" crlf)
	(printout t "#7 Other:" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (religion ?answer)))	; stores the user's selection
)

;RULE: christianity:
(defrule christianity ;rule to to add the statistical analysis for the christianity classification
	(characteristics (religion 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: islam
(defrule islam ;rule to to add the statistical analysis for the islam classification
	(characteristics (religion 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 30))
	(assert (task ask-again))
)

;RULE: unaffiliated
(defrule unaffiliated ;rule to to add the statistical analysis for the unaffiliated classification
	(characteristics (religion 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: hinduism
(defrule hinduism ;rule to to add the statistical analysis for the hinduism classification
	(characteristics (religion 4))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 14))
	(assert (task ask-again))
)

;RULE: buddhism
(defrule buddhism ;rule to to add the statistical analysis for the buddhism classification
	(characteristics (religion 5))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 75))
	(assert (task ask-again))
)

;RULE: folk
(defrule folk ;rule to to add the statistical analysis for the folk classification
	(characteristics (religion 6))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 43))
	(assert (task ask-again))
)

;RULE: other
(defrule other ;rule to to add the statistical analysis for the other classification
	(characteristics (religion 7))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: sexual-oriantation
(defrule sexual-oriantation ; prints out to the user the sexual oriantation menu
	(option (number 5))
=>
	(printout t "Please enter the sexual oriantation:" crlf)
	(printout t "#1 Straight" crlf)
	(printout t "#2 Gay:" crlf)
	(printout t "#3 Lesbian:" crlf)
	(printout t "#4 Bi-sexual:" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (sexual_orientation ?answer)))	; stores the user's selection
)

;RULE: straight:
(defrule straight ;rule to to add the statistical analysis for the straight classification
	(characteristics (sexual_orientation 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: gay
(defrule gay ;rule to to add the statistical analysis for the gay classification
	(characteristics (sexual_orientation 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: lesbian
(defrule lesbian ;rule to to add the statistical analysis for the lesbian classification
	(characteristics (sexual_orientation 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: bi-curious
(defrule bi-curious ;rule to to add the statistical analysis for the bi-curiouse classification
	(characteristics (sexual_orientation 4))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: occupation
(defrule occupation ; prints out to the user the occupation level menu
	(option (number 6))
=>
	(printout t "Please enter the Occupation:" crlf)
	(printout t "#1 White-collar" crlf)
	(printout t "#2 Blue-collar:" crlf)
	(printout t "#3 Farmer:" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (occupation ?answer)))	; stores the user's selection
)

;RULE: white-collar:
(defrule white-collar ;rule to to add the statistical analysis for the white collar classification
	(characteristics (occupation 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: blue-collar
(defrule blue-collar ;rule to to add the statistical analysis for the blue collar classification
	(characteristics (occupation 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: farmer
(defrule farmer ;rule to to add the statistical analysis for the farmer classification
	(characteristics (occupation 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: lifestyle
(defrule lifestyle ; prints out to the user the lifestyle option menu
	(option (number 7))
=>
	(printout t "Please enter the lifestyle:" crlf)
	(printout t "#1 Athletic" crlf)
	(printout t "#2 Business" crlf)
	(printout t "#3 Goth" crlf)
	(printout t "#4 Emo" crlf)
	(printout t "#5 Nerd" crlf)
	(printout t "#6 Overweight" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (lifestyle ?answer)))	; stores the user's selection
)

;RULE: athletic:
(defrule athletic ;rule to to add the statistical analysis for the athletic classification
	(characteristics (lifestyle 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: business
(defrule business ;rule to to add the statistical analysis for the business classification
	(characteristics (lifestyle 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: goth
(defrule goth ;rule to to add the statistical analysis for the goth classification
	(characteristics (lifestyle 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: emo
(defrule emo ;rule to to add the statistical analysis for the emo classification
	(characteristics (lifestyle 4))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: nerd
(defrule nerd ;rule to to add the statistical analysis for the nerd classification
	(characteristics (lifestyle 5))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: overweight
(defrule overweight ;rule to to add the statistical analysis for the overweight classification
	(characteristics (lifestyle 6))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: age
(defrule age ; prints out to the user the age group menu
	(option (number 8))
=>
	(printout t "Please enter the age group:" crlf)
	(printout t "#1 Ages 18-25" crlf)
	(printout t "#2 Ages 26-30" crlf)
	(printout t "#3 Ages 31-35" crlf)
	(printout t "#4 Ages 36-40" crlf)
	(printout t "#5 Ages 41-45" crlf)
	(printout t "#6 Ages 46-50" crlf)
	(printout t "#7 Ages 51-55" crlf)
	(printout t "#8 Ages 56-60" crlf)
	(printout t "#9 Ages 61-65" crlf)
	(printout t "#10 Ages 66-70" crlf)
	(printout t "#11 Ages 71-75" crlf)
	(printout t "#12 Ages 76-80" crlf)
	(printout t "#13 Ages 81-85" crlf)
	(printout t "#14 Ages 86-90" crlf)
	(printout t "#15 Ages 91-95" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (age ?answer)))	; stores the user's selection
)

;RULE: ages-18-25:
(defrule ages-18-25 ;rule to to add the statistical analysis for the ages 18-25 classification
	(characteristics (age 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: ages-26-30
(defrule ages-26-30 ;rule to to add the statistical analysis for the ages 26 - 30 classification
	(characteristics (age 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: ages-31-35
(defrule ages-31-35 ;rule to to add the statistical analysis for the ages 31-35 classification
	(characteristics (age 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-36-40
(defrule ages-36-40 ;rule to to add the statistical analysis for the ages 36-40 classification
	(characteristics (age 4))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-41-45
(defrule ages-41-45 ;rule to to add the statistical analysis for the ages 41-45 classification
	(characteristics (age 5))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-46-50
(defrule ages-46-50 ;rule to to add the statistical analysis for the ages 46-50 classification
	(characteristics (age 6))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-51-55
(defrule ages-51-55 ;rule to to add the statistical analysis for the ages 51-55 classification
	(characteristics (age 7))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-56-60
(defrule ages-56-60 ;rule to to add the statistical analysis for the ages 56-50 classification
	(characteristics (age 8))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-61-65
(defrule ages-61-65 ;rule to to add the statistical analysis for the ages 61-65 classification
	(characteristics (age 9))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-66-70
(defrule ages-66-70 ;rule to to add the statistical analysis for the ages 66-70 classification
	(characteristics (age 10))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-71-75
(defrule ages-71-75 ;rule to to add the statistical analysis for the ages 71-75 classification
	(characteristics (age 11))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-76-80
(defrule ages-76-80 ;rule to to add the statistical analysis for the ages 76-80 classification
	(characteristics (age 12))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-81-85
(defrule ages-81-85 ;rule to to add the statistical analysis for the ages 81-85 classification
	(characteristics (age 13))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-86-90
(defrule ages-86-90 ;rule to to add the statistical analysis for the ages 86-90 classification
	(characteristics (age 14))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)


;RULE: ages-91-95
(defrule ages-91-95 ;rule to to add the statistical analysis for the ages 91-95 classification
	(characteristics (age 15))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: material-status
(defrule material-status ; prints out to the user the material status menu
	(option (number 9))
=>
	(printout t "Please enter the lifestyle:" crlf)
	(printout t "#1 Single" crlf)
	(printout t "#2 Married" crlf)
	(printout t "#3 Seperated" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (marital_status ?answer)))	; stores the user's selection
)

;RULE: single:
(defrule single ;rule to to add the statistical analysis for the single classification
	(characteristics (marital_status 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: married
(defrule married ;rule to to add the statistical analysis for the married classification
	(characteristics (marital_status 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: seperated
(defrule seperated ;rule to to add the statistical analysis for the seperated classification
	(characteristics (marital_status 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: living-situation
(defrule living-situation ; prints out to the user the living situations menu
	(option (number 10))
=>
	(printout t "Please enter the Living Situation:" crlf)
	(printout t "#1 Rent" crlf)
	(printout t "#2 Homeless" crlf)
	(printout t "#3 Own" crlf)
	(printout t "#4 Financed" crlf)
	(bind ?answer (read)) ; sets the value to answer
	(assert (characteristics (living_situation ?answer)))	; stores the user's selection
)

;RULE: rent:
(defrule rent ;rule to to add the statistical analysis for the rent classification
	(characteristics (living_situation 1))
=>	
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: homeless
(defrule homeless;rule to to add the statistical analysis for the homeless classification
	(characteristics (living_situation 2))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: own
(defrule own ;rule to to add the statistical analysis for the own classification
	(characteristics (living_situation 3))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

;RULE: financed
(defrule financed ;rule to to add the statistical analysis for the financed classification
	(characteristics (living_situation 4))
=>
	(bind ?*gray-code* (+ ?*gray-code* 1))
	(bind ?*tally-code* (+ ?*tally-code* 50))
	(assert (task ask-again))
)

