(defparameter *grammar1*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *grammar1*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> He Her)
    (He   -> obama george)
    (Her  -> michelle laura)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> He Her it these those that)))

(defparameter *grammar2*
	'((Sentence -> (Nounphrase Verbphrase))  
	 (Nounphrase -> Boy Girl)           
	 (Boy -> john ajit)
	 (Girl -> pima barkha)
	 (Verbphrase -> (Verb Modlist Adverb with  Nounphrase))
	 (Verb -> runs  walks )                 
	 (Modlist ->  () (very Modlist))
	 (Adverb -> (quickly  slowly))))

(defparameter *grammar3*
        '((Schedule -> Major)
	 (Major -> Science Arts)
	 (Science -> Computer_Science Chemistry Mechanical_and_Aerospace_Engineering Biology)
	 (Arts -> English Philosphy History International_Studies) 
	 

	 ;Standard set of GECs that all students must take.   ;
	 ;                                                    ;
	 ;                                                    ;
	 ;Also, it won't work in it's current state. I need   ;
	 ;to basically implement the hash table functionality ;
	 ;used in 2c to cache what GECs are already in the    ;
	 ;course plan. If not, it will sometimes show a course;
	 ;plan with a GEC listed twice. I'll keep it for now, ;
	 ;but when I get 2c finished, I'll try and fix 2a to  ;
	 ;account for this. I'm thinking something along the  ;
	 ;lines of what an attributed grammar does. ^_^       ;
	 (GEC_1 -> soca105 phil140 dance101 geo110 com101 film102 pet101 thet101 engl132 psyc101)
	 (GEC_2 -> GEC_1 econ201 econ202 comm202 soca221 cs101 hist201 relg219 phil260) 
 

	 ;*************Science based Course Plans**************;
	 ;I broke up each type of major into specific clusters ;
	 ;to make things easier to read. These clusters  are   ;
	 ;the standard classes that each engineering or medical;
	 ;science student must take to graduate. I referenced  ;
	 ;the WVU Course Catalog for this.
	  

	 ;*****Engineering Major Cluster (CS and MAE)*******;
	 (Engineering_Cluster0 -> (math156 GEC_1 GEC_1 EC0PreReqs0))
	 (EC0PreReqs0 -> (engr199 engr101 engl101 chem115 math155))

	 (Engineering_Cluster1 -> (stat215 (EC1PreReqs1)))
	 (EC1PreReqs1 -> (math251 engl102 phys112))
     
	 ;Computer Science Course Plan;
	 (Computer_Science -> (ComputerScienceMajor CSYear3)) 
	 (CSYear3 -> (cs310 cs350 cpe310 cpe311 CS400xx CS400xx GEC_SubGroup CSYear2))
	 (CSYear2 -> (cs210 cs220 cs221 cs230 cpe271 cpe272 Engineering_Cluster1 CSYear1))
	 (CSYear1 -> (cs110 cs111 Engineering_Cluster0))
	 (CS400xx -> cs410 cs426 cs430 cs440 cs450 cs453 cs472 cs493)
	 (GEC_SubGroup -> (GEC_2 GEC_2 GEC_2 GEC_2))

	 ;Mechanical & Aerospace Engineering Course Plan;
	 (Mechanical_and_Aerospace_Engineering -> (MechanicalandAreospaceEngineeringMajor MAEYear3))
	 (MAEYear3 -> (mae316 mae320 mae335 mae343 ee221 ee222 mae336 mae345 mae365 GEC_2 MAEYear2))
	 (MAEYear2 -> (mae215 mae241 mae242 mae243 mae244 Engineering_Cluster1 MAEYear1))
	 (MAEYear1 -> (engr102 Engineering_Cluster0))
	 
	 ;*******Medical Science Clusters (Chemistry and Biology)*******;
	 (Medical_Science_Cluster0 -> (phys111 phys122 MedSciPreReqs0) (phys101 phys102 MedSciPreReqs0))
	 (MedSciPreReqs0 -> (univ199 engl101 ((chem115 chem116) (chem117 chem118)) GEC_1))

	 (Medical_Science_Cluster1 -> (chem233 chem234 chem235 chem236 MedSciPreReqs1))
	 (MedSciPreReqs1 -> (engl102 (math155 (math153 math154)) GEC_1))

	 ;Chemistry Course Plan;
	 (Chemistry -> (ChemistryMajor ChemYear3))
	 (ChemYear3 -> (math251 chem310 chem313 chem346 chem347 chem348 chem349 GEC_2 ChemYear2))
	 (ChemYear2 -> (math156 Medical_Science_Cluster1 GEC_2 GEC_2 ChemYear1))
	 (ChemYear1 -> (Medical_Science_Cluster0 GEC_1 GEC_1))

	 ;Biology Course Plan;
	 (Biology -> (BiologyMajor BIOYear3))
	 (BIOYear3 -> (bio321 BioFocuses))
	 (BIOYear2 -> (stat211 bio219 bio221 Medical_Science_Cluster1 BIOYear1))
	 (BIOYear1 -> (bio115 bio117 Medical_Science_Cluster0))
	 
	 ;Biology has 4 unique sub focuses that students can take. The grouping;
	 ;represents these focuses with their respective class.                 ;
	 (BioFocuses -> Focus1 Focus2 Focus3 Focus4)
	 (Focus1 -> (bio310 bio311 bio312 bio313 bio315 bio316 bio324 bio325))
	 (Focus2 -> (bio336 bio337 bio339 bio340 bio341 bio348 bio350 bio352 bio353))
	 (Focus3 -> (bio301 bio338 bio351 bio361 bio362 bio363))
	 (Focus4 -> (bio302 phys225 SubFocus1))
	 (SubFocus1 -> agbi420 bioc339 bioc531)



	 ;*************Art Major based Course Plans************ ;
	 ;I started with the Science based course plans, as I am:
	 ;more familiar with them. For an Art major, I've just  ;
	 ;determined that each has their major requirements, and;
	 ;made up the other requirements. For example, I simply ;
	 ;have all BA students take a language path, on top of  ;
	 ;normal GECs.                                          :

	 
	 ;The language GEC block for all Arts Student. ;
	 ;I'm thinking I made throw this in some of the;
	 ;Science major course plans.                  ;
	 (Language_GEC -> Spanish Japanese)
	 (Spanish -> (span204 PreReqSP204))
	 (PreReqSP204 -> (span203 PreReqSP203))
	 (PreReqSP203 -> (span102 PreReqSP102))
	 (PreReqSP102 -> span101)
	 (Japanese -> (japn204 PreReqJP204))
	 (PreReqJP204 -> (japn203 PreReqJP203))
	 (PreReqJP203 -> (japn102 PreReqJP102))
	 (PreReqJP102 -> japn101)

	 ;Basic required courses for all Arts students.  ;
	 ;Trying to cut down on redundancy ^_^           ;
	 (Arts_Cluster0 -> (univ199 engl101 engl102 GEC_1 GEC_1))
	  
	 ;         English Course Plan          ;
	 (English -> (EnglishMajor ENGLYear3))
	 (ENGLYear3 -> (engl221 engl226 engl263 engl301 engl309 engl319 engl337 GEC_2 ENGLYear2))
	 (ENGLYear2 -> (engl241 engl242 engl261 GEC_2 GEC_2 GEC_2 GEC_2 ENGLYear1))
	 (ENGLYear1 -> (engl200 Arts_Cluster0 Language_GEC))
	 
	 ;         Philosphy Course Plan        ;
	 (Philosophy -> (PhilosophyMajor PHILYear3))
	 (PHILYear3 -> (phil301 phil302 phil321 phil346 phil494 phil496 GEC_2 PHILYear2))
	 (PHILYear2 -> (phil244 phil248 phil260 GEC_2 GEC_2 PHILYear1))
	 (PHILYear1 -> (Arts_Cluster0 Language_GEC GEC_1 GEC_2 GEC_2))

	 ;         History Course Plan          ;
	 (History -> (HistoryMajor HISTYear3))
	 (HISTYear3 -> (hist330 hist331 hist332 hist334 hist358 hist359 HISTYear2))
	 (HISTYear2 -> (hist271 hist272 hist220 hist210 hist221 GEC_2 GEC_2 HISTYear1))
	 (HISTYear1 -> (hist101 hist102 hist104 hist105 Arts_Cluster0 Language_GEC GEC_2 GEC_2))

         ;   International Studies Course Plan  ;
	 ;Contrary to most Arts majors, this    ;
	 ;major is pretty well documented. They ;
	 ;layout all kinds of different areas of;
	 ;emphasis, but it's too much. There is ;
	 ;no way I'm coding them all just for   ;
	 ;this project. I hope it doesn't cost  ;
	 ;me any points lol. ^_^                ;
	 ;                                      ;
	 ;Also, something I need to point out is;
	 ;that I specifically didn't add the    ;
	 ;Language_GEC cluster to this, because ;
	 ;I just chose "The Americas Required"  ;
	 ;courses emphasis. Wouldn't make much  ;
	 ;to randomly assign a language GEC with;
	 ;"required" courses in Spanish if the  ;
	 ;grammer randomly chooses Japanese -_- ;
	 ;                                      ;
	 ;That would totally be funny though.   ;
	 (International_Studies -> (InternationalStudiesMajor ISYear3))
	 (ISYear3 -> (span330 span331 span332 span431 span461 span464 ISYear2))
	 (ISYear2 -> (econ201 econ202 span203 span204 span301 span302 geo215 geo243 GEC_2 ISYear1))
	 (ISYear1 -> (flit113 flit114 flit115 flit116 Arts_Cluster0 span101 span102))))


(defparameter *grammar4*
  '((Start -> (Earth !IsStressed))
    (!IsStressed -> !Catestrophes !Collision !Science !Attack)
    
    (!Catestrophes -> (!Catestrophe !PossibleMegaDeath))
    (!Collision -> (isStruckByAGiant !Floater !AndThen))
    (!Attack -> (isAttackedBy !Sizes !Extraterestrial !Beings !Whichetc))
    (!Science -> (scientists !DoScience !Sizes !Beings !Whichetc))
    
    ;Catestrophes;
    (!Catestrophe -> burnsUp freezes fallsIntoSun)
    
    ;Collision;
    (!Floater -> comet asteroid cloud)
    (!AndThen -> butIsSaved andIsDestroyed (andNotDestroyed !PossibleMegaDeath))
    (!PossibleMegaDeath -> everybodyDie (!SomeSaved !GoOn))
    (!SomeSaved -> somePeople everybody almostEverybody)
    (!GoOn -> dies !Rescued !Saved)
    
    ;Possible Collision Aftermath;
    (!Rescued -> (isRescued !Sizes !Extraterestrial !Beings))
    (!Saved -> butIsSavedBy !SomeOne !Science)
    
    (!DoScience -> invent discover)
    (!SomeOne -> earth !Extraterestrial)
    
    ;Attacks;
    (!Extraterestrial -> martian lunar extraGalactic)
    (!Beings -> bugs reptiles blobs superbeings)
    (!Sizes -> tiny giant enormous)
    (!Whichetc -> who WantSomething)
    (!WhichEtc -> )
    
    (!WantSomething -> !WantWomen (areFriendly !DenoumentOrHappyEnding) (!Understand !ButEtc))
    (!WantWomen -> (wantOurWomen !AndTakeAfewAndLeave))
    (!ButEtc -> (!AndAre radioactive !TryToKill))

    (!Understand -> (areFriendly butMisunderstood) misunderstandUs understandUsAllTooWell !Hungry)
    (!Hungry -> lookUponUsAsASourceOfNourishment)

    (!Dine -> (!Hungry and eat us Denoument?))
    
    (!AndAre -> andAre andAreNot)
    
    (!TryToKill -> (can be killed by !Killers) (can not be killed by !Killers !SoEtc))
    
    (!Killers -> !Killer (!Killer and !Killer))
    (!Killer -> aCrowdofPeasants theArmy theNavy theAirForce theMarines theCoastGuard theAtomBomb)
    
    (!SoEtc -> butTheyDieFromCatchingACold soTheyKillus soTheyPutUsUnderABenignDictatorShip
	    soTheyEatUs (soScientistsInventAWeapon !Which))
    (!Which -> whichTurnsThemIntoDisgustingLumps whichKillsThem (whichFails !SoEtc))

    (!Denoument? ->   !Denoument)
    (!Denoument? -> )

    (!DenoumentOrHappyEnding -> !Denoument !HappyEnding)
    (!Denoument -> (!Result !Ending))
    (!Result -> aCuteLittleKidConvincesThemPeopleAreOk aPriestTalksToThemOfGod theyFallInLoveWithThisBeautifulGirl)  
    
    (!Ending -> !Tragic !Happy)
    (!Tragic -> andTheyDie andTheyLeave andTheyTurnIntoDisgustingLumps)
    (!Happy -> andTheyGetMarriedAndLiveHappilyForeverAfter)))