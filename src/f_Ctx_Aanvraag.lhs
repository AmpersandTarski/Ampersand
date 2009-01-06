> module Main where
>  import UU_Scanner
>  import Classification
>  import Typology
>  import ADLdef
>  import ShowHS (showHS)
>  import Data.Fspec

>  main = putStr (showHS "\n>  " f_Ctx_Aanvraag)

>  f_Ctx_Aanvraag
>   = Fspc(FS_id "Aanvraag")
>     {- themes:    -}  [f_Thm_Permits,f_Thm_OtherTopics]
>     {- datasets:  -}  [f_DS_Person,f_DS_IDdocument,f_DS_Application,f_DS_Employee,f_DS_Product,f_DS_Decision,f_DS_Area]
>     {- serviceS:  -}  [oDef_Person,oDef_Persons,oDef_IDdocuments]
>     {- serviceG:  -}  [oDef_Person,oDef_Persons,oDef_IDdocument,oDef_IDdocuments,oDef_Application,oDef_Applications,oDef_Employee,oDef_Product,oDef_Decision,oDef_Decisions,oDef_Area,oDef_Areas]
>     {- views:     -}  [f_View_Person,f_View_Persons,f_View_IDdocuments]
>     {- rules:     -}  [f_rule_Rule1,f_rule_Rule2,f_rule_Rule3,f_rule_Rule4,f_rule_Rule5,f_rule_Rule6,f_rule_Rule7,f_rule_Rule8,f_rule_Rule9,f_rule_Rule10,f_rule_Rule11,f_rule_Rule12,f_rule_Rule13,f_rule_Rule14,f_rule_Rule15,f_rule_Rule16,f_rule_Rule17,f_rule_Rule18,f_rule_Rule19]
>     {- relations: -}  [rel_authenticPersonIDdocument,rel_applicantApplicationPerson,rel_checkedApplicationIDdocument,rel_assignedApplicationEmployee,rel_authEmployeeProduct,rel_kindApplicationProduct,rel_kindDecisionProduct,rel_inhabitantPersonArea,rel_areaEmployeeArea,rel_leadstoApplicationDecision]
>      isa 
>     where
>      isa = Isa []
>                [C "Person" gE [],C "IDdocument" gE [],C "Application" gE [],C "Employee" gE [],C "Product" gE [],C "Decision" gE [],C "Area" gE []]
>      gE = genEq (typology isa)
>-- ***VIEWS***: 
>      f_View_Person
>       = Fview (DS (C "Person" gE [])   [ Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 88 13,"::")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea   ])
>              (Obj "Person" (FilePos ("Aanvraag.adl",Pos 3 11,"Person"))
>                   (Tm (I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True) )
>                   [ Obj "authentic IDdocument" (FilePos ("Aanvraag.adl",Pos 4 8,"authentic IDdocument"))
>                         (Tm (Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument) )
>                         [ Obj "IDdocument" (FilePos ("Aanvraag.adl",Pos 5 13,"IDdocument"))
>                               (Tm (I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True) ) []
>                         ]
>                   , Obj "inhabitant Area" (FilePos ("Aanvraag.adl",Pos 10 8,"inhabitant Area"))
>                         (Tm (Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea) )
>                         [ Obj "Area" (FilePos ("Aanvraag.adl",Pos 11 13,"Area"))
>                               (Tm (I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True) ) []
>                         ]
>                   , Obj "applicant Application" (FilePos ("Aanvraag.adl",Pos 14 8,"applicant Application"))
>                         (Tm (Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Person" gE [],C "Application" gE []) False rel_applicantApplicationPerson) )
>                         [ Obj "Application" (FilePos ("Aanvraag.adl",Pos 15 13,"Application"))
>                               (Tm (I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True) ) []
>                         ]
>                   ])
>              [ Sspc (FS_id "getEachPerson")
>                     [ I [] (C "Person" gE []) (C "Person" gE []) True
>                     ] -- these are the visible morphisms: <sees> 
>                     []   -- no relations will be changed
>                     []   -- there are no input parameters
>                     [ Aspc (FS_id "objs") "[PersonHandle]"] -- these are the output parameters: <output> 
>                     []   -- there are no rules
>                     []   -- there are no preconditions
>                     [ "{\\tt objs}= I[Person]"
>                     ] -- postconditions
>              , Sspc (FS_id "createPerson")
>                     [ I [] (C "Person" gE []) (C "Person" gE []) True
>                     , I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True
>                     , Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument
>                     , I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True
>                     , Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea
>                     , I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True
>                     , Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Application" gE [],C "Person" gE []) True rel_applicantApplicationPerson
>                     , I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True
>                     ] -- these are the visible morphisms: <sees> 
>                     [ I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True
>                     , Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument
>                     , I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True
>                     , Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea
>                     , I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True
>                     , Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Application" gE [],C "Person" gE []) True rel_applicantApplicationPerson
>                     , I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True
>                     ] -- these are the morphisms that may be altered: <changes> 
>                     [ Aspc (FS_id "a1") "authentic IDdocument",Aspc (FS_id "i") "inhabitant Area",Aspc (FS_id "a2") "applicant Application"] -- these are the input parameters: <input>
>                     [ Aspc (FS_id "obj") "PersonHandle"] -- these are the output parameters: <output> 
>                     []   -- there are no rules
>                     []   -- there are no preconditions
>                     [ "{\\tt obj.authentic IDdocument}={\\tt a1}"
>                     , "{\\tt obj.inhabitant Area}={\\tt i}"
>                     , "{\\tt obj.applicant Application}={\\tt a2}"
>                     ] -- postconditions
>              , Sspc (FS_id "readPerson")
>                     [ I [] (C "Person" gE []) (C "Person" gE []) True
>                     , I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True
>                     , Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument
>                     , I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True
>                     , Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea
>                     , I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True
>                     , Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Application" gE [],C "Person" gE []) True rel_applicantApplicationPerson
>                     , I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True
>                     ] -- these are the visible morphisms: <sees> 
>                     []   -- no relations will be changed
>                     [ Aspc (FS_id "x") "PersonHandle"] -- these are the input parameters: <input>
>                     [ Aspc (FS_id "a1") "authentic IDdocument",Aspc (FS_id "i") "inhabitant Area",Aspc (FS_id "a2") "applicant Application"] -- these are the output parameters: <output> 
>                     []   -- there are no rules
>                     [ "{\\tt x.authentic IDdocument}=\\id{iddocumentAuthentic\\ IDdocument}"
>                     , "{\\tt x.inhabitant Area}=\\id{areaInhabitant\\ Area}"
>                     , "{\\tt x.applicant Application}=\\id{applicationApplicant\\ Application}"
>                     ] -- preconditions
>                     [ "{\\tt a1}=\\id{iddocumentAuthentic\\ IDdocument}"
>                     , "{\\tt i}=\\id{areaInhabitant\\ Area}"
>                     , "{\\tt a2}=\\id{applicationApplicant\\ Application}"
>                     ] -- postconditions
>              , Sspc (FS_id "deletePerson")
>                     [ I [] (C "Person" gE []) (C "Person" gE []) True
>                     ] -- these are the visible morphisms: <sees> 
>                     [ I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True
>                     , Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument
>                     , I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True
>                     , Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea
>                     , I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True
>                     , Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Application" gE [],C "Person" gE []) True rel_applicantApplicationPerson
>                     , I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True
>                     ] -- these are the morphisms that may be altered: <changes> 
>                     [ Aspc (FS_id "x") "PersonHandle"] -- these are the input parameters: <input>
>                     []   -- no output parameters
>                     []   -- there are no rules
>                     [ "$\\begin{array}[t]{lll}\n&{\\tt x.authentic IDdocument}=\\id{iddocumentAuthentic\\ IDdocument}\\\\\nand&{\\tt x.inhabitant Area}=\\id{areaInhabitant\\ Area}\\\\\nand&{\\tt x.applicant Application}=\\id{applicationApplicant\\ Application}&\n\\end{array}$"
>                     ] -- preconditions
>                     [ "\\hbox{\\tt obj}\\inPerson\\ \\hbox{implies that not}\\\\\\hspace{2em}$\\begin{array}[b]{lll}\n(&{\\tt obj.authentic IDdocument}=\\id{iddocumentAuthentic\\ IDdocument}\\\\\nand&{\\tt obj.inhabitant Area}=\\id{areaInhabitant\\ Area}\\\\\nand&{\\tt obj.applicant Application}=\\id{applicationApplicant\\ Application}&)\n\\end{array}$"
>                     ] -- postconditions
>              , Sspc (FS_id "updatePerson")
>                     [ I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True
>                     , Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument
>                     , I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True
>                     , Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea
>                     , I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True
>                     , Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Application" gE [],C "Person" gE []) True rel_applicantApplicationPerson
>                     , I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True
>                     ] -- these are the visible morphisms: <sees> 
>                     [ I [C "Person" gE []] (C "Person" gE []) (C "Person" gE []) True
>                     , Mph "authentic" (FilePos ("Aanvraag.adl",Pos 4 33,"authentic")) [] (C "Person" gE [],C "IDdocument" gE []) True rel_authenticPersonIDdocument
>                     , I [C "IDdocument" gE []] (C "IDdocument" gE []) (C "IDdocument" gE []) True
>                     , Mph "inhabitant" (FilePos ("Aanvraag.adl",Pos 10 28,"inhabitant")) [] (C "Person" gE [],C "Area" gE []) True rel_inhabitantPersonArea
>                     , I [C "Area" gE []] (C "Area" gE []) (C "Area" gE []) True
>                     , Mph "applicant" (FilePos ("Aanvraag.adl",Pos 14 34,"applicant")) [] (C "Application" gE [],C "Person" gE []) True rel_applicantApplicationPerson
>                     , I [C "Application" gE []] (C "Application" gE []) (C "Application" gE []) True
>                     ] -- these are the morphisms that may be altered: <changes> 
>                     [ Aspc (FS_id "x") "PersonHandle",Aspc (FS_id "a1") "authentic IDdocument",Aspc (FS_id "i") "inhabitant Area",Aspc (FS_id "a2") "applicant Application"] -- these are the input parameters: <input>
>                     []   -- no output parameters
>                     []   -- there are no rules
>                     []   -- there are no preconditions
>                     [ "{\\tt x.authentic IDdocument}={\\tt a1}"
>                     , "{\\tt x.inhabitant Area}={\\tt i}"
>                     , "{\\tt x.applicant A