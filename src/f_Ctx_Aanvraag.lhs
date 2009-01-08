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
>     {- rules:     -}  [f_rule_Rule1,f_rule_Rule2,f_rule_Rule3,f_rule_Rule4,f_rule_Rule5,f_rule_Rule6,f_rule_Rule7,f_rule_Rule8,f_rule_Rule9,f_rule_Rule10,f_rule_Rule11,f_rule_Rule12,f_rule_Rule13,f_rule_Rule14,f_rule_Rule15,f_rule_Rule16,f_rule_Rule17,f_rule_Rule18,f_rule_Rule19]
>     {- relations: -}  [rel_authenticPersonIDdocument,rel_applicantApplicationPerson,rel_checkedApplicationIDdocument,rel_assignedApplicationEmployee,rel_authEmployeeProduct,rel_kindApplicationProduct,rel_kindDecisionProduct,rel_inhabitantPersonArea,rel_areaEmployeeArea,rel_leadstoApplicationDecision]
>      isa 
>     where
>      isa = Isa []
>                [C "Person" gE [],C "IDdocument" gE [],C "Application" gE [],C "Employee" gE [],C "Product" gE [],C "Decision" gE [],C "Area" gE []]
>      gE = genEq (typology isa)
>-- ***THEMES***: 
>      f_Thm