<?php // generated with ADL vs. 0.8.10
  
  /********* on line 3, file "..\\..\\prive\\myeclipseworkspace\\ADL\\Test bestanden\\Aanvraag.adl"
   * Behandelaar[Employee] : V
   *  = [ aanvragen[Application] : assigned~
   *       = [ applicant[Person] : applicant
   *         , checked[IDdocument] : checked
   *         , kind[Product] : kind
   *         , assigned[Employee] : assigned
   *         , decision[Decision] : leadsto
   *        ]
   *   ]
   *********/
  
  function getobject_Behandelaar(){
    return   new object("Behandelaar", array
       ( new oRef( new oMulti( true,false,false,false ) // derived from assigned~
           , new object("aanvragen", array
               ( new oRef( new oMulti( false,true,false,true ) // derived from applicant
                   , new object("applicant", array(), "Person.php")
                   ) 
               , new oRef( new oMulti( false,true,false,false ) // derived from checked
                   , new object("checked", array())
                   ) 
               , new oRef( new oMulti( false,true,false,true ) // derived from kind
                   , new object("kind", array())
                   ) 
               , new oRef( new oMulti( false,true,false,false ) // derived from assigned
                   , new object("assigned", array(), "Behandelaar.php")
                   ) 
               , new oRef( new oMulti( true,true,true,false ) // derived from leadsto
                   , new object("decision", array(), "Decision.php")
                   ) 
               ), "Application.php")
           ) 
       ), "Behandelaar.php");
  }

  class Behandelaar {
    var $id;
    var $aanvragen;
    function Behandelaar($id=null, $aanvragen=null){
        $this->id=$id;
        $this->aanvragen=$aanvragen;
        if(!isset($aanvragen)){
          if(isset($id)){
            $this->aanvragen = array();
            foreach(DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_pplication
                           FROM T4_assigned AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'') as $i=>$v){
              $this->aanvragen[]=new Behandelaar_aanvragen($v['AttA_pplication']);
            }
          } else $this->aanvragen=array();
        }
    }
    function add_aanvragen(Behandelaar_aanvragen $aanvragen){
      return $this->aanvragen[]=$aanvragen;
    }
    function getEach_aanvragen(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttA_pplication
                                FROM C3_A_pplication
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttA_pplication'];
      }
      return $res;
    }
    function addGen($type,$value){
      if($type=='aanvragen') return $this->add_aanvragen($value);
      else return false;
    }
  }
  class Behandelaar_aanvragen {
    var $id;
    var $applicant;
    var $checked;
    var $kind;
    var $assigned;
    var $decision;
    function Behandelaar_aanvragen($id=null, $applicant=null, $checked=null, $kind=null, $assigned=null, $decision=null){
        $this->id=$id;
        $this->applicant=$applicant;
        $this->checked=$checked;
        $this->kind=$kind;
        $this->assigned=$assigned;
        $this->decision=$decision;
        if(!isset($applicant)){
          if(isset($id)){
            $this->applicant = array();
            foreach(DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'') as $i=>$v){
              $this->applicant[]=new Behandelaar_aanvragen_applicant($v['AttP_erson']);
            }
          } else $this->applicant=array();
        }
        if(count($this->applicant)==0) $this->applicant[] = new Behandelaar_aanvragen_applicant();
        if(count($this->applicant)>1){
          $last=$this->applicant[count($this->applicant)-1];
          $this->applicant = array();
          $this->applicant[] = $last;
        }
        if(!isset($checked)){
          if(isset($id)){
            $this->checked = array();
            foreach(DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'') as $i=>$v){
              $this->checked[]=new Behandelaar_aanvragen_checked($v['AttID_document']);
            }
          } else $this->checked=array();
        }
        if(count($this->checked)>1){
          $last=$this->checked[count($this->checked)-1];
          $this->checked = array();
          $this->checked[] = $last;
        }
        if(!isset($kind)){
          if(isset($id)){
            $this->kind = array();
            foreach(DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'') as $i=>$v){
              $this->kind[]=new Behandelaar_aanvragen_kind($v['AttP_roduct']);
            }
          } else $this->kind=array();
        }
        if(count($this->kind)==0) $this->kind[] = new Behandelaar_aanvragen_kind();
        if(count($this->kind)>1){
          $last=$this->kind[count($this->kind)-1];
          $this->kind = array();
          $this->kind[] = $last;
        }
        if(!isset($assigned)){
          if(isset($id)){
            $this->assigned = array();
            foreach(DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'') as $i=>$v){
              $this->assigned[]=new Behandelaar_aanvragen_assigned($v['AttE_mployee']);
            }
          } else $this->assigned=array();
        }
        if(count($this->assigned)>1){
          $last=$this->assigned[count($this->assigned)-1];
          $this->assigned = array();
          $this->assigned[] = $last;
        }
        if(!isset($decision)){
          if(isset($id)){
            $this->decision = array();
            foreach(DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'') as $i=>$v){
              $this->decision[]=new Behandelaar_aanvragen_decision($v['AttD_ecision']);
            }
          } else $this->decision=array();
        }
        if(count($this->decision)>1){
          $last=$this->decision[count($this->decision)-1];
          $this->decision = array();
          $this->decision[] = $last;
        }
    }
    function add_applicant(Behandelaar_aanvragen_applicant $applicant){
      return $this->applicant[0]=$applicant;
    }
    function getEach_applicant(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttP_erson
                                FROM C1_P_erson
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttP_erson'];
      }
      return $res;
    }
    function add_checked(Behandelaar_aanvragen_checked $checked){
      return $this->checked[0]=$checked;
    }
    function getEach_checked(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttID_document
                                FROM C2_ID_document
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttID_document'];
      }
      return $res;
    }
    function add_kind(Behandelaar_aanvragen_kind $kind){
      return $this->kind[0]=$kind;
    }
    function getEach_kind(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttP_roduct
                                FROM C5_P_roduct
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttP_roduct'];
      }
      return $res;
    }
    function add_assigned(Behandelaar_aanvragen_assigned $assigned){
      return $this->assigned[0]=$assigned;
    }
    function getEach_assigned(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttE_mployee
                                FROM C4_E_mployee
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttE_mployee'];
      }
      return $res;
    }
    function add_decision(Behandelaar_aanvragen_decision $decision){
      return $this->decision[0]=$decision;
    }
    function getEach_decision(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttD_ecision
                                FROM C6_D_ecision
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttD_ecision'];
      }
      return $res;
    }
    function addGen($type,$value){
      if($type=='applicant') return $this->add_applicant($value);
      if($type=='checked') return $this->add_checked($value);
      if($type=='kind') return $this->add_kind($value);
      if($type=='assigned') return $this->add_assigned($value);
      if($type=='decision') return $this->add_decision($value);
      else return false;
    }
  }
  class Behandelaar_aanvragen_applicant {
    var $id;
    function Behandelaar_aanvragen_applicant($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  class Behandelaar_aanvragen_checked {
    var $id;
    function Behandelaar_aanvragen_checked($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  class Behandelaar_aanvragen_kind {
    var $id;
    function Behandelaar_aanvragen_kind($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  class Behandelaar_aanvragen_assigned {
    var $id;
    function Behandelaar_aanvragen_assigned($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  class Behandelaar_aanvragen_decision {
    var $id;
    function Behandelaar_aanvragen_decision($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  function getEachBehandelaar(){
      return DB_doquer('SELECT DISTINCT AttE_mployee
                           FROM C4_E_mployee
                          WHERE 1');
  }
  function createBehandelaar(Behandelaar &$obj){
      return updateBehandelaar($obj,true);
  }
  function readBehandelaar($id){
      // check existence of $id
      $ctx = DB_doquer('SELECT DISTINCT isect0.AttE_mployee
                           FROM 
                             ( SELECT DISTINCT AttE_mployee
                                 FROM C4_E_mployee
                                WHERE 1
                             ) AS isect0
                          WHERE isect0.AttE_mployee = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj = new Behandelaar($id);
      return $obj;
  }
  function updateBehandelaar(Behandelaar $Behandelaar,$new=false){
      global $DB_link,$DB_err,$DB_lastquer;
      $preErr= $new ? 'Cannot create new Employee: ':'Cannot update Employee: ';
      DB_doquer('START TRANSACTION');
      if($new){ // create a new object
        if(!isset($Behandelaar->id)){ // find a unique id
           $nextNum = DB_doquer('SELECT max(1+AttE_mployee) FROM C4_E_mployee GROUP BY \'1\'');
           $Behandelaar->id = @$nextNum[0][0]+0;
        }
        if(DB_plainquer('INSERT INTO C4_E_mployee (AttE_mployee) VALUES (\''.addslashes($Behandelaar->id).'\')',$errno)===false){
            $DB_err=$preErr.(($errno==1062) ? 'Employee \''.$Behandelaar->id.'\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);
            DB_doquer('ROLLBACK');
            return false;
        }
      }else
      if(!$new){
        // destroy old attribute values
        $effected = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_pplication
                           FROM T4_assigned AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($Behandelaar->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_pplication']).'\'';
        }
        $Behandelaar_aanvragen_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T4_assigned WHERE AttE_mployee=\''.addslashes($Behandelaar->id).'\'');
      }
      foreach($Behandelaar->aanvragen as $i=>$Behandelaar_aanvragen){
        if(!isset($Behandelaar_aanvragen->id)){
           $nextNum = DB_doquer('SELECT max(1+AttA_pplication) FROM C3_A_pplication GROUP BY \'1\'');
           $Behandelaar_aanvragen->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C3_A_pplication (AttA_pplication) VALUES (\''.addslashes($Behandelaar_aanvragen->id).'\')');
        DB_doquer('INSERT IGNORE INTO T4_assigned (AttE_mployee,AttA_pplication) VALUES (\''.addslashes($Behandelaar->id).'\',\''.addslashes($Behandelaar_aanvragen->id).'\')');
          if(!$new){
            // destroy old attribute values
            $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Behandelaar_aanvragen->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttP_erson']).'\'';
            }
            $Behandelaar_aanvragen_applicant_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T2_applicant WHERE AttA_pplication=\''.addslashes($Behandelaar_aanvragen->id).'\'');
            $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Behandelaar_aanvragen->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttID_document']).'\'';
            }
            $Behandelaar_aanvragen_checked_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T3_checked WHERE AttA_pplication=\''.addslashes($Behandelaar_aanvragen->id).'\'');
            $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Behandelaar_aanvragen->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
            }
            $Behandelaar_aanvragen_kind_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T6_kind WHERE AttA_pplication=\''.addslashes($Behandelaar_aanvragen->id).'\'');
            $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Behandelaar_aanvragen->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttE_mployee']).'\'';
            }
            $Behandelaar_aanvragen_assigned_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T4_assigned WHERE AttA_pplication=\''.addslashes($Behandelaar_aanvragen->id).'\'');
            $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Behandelaar_aanvragen->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttD_ecision']).'\'';
            }
            $Behandelaar_aanvragen_decision_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T10_leadsto WHERE AttA_pplication=\''.addslashes($Behandelaar_aanvragen->id).'\'');
          }
          foreach($Behandelaar_aanvragen->applicant as $i=>$Behandelaar_aanvragen_applicant){
            if(!isset($Behandelaar_aanvragen_applicant->id)){
               $nextNum = DB_doquer('SELECT max(1+AttP_erson) FROM C1_P_erson GROUP BY \'1\'');
               $Behandelaar_aanvragen_applicant->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C1_P_erson (AttP_erson) VALUES (\''.addslashes($Behandelaar_aanvragen_applicant->id).'\')');
            DB_doquer('INSERT IGNORE INTO T2_applicant (AttA_pplication,AttP_erson) VALUES (\''.addslashes($Behandelaar_aanvragen->id).'\',\''.addslashes($Behandelaar_aanvragen_applicant->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          foreach($Behandelaar_aanvragen->checked as $i=>$Behandelaar_aanvragen_checked){
            if(!isset($Behandelaar_aanvragen_checked->id)){
               $nextNum = DB_doquer('SELECT max(1+AttID_document) FROM C2_ID_document GROUP BY \'1\'');
               $Behandelaar_aanvragen_checked->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C2_ID_document (AttID_document) VALUES (\''.addslashes($Behandelaar_aanvragen_checked->id).'\')');
            DB_doquer('INSERT IGNORE INTO T3_checked (AttA_pplication,AttID_document) VALUES (\''.addslashes($Behandelaar_aanvragen->id).'\',\''.addslashes($Behandelaar_aanvragen_checked->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          foreach($Behandelaar_aanvragen->kind as $i=>$Behandelaar_aanvragen_kind){
            if(!isset($Behandelaar_aanvragen_kind->id)){
               $nextNum = DB_doquer('SELECT max(1+AttP_roduct) FROM C5_P_roduct GROUP BY \'1\'');
               $Behandelaar_aanvragen_kind->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C5_P_roduct (AttP_roduct) VALUES (\''.addslashes($Behandelaar_aanvragen_kind->id).'\')');
            DB_doquer('INSERT IGNORE INTO T6_kind (AttA_pplication,AttP_roduct) VALUES (\''.addslashes($Behandelaar_aanvragen->id).'\',\''.addslashes($Behandelaar_aanvragen_kind->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          foreach($Behandelaar_aanvragen->assigned as $i=>$Behandelaar_aanvragen_assigned){
            if(!isset($Behandelaar_aanvragen_assigned->id)){
               $nextNum = DB_doquer('SELECT max(1+AttE_mployee) FROM C4_E_mployee GROUP BY \'1\'');
               $Behandelaar_aanvragen_assigned->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C4_E_mployee (AttE_mployee) VALUES (\''.addslashes($Behandelaar_aanvragen_assigned->id).'\')');
            DB_doquer('INSERT IGNORE INTO T4_assigned (AttA_pplication,AttE_mployee) VALUES (\''.addslashes($Behandelaar_aanvragen->id).'\',\''.addslashes($Behandelaar_aanvragen_assigned->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          foreach($Behandelaar_aanvragen->decision as $i=>$Behandelaar_aanvragen_decision){
            if(!isset($Behandelaar_aanvragen_decision->id)){
               $nextNum = DB_doquer('SELECT max(1+AttD_ecision) FROM C6_D_ecision GROUP BY \'1\'');
               $Behandelaar_aanvragen_decision->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C6_D_ecision (AttD_ecision) VALUES (\''.addslashes($Behandelaar_aanvragen_decision->id).'\')');
            DB_doquer('INSERT IGNORE INTO T10_leadsto (AttA_pplication,AttD_ecision) VALUES (\''.addslashes($Behandelaar_aanvragen->id).'\',\''.addslashes($Behandelaar_aanvragen_decision->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          if(!$new && strlen($Behandelaar_aanvragen_applicant_str))
            DB_doquer('DELETE FROM C1_P_erson
              WHERE AttP_erson IN ('.$Behandelaar_aanvragen_applicant_str.')
                AND NOT EXISTS (SELECT * FROM T8_inhabitant
                                 WHERE C1_P_erson.AttP_erson = T8_inhabitant.AttP_erson
                               )
                AND NOT EXISTS (SELECT * FROM T1_authentic
                                 WHERE C1_P_erson.AttP_erson = T1_authentic.AttP_erson
                               )
                AND NOT EXISTS (SELECT * FROM T2_applicant
                                 WHERE C1_P_erson.AttP_erson = T2_applicant.AttP_erson
                               )
            ');
          if(!$new && strlen($Behandelaar_aanvragen_checked_str))
            DB_doquer('DELETE FROM C2_ID_document
              WHERE AttID_document IN ('.$Behandelaar_aanvragen_checked_str.')
                AND NOT EXISTS (SELECT * FROM T3_checked
                                 WHERE C2_ID_document.AttID_document = T3_checked.AttID_document
                               )
                AND NOT EXISTS (SELECT * FROM T1_authentic
                                 WHERE C2_ID_document.AttID_document = T1_authentic.AttID_document
                               )
            ');
          if(!$new && strlen($Behandelaar_aanvragen_kind_str))
            DB_doquer('DELETE FROM C5_P_roduct
              WHERE AttP_roduct IN ('.$Behandelaar_aanvragen_kind_str.')
                AND NOT EXISTS (SELECT * FROM T6_kind
                                 WHERE C5_P_roduct.AttP_roduct = T6_kind.AttP_roduct
                               )
                AND NOT EXISTS (SELECT * FROM T7_kind
                                 WHERE C5_P_roduct.AttP_roduct = T7_kind.AttP_roduct
                               )
                AND NOT EXISTS (SELECT * FROM T5_auth
                                 WHERE C5_P_roduct.AttP_roduct = T5_auth.AttP_roduct
                               )
            ');
          if(!$new && strlen($Behandelaar_aanvragen_assigned_str))
            DB_doquer('DELETE FROM C4_E_mployee
              WHERE AttE_mployee IN ('.$Behandelaar_aanvragen_assigned_str.')
                AND NOT EXISTS (SELECT * FROM T9_area
                                 WHERE C4_E_mployee.AttE_mployee = T9_area.AttE_mployee
                               )
                AND NOT EXISTS (SELECT * FROM T5_auth
                                 WHERE C4_E_mployee.AttE_mployee = T5_auth.AttE_mployee
                               )
                AND NOT EXISTS (SELECT * FROM T4_assigned
                                 WHERE C4_E_mployee.AttE_mployee = T4_assigned.AttE_mployee
                               )
            ');
          if(!$new && strlen($Behandelaar_aanvragen_decision_str))
            DB_doquer('DELETE FROM C6_D_ecision
              WHERE AttD_ecision IN ('.$Behandelaar_aanvragen_decision_str.')
                AND NOT EXISTS (SELECT * FROM T7_kind
                                 WHERE C6_D_ecision.AttD_ecision = T7_kind.AttD_ecision
                               )
                AND NOT EXISTS (SELECT * FROM T10_leadsto
                                 WHERE C6_D_ecision.AttD_ecision = T10_leadsto.AttD_ecision
                               )
            ');
      }
      if(!$new && strlen($Behandelaar_aanvragen_str))
        DB_doquer('DELETE FROM C3_A_pplication
          WHERE AttA_pplication IN ('.$Behandelaar_aanvragen_str.')
            AND NOT EXISTS (SELECT * FROM T10_leadsto
                             WHERE C3_A_pplication.AttA_pplication = T10_leadsto.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T6_kind
                             WHERE C3_A_pplication.AttA_pplication = T6_kind.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T4_assigned
                             WHERE C3_A_pplication.AttA_pplication = T4_assigned.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T2_applicant
                             WHERE C3_A_pplication.AttA_pplication = T2_applicant.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T3_checked
                             WHERE C3_A_pplication.AttA_pplication = T3_checked.AttA_pplication
                           )
        ');
    if (!checkRule1()){
      $DB_err=$preErr.'\"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product.\"';
    } else
    if (!checkRule2()){
      $DB_err=$preErr.'\"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.\"';
    } else
    if (!checkRule3()){
      $DB_err=$preErr.'\"An application for a permit is accepted only from individuals whose identity is authenticated.\"';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'\"Applications for permits are treated by authorized personnel only.\"';
    } else
    if (!checkRule5()){
      $DB_err=$preErr.'\"authentic[Person*IDdocument] is injective\"';
    } else
    if (!checkRule6()){
      $DB_err=$preErr.'\"authentic[Person*IDdocument] is surjective\"';
    } else
    if (!checkRule7()){
      $DB_err=$preErr.'\"applicant[Application*Person] is univalent\"';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'\"applicant[Application*Person] is total\"';
    } else
    if (!checkRule9()){
      $DB_err=$preErr.'\"checked[Application*IDdocument] is univalent\"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'\"assigned[Application*Employee] is univalent\"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'\"kind[Application*Product] is univalent\"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'\"kind[Application*Product] is total\"';
    } else
    if (!checkRule13()){
      $DB_err=$preErr.'\"kind[Decision*Product] is univalent\"';
    } else
    if (!checkRule14()){
      $DB_err=$preErr.'\"kind[Decision*Product] is total\"';
    } else
    if (!checkRule16()){
      $DB_err=$preErr.'\"inhabitant[Person*Area] is total\"';
    } else
    if (!checkRule18()){
      $DB_err=$preErr.'\"leadsto[Application*Decision] is injective\"';
    } else
    if (!checkRule19()){
      $DB_err=$preErr.'\"leadsto[Application*Decision] is univalent\"';
    } else
    if (!checkRule20()){
      $DB_err=$preErr.'\"leadsto[Application*Decision] is surjective\"';
    } else
      if(true){ // all rules are met
          DB_doquer('COMMIT');
          return $Behandelaar->id;
      }
      DB_doquer('ROLLBACK');
      return false;
  }
  function deleteBehandelaar($id){
    global $DB_err;
    $preErr= 'Cannot delete Employee: ';
    DB_doquer('START TRANSACTION');
    
      $taken = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_rea
                           FROM T9_area AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
      if(count($taken)) {
        $DB_err = 'Cannot delete Behandelaar: Employee \''.addslashes($id).'\' treats applications from area \''.addslashes($taken[0]['AttA_rea']).'\'';
        DB_doquer('ROLLBACK');
        return false;
      }
      $taken = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttP_roduct
                           FROM T5_auth AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
      if(count($taken)) {
        $DB_err = 'Cannot delete Behandelaar: Employee \''.addslashes($id).'\' is authorized to treat product \''.addslashes($taken[0]['AttP_roduct']).'\'';
        DB_doquer('ROLLBACK');
        return false;
      }
  /*
  assigned
  *************
  area
  auth
  I
  assigned
  V
  */
        $effected = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_pplication
                           FROM T4_assigned AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_pplication']).'\'';
        }
        $aanvragen_str=join(',',$arr);
        DB_doquer ('DELETE FROM T4_assigned WHERE AttE_mployee=\''.addslashes($id).'\'');
    DB_doquer('DELETE FROM C4_E_mployee WHERE AttE_mployee=\''.addslashes($id).'\'');
    if(strlen($aanvragen_str))
      DB_doquer('DELETE FROM C3_A_pplication
        WHERE AttA_pplication IN ('.$aanvragen_str.')
          AND NOT EXISTS (SELECT * FROM T10_leadsto
                           WHERE C3_A_pplication.AttA_pplication = T10_leadsto.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T6_kind
                           WHERE C3_A_pplication.AttA_pplication = T6_kind.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T4_assigned
                           WHERE C3_A_pplication.AttA_pplication = T4_assigned.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T2_applicant
                           WHERE C3_A_pplication.AttA_pplication = T2_applicant.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T3_checked
                           WHERE C3_A_pplication.AttA_pplication = T3_checked.AttA_pplication
                         )
      ');
    if (!checkRule1()){
      $DB_err=$preErr.'\"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product.\"';
    } else
    if (!checkRule2()){
      $DB_err=$preErr.'\"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.\"';
    } else
    if (!checkRule3()){
      $DB_err=$preErr.'\"An application for a permit is accepted only from individuals whose identity is authenticated.\"';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'\"Applications for permits are treated by authorized personnel only.\"';
    } else
    if (!checkRule5()){
      $DB_err=$preErr.'\"authentic[Person*IDdocument] is injective\"';
    } else
    if (!checkRule6()){
      $DB_err=$preErr.'\"authentic[Person*IDdocument] is surjective\"';
    } else
    if (!checkRule7()){
      $DB_err=$preErr.'\"applicant[Application*Person] is univalent\"';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'\"applicant[Application*Person] is total\"';
    } else
    if (!checkRule9()){
      $DB_err=$preErr.'\"checked[Application*IDdocument] is univalent\"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'\"assigned[Application*Employee] is univalent\"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'\"kind[Application*Product] is univalent\"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'\"kind[Application*Product] is total\"';
    } else
    if (!checkRule13()){
      $DB_err=$preErr.'\"kind[Decision*Product] is univalent\"';
    } else
    if (!checkRule14()){
      $DB_err=$preErr.'\"kind[Decision*Product] is total\"';
    } else
    if (!checkRule16()){
      $DB_err=$preErr.'\"inhabitant[Person*Area] is total\"';
    } else
    if (!checkRule18()){
      $DB_err=$preErr.'\"leadsto[Application*Decision] is injective\"';
    } else
    if (!checkRule19()){
      $DB_err=$preErr.'\"leadsto[Application*Decision] is univalent\"';
    } else
    if (!checkRule20()){
      $DB_err=$preErr.'\"leadsto[Application*Decision] is surjective\"';
    } else
    if(true) {
      DB_doquer('COMMIT');
      return true;
    }
    DB_doquer('ROLLBACK');
    return false;
  }
?>