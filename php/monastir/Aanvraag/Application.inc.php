<?php // generated with ADL vs. 0.8.10
  
  /********* on line 16, file "Aanvraag.adl"
   * Application[Application] : V
   *  = [ applicant[Person] : applicant
   *       = [ person[Person] : I
   *         , idDocument[IDdocument] : authentic
   *         , residence[Area] : inhabitant
   *        ]
   *    , checked[IDdocument] : checked
   *    , kind[Product] : kind
   *    , assigned[Employee] : assigned
   *    , decision[Decision] : leadsto
   *   ]
   *********/
  
  function getobject_Application(){  return   new object("Application", array
       ( new oRef( new oMulti( false,true,false,true ) // derived from applicant
           , new object("applicant", array
               ( new oRef( new oMulti( true,true,true,true ) // derived from I
                   , new object("person", array(), "Person.php")
                   ) 
               , new oRef( new oMulti( true,false,true,false ) // derived from authentic
                   , new object("idDocument", array())
                   ) 
               , new oRef( new oMulti( false,true,false,true ) // derived from inhabitant
                   , new object("residence", array())
                   ) 
               ), "Person.php")
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
       ), "Application.php");
  }

  class Application {
    var $id;
    var $applicant;
    var $checked;
    var $kind;
    var $assigned;
    var $decision;
    function Application($id=null, $applicant=array(), $checked=array(), $kind=array(), $assigned=array(), $decision=array()){
        $this->id=$id;
        $this->applicant=$applicant;
        $this->checked=$checked;
        $this->kind=$kind;
        $this->assigned=$assigned;
        $this->decision=$decision;
    }
    function add_applicant(Application_applicant $applicant){
      return $this->applicant[]=$applicant;
    }
    function read_applicant($id){
      $obj = new Application_applicant($id, array(), array(), array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttP_erson1 AS AttP_erson
                           FROM 
                             ( SELECT DISTINCT AttP_erson, AttP_erson AS AttP_erson1
                                 FROM C1_P_erson
                                WHERE 1
                             ) AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_person($v['AttP_erson']);
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttID_document
                           FROM T1_authentic AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_idDocument($v['AttID_document']);
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_rea
                           FROM T8_inhabitant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_residence($v['AttA_rea']);
      }
      $this->add_applicant($obj);
      return $obj;
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
    function add_checked(Application_checked $checked){
      return $this->checked[]=$checked;
    }
    function read_checked($id){
      $obj = new Application_checked($id);
      $this->add_checked($obj);
      return $obj;
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
    function add_kind(Application_kind $kind){
      return $this->kind[]=$kind;
    }
    function read_kind($id){
      $obj = new Application_kind($id);
      $this->add_kind($obj);
      return $obj;
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
    function add_assigned(Application_assigned $assigned){
      return $this->assigned[]=$assigned;
    }
    function read_assigned($id){
      $obj = new Application_assigned($id);
      $this->add_assigned($obj);
      return $obj;
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
    function add_decision(Application_decision $decision){
      return $this->decision[]=$decision;
    }
    function read_decision($id){
      $obj = new Application_decision($id);
      $this->add_decision($obj);
      return $obj;
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
    function readGen($type,$value){
      if($type=='applicant') return $this->read_applicant($value);
      if($type=='checked') return $this->read_checked($value);
      if($type=='kind') return $this->read_kind($value);
      if($type=='assigned') return $this->read_assigned($value);
      if($type=='decision') return $this->read_decision($value);
      else return false;
    }
  }
  class Application_applicant {
    var $id;
    var $person;
    var $idDocument;
    var $residence;
    function Application_applicant($id=null, $person=array(), $idDocument=array(), $residence=array()){
        $this->id=$id;
        $this->person=$person;
        $this->idDocument=$idDocument;
        $this->residence=$residence;
    }
    function add_person(Application_applicant_person $person){
      return $this->person[]=$person;
    }
    function read_person($id){
      $obj = new Application_applicant_person($id);
      $this->add_person($obj);
      return $obj;
    }
    function getEach_person(){
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
    function add_idDocument(Application_applicant_idDocument $idDocument){
      return $this->idDocument[]=$idDocument;
    }
    function read_idDocument($id){
      $obj = new Application_applicant_idDocument($id);
      $this->add_idDocument($obj);
      return $obj;
    }
    function getEach_idDocument(){
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
    function add_residence(Application_applicant_residence $residence){
      return $this->residence[]=$residence;
    }
    function read_residence($id){
      $obj = new Application_applicant_residence($id);
      $this->add_residence($obj);
      return $obj;
    }
    function getEach_residence(){
      // currently, this returns all concepts.. why not let it return only the valid ones?
      $v = DB_doquer('SELECT DISTINCT AttA_rea
                                FROM C7_A_rea
                               WHERE 1');
      $res = array();
      foreach($v as $i=>$j){
        $res[]=$j['AttA_rea'];
      }
      return $res;
    }
    function addGen($type,$value){
      if($type=='person') return $this->add_person($value);
      if($type=='idDocument') return $this->add_idDocument($value);
      if($type=='residence') return $this->add_residence($value);
      else return false;
    }
    function readGen($type,$value){
      if($type=='person') return $this->read_person($value);
      if($type=='idDocument') return $this->read_idDocument($value);
      if($type=='residence') return $this->read_residence($value);
      else return false;
    }
  }
  class Application_applicant_person {
    var $id;
    function Application_applicant_person($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  class Application_applicant_idDocument {
    var $id;
    function Application_applicant_idDocument($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  class Application_applicant_residence {
    var $id;
    function Application_applicant_residence($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  class Application_checked {
    var $id;
    function Application_checked($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  class Application_kind {
    var $id;
    function Application_kind($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  class Application_assigned {
    var $id;
    function Application_assigned($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  class Application_decision {
    var $id;
    function Application_decision($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
    }
  }
  function getEachApplication(){
      return DB_doquer('SELECT DISTINCT AttA_pplication
                           FROM C3_A_pplication
                          WHERE 1');
  }
  function createApplication(Application &$obj){
      return updateApplication($obj,true);
  }
  function readApplication($id){
      // check existence of $id
      $ctx = DB_doquer('SELECT DISTINCT isect0.AttA_pplication
                           FROM C3_A_pplication AS isect0
                          WHERE isect0.AttA_pplication = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj = new Application($id, array(), array(), array(), array(), array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_applicant($v['AttP_erson']);
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_checked($v['AttID_document']);
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_kind($v['AttP_roduct']);
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_assigned($v['AttE_mployee']);
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_decision($v['AttD_ecision']);
      }
      return $obj;
  }
  function updateApplication(Application $Application,$new=false){
      global $DB_link,$DB_err,$DB_lastquer;
      $preErr= $new ? 'Cannot create new Application: ':'Cannot update Application: ';
      DB_doquer('START TRANSACTION');
      if($new){ // create a new object
        if(!isset($Application->id)){ // find a unique id
           $nextNum = DB_doquer('SELECT max(1+AttA_pplication) FROM C3_A_pplication GROUP BY \'1\'');
           $Application->id = @$nextNum[0][0]+0;
        }
        if(DB_plainquer('INSERT INTO C3_A_pplication (AttA_pplication) VALUES (\''.addslashes($Application->id).'\')',$errno)===false){
            $DB_err=$preErr.(($errno==1062) ? 'Application \''.$Application->id.'\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);
            DB_doquer('ROLLBACK');
            return false;
        }
      }else
      if(!$new){
        // destroy old attribute values
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_erson']).'\'';
        }
        $Application_applicant_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T2_applicant WHERE AttA_pplication=\''.addslashes($Application->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttID_document']).'\'';
        }
        $Application_checked_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T3_checked WHERE AttA_pplication=\''.addslashes($Application->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $Application_kind_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T6_kind WHERE AttA_pplication=\''.addslashes($Application->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttE_mployee']).'\'';
        }
        $Application_assigned_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T4_assigned WHERE AttA_pplication=\''.addslashes($Application->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttD_ecision']).'\'';
        }
        $Application_decision_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T10_leadsto WHERE AttA_pplication=\''.addslashes($Application->id).'\'');
      }
      foreach($Application->applicant as $i=>$Application_applicant){
        if(isset($Application_applicant->person[0]->id)){
          if(count(DB_doquer('SELECT DISTINCT isect0.AttP_erson
                           FROM C1_P_erson AS isect0
                          WHERE isect0.AttP_erson = \''.addslashes($Application_applicant->person[0]->id).'\''))==0)
            DB_doquer('INSERT IGNORE INTO C1_P_erson (AttP_erson) VALUES (\''.addslashes($Application_applicant->id).'\')');
            if(!$new){
              // destroy old attribute values
            }
        }
        $Application_applicant->id = @$Application_applicant->person[0]->id;
        if(!isset($Application_applicant->id)){
           $nextNum = DB_doquer('SELECT max(1+AttP_erson) FROM C1_P_erson GROUP BY \'1\'');
           $Application_applicant->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C1_P_erson (AttP_erson) VALUES (\''.addslashes($Application_applicant->id).'\')');
        DB_doquer('INSERT IGNORE INTO T2_applicant (AttA_pplication,AttP_erson) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($Application_applicant->id).'\')');
          if(!$new){
            // destroy old attribute values
            $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttID_document
                           FROM T1_authentic AS fst
                          WHERE fst.AttP_erson = \''.addslashes($Application_applicant->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttID_document']).'\'';
            }
            $Application_applicant_idDocument_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T1_authentic WHERE AttP_erson=\''.addslashes($Application_applicant->id).'\'');
            $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_rea
                           FROM T8_inhabitant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($Application_applicant->id).'\'');
            $arr=array();
            foreach($effected as $i=>$v){
                $arr[]='\''.addslashes($v['AttA_rea']).'\'';
            }
            $Application_applicant_residence_str=join(',',$arr);
            DB_doquer( 'DELETE FROM T8_inhabitant WHERE AttP_erson=\''.addslashes($Application_applicant->id).'\'');
          }
          foreach($Application_applicant->idDocument as $i=>$Application_applicant_idDocument){
            if(!isset($Application_applicant_idDocument->id)){
               $nextNum = DB_doquer('SELECT max(1+AttID_document) FROM C2_ID_document GROUP BY \'1\'');
               $Application_applicant_idDocument->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C2_ID_document (AttID_document) VALUES (\''.addslashes($Application_applicant_idDocument->id).'\')');
            DB_doquer('INSERT IGNORE INTO T1_authentic (AttP_erson,AttID_document) VALUES (\''.addslashes($Application_applicant->id).'\',\''.addslashes($Application_applicant_idDocument->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          foreach($Application_applicant->residence as $i=>$Application_applicant_residence){
            if(!isset($Application_applicant_residence->id)){
               $nextNum = DB_doquer('SELECT max(1+AttA_rea) FROM C7_A_rea GROUP BY \'1\'');
               $Application_applicant_residence->id = @$nextNum[0][0]+0;
            }
            DB_doquer('INSERT IGNORE INTO C7_A_rea (AttA_rea) VALUES (\''.addslashes($Application_applicant_residence->id).'\')');
            DB_doquer('INSERT IGNORE INTO T8_inhabitant (AttP_erson,AttA_rea) VALUES (\''.addslashes($Application_applicant->id).'\',\''.addslashes($Application_applicant_residence->id).'\')');
              if(!$new){
                // destroy old attribute values
              }
          }
          if(!$new && strlen($Application_applicant_idDocument_str))
            DB_doquer('DELETE FROM C2_ID_document
              WHERE AttID_document IN ('.$Application_applicant_idDocument_str.')
                AND NOT EXISTS (SELECT * FROM T3_checked
                                 WHERE C2_ID_document.AttID_document = T3_checked.AttID_document
                               )
                AND NOT EXISTS (SELECT * FROM T1_authentic
                                 WHERE C2_ID_document.AttID_document = T1_authentic.AttID_document
                               )
            ');
          if(!$new && strlen($Application_applicant_residence_str))
            DB_doquer('DELETE FROM C7_A_rea
              WHERE AttA_rea IN ('.$Application_applicant_residence_str.')
                AND NOT EXISTS (SELECT * FROM T8_inhabitant
                                 WHERE C7_A_rea.AttA_rea = T8_inhabitant.AttA_rea
                               )
                AND NOT EXISTS (SELECT * FROM T9_area
                                 WHERE C7_A_rea.AttA_rea = T9_area.AttA_rea
                               )
            ');
      }
      foreach($Application->checked as $i=>$Application_checked){
        if(!isset($Application_checked->id)){
           $nextNum = DB_doquer('SELECT max(1+AttID_document) FROM C2_ID_document GROUP BY \'1\'');
           $Application_checked->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C2_ID_document (AttID_document) VALUES (\''.addslashes($Application_checked->id).'\')');
        DB_doquer('INSERT IGNORE INTO T3_checked (AttA_pplication,AttID_document) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($Application_checked->id).'\')');
          if(!$new){
            // destroy old attribute values
          }
      }
      foreach($Application->kind as $i=>$Application_kind){
        if(!isset($Application_kind->id)){
           $nextNum = DB_doquer('SELECT max(1+AttP_roduct) FROM C5_P_roduct GROUP BY \'1\'');
           $Application_kind->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C5_P_roduct (AttP_roduct) VALUES (\''.addslashes($Application_kind->id).'\')');
        DB_doquer('INSERT IGNORE INTO T6_kind (AttA_pplication,AttP_roduct) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($Application_kind->id).'\')');
          if(!$new){
            // destroy old attribute values
          }
      }
      foreach($Application->assigned as $i=>$Application_assigned){
        if(!isset($Application_assigned->id)){
           $nextNum = DB_doquer('SELECT max(1+AttE_mployee) FROM C4_E_mployee GROUP BY \'1\'');
           $Application_assigned->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C4_E_mployee (AttE_mployee) VALUES (\''.addslashes($Application_assigned->id).'\')');
        DB_doquer('INSERT IGNORE INTO T4_assigned (AttA_pplication,AttE_mployee) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($Application_assigned->id).'\')');
          if(!$new){
            // destroy old attribute values
          }
      }
      foreach($Application->decision as $i=>$Application_decision){
        if(!isset($Application_decision->id)){
           $nextNum = DB_doquer('SELECT max(1+AttD_ecision) FROM C6_D_ecision GROUP BY \'1\'');
           $Application_decision->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C6_D_ecision (AttD_ecision) VALUES (\''.addslashes($Application_decision->id).'\')');
        DB_doquer('INSERT IGNORE INTO T10_leadsto (AttA_pplication,AttD_ecision) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($Application_decision->id).'\')');
          if(!$new){
            // destroy old attribute values
          }
      }
      if(!$new && strlen($Application_applicant_str))
        DB_doquer('DELETE FROM C1_P_erson
          WHERE AttP_erson IN ('.$Application_applicant_str.')
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
      if(!$new && strlen($Application_checked_str))
        DB_doquer('DELETE FROM C2_ID_document
          WHERE AttID_document IN ('.$Application_checked_str.')
            AND NOT EXISTS (SELECT * FROM T3_checked
                             WHERE C2_ID_document.AttID_document = T3_checked.AttID_document
                           )
            AND NOT EXISTS (SELECT * FROM T1_authentic
                             WHERE C2_ID_document.AttID_document = T1_authentic.AttID_document
                           )
        ');
      if(!$new && strlen($Application_kind_str))
        DB_doquer('DELETE FROM C5_P_roduct
          WHERE AttP_roduct IN ('.$Application_kind_str.')
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
      if(!$new && strlen($Application_assigned_str))
        DB_doquer('DELETE FROM C4_E_mployee
          WHERE AttE_mployee IN ('.$Application_assigned_str.')
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
      if(!$new && strlen($Application_decision_str))
        DB_doquer('DELETE FROM C6_D_ecision
          WHERE AttD_ecision IN ('.$Application_decision_str.')
            AND NOT EXISTS (SELECT * FROM T7_kind
                             WHERE C6_D_ecision.AttD_ecision = T7_kind.AttD_ecision
                           )
            AND NOT EXISTS (SELECT * FROM T10_leadsto
                             WHERE C6_D_ecision.AttD_ecision = T10_leadsto.AttD_ecision
                           )
        ');
    if (!checkRule1()){
      $DB_err=$preErr.'"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product."';
    } else
    if (!checkRule2()){
      $DB_err=$preErr.'"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."';
    } else
    if (!checkRule3()){
      $DB_err=$preErr.'"An application for a permit is accepted only from individuals whose identity is authenticated."';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'"Applications for permits are treated by authorized personnel only."';
    } else
    if (!checkRule5()){
      $DB_err=$preErr.'"authentic[Person*IDdocument] is injective"';
    } else
    if (!checkRule6()){
      $DB_err=$preErr.'"authentic[Person*IDdocument] is surjective"';
    } else
    if (!checkRule7()){
      $DB_err=$preErr.'"applicant[Application*Person] is univalent"';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'"applicant[Application*Person] is total"';
    } else
    if (!checkRule9()){
      $DB_err=$preErr.'"checked[Application*IDdocument] is univalent"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'"assigned[Application*Employee] is univalent"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'"kind[Application*Product] is univalent"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'"kind[Application*Product] is total"';
    } else
    if (!checkRule13()){
      $DB_err=$preErr.'"kind[Decision*Product] is univalent"';
    } else
    if (!checkRule14()){
      $DB_err=$preErr.'"kind[Decision*Product] is total"';
    } else
    if (!checkRule15()){
      $DB_err=$preErr.'"inhabitant[Person*Area] is univalent"';
    } else
    if (!checkRule16()){
      $DB_err=$preErr.'"inhabitant[Person*Area] is total"';
    } else
    if (!checkRule17()){
      $DB_err=$preErr.'"area[Employee*Area] is surjective"';
    } else
    if (!checkRule18()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is injective"';
    } else
    if (!checkRule19()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is univalent"';
    } else
    if (!checkRule20()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is surjective"';
    } else
      if(true){ // all rules are met
          DB_doquer('COMMIT');
          return $Application->id;
      }
      DB_doquer('ROLLBACK');
      return false;
  }
  function deleteApplication($id){
    global $DB_err;
    $preErr= 'Cannot delete Application: ';
    DB_doquer('START TRANSACTION');
    
  /*
  applicant
  checked
  kind
  assigned
  leadsto
  *************
  leadsto
  kind
  assigned
  applicant
  checked
  I
  V
  */
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_erson']).'\'';
        }
        $applicant_str=join(',',$arr);
        DB_doquer ('DELETE FROM T2_applicant WHERE AttA_pplication=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttID_document']).'\'';
        }
        $checked_str=join(',',$arr);
        DB_doquer ('DELETE FROM T3_checked WHERE AttA_pplication=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $kind_str=join(',',$arr);
        DB_doquer ('DELETE FROM T6_kind WHERE AttA_pplication=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttE_mployee']).'\'';
        }
        $assigned_str=join(',',$arr);
        DB_doquer ('DELETE FROM T4_assigned WHERE AttA_pplication=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttD_ecision']).'\'';
        }
        $decision_str=join(',',$arr);
        DB_doquer ('DELETE FROM T10_leadsto WHERE AttA_pplication=\''.addslashes($id).'\'');
    DB_doquer('DELETE FROM C3_A_pplication WHERE AttA_pplication=\''.addslashes($id).'\'');
    if(strlen($applicant_str))
      DB_doquer('DELETE FROM C1_P_erson
        WHERE AttP_erson IN ('.$applicant_str.')
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
    if(strlen($checked_str))
      DB_doquer('DELETE FROM C2_ID_document
        WHERE AttID_document IN ('.$checked_str.')
          AND NOT EXISTS (SELECT * FROM T3_checked
                           WHERE C2_ID_document.AttID_document = T3_checked.AttID_document
                         )
          AND NOT EXISTS (SELECT * FROM T1_authentic
                           WHERE C2_ID_document.AttID_document = T1_authentic.AttID_document
                         )
      ');
    if(strlen($kind_str))
      DB_doquer('DELETE FROM C5_P_roduct
        WHERE AttP_roduct IN ('.$kind_str.')
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
    if(strlen($assigned_str))
      DB_doquer('DELETE FROM C4_E_mployee
        WHERE AttE_mployee IN ('.$assigned_str.')
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
    if(strlen($decision_str))
      DB_doquer('DELETE FROM C6_D_ecision
        WHERE AttD_ecision IN ('.$decision_str.')
          AND NOT EXISTS (SELECT * FROM T7_kind
                           WHERE C6_D_ecision.AttD_ecision = T7_kind.AttD_ecision
                         )
          AND NOT EXISTS (SELECT * FROM T10_leadsto
                           WHERE C6_D_ecision.AttD_ecision = T10_leadsto.AttD_ecision
                         )
      ');
    if (!checkRule1()){
      $DB_err=$preErr.'"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product."';
    } else
    if (!checkRule2()){
      $DB_err=$preErr.'"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."';
    } else
    if (!checkRule3()){
      $DB_err=$preErr.'"An application for a permit is accepted only from individuals whose identity is authenticated."';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'"Applications for permits are treated by authorized personnel only."';
    } else
    if (!checkRule5()){
      $DB_err=$preErr.'"authentic[Person*IDdocument] is injective"';
    } else
    if (!checkRule6()){
      $DB_err=$preErr.'"authentic[Person*IDdocument] is surjective"';
    } else
    if (!checkRule7()){
      $DB_err=$preErr.'"applicant[Application*Person] is univalent"';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'"applicant[Application*Person] is total"';
    } else
    if (!checkRule9()){
      $DB_err=$preErr.'"checked[Application*IDdocument] is univalent"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'"assigned[Application*Employee] is univalent"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'"kind[Application*Product] is univalent"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'"kind[Application*Product] is total"';
    } else
    if (!checkRule13()){
      $DB_err=$preErr.'"kind[Decision*Product] is univalent"';
    } else
    if (!checkRule14()){
      $DB_err=$preErr.'"kind[Decision*Product] is total"';
    } else
    if (!checkRule15()){
      $DB_err=$preErr.'"inhabitant[Person*Area] is univalent"';
    } else
    if (!checkRule16()){
      $DB_err=$preErr.'"inhabitant[Person*Area] is total"';
    } else
    if (!checkRule17()){
      $DB_err=$preErr.'"area[Employee*Area] is surjective"';
    } else
    if (!checkRule18()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is injective"';
    } else
    if (!checkRule19()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is univalent"';
    } else
    if (!checkRule20()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is surjective"';
    } else
    if(true) {
      DB_doquer('COMMIT');
      return true;
    }
    DB_doquer('ROLLBACK');
    return false;
  }
?>