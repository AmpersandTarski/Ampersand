<?php // generated with ADL vs. 0.8.09
  
  /********* file Aanvraag on line line 18, file "Aanvraag.adl"
   * OBJECT Application Application
   *     applicant : applicant
   *     checked : checked
   *     kind : kind
   *     assigned : assigned
   *     decision : leadsto
   * ENDOBJECT
   *********/
  
  function getobject_Application(){
    return new object("Application",array
      (new oRef( new oMulti( false,true,false,true ) // derived from applicant
             , new object("applicant",array(), "Person.php") // Person
             )
      ,new oRef( new oMulti( false,false,false,true ) // derived from checked
             , new object("checked",array()) // IDdocument
             )
      ,new oRef( new oMulti( false,true,false,true ) // derived from kind
             , new object("kind",array()) // Product
             )
      ,new oRef( new oMulti( true,false,false,false ) // derived from assigned
             , new object("assigned",array(), "Behandelaar.php") // Employee
             )
      ,new oRef( new oMulti( true,true,true,false ) // derived from leadsto
             , new object("decision",array(), "Decision.php") // Decision
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
    function add_checked(Application_checked $checked){
      return $this->checked[]=$checked;
    }
    function add_kind(Application_kind $kind){
      return $this->kind[]=$kind;
    }
    function add_assigned(Application_assigned $assigned){
      return $this->assigned[]=$assigned;
    }
    function add_decision(Application_decision $decision){
      return $this->decision[]=$decision;
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
  class Application_applicant {
      var $id;
      function Application_applicant($id) {
          $this->id=$id;
      }
  }
  class Application_checked {
      var $id;
      function Application_checked($id) {
          $this->id=$id;
      }
  }
  class Application_kind {
      var $id;
      function Application_kind($id) {
          $this->id=$id;
      }
  }
  class Application_assigned {
      var $id;
      function Application_assigned($id) {
          $this->id=$id;
      }
  }
  class Application_decision {
      var $id;
      function Application_decision($id) {
          $this->id=$id;
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
      $ctx = DB_doquer('SELECT DISTINCT isect0.AttA_pplication
                           FROM C3_A_pplication AS isect0
                          WHERE isect0.AttA_pplication = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj = new Application($id, array(), array(), array(), array(), array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_applicant(new Application_applicant($v['AttP_erson']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_checked(new Application_checked($v['AttID_document']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_kind(new Application_kind($v['AttP_roduct']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_assigned(new Application_assigned($v['AttE_mployee']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_decision(new Application_decision($v['AttD_ecision']));
      }
      return $obj;
  }
  function updateApplication(Application $Application,$new=false){
      global $DB_link,$DB_err,$DB_lastquer;
      $preErr= $new ? 'Cannot create new Application: ':'Cannot update Application: ';
      DB_doquer('START TRANSACTION');
      if($new){ // create a new object
        if(!isset($Application)){ // find a unique id
           $nextNum = DB_doquer('SELECT max(1+AttA_pplication) FROM C3_A_pplication GROUP BY \'1\'');
           $Application->id = $nextNum[0][0];
        }
        if(DB_plainquer('INSERT INTO C3_A_pplication (AttA_pplication) VALUES (\''.addslashes($Application->id).'\')',$errno)===false){
            $DB_err=$preErr.(($errno==1062) ? 'Application \''.$Application->id.'\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);
            DB_doquer('ROLLBACK');
            return false;
        }
      }else{
        // destroy old attribute values
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_erson']).'\'';
        }
        $applicant_str=join(',',$arr);
        DB_doquer('DELETE FROM T2_applicant WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttID_document']).'\'';
        }
        $checked_str=join(',',$arr);
        DB_doquer('DELETE FROM T3_checked WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $kind_str=join(',',$arr);
        DB_doquer('DELETE FROM T6_kind WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttE_mployee']).'\'';
        }
        $assigned_str=join(',',$arr);
        DB_doquer('DELETE FROM T4_assigned WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($Application->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttD_ecision']).'\'';
        }
        $decision_str=join(',',$arr);
        DB_doquer('DELETE FROM T10_leadsto WHERE AttA_pplication=\'addslashes($id)\'');
      }
      foreach($Application->applicant as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttP_erson) FROM C1_P_erson GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C1_P_erson (AttP_erson) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T2_applicant (AttA_pplication,AttP_erson) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Application->checked as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttID_document) FROM C2_ID_document GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C2_ID_document (AttID_document) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T3_checked (AttA_pplication,AttID_document) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Application->kind as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttP_roduct) FROM C5_P_roduct GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C5_P_roduct (AttP_roduct) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T6_kind (AttA_pplication,AttP_roduct) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Application->assigned as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttE_mployee) FROM C4_E_mployee GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C4_E_mployee (AttE_mployee) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T4_assigned (AttA_pplication,AttE_mployee) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Application->decision as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttD_ecision) FROM C6_D_ecision GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C6_D_ecision (AttD_ecision) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T10_leadsto (AttA_pplication,AttD_ecision) VALUES (\''.addslashes($Application->id).'\',\''.addslashes($v->id).'\')');
      }
      if(!$new && strlen($applicant_str))
        DB_doquer('DELETE FROM C1_P_erson
          WHERE AttP_erson IN ('.$applicant_str.')
            AND NOT EXISTS (SELECT * FROM T8_inhabitant
                             WHERE C1_P_erson.AttP_erson = T8_inhabitant.AttA_rea
                           )
            AND NOT EXISTS (SELECT * FROM T1_authentic
                             WHERE C1_P_erson.AttP_erson = T1_authentic.AttID_document
                           )
            AND NOT EXISTS (SELECT * FROM T2_applicant
                             WHERE C1_P_erson.AttP_erson = T2_applicant.AttA_pplication
                           )
        ');
      if(!$new && strlen($checked_str))
        DB_doquer('DELETE FROM C2_ID_document
          WHERE AttID_document IN ('.$checked_str.')
            AND NOT EXISTS (SELECT * FROM T1_authentic
                             WHERE C2_ID_document.AttID_document = T1_authentic.AttP_erson
                           )
            AND NOT EXISTS (SELECT * FROM T3_checked
                             WHERE C2_ID_document.AttID_document = T3_checked.AttA_pplication
                           )
        ');
      if(!$new && strlen($kind_str))
        DB_doquer('DELETE FROM C5_P_roduct
          WHERE AttP_roduct IN ('.$kind_str.')
            AND NOT EXISTS (SELECT * FROM T5_auth
                             WHERE C5_P_roduct.AttP_roduct = T5_auth.AttE_mployee
                           )
            AND NOT EXISTS (SELECT * FROM T6_kind
                             WHERE C5_P_roduct.AttP_roduct = T6_kind.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T7_kind
                             WHERE C5_P_roduct.AttP_roduct = T7_kind.AttD_ecision
                           )
        ');
      if(!$new && strlen($assigned_str))
        DB_doquer('DELETE FROM C4_E_mployee
          WHERE AttE_mployee IN ('.$assigned_str.')
            AND NOT EXISTS (SELECT * FROM T4_assigned
                             WHERE C4_E_mployee.AttE_mployee = T4_assigned.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T9_area
                             WHERE C4_E_mployee.AttE_mployee = T9_area.AttA_rea
                           )
            AND NOT EXISTS (SELECT * FROM T5_auth
                             WHERE C4_E_mployee.AttE_mployee = T5_auth.AttP_roduct
                           )
        ');
      if(!$new && strlen($decision_str))
        DB_doquer('DELETE FROM C6_D_ecision
          WHERE AttD_ecision IN ('.$decision_str.')
            AND NOT EXISTS (SELECT * FROM T10_leadsto
                             WHERE C6_D_ecision.AttD_ecision = T10_leadsto.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T7_kind
                             WHERE C6_D_ecision.AttD_ecision = T7_kind.AttP_roduct
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
    if (!checkRule7()){
      $DB_err=$preErr.'\"applicant[Application*Person] is univalent\"';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'\"applicant[Application*Person] is total\"';
    } else
    if (!checkRule9()){
      $DB_err=$preErr.'\"checked[Application*IDdocument] is total\"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'\"assigned[Application*Employee] is injective\"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'\"kind[Application*Product] is univalent\"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'\"kind[Application*Product] is total\"';
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
          return $Application->id;
      }
      DB_doquer('ROLLBACK');
      return false;
  }
  function deleteApplication($id){
    global $DB_err;
    DB_doquer('START TRANSACTION');
    
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_erson
                           FROM T2_applicant AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_erson']).'\'';
        }
        $applicant_str=join(',',$arr);
        DB_doquer('DELETE FROM T2_applicant WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttID_document
                           FROM T3_checked AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttID_document']).'\'';
        }
        $checked_str=join(',',$arr);
        DB_doquer('DELETE FROM T3_checked WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttP_roduct
                           FROM T6_kind AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $kind_str=join(',',$arr);
        DB_doquer('DELETE FROM T6_kind WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttE_mployee
                           FROM T4_assigned AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttE_mployee']).'\'';
        }
        $assigned_str=join(',',$arr);
        DB_doquer('DELETE FROM T4_assigned WHERE AttA_pplication=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttA_pplication, fst.AttD_ecision
                           FROM T10_leadsto AS fst
                          WHERE fst.AttA_pplication = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttD_ecision']).'\'';
        }
        $decision_str=join(',',$arr);
        DB_doquer('DELETE FROM T10_leadsto WHERE AttA_pplication=\'addslashes($id)\'');
    DB_doquer('DELETE FROM C3_A_pplication WHERE AttA_pplication=\''.addslashes($id).'\'');
    if(strlen($applicant_str))
      DB_doquer('DELETE FROM C1_P_erson
        WHERE AttP_erson IN ('.$applicant_str.')
          AND NOT EXISTS (SELECT * FROM T8_inhabitant
                           WHERE C1_P_erson.AttP_erson = T8_inhabitant.AttA_rea
                         )
          AND NOT EXISTS (SELECT * FROM T1_authentic
                           WHERE C1_P_erson.AttP_erson = T1_authentic.AttID_document
                         )
          AND NOT EXISTS (SELECT * FROM T2_applicant
                           WHERE C1_P_erson.AttP_erson = T2_applicant.AttA_pplication
                         )
      ');
    if(strlen($checked_str))
      DB_doquer('DELETE FROM C2_ID_document
        WHERE AttID_document IN ('.$checked_str.')
          AND NOT EXISTS (SELECT * FROM T1_authentic
                           WHERE C2_ID_document.AttID_document = T1_authentic.AttP_erson
                         )
          AND NOT EXISTS (SELECT * FROM T3_checked
                           WHERE C2_ID_document.AttID_document = T3_checked.AttA_pplication
                         )
      ');
    if(strlen($kind_str))
      DB_doquer('DELETE FROM C5_P_roduct
        WHERE AttP_roduct IN ('.$kind_str.')
          AND NOT EXISTS (SELECT * FROM T5_auth
                           WHERE C5_P_roduct.AttP_roduct = T5_auth.AttE_mployee
                         )
          AND NOT EXISTS (SELECT * FROM T6_kind
                           WHERE C5_P_roduct.AttP_roduct = T6_kind.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T7_kind
                           WHERE C5_P_roduct.AttP_roduct = T7_kind.AttD_ecision
                         )
      ');
    if(strlen($assigned_str))
      DB_doquer('DELETE FROM C4_E_mployee
        WHERE AttE_mployee IN ('.$assigned_str.')
          AND NOT EXISTS (SELECT * FROM T4_assigned
                           WHERE C4_E_mployee.AttE_mployee = T4_assigned.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T9_area
                           WHERE C4_E_mployee.AttE_mployee = T9_area.AttA_rea
                         )
          AND NOT EXISTS (SELECT * FROM T5_auth
                           WHERE C4_E_mployee.AttE_mployee = T5_auth.AttP_roduct
                         )
      ');
    if(strlen($decision_str))
      DB_doquer('DELETE FROM C6_D_ecision
        WHERE AttD_ecision IN ('.$decision_str.')
          AND NOT EXISTS (SELECT * FROM T10_leadsto
                           WHERE C6_D_ecision.AttD_ecision = T10_leadsto.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T7_kind
                           WHERE C6_D_ecision.AttD_ecision = T7_kind.AttP_roduct
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
    if (!checkRule7()){
      $DB_err=$preErr.'\"applicant[Application*Person] is univalent\"';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'\"applicant[Application*Person] is total\"';
    } else
    if (!checkRule9()){
      $DB_err=$preErr.'\"checked[Application*IDdocument] is total\"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'\"assigned[Application*Employee] is injective\"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'\"kind[Application*Product] is univalent\"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'\"kind[Application*Product] is total\"';
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