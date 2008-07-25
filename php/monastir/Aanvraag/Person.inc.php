<?php // generated with ADL vs. 0.8.09
  
  /********* file Aanvraag on line line 12, file "Aanvraag.adl"
   * OBJECT Person Person
   *     idDocument : authentic
   *     residence : inhabitant
   *     application : applicant~
   * ENDOBJECT
   *********/
  
  function getobject_Person(){
    return new object("Person",array
      (new oRef( new oMulti( true,false,true,false ) // derived from authentic
             , new object("idDocument",array()) // IDdocument
             )
      ,new oRef( new oMulti( false,true,false,true ) // derived from inhabitant
             , new object("residence",array()) // Area
             )
      ,new oRef( new oMulti( true,false,true,false ) // derived from applicant~
             , new object("application",array(), "Application.php") // Application
             )
      ), "Person.php");
  }
  
  class Person {
    var $id;
    var $idDocument;
    var $residence;
    var $application;
    function Person($id=null, $idDocument=array(), $residence=array(), $application=array()){
        $this->id=$id;
        $this->idDocument=$idDocument;
        $this->residence=$residence;
        $this->application=$application;
    }
    function add_idDocument(Person_idDocument $idDocument){
      return $this->idDocument[]=$idDocument;
    }
    function add_residence(Person_residence $residence){
      return $this->residence[]=$residence;
    }
    function add_application(Person_application $application){
      return $this->application[]=$application;
    }
    function addGen($type,$value){
      if($type=='idDocument') return $this->add_idDocument($value);
      if($type=='residence') return $this->add_residence($value);
      if($type=='application') return $this->add_application($value);
      else return false;
    }
  }
  class Person_idDocument {
      var $id;
      function Person_idDocument($id) {
          $this->id=$id;
      }
  }
  class Person_residence {
      var $id;
      function Person_residence($id) {
          $this->id=$id;
      }
  }
  class Person_application {
      var $id;
      function Person_application($id) {
          $this->id=$id;
      }
  }
  function getEachPerson(){
      return DB_doquer('SELECT DISTINCT AttP_erson
                           FROM C1_P_erson
                          WHERE 1');
  }
  function createPerson(Person &$obj){
      return updatePerson($obj,true);
  }
  function readPerson($id){
      $ctx = DB_doquer('SELECT DISTINCT isect0.AttP_erson
                           FROM C1_P_erson AS isect0
                          WHERE isect0.AttP_erson = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj = new Person($id, array(), array(), array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttID_document
                           FROM T1_authentic AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_idDocument(new Person_idDocument($v['AttID_document']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_rea
                           FROM T8_inhabitant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_residence(new Person_residence($v['AttA_rea']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_pplication
                           FROM T2_applicant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_application(new Person_application($v['AttA_pplication']));
      }
      return $obj;
  }
  function updatePerson(Person $Person,$new=false){
      global $DB_link,$DB_err,$DB_lastquer;
      $preErr= $new ? 'Cannot create new Person: ':'Cannot update Person: ';
      DB_doquer('START TRANSACTION');
      if($new){ // create a new object
        if(!isset($Person->id)){ // find a unique id
           $nextNum = DB_doquer('SELECT max(1+AttP_erson) FROM C1_P_erson GROUP BY \'1\'');
           $Person->id = $nextNum[0][0];
        }
        if(DB_plainquer('INSERT INTO C1_P_erson (AttP_erson) VALUES (\''.addslashes($Person->id).'\')',$errno)===false){
            $DB_err=$preErr.(($errno==1062) ? 'Person \''.$Person->id.'\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);
            DB_doquer('ROLLBACK');
            return false;
        }
      }else{
        // destroy old attribute values
        $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttID_document
                           FROM T1_authentic AS fst
                          WHERE fst.AttP_erson = \''.addslashes($Person->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttID_document']).'\'';
        }
        $idDocument_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T1_authentic WHERE AttP_erson=\''.addslashes($Person->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_rea
                           FROM T8_inhabitant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($Person->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_rea']).'\'';
        }
        $residence_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T8_inhabitant WHERE AttP_erson=\''.addslashes($Person->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_pplication
                           FROM T2_applicant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($Person->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_pplication']).'\'';
        }
        $application_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T2_applicant WHERE AttP_erson=\''.addslashes($Person->id).'\'');
      }
      foreach($Person->idDocument as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttID_document) FROM C2_ID_document GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C2_ID_document (AttID_document) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T1_authentic (AttP_erson,AttID_document) VALUES (\''.addslashes($Person->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Person->residence as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttA_rea) FROM C7_A_rea GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C7_A_rea (AttA_rea) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T8_inhabitant (AttP_erson,AttA_rea) VALUES (\''.addslashes($Person->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Person->application as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttA_pplication) FROM C3_A_pplication GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C3_A_pplication (AttA_pplication) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T2_applicant (AttP_erson,AttA_pplication) VALUES (\''.addslashes($Person->id).'\',\''.addslashes($v->id).'\')');
      }
      if(!$new && strlen($idDocument_str))
        DB_doquer('DELETE FROM C2_ID_document
          WHERE AttID_document IN ('.$idDocument_str.')
            AND NOT EXISTS (SELECT * FROM T1_authentic
                             WHERE C2_ID_document.AttID_document = T1_authentic.AttID_document
                           )
            AND NOT EXISTS (SELECT * FROM T3_checked
                             WHERE C2_ID_document.AttID_document = T3_checked.AttID_document
                           )
        ');
      if(!$new && strlen($residence_str))
        DB_doquer('DELETE FROM C7_A_rea
          WHERE AttA_rea IN ('.$residence_str.')
            AND NOT EXISTS (SELECT * FROM T8_inhabitant
                             WHERE C7_A_rea.AttA_rea = T8_inhabitant.AttA_rea
                           )
            AND NOT EXISTS (SELECT * FROM T9_area
                             WHERE C7_A_rea.AttA_rea = T9_area.AttA_rea
                           )
        ');
      if(!$new && strlen($application_str))
        DB_doquer('DELETE FROM C3_A_pplication
          WHERE AttA_pplication IN ('.$application_str.')
            AND NOT EXISTS (SELECT * FROM T6_kind
                             WHERE C3_A_pplication.AttA_pplication = T6_kind.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T2_applicant
                             WHERE C3_A_pplication.AttA_pplication = T2_applicant.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T3_checked
                             WHERE C3_A_pplication.AttA_pplication = T3_checked.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T4_assigned
                             WHERE C3_A_pplication.AttA_pplication = T4_assigned.AttA_pplication
                           )
            AND NOT EXISTS (SELECT * FROM T10_leadsto
                             WHERE C3_A_pplication.AttA_pplication = T10_leadsto.AttA_pplication
                           )
        ');
    if (!checkRule2()){
      $DB_err=$preErr.'\"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.\"';
    } else
    if (!checkRule3()){
      $DB_err=$preErr.'\"An application for a permit is accepted only from individuals whose identity is authenticated.\"';
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
    if (!checkRule15()){
      $DB_err=$preErr.'\"inhabitant[Person*Area] is univalent\"';
    } else
    if (!checkRule16()){
      $DB_err=$preErr.'\"inhabitant[Person*Area] is total\"';
    } else
      if(true){ // all rules are met
          DB_doquer('COMMIT');
          return $Person->id;
      }
      DB_doquer('ROLLBACK');
      return false;
  }
  function deletePerson($id){
    global $DB_err;
    DB_doquer('START TRANSACTION');
    
        $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttID_document
                           FROM T1_authentic AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttID_document']).'\'';
        }
        $idDocument_str=join(',',$arr);
        DB_doquer ('DELETE FROM T1_authentic WHERE AttP_erson=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_rea
                           FROM T8_inhabitant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_rea']).'\'';
        }
        $residence_str=join(',',$arr);
        DB_doquer ('DELETE FROM T8_inhabitant WHERE AttP_erson=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttP_erson, fst.AttA_pplication
                           FROM T2_applicant AS fst
                          WHERE fst.AttP_erson = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_pplication']).'\'';
        }
        $application_str=join(',',$arr);
        DB_doquer ('DELETE FROM T2_applicant WHERE AttP_erson=\''.addslashes($id).'\'');
    DB_doquer('DELETE FROM C1_P_erson WHERE AttP_erson=\''.addslashes($id).'\'');
    if(strlen($idDocument_str))
      DB_doquer('DELETE FROM C2_ID_document
        WHERE AttID_document IN ('.$idDocument_str.')
          AND NOT EXISTS (SELECT * FROM T1_authentic
                           WHERE C2_ID_document.AttID_document = T1_authentic.AttID_document
                         )
          AND NOT EXISTS (SELECT * FROM T3_checked
                           WHERE C2_ID_document.AttID_document = T3_checked.AttID_document
                         )
      ');
    if(strlen($residence_str))
      DB_doquer('DELETE FROM C7_A_rea
        WHERE AttA_rea IN ('.$residence_str.')
          AND NOT EXISTS (SELECT * FROM T8_inhabitant
                           WHERE C7_A_rea.AttA_rea = T8_inhabitant.AttA_rea
                         )
          AND NOT EXISTS (SELECT * FROM T9_area
                           WHERE C7_A_rea.AttA_rea = T9_area.AttA_rea
                         )
      ');
    if(strlen($application_str))
      DB_doquer('DELETE FROM C3_A_pplication
        WHERE AttA_pplication IN ('.$application_str.')
          AND NOT EXISTS (SELECT * FROM T6_kind
                           WHERE C3_A_pplication.AttA_pplication = T6_kind.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T2_applicant
                           WHERE C3_A_pplication.AttA_pplication = T2_applicant.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T3_checked
                           WHERE C3_A_pplication.AttA_pplication = T3_checked.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T4_assigned
                           WHERE C3_A_pplication.AttA_pplication = T4_assigned.AttA_pplication
                         )
          AND NOT EXISTS (SELECT * FROM T10_leadsto
                           WHERE C3_A_pplication.AttA_pplication = T10_leadsto.AttA_pplication
                         )
      ');
    if (!checkRule2()){
      $DB_err=$preErr.'\"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.\"';
    } else
    if (!checkRule3()){
      $DB_err=$preErr.'\"An application for a permit is accepted only from individuals whose identity is authenticated.\"';
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
    if (!checkRule15()){
      $DB_err=$preErr.'\"inhabitant[Person*Area] is univalent\"';
    } else
    if (!checkRule16()){
      $DB_err=$preErr.'\"inhabitant[Person*Area] is total\"';
    } else
    if(true) {
      DB_doquer('COMMIT');
      return true;
    }
    DB_doquer('ROLLBACK');
    return false;
  }
?>