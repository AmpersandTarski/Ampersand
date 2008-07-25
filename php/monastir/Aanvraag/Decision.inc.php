<?php // generated with ADL vs. 0.8.09
  
  /********* file Aanvraag on line line 26, file "Aanvraag.adl"
   * OBJECT Decision Decision
   *     application : leadsto~
   *     kind : kind
   * ENDOBJECT
   *********/
  
  function getobject_Decision(){
    return new object("Decision",array
      (new oRef( new oMulti( true,true,false,true ) // derived from leadsto~
             , new object("application",array(), "Application.php") // Application
             )
      ,new oRef( new oMulti( false,true,false,true ) // derived from kind
             , new object("kind",array()) // Product
             )
      ), "Decision.php");
  }
  
  class Decision {
    var $id;
    var $application;
    var $kind;
    function Decision($id=null, $application=array(), $kind=array()){
        $this->id=$id;
        $this->application=$application;
        $this->kind=$kind;
    }
    function add_application(Decision_application $application){
      return $this->application[]=$application;
    }
    function add_kind(Decision_kind $kind){
      return $this->kind[]=$kind;
    }
    function addGen($type,$value){
      if($type=='application') return $this->add_application($value);
      if($type=='kind') return $this->add_kind($value);
      else return false;
    }
  }
  class Decision_application {
      var $id;
      function Decision_application($id) {
          $this->id=$id;
      }
  }
  class Decision_kind {
      var $id;
      function Decision_kind($id) {
          $this->id=$id;
      }
  }
  function getEachDecision(){
      return DB_doquer('SELECT DISTINCT AttD_ecision
                           FROM C6_D_ecision
                          WHERE 1');
  }
  function createDecision(Decision &$obj){
      return updateDecision($obj,true);
  }
  function readDecision($id){
      $ctx = DB_doquer('SELECT DISTINCT isect0.AttD_ecision
                           FROM C6_D_ecision AS isect0
                          WHERE isect0.AttD_ecision = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj = new Decision($id, array(), array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttD_ecision, fst.AttA_pplication
                           FROM T10_leadsto AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_application(new Decision_application($v['AttA_pplication']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttD_ecision AS AttA_pplication, fst.AttP_roduct
                           FROM 
                             ( SELECT DISTINCT AttA_pplication AS AttD_ecision, AttP_roduct
                                 FROM T6_kind
                                WHERE 1
                             ) AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
          $obj->add_kind(new Decision_kind($v['AttP_roduct']));
      }
      return $obj;
  }
  function updateDecision(Decision $Decision,$new=false){
      global $DB_link,$DB_err,$DB_lastquer;
      $preErr= $new ? 'Cannot create new Decision: ':'Cannot update Decision: ';
      DB_doquer('START TRANSACTION');
      if($new){ // create a new object
        if(!isset($Decision)){ // find a unique id
           $nextNum = DB_doquer('SELECT max(1+AttD_ecision) FROM C6_D_ecision GROUP BY \'1\'');
           $Decision->id = $nextNum[0][0];
        }
        if(DB_plainquer('INSERT INTO C6_D_ecision (AttD_ecision) VALUES (\''.addslashes($Decision->id).'\')',$errno)===false){
            $DB_err=$preErr.(($errno==1062) ? 'Decision \''.$Decision->id.'\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);
            DB_doquer('ROLLBACK');
            return false;
        }
      }else{
        // destroy old attribute values
        $effected = DB_doquer('SELECT DISTINCT fst.AttD_ecision, fst.AttA_pplication
                           FROM T10_leadsto AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($Decision->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_pplication']).'\'';
        }
        $application_str=join(',',$arr);
        DB_doquer('DELETE FROM T10_leadsto WHERE AttD_ecision=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttD_ecision AS AttA_pplication, fst.AttP_roduct
                           FROM 
                             ( SELECT DISTINCT AttA_pplication AS AttD_ecision, AttP_roduct
                                 FROM T6_kind
                                WHERE 1
                             ) AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($Decision->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $kind_str=join(',',$arr);
        DB_doquer('DELETE FROM T6_kind WHERE AttA_pplication=\'addslashes($id)\'');
      }
      foreach($Decision->application as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttA_pplication) FROM C3_A_pplication GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C3_A_pplication (AttA_pplication) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T10_leadsto (AttD_ecision,AttA_pplication) VALUES (\''.addslashes($Decision->id).'\',\''.addslashes($v->id).'\')');
      }
      foreach($Decision->kind as $i=>$v){
        if(!isset($v->id)){
           $nextNum = DB_doquer('SELECT max(1+AttP_roduct) FROM C5_P_roduct GROUP BY \'1\'');
           $v->id = $nextNum[0][0];
        }else{
           // check cardinalities...
        }
        DB_doquer('INSERT IGNORE INTO C5_P_roduct (AttP_roduct) VALUES (\''.addslashes($v->id).'\')');
        DB_doquer('INSERT IGNORE INTO T6_kind (AttA_pplication,AttP_roduct) VALUES (\''.addslashes($Decision->id).'\',\''.addslashes($v->id).'\')');
      }
      if(!$new && strlen($application_str))
        DB_doquer('DELETE FROM C3_A_pplication
          WHERE AttA_pplication IN ('.$application_str.')
            AND NOT EXISTS (SELECT * FROM T6_kind
                             WHERE C3_A_pplication.AttA_pplication = T6_kind.AttP_roduct
                           )
            AND NOT EXISTS (SELECT * FROM T2_applicant
                             WHERE C3_A_pplication.AttA_pplication = T2_applicant.AttP_erson
                           )
            AND NOT EXISTS (SELECT * FROM T3_checked
                             WHERE C3_A_pplication.AttA_pplication = T3_checked.AttID_document
                           )
            AND NOT EXISTS (SELECT * FROM T4_assigned
                             WHERE C3_A_pplication.AttA_pplication = T4_assigned.AttE_mployee
                           )
            AND NOT EXISTS (SELECT * FROM T10_leadsto
                             WHERE C3_A_pplication.AttA_pplication = T10_leadsto.AttD_ecision
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
    if (!checkRule1()){
      $DB_err=$preErr.'\"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product.\"';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'\"Applications for permits are treated by authorized personnel only.\"';
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
          return $Decision->id;
      }
      DB_doquer('ROLLBACK');
      return false;
  }
  function deleteDecision($id){
    global $DB_err;
    DB_doquer('START TRANSACTION');
    
      $taken = DB_doquer('SELECT DISTINCT fst.AttD_ecision, fst.AttP_roduct
                           FROM T7_kind AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($id).'\'');
      if(count($taken)) {
        $DB_err = 'Cannot delete Decision: Decision \''.addslashes($id).'\' is about a \''.addslashes($taken[0]['AttP_roduct']).'\'';
        DB_doquer('ROLLBACK');
        return false;
      }
        $effected = DB_doquer('SELECT DISTINCT fst.AttD_ecision, fst.AttA_pplication
                           FROM T10_leadsto AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_pplication']).'\'';
        }
        $application_str=join(',',$arr);
        DB_doquer('DELETE FROM T10_leadsto WHERE AttD_ecision=\'addslashes($id)\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttD_ecision AS AttA_pplication, fst.AttP_roduct
                           FROM 
                             ( SELECT DISTINCT AttA_pplication AS AttD_ecision, AttP_roduct
                                 FROM T6_kind
                                WHERE 1
                             ) AS fst
                          WHERE fst.AttD_ecision = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $kind_str=join(',',$arr);
        DB_doquer('DELETE FROM T6_kind WHERE AttA_pplication=\'addslashes($id)\'');
    DB_doquer('DELETE FROM C6_D_ecision WHERE AttD_ecision=\''.addslashes($id).'\'');
    if(strlen($application_str))
      DB_doquer('DELETE FROM C3_A_pplication
        WHERE AttA_pplication IN ('.$application_str.')
          AND NOT EXISTS (SELECT * FROM T6_kind
                           WHERE C3_A_pplication.AttA_pplication = T6_kind.AttP_roduct
                         )
          AND NOT EXISTS (SELECT * FROM T2_applicant
                           WHERE C3_A_pplication.AttA_pplication = T2_applicant.AttP_erson
                         )
          AND NOT EXISTS (SELECT * FROM T3_checked
                           WHERE C3_A_pplication.AttA_pplication = T3_checked.AttID_document
                         )
          AND NOT EXISTS (SELECT * FROM T4_assigned
                           WHERE C3_A_pplication.AttA_pplication = T4_assigned.AttE_mployee
                         )
          AND NOT EXISTS (SELECT * FROM T10_leadsto
                           WHERE C3_A_pplication.AttA_pplication = T10_leadsto.AttD_ecision
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
    if (!checkRule1()){
      $DB_err=$preErr.'\"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product.\"';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'\"Applications for permits are treated by authorized personnel only.\"';
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