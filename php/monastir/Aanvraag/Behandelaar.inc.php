<?php // generated with ADL vs. 0.8.10
  
  /********* on line 3, file "Aanvraag.adl"
   * Behandelaar[Employee] : V
   *  = [ aanvragen[Application] : assigned~
   *   ]
   *********/
  
  function getobject_Behandelaar(){  return   new object("Behandelaar", array
       ( new oRef( new oMulti( true,false,false,false ) // derived from assigned~
           , new object("aanvragen", array(), "Application.php")
           ) 
       ), "Behandelaar.php");
  }

  class Behandelaar {
    var $id;
    var $aanvragen;
    function Behandelaar($id=null, $aanvragen=array()){
        $this->id=$id;
        $this->aanvragen=$aanvragen;
    }
    function add_aanvragen(Behandelaar_aanvragen $aanvragen){
      return $this->aanvragen[]=$aanvragen;
    }
    function read_aanvragen($id){
      $obj = new Behandelaar_aanvragen($id);
      $this->add_aanvragen($obj);
      return $obj;
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
    function readGen($type,$value){
      if($type=='aanvragen') return $this->read_aanvragen($value);
      else return false;
    }
  }
  class Behandelaar_aanvragen {
    var $id;
    function Behandelaar_aanvragen($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
    function readGen($type,$value){
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
                           FROM C4_E_mployee AS isect0
                          WHERE isect0.AttE_mployee = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj = new Behandelaar($id, array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_pplication
                           FROM T4_assigned AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v){
        $obj->read_aanvragen($v['AttA_pplication']);
      }
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
          }
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
    if (!checkRule2()){
      $DB_err=$preErr.'"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'"Applications for permits are treated by authorized personnel only."';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'"applicant[Application*Person] is total"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'"assigned[Application*Employee] is univalent"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'"kind[Application*Product] is total"';
    } else
    if (!checkRule18()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is injective"';
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
    if (!checkRule2()){
      $DB_err=$preErr.'"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'"Applications for permits are treated by authorized personnel only."';
    } else
    if (!checkRule8()){
      $DB_err=$preErr.'"applicant[Application*Person] is total"';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'"assigned[Application*Employee] is univalent"';
    } else
    if (!checkRule12()){
      $DB_err=$preErr.'"kind[Application*Product] is total"';
    } else
    if (!checkRule18()){
      $DB_err=$preErr.'"leadsto[Application*Decision] is injective"';
    } else
    if(true) {
      DB_doquer('COMMIT');
      return true;
    }
    DB_doquer('ROLLBACK');
    return false;
  }
?>