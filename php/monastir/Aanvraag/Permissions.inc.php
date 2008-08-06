<?php // generated with ADL vs. 0.8.10
  
  /********* on line 7, file "Aanvraag.adl"
   * Permissions[Employee] : V
   *  = [ product[Product] : auth
   *    , area[Area] : area
   *   ]
   *********/
  
  function getobject_Permissions(){
    return   new object("Permissions", array
       ( new oRef( new oMulti( false,false,false,false ) // derived from auth
           , new object("product", array())
           ) 
       , new oRef( new oMulti( false,false,true,false ) // derived from area
           , new object("area", array())
           ) 
       ), "Behandelaar.php");
  }
  
  class Permissions {
    var $id;
    var $product;
    var $area;
    function Permissions($id=null, $product=array(), $area=array()){
        $this->id=$id;
        $this->product=$product;
        $this->area=$area;
    }
    function add_product(Permissions_product $product){
      return $this->product[]=$product;
    }
    function add_area(Permissions_area $area){
      return $this->area[]=$area;
    }
    function addGen($type,$value){
      if($type=='product') return $this->add_product($value);
      if($type=='area') return $this->add_area($value);
      else return false;
    }
  }
  class Permissions_product {
    var $id;
    function Permissions_product($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  class Permissions_area {
    var $id;
    function Permissions_area($id=null){
        $this->id=$id;
    }
    function addGen($type,$value){
    }
  }
  function getEachPermissions(){
      return DB_doquer('SELECT DISTINCT AttE_mployee
                           FROM C4_E_mployee
                          WHERE 1');
  }
  function createPermissions(Permissions &$obj){
      return updatePermissions($obj,true);
  }
  function readPermissions($id){
      // check existance of $id
      $ctx = DB_doquer('SELECT DISTINCT isect0.AttE_mployee
                           FROM C4_E_mployee AS isect0
                          WHERE isect0.AttE_mployee = \''.addslashes($id).'\'');
      if(count($ctx)==0) return false;
      $obj1 = new Permissions($id, array(), array());
      $ctx = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttP_roduct
                           FROM T5_auth AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v1){
          $obj2=$obj1->add_product(new Permissions_product($v1['AttP_roduct']));
      }
      $ctx = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_rea
                           FROM T9_area AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
      foreach($ctx as $i=>$v1){
          $obj2=$obj1->add_area(new Permissions_area($v1['AttA_rea']));
      }
      return $obj1;
  }
  function updatePermissions(Permissions $Permissions,$new=false){
      global $DB_link,$DB_err,$DB_lastquer;
      $preErr= $new ? 'Cannot create new Employee: ':'Cannot update Employee: ';
      DB_doquer('START TRANSACTION');
      if($new){ // create a new object
        if(!isset($Permissions->id)){ // find a unique id
           $nextNum = DB_doquer('SELECT max(1+AttE_mployee) FROM C4_E_mployee GROUP BY \'1\'');
           $Permissions->id = @$nextNum[0][0]+0;
        }
        if(DB_plainquer('INSERT INTO C4_E_mployee (AttE_mployee) VALUES (\''.addslashes($Permissions->id).'\')',$errno)===false){
            $DB_err=$preErr.(($errno==1062) ? 'Employee \''.$Permissions->id.'\' allready exists' : 'Error '.$errno.' in query '.$DB_lastquer);
            DB_doquer('ROLLBACK');
            return false;
        }
      }else
      if(!$new){
        // destroy old attribute values
        $effected = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttP_roduct
                           FROM T5_auth AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($Permissions->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $Permissions_product_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T5_auth WHERE AttE_mployee=\''.addslashes($Permissions->id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_rea
                           FROM T9_area AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($Permissions->id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_rea']).'\'';
        }
        $Permissions_area_str=join(',',$arr);
        DB_doquer( 'DELETE FROM T9_area WHERE AttE_mployee=\''.addslashes($Permissions->id).'\'');
      }
      foreach($Permissions->product as $i=>$Permissions_product){
        if(!isset($Permissions_product->id)){
           $nextNum = DB_doquer('SELECT max(1+AttP_roduct) FROM C5_P_roduct GROUP BY \'1\'');
           $Permissions_product->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C5_P_roduct (AttP_roduct) VALUES (\''.addslashes($Permissions_product->id).'\')');
        DB_doquer('INSERT IGNORE INTO T5_auth (AttE_mployee,AttP_roduct) VALUES (\''.addslashes($Permissions->id).'\',\''.addslashes($Permissions_product->id).'\')');
          if(!$new){
            // destroy old attribute values
          }
      }
      foreach($Permissions->area as $i=>$Permissions_area){
        if(!isset($Permissions_area->id)){
           $nextNum = DB_doquer('SELECT max(1+AttA_rea) FROM C7_A_rea GROUP BY \'1\'');
           $Permissions_area->id = @$nextNum[0][0]+0;
        }
        DB_doquer('INSERT IGNORE INTO C7_A_rea (AttA_rea) VALUES (\''.addslashes($Permissions_area->id).'\')');
        DB_doquer('INSERT IGNORE INTO T9_area (AttE_mployee,AttA_rea) VALUES (\''.addslashes($Permissions->id).'\',\''.addslashes($Permissions_area->id).'\')');
          if(!$new){
            // destroy old attribute values
          }
      }
      if(!$new && strlen($Permissions_product_str))
        DB_doquer('DELETE FROM C5_P_roduct
          WHERE AttP_roduct IN ('.$Permissions_product_str.')
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
      if(!$new && strlen($Permissions_area_str))
        DB_doquer('DELETE FROM C7_A_rea
          WHERE AttA_rea IN ('.$Permissions_area_str.')
            AND NOT EXISTS (SELECT * FROM T8_inhabitant
                             WHERE C7_A_rea.AttA_rea = T8_inhabitant.AttA_rea
                           )
            AND NOT EXISTS (SELECT * FROM T9_area
                             WHERE C7_A_rea.AttA_rea = T9_area.AttA_rea
                           )
        ');
    if (!checkRule2()){
      $DB_err=$preErr.'"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'"Applications for permits are treated by authorized personnel only."';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'"assigned[Application*Employee] is univalent"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'"kind[Application*Product] is univalent"';
    } else
    if (!checkRule13()){
      $DB_err=$preErr.'"kind[Decision*Product] is univalent"';
    } else
    if (!checkRule15()){
      $DB_err=$preErr.'"inhabitant[Person*Area] is univalent"';
    } else
    if (!checkRule17()){
      $DB_err=$preErr.'"area[Employee*Area] is surjective"';
    } else
      if(true){ // all rules are met
          DB_doquer('COMMIT');
          return $Permissions->id;
      }
      DB_doquer('ROLLBACK');
      return false;
  }
  function deletePermissions($id){
    global $DB_err;
    DB_doquer('START TRANSACTION');
    
      $taken = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_pplication
                           FROM T4_assigned AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
      if(count($taken)) {
        $DB_err = 'Cannot delete Permissions: Application \''.addslashes($id).'\' was treated by \''.addslashes($taken[0]['AttA_pplication']).'\'';
        DB_doquer('ROLLBACK');
        return false;
      }
  /*
  auth
  area
  *************
  area
  auth
  I
  assigned
  V
  */
        $effected = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttP_roduct
                           FROM T5_auth AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttP_roduct']).'\'';
        }
        $product_str=join(',',$arr);
        DB_doquer ('DELETE FROM T5_auth WHERE AttE_mployee=\''.addslashes($id).'\'');
        $effected = DB_doquer('SELECT DISTINCT fst.AttE_mployee, fst.AttA_rea
                           FROM T9_area AS fst
                          WHERE fst.AttE_mployee = \''.addslashes($id).'\'');
        $arr=array();
        foreach($effected as $i=>$v){
            $arr[]='\''.addslashes($v['AttA_rea']).'\'';
        }
        $area_str=join(',',$arr);
        DB_doquer ('DELETE FROM T9_area WHERE AttE_mployee=\''.addslashes($id).'\'');
    DB_doquer('DELETE FROM C4_E_mployee WHERE AttE_mployee=\''.addslashes($id).'\'');
    if(strlen($product_str))
      DB_doquer('DELETE FROM C5_P_roduct
        WHERE AttP_roduct IN ('.$product_str.')
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
    if(strlen($area_str))
      DB_doquer('DELETE FROM C7_A_rea
        WHERE AttA_rea IN ('.$area_str.')
          AND NOT EXISTS (SELECT * FROM T8_inhabitant
                           WHERE C7_A_rea.AttA_rea = T8_inhabitant.AttA_rea
                         )
          AND NOT EXISTS (SELECT * FROM T9_area
                           WHERE C7_A_rea.AttA_rea = T9_area.AttA_rea
                         )
      ');
    if (!checkRule2()){
      $DB_err=$preErr.'"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."';
    } else
    if (!checkRule4()){
      $DB_err=$preErr.'"Applications for permits are treated by authorized personnel only."';
    } else
    if (!checkRule10()){
      $DB_err=$preErr.'"assigned[Application*Employee] is univalent"';
    } else
    if (!checkRule11()){
      $DB_err=$preErr.'"kind[Application*Product] is univalent"';
    } else
    if (!checkRule13()){
      $DB_err=$preErr.'"kind[Decision*Product] is univalent"';
    } else
    if (!checkRule15()){
      $DB_err=$preErr.'"inhabitant[Person*Area] is univalent"';
    } else
    if (!checkRule17()){
      $DB_err=$preErr.'"area[Employee*Area] is surjective"';
    } else
    if(true) {
      DB_doquer('COMMIT');
      return true;
    }
    DB_doquer('ROLLBACK');
    return false;
  }
?>