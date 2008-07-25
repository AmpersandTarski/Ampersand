<?php // generated with ADL vs. 0.8.09
  $DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');
  $DB_slct = mysql_select_db('Aanvraag',$DB_link);
  
  
  function checkRule1(){
    // No violations should occur in (leadsto~;kind |- kind COMPUTING [kind])
    //            rule':: leadsto~;kind/\-kind
    // sqlExprSrc rule':: AttD_ecision
     $v=DB_doquer('SELECT DISTINCT isect0.AttD_ecision, isect0.AttP_roduct
                     FROM 
                       ( SELECT DISTINCT fst.AttD_ecision, snd.AttP_roduct
                           FROM T10_leadsto AS fst, T6_kind AS snd
                          WHERE fst.AttA_pplication = snd.AttA_pplication
                       ) AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM T7_kind AS cp
                                WHERE isect0.AttD_ecision=cp.AttD_ecision AND isect0.AttP_roduct=cp.AttP_roduct)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product.\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule2(){
    // No violations should occur in (assigned~;applicant;inhabitant |- area)
    //            rule':: assigned~;applicant;inhabitant/\-area
    // sqlExprSrc rule':: AttE_mployee
     $v=DB_doquer('SELECT DISTINCT isect0.AttE_mployee, isect0.AttA_rea
                     FROM 
                       ( SELECT DISTINCT fst.AttE_mployee, snd.AttA_rea
                           FROM T4_assigned AS fst, 
                             ( SELECT DISTINCT fst.AttA_pplication, snd.AttA_rea
                                 FROM T2_applicant AS fst, T8_inhabitant AS snd
                                WHERE fst.AttP_erson = snd.AttP_erson
                             ) AS snd
                          WHERE fst.AttA_pplication = snd.AttA_pplication
                       ) AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM T9_area AS cp
                                WHERE isect0.AttE_mployee=cp.AttE_mployee AND isect0.AttA_rea=cp.AttA_rea)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only.\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule3(){
    // No violations should occur in (applicant |- checked;authentic~)
    //            rule':: applicant/\-(checked;authentic~)
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication, isect0.AttP_erson
                     FROM T2_applicant AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttA_pplication, snd.AttP_erson
                                       FROM T3_checked AS fst, T1_authentic AS snd
                                      WHERE fst.AttID_document = snd.AttID_document
                                   ) AS cp
                                WHERE isect0.AttA_pplication=cp.AttA_pplication AND isect0.AttP_erson=cp.AttP_erson)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"An application for a permit is accepted only from individuals whose identity is authenticated.\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule4(){
    // No violations should occur in (assigned |- kind;auth~)
    //            rule':: assigned/\-(kind;auth~)
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication, isect0.AttE_mployee
                     FROM T4_assigned AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttA_pplication, snd.AttE_mployee
                                       FROM T6_kind AS fst, T5_auth AS snd
                                      WHERE fst.AttP_roduct = snd.AttP_roduct
                                   ) AS cp
                                WHERE isect0.AttA_pplication=cp.AttA_pplication AND isect0.AttE_mployee=cp.AttE_mployee)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"Applications for permits are treated by authorized personnel only.\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule5(){
    // No violations should occur in (authentic;authentic~ |- I)
    //            rule':: authentic;authentic~/\-I
    // sqlExprSrc rule':: AttP_erson
     $v=DB_doquer('SELECT DISTINCT isect0.AttP_erson, isect0.AttP_erson1 AS AttP_erson
                     FROM 
                       ( SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1
                           FROM T1_authentic AS fst, T1_authentic AS snd
                          WHERE fst.AttID_document = snd.AttID_document
                       ) AS isect0
                    WHERE isect0.AttP_erson <> isect0.AttP_erson1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"authentic[Person*IDdocument] is injective\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule6(){
    // No violations should occur in (I |- authentic~;authentic)
    //            rule':: I/\-(authentic~;authentic)
    // sqlExprSrc rule':: AttID_document
     $v=DB_doquer('SELECT DISTINCT isect0.AttID_document
                     FROM C2_ID_document AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttID_document, snd.AttID_document AS AttID_document1
                                       FROM T1_authentic AS fst, T1_authentic AS snd
                                      WHERE fst.AttP_erson = snd.AttP_erson
                                   ) AS cp
                                WHERE isect0.AttID_document=cp.AttID_document AND isect0.AttID_document=cp.AttID_document1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"authentic[Person*IDdocument] is surjective\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule7(){
    // No violations should occur in (applicant~;applicant |- I)
    //            rule':: applicant~;applicant/\-I
    // sqlExprSrc rule':: AttP_erson
     $v=DB_doquer('SELECT DISTINCT isect0.AttP_erson, isect0.AttP_erson1 AS AttP_erson
                     FROM 
                       ( SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1
                           FROM T2_applicant AS fst, T2_applicant AS snd
                          WHERE fst.AttA_pplication = snd.AttA_pplication
                       ) AS isect0
                    WHERE isect0.AttP_erson <> isect0.AttP_erson1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"applicant[Application*Person] is univalent\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule8(){
    // No violations should occur in (I |- applicant;applicant~)
    //            rule':: I/\-(applicant;applicant~)
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication
                     FROM C3_A_pplication AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1
                                       FROM T2_applicant AS fst, T2_applicant AS snd
                                      WHERE fst.AttP_erson = snd.AttP_erson
                                   ) AS cp
                                WHERE isect0.AttA_pplication=cp.AttA_pplication AND isect0.AttA_pplication=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"applicant[Application*Person] is total\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule9(){
    // No violations should occur in (I |- checked;checked~)
    //            rule':: I/\-(checked;checked~)
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication
                     FROM C3_A_pplication AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1
                                       FROM T3_checked AS fst, T3_checked AS snd
                                      WHERE fst.AttID_document = snd.AttID_document
                                   ) AS cp
                                WHERE isect0.AttA_pplication=cp.AttA_pplication AND isect0.AttA_pplication=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"checked[Application*IDdocument] is total\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule10(){
    // No violations should occur in (assigned;assigned~ |- I)
    //            rule':: assigned;assigned~/\-I
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication, isect0.AttA_pplication1 AS AttA_pplication
                     FROM 
                       ( SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1
                           FROM T4_assigned AS fst, T4_assigned AS snd
                          WHERE fst.AttE_mployee = snd.AttE_mployee
                       ) AS isect0
                    WHERE isect0.AttA_pplication <> isect0.AttA_pplication1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"assigned[Application*Employee] is injective\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule11(){
    // No violations should occur in (kind~;kind |- I)
    //            rule':: kind~;kind/\-I
    // sqlExprSrc rule':: AttP_roduct
     $v=DB_doquer('SELECT DISTINCT isect0.AttP_roduct, isect0.AttP_roduct1 AS AttP_roduct
                     FROM 
                       ( SELECT DISTINCT fst.AttP_roduct, snd.AttP_roduct AS AttP_roduct1
                           FROM T6_kind AS fst, T6_kind AS snd
                          WHERE fst.AttA_pplication = snd.AttA_pplication
                       ) AS isect0
                    WHERE isect0.AttP_roduct <> isect0.AttP_roduct1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"kind[Application*Product] is univalent\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule12(){
    // No violations should occur in (I |- kind;kind~)
    //            rule':: I/\-(kind;kind~)
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication
                     FROM C3_A_pplication AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1
                                       FROM T6_kind AS fst, T6_kind AS snd
                                      WHERE fst.AttP_roduct = snd.AttP_roduct
                                   ) AS cp
                                WHERE isect0.AttA_pplication=cp.AttA_pplication AND isect0.AttA_pplication=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"kind[Application*Product] is total\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule13(){
    // No violations should occur in (kind~;kind |- I)
    //            rule':: kind~;kind/\-I
    // sqlExprSrc rule':: AttP_roduct
     $v=DB_doquer('SELECT DISTINCT isect0.AttP_roduct, isect0.AttP_roduct1 AS AttP_roduct
                     FROM 
                       ( SELECT DISTINCT fst.AttP_roduct, snd.AttP_roduct AS AttP_roduct1
                           FROM T7_kind AS fst, T7_kind AS snd
                          WHERE fst.AttD_ecision = snd.AttD_ecision
                       ) AS isect0
                    WHERE isect0.AttP_roduct <> isect0.AttP_roduct1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"kind[Decision*Product] is univalent\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule14(){
    // No violations should occur in (I |- kind;kind~)
    //            rule':: I/\-(kind;kind~)
    // sqlExprSrc rule':: AttD_ecision
     $v=DB_doquer('SELECT DISTINCT isect0.AttD_ecision
                     FROM C6_D_ecision AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttD_ecision, snd.AttD_ecision AS AttD_ecision1
                                       FROM T7_kind AS fst, T7_kind AS snd
                                      WHERE fst.AttP_roduct = snd.AttP_roduct
                                   ) AS cp
                                WHERE isect0.AttD_ecision=cp.AttD_ecision AND isect0.AttD_ecision=cp.AttD_ecision1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"kind[Decision*Product] is total\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule15(){
    // No violations should occur in (inhabitant~;inhabitant |- I)
    //            rule':: inhabitant~;inhabitant/\-I
    // sqlExprSrc rule':: AttA_rea
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_rea, isect0.AttA_rea1 AS AttA_rea
                     FROM 
                       ( SELECT DISTINCT fst.AttA_rea, snd.AttA_rea AS AttA_rea1
                           FROM T8_inhabitant AS fst, T8_inhabitant AS snd
                          WHERE fst.AttP_erson = snd.AttP_erson
                       ) AS isect0
                    WHERE isect0.AttA_rea <> isect0.AttA_rea1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"inhabitant[Person*Area] is univalent\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule16(){
    // No violations should occur in (I |- inhabitant;inhabitant~)
    //            rule':: I/\-(inhabitant;inhabitant~)
    // sqlExprSrc rule':: AttP_erson
     $v=DB_doquer('SELECT DISTINCT isect0.AttP_erson
                     FROM C1_P_erson AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1
                                       FROM T8_inhabitant AS fst, T8_inhabitant AS snd
                                      WHERE fst.AttA_rea = snd.AttA_rea
                                   ) AS cp
                                WHERE isect0.AttP_erson=cp.AttP_erson AND isect0.AttP_erson=cp.AttP_erson1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"inhabitant[Person*Area] is total\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule17(){
    // No violations should occur in (I |- area~;area)
    //            rule':: I/\-(area~;area)
    // sqlExprSrc rule':: AttA_rea
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_rea
                     FROM C7_A_rea AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttA_rea, snd.AttA_rea AS AttA_rea1
                                       FROM T9_area AS fst, T9_area AS snd
                                      WHERE fst.AttE_mployee = snd.AttE_mployee
                                   ) AS cp
                                WHERE isect0.AttA_rea=cp.AttA_rea AND isect0.AttA_rea=cp.AttA_rea1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"area[Employee*Area] is surjective\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule18(){
    // No violations should occur in (leadsto;leadsto~ |- I)
    //            rule':: leadsto;leadsto~/\-I
    // sqlExprSrc rule':: AttA_pplication
     $v=DB_doquer('SELECT DISTINCT isect0.AttA_pplication, isect0.AttA_pplication1 AS AttA_pplication
                     FROM 
                       ( SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1
                           FROM T10_leadsto AS fst, T10_leadsto AS snd
                          WHERE fst.AttD_ecision = snd.AttD_ecision
                       ) AS isect0
                    WHERE isect0.AttA_pplication <> isect0.AttA_pplication1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"leadsto[Application*Decision] is injective\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule19(){
    // No violations should occur in (leadsto~;leadsto |- I)
    //            rule':: leadsto~;leadsto/\-I
    // sqlExprSrc rule':: AttD_ecision
     $v=DB_doquer('SELECT DISTINCT isect0.AttD_ecision, isect0.AttD_ecision1 AS AttD_ecision
                     FROM 
                       ( SELECT DISTINCT fst.AttD_ecision, snd.AttD_ecision AS AttD_ecision1
                           FROM T10_leadsto AS fst, T10_leadsto AS snd
                          WHERE fst.AttA_pplication = snd.AttA_pplication
                       ) AS isect0
                    WHERE isect0.AttD_ecision <> isect0.AttD_ecision1');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"leadsto[Application*Decision] is univalent\"<BR>',3);
      return false;
    }return true;
  }
  
  function checkRule20(){
    // No violations should occur in (I |- leadsto~;leadsto)
    //            rule':: I/\-(leadsto~;leadsto)
    // sqlExprSrc rule':: AttD_ecision
     $v=DB_doquer('SELECT DISTINCT isect0.AttD_ecision
                     FROM C6_D_ecision AS isect0
                    WHERE NOT EXISTS (SELECT *
                                 FROM 
                                   ( SELECT DISTINCT fst.AttD_ecision, snd.AttD_ecision AS AttD_ecision1
                                       FROM T10_leadsto AS fst, T10_leadsto AS snd
                                      WHERE fst.AttA_pplication = snd.AttA_pplication
                                   ) AS cp
                                WHERE isect0.AttD_ecision=cp.AttD_ecision AND isect0.AttD_ecision=cp.AttD_ecision1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: \"leadsto[Application*Decision] is surjective\"<BR>',3);
      return false;
    }return true;
  }
  
  if(!$DB_slct){
        DB_debug( "Warning: error connecting to database, building database",2 );
        mysql_query("CREATE DATABASE Aanvraag",$DB_link) or die('Could not create DB Aanvraag');
        $DB_slct = mysql_select_db('Aanvraag',$DB_link) or die ('Could not select DB Aanvraag');
        $DB_errs = false;
        
        DB_doquer("CREATE TABLE T1_authentic (AttP_erson varchar(380) NOT NULL default '', AttID_document varchar(380) NOT NULL default '', UNIQUE  (AttP_erson,AttID_document) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T2_applicant (AttA_pplication varchar(380) NOT NULL default '', AttP_erson varchar(380) NOT NULL default '', UNIQUE  (AttA_pplication,AttP_erson) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T3_checked (AttA_pplication varchar(380) NOT NULL default '', AttID_document varchar(380) NOT NULL default '', UNIQUE  (AttA_pplication,AttID_document) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T4_assigned (AttA_pplication varchar(380) NOT NULL default '', AttE_mployee varchar(380) NOT NULL default '', UNIQUE  (AttA_pplication,AttE_mployee) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T5_auth (AttE_mployee varchar(380) NOT NULL default '', AttP_roduct varchar(380) NOT NULL default '', UNIQUE  (AttE_mployee,AttP_roduct) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T6_kind (AttA_pplication varchar(380) NOT NULL default '', AttP_roduct varchar(380) NOT NULL default '', UNIQUE  (AttA_pplication,AttP_roduct) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T7_kind (AttD_ecision varchar(380) NOT NULL default '', AttP_roduct varchar(380) NOT NULL default '', UNIQUE  (AttD_ecision,AttP_roduct) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T8_inhabitant (AttP_erson varchar(380) NOT NULL default '', AttA_rea varchar(380) NOT NULL default '', UNIQUE  (AttP_erson,AttA_rea) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T9_area (AttE_mployee varchar(380) NOT NULL default '', AttA_rea varchar(380) NOT NULL default '', UNIQUE  (AttE_mployee,AttA_rea) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE T10_leadsto (AttA_pplication varchar(380) NOT NULL default '', AttD_ecision varchar(380) NOT NULL default '', UNIQUE  (AttA_pplication,AttD_ecision) ) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C1_P_erson (AttP_erson varchar(380) NOT NULL default '', UNIQUE  (AttP_erson)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C2_ID_document (AttID_document varchar(380) NOT NULL default '', UNIQUE  (AttID_document)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C3_A_pplication (AttA_pplication varchar(380) NOT NULL default '', UNIQUE  (AttA_pplication)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C4_E_mployee (AttE_mployee varchar(380) NOT NULL default '', UNIQUE  (AttE_mployee)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C5_P_roduct (AttP_roduct varchar(380) NOT NULL default '', UNIQUE  (AttP_roduct)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C6_D_ecision (AttD_ecision varchar(380) NOT NULL default '', UNIQUE  (AttD_ecision)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        DB_doquer("CREATE TABLE C7_A_rea (AttA_rea varchar(380) NOT NULL default '', UNIQUE  (AttA_rea)) TYPE=InnoDB DEFAULT CHARACTER SET latin1");
        
        
        
        
        
        
        
        
        if($DB_errs || !(checkRule1() && checkRule2() && checkRule3() && checkRule4()))
        {  DB_debug( "DB errors, removing database",5);
           mysql_query("DROP DATABASE Aanvraag",$DB_link) or die('Could not delete DB Aanvraag');
           die ('Errors creating database');
          } else {
             DB_doquer('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');
          }
    }else{
      DB_debug( "Connected to database",2 );
    }
  
  if($DB_debug>=3){
    checkRule1();
    checkRule2();
    checkRule3();
    checkRule4();
    checkRule5();
    checkRule6();
    checkRule7();
    checkRule8();
    checkRule9();
    checkRule10();
    checkRule11();
    checkRule12();
    checkRule13();
    checkRule14();
    checkRule15();
    checkRule16();
    checkRule17();
    checkRule18();
    checkRule19();
    checkRule20();
  }?>