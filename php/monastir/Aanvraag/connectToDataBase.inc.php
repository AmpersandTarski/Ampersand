<?
	$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');
	$DB_slct = mysql_select_db($dbName,$DB_link);
    function checkRule1(){
    // No violations should occur in (leadsto~;kind -: kind COMPUTING [kind])
     // pDebug:: kind-/\leadsto~;kind
     $v=DB_doquer('SELECT DISTINCT vfi.AttD_ecision, vfi.AttP_roduct FROM (SELECT DISTINCT fst.AttD_ecision, snd.AttP_roduct FROM T10_leadsto AS fst, T6_kind AS snd WHERE fst.AttA_pplication = snd.AttA_pplication) AS vfi WHERE NOT EXISTS (SELECT * FROM T7_kind AS cp WHERE vfi.AttD_ecision=cp.AttD_ecision AND vfi.AttP_roduct=cp.AttP_roduct)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "Every application leads to a decision. An application for a particular product (the type of permit) leads to a decision about that same product."<BR>',3);
      return false;
    }return true;
  }
  function checkRule2(){
    // No violations should occur in (assigned -: applicant;inhabitant;area~)
     // pDebug:: (applicant;inhabitant;area~)-/\assigned
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttE_mployee FROM T4_assigned AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttE_mployee FROM T2_applicant AS fst, (SELECT DISTINCT fst.AttP_erson, snd.AttE_mployee FROM T8_inhabitant AS fst, T9_area AS snd WHERE fst.AttA_rea = snd.AttA_rea) AS snd WHERE fst.AttP_erson = snd.AttP_erson) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttE_mployee=cp.AttE_mployee)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "Employees get assigned to particular areas. This means that an assigned employee treats request from these areas only."<BR>',3);
      return false;
    }return true;
  }
  function checkRule3(){
    // No violations should occur in (applicant -: checked;authentic~)
     // pDebug:: (checked;authentic~)-/\applicant
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttP_erson FROM T2_applicant AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttP_erson FROM T3_checked AS fst, T1_authentic AS snd WHERE fst.AttID_document = snd.AttID_document) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttP_erson=cp.AttP_erson)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "An application for a permit is accepted only from individuals whose identity is authenticated."<BR>',3);
      return false;
    }return true;
  }
  function checkRule4(){
    // No violations should occur in (assigned -: kind;auth~)
     // pDebug:: (kind;auth~)-/\assigned
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttE_mployee FROM T4_assigned AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttE_mployee FROM T6_kind AS fst, T5_auth AS snd WHERE fst.AttP_roduct = snd.AttP_roduct) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttE_mployee=cp.AttE_mployee)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "Applications for permits are treated by authorized personnel only."<BR>',3);
      return false;
    }return true;
  }
  function checkRule5(){
    // No violations should occur in (authentic;authentic~ -: I[Person])
     // pDebug:: I[Person]-/\authentic;authentic~
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_erson, vfi.AttP_erson1 FROM (SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1 FROM T1_authentic AS fst, T1_authentic AS snd WHERE fst.AttID_document = snd.AttID_document) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttP_erson, AttP_erson AS AttP_erson1 FROM C1_P_erson WHERE 1) AS cp WHERE vfi.AttP_erson=cp.AttP_erson AND vfi.AttP_erson1=cp.AttP_erson1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "authentic[Person*IDdocument] is injective"<BR>',3);
      return false;
    }return true;
  }
  function checkRule6(){
    // No violations should occur in (I[IDdocument] -: authentic~;authentic)
     // pDebug:: (authentic~;authentic)-/\I[IDdocument]
     $v=DB_doquer('SELECT DISTINCT vfi.AttID_document, vfi.AttID_document1 FROM (SELECT DISTINCT AttID_document, AttID_document AS AttID_document1 FROM C2_ID_document WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttID_document, snd.AttID_document AS AttID_document1 FROM T1_authentic AS fst, T1_authentic AS snd WHERE fst.AttP_erson = snd.AttP_erson) AS cp WHERE vfi.AttID_document=cp.AttID_document AND vfi.AttID_document1=cp.AttID_document1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "authentic[Person*IDdocument] is surjective"<BR>',3);
      return false;
    }return true;
  }
  function checkRule7(){
    // No violations should occur in (I[Person] -: authentic;authentic~)
     // pDebug:: (authentic;authentic~)-/\I[Person]
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_erson, vfi.AttP_erson1 FROM (SELECT DISTINCT AttP_erson, AttP_erson AS AttP_erson1 FROM C1_P_erson WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1 FROM T1_authentic AS fst, T1_authentic AS snd WHERE fst.AttID_document = snd.AttID_document) AS cp WHERE vfi.AttP_erson=cp.AttP_erson AND vfi.AttP_erson1=cp.AttP_erson1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "authentic[Person*IDdocument] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule8(){
    // No violations should occur in (applicant~;applicant -: I[Person])
     // pDebug:: I[Person]-/\applicant~;applicant
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_erson, vfi.AttP_erson1 FROM (SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1 FROM T2_applicant AS fst, T2_applicant AS snd WHERE fst.AttA_pplication = snd.AttA_pplication) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttP_erson, AttP_erson AS AttP_erson1 FROM C1_P_erson WHERE 1) AS cp WHERE vfi.AttP_erson=cp.AttP_erson AND vfi.AttP_erson1=cp.AttP_erson1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "applicant[Application*Person] is univalent"<BR>',3);
      return false;
    }return true;
  }
  function checkRule9(){
    // No violations should occur in (I[Application] -: applicant;applicant~)
     // pDebug:: (applicant;applicant~)-/\I[Application]
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttA_pplication1 FROM (SELECT DISTINCT AttA_pplication, AttA_pplication AS AttA_pplication1 FROM C3_A_pplication WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1 FROM T2_applicant AS fst, T2_applicant AS snd WHERE fst.AttP_erson = snd.AttP_erson) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttA_pplication1=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "applicant[Application*Person] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule10(){
    // No violations should occur in (assigned~;assigned -: I[Employee])
     // pDebug:: I[Employee]-/\assigned~;assigned
     $v=DB_doquer('SELECT DISTINCT vfi.AttE_mployee, vfi.AttE_mployee1 FROM (SELECT DISTINCT fst.AttE_mployee, snd.AttE_mployee AS AttE_mployee1 FROM T4_assigned AS fst, T4_assigned AS snd WHERE fst.AttA_pplication = snd.AttA_pplication) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttE_mployee, AttE_mployee AS AttE_mployee1 FROM C4_E_mployee WHERE 1) AS cp WHERE vfi.AttE_mployee=cp.AttE_mployee AND vfi.AttE_mployee1=cp.AttE_mployee1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "assigned[Application*Employee] is univalent"<BR>',3);
      return false;
    }return true;
  }
  function checkRule11(){
    // No violations should occur in (I[Application] -: assigned;assigned~)
     // pDebug:: (assigned;assigned~)-/\I[Application]
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttA_pplication1 FROM (SELECT DISTINCT AttA_pplication, AttA_pplication AS AttA_pplication1 FROM C3_A_pplication WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1 FROM T4_assigned AS fst, T4_assigned AS snd WHERE fst.AttE_mployee = snd.AttE_mployee) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttA_pplication1=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "assigned[Application*Employee] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule12(){
    // No violations should occur in (I[Employee] -: auth;auth~)
     // pDebug:: (auth;auth~)-/\I[Employee]
     $v=DB_doquer('SELECT DISTINCT vfi.AttE_mployee, vfi.AttE_mployee1 FROM (SELECT DISTINCT AttE_mployee, AttE_mployee AS AttE_mployee1 FROM C4_E_mployee WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttE_mployee, snd.AttE_mployee AS AttE_mployee1 FROM T5_auth AS fst, T5_auth AS snd WHERE fst.AttP_roduct = snd.AttP_roduct) AS cp WHERE vfi.AttE_mployee=cp.AttE_mployee AND vfi.AttE_mployee1=cp.AttE_mployee1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "auth[Employee*Product] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule13(){
    // No violations should occur in (I[Product] -: auth~;auth)
     // pDebug:: (auth~;auth)-/\I[Product]
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_roduct, vfi.AttP_roduct1 FROM (SELECT DISTINCT AttP_roduct, AttP_roduct AS AttP_roduct1 FROM C5_P_roduct WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttP_roduct, snd.AttP_roduct AS AttP_roduct1 FROM T5_auth AS fst, T5_auth AS snd WHERE fst.AttE_mployee = snd.AttE_mployee) AS cp WHERE vfi.AttP_roduct=cp.AttP_roduct AND vfi.AttP_roduct1=cp.AttP_roduct1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "auth[Employee*Product] is surjective"<BR>',3);
      return false;
    }return true;
  }
  function checkRule14(){
    // No violations should occur in (kind~;kind -: I[Product])
     // pDebug:: I[Product]-/\kind~;kind
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_roduct, vfi.AttP_roduct1 FROM (SELECT DISTINCT fst.AttP_roduct, snd.AttP_roduct AS AttP_roduct1 FROM T6_kind AS fst, T6_kind AS snd WHERE fst.AttA_pplication = snd.AttA_pplication) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttP_roduct, AttP_roduct AS AttP_roduct1 FROM C5_P_roduct WHERE 1) AS cp WHERE vfi.AttP_roduct=cp.AttP_roduct AND vfi.AttP_roduct1=cp.AttP_roduct1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "kind[Application*Product] is univalent"<BR>',3);
      return false;
    }return true;
  }
  function checkRule15(){
    // No violations should occur in (I[Application] -: kind;kind~)
     // pDebug:: (kind;kind~)-/\I[Application]
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttA_pplication1 FROM (SELECT DISTINCT AttA_pplication, AttA_pplication AS AttA_pplication1 FROM C3_A_pplication WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1 FROM T6_kind AS fst, T6_kind AS snd WHERE fst.AttP_roduct = snd.AttP_roduct) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttA_pplication1=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "kind[Application*Product] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule16(){
    // No violations should occur in (kind~;kind -: I[Product])
     // pDebug:: I[Product]-/\kind~;kind
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_roduct, vfi.AttP_roduct1 FROM (SELECT DISTINCT fst.AttP_roduct, snd.AttP_roduct AS AttP_roduct1 FROM T7_kind AS fst, T7_kind AS snd WHERE fst.AttD_ecision = snd.AttD_ecision) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttP_roduct, AttP_roduct AS AttP_roduct1 FROM C5_P_roduct WHERE 1) AS cp WHERE vfi.AttP_roduct=cp.AttP_roduct AND vfi.AttP_roduct1=cp.AttP_roduct1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "kind[Decision*Product] is univalent"<BR>',3);
      return false;
    }return true;
  }
  function checkRule17(){
    // No violations should occur in (I[Decision] -: kind;kind~)
     // pDebug:: (kind;kind~)-/\I[Decision]
     $v=DB_doquer('SELECT DISTINCT vfi.AttD_ecision, vfi.AttD_ecision1 FROM (SELECT DISTINCT AttD_ecision, AttD_ecision AS AttD_ecision1 FROM C6_D_ecision WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttD_ecision, snd.AttD_ecision AS AttD_ecision1 FROM T7_kind AS fst, T7_kind AS snd WHERE fst.AttP_roduct = snd.AttP_roduct) AS cp WHERE vfi.AttD_ecision=cp.AttD_ecision AND vfi.AttD_ecision1=cp.AttD_ecision1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "kind[Decision*Product] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule18(){
    // No violations should occur in (I[Area] -: inhabitant~;inhabitant)
     // pDebug:: (inhabitant~;inhabitant)-/\I[Area]
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_rea, vfi.AttA_rea1 FROM (SELECT DISTINCT AttA_rea, AttA_rea AS AttA_rea1 FROM C7_A_rea WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_rea, snd.AttA_rea AS AttA_rea1 FROM T8_inhabitant AS fst, T8_inhabitant AS snd WHERE fst.AttP_erson = snd.AttP_erson) AS cp WHERE vfi.AttA_rea=cp.AttA_rea AND vfi.AttA_rea1=cp.AttA_rea1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "inhabitant[Person*Area] is surjective"<BR>',3);
      return false;
    }return true;
  }
  function checkRule19(){
    // No violations should occur in (inhabitant~;inhabitant -: I[Area])
     // pDebug:: I[Area]-/\inhabitant~;inhabitant
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_rea, vfi.AttA_rea1 FROM (SELECT DISTINCT fst.AttA_rea, snd.AttA_rea AS AttA_rea1 FROM T8_inhabitant AS fst, T8_inhabitant AS snd WHERE fst.AttP_erson = snd.AttP_erson) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttA_rea, AttA_rea AS AttA_rea1 FROM C7_A_rea WHERE 1) AS cp WHERE vfi.AttA_rea=cp.AttA_rea AND vfi.AttA_rea1=cp.AttA_rea1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "inhabitant[Person*Area] is univalent"<BR>',3);
      return false;
    }return true;
  }
  function checkRule20(){
    // No violations should occur in (I[Person] -: inhabitant;inhabitant~)
     // pDebug:: (inhabitant;inhabitant~)-/\I[Person]
     $v=DB_doquer('SELECT DISTINCT vfi.AttP_erson, vfi.AttP_erson1 FROM (SELECT DISTINCT AttP_erson, AttP_erson AS AttP_erson1 FROM C1_P_erson WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttP_erson, snd.AttP_erson AS AttP_erson1 FROM T8_inhabitant AS fst, T8_inhabitant AS snd WHERE fst.AttA_rea = snd.AttA_rea) AS cp WHERE vfi.AttP_erson=cp.AttP_erson AND vfi.AttP_erson1=cp.AttP_erson1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "inhabitant[Person*Area] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule21(){
    // No violations should occur in (I[Employee] -: area;area~)
     // pDebug:: (area;area~)-/\I[Employee]
     $v=DB_doquer('SELECT DISTINCT vfi.AttE_mployee, vfi.AttE_mployee1 FROM (SELECT DISTINCT AttE_mployee, AttE_mployee AS AttE_mployee1 FROM C4_E_mployee WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttE_mployee, snd.AttE_mployee AS AttE_mployee1 FROM T9_area AS fst, T9_area AS snd WHERE fst.AttA_rea = snd.AttA_rea) AS cp WHERE vfi.AttE_mployee=cp.AttE_mployee AND vfi.AttE_mployee1=cp.AttE_mployee1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "area[Employee*Area] is total"<BR>',3);
      return false;
    }return true;
  }
  function checkRule22(){
    // No violations should occur in (I[Area] -: area~;area)
     // pDebug:: (area~;area)-/\I[Area]
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_rea, vfi.AttA_rea1 FROM (SELECT DISTINCT AttA_rea, AttA_rea AS AttA_rea1 FROM C7_A_rea WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_rea, snd.AttA_rea AS AttA_rea1 FROM T9_area AS fst, T9_area AS snd WHERE fst.AttE_mployee = snd.AttE_mployee) AS cp WHERE vfi.AttA_rea=cp.AttA_rea AND vfi.AttA_rea1=cp.AttA_rea1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "area[Employee*Area] is surjective"<BR>',3);
      return false;
    }return true;
  }
  function checkRule23(){
    // No violations should occur in (leadsto~;leadsto -: I[Decision])
     // pDebug:: I[Decision]-/\leadsto~;leadsto
     $v=DB_doquer('SELECT DISTINCT vfi.AttD_ecision, vfi.AttD_ecision1 FROM (SELECT DISTINCT fst.AttD_ecision, snd.AttD_ecision AS AttD_ecision1 FROM T10_leadsto AS fst, T10_leadsto AS snd WHERE fst.AttA_pplication = snd.AttA_pplication) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT AttD_ecision, AttD_ecision AS AttD_ecision1 FROM C6_D_ecision WHERE 1) AS cp WHERE vfi.AttD_ecision=cp.AttD_ecision AND vfi.AttD_ecision1=cp.AttD_ecision1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "leadsto[Application*Decision] is univalent"<BR>',3);
      return false;
    }return true;
  }
  function checkRule24(){
    // No violations should occur in (I[Application] -: leadsto;leadsto~)
     // pDebug:: (leadsto;leadsto~)-/\I[Application]
     $v=DB_doquer('SELECT DISTINCT vfi.AttA_pplication, vfi.AttA_pplication1 FROM (SELECT DISTINCT AttA_pplication, AttA_pplication AS AttA_pplication1 FROM C3_A_pplication WHERE 1) AS vfi WHERE NOT EXISTS (SELECT * FROM (SELECT DISTINCT fst.AttA_pplication, snd.AttA_pplication AS AttA_pplication1 FROM T10_leadsto AS fst, T10_leadsto AS snd WHERE fst.AttD_ecision = snd.AttD_ecision) AS cp WHERE vfi.AttA_pplication=cp.AttA_pplication AND vfi.AttA_pplication1=cp.AttA_pplication1)');
     if(count($v)) {
      DB_debug('Overtreding van de regel: "leadsto[Application*Decision] is total"<BR>',3);
      return false;
    }return true;
  }
  
  if(!$DB_slct){
        DB_debug( "Warning: error connecting to database, building database",2 );
        mysql_query("CREATE DATABASE ".$DB_daba,$DB_link) or die('Could not create DB');
        $DB_slct = mysql_select_db($DB_daba,$DB_link) or die ('Could not select DB');
        $DB_errs = false;
        
        DB_doquer("CREATE TABLE T1_authentic (AttP_erson varchar(200) NOT NULL default '', AttID_document varchar(200) NOT NULL default '', UNIQUE  (AttP_erson,AttID_document) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T1_authentic (AttP_erson,AttID_document) VALUES ('Joosten','377748C'), ('Lemmen','M18263772'), ('Mans','NL3955040339'), ('Old\' Bevelsborg','M18294322'), ('Pietersen','NL3133606015')");
        DB_doquer("CREATE TABLE T2_applicant (AttA_pplication varchar(200) NOT NULL default '', AttP_erson varchar(200) NOT NULL default '', UNIQUE  (AttA_pplication,AttP_erson) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T2_applicant (AttA_pplication,AttP_erson) VALUES ('Q87','Pietersen'), ('Q88','Lemmen')");
        DB_doquer("CREATE TABLE T3_checked (AttA_pplication varchar(200) NOT NULL default '', AttID_document varchar(200) NOT NULL default '', UNIQUE  (AttA_pplication,AttID_document) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T3_checked (AttA_pplication,AttID_document) VALUES ('Q87','NL3133606015'), ('Q88','M18263772')");
        DB_doquer("CREATE TABLE T4_assigned (AttA_pplication varchar(200) NOT NULL default '', AttE_mployee varchar(200) NOT NULL default '', UNIQUE  (AttA_pplication,AttE_mployee) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T4_assigned (AttA_pplication,AttE_mployee) VALUES ('Q87','Mans'), ('Q88','Old\' Bevelsborg')");
        DB_doquer("CREATE TABLE T5_auth (AttE_mployee varchar(200) NOT NULL default '', AttP_roduct varchar(200) NOT NULL default '', UNIQUE  (AttE_mployee,AttP_roduct) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T5_auth (AttE_mployee,AttP_roduct) VALUES ('Mans','building permit'), ('Mans','hunting permit'), ('Old\' Bevelsborg','building permit'), ('Old\' Bevelsborg','work permit'), ('v.d.Knaap','building permit'), ('v.d.Knaap','hunting permit')");
        DB_doquer("CREATE TABLE T6_kind (AttA_pplication varchar(200) NOT NULL default '', AttP_roduct varchar(200) NOT NULL default '', UNIQUE  (AttA_pplication,AttP_roduct) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T6_kind (AttA_pplication,AttP_roduct) VALUES ('Q87','hunting permit'), ('Q88','work permit')");
        DB_doquer("CREATE TABLE T7_kind (AttD_ecision varchar(200) NOT NULL default '', AttP_roduct varchar(200) NOT NULL default '', UNIQUE  (AttD_ecision,AttP_roduct) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T7_kind (AttD_ecision,AttP_roduct) VALUES ('D87-2005/04/29','hunting permit'), ('D88-2005/05/10','work permit')");
        DB_doquer("CREATE TABLE T8_inhabitant (AttP_erson varchar(200) NOT NULL default '', AttA_rea varchar(200) NOT NULL default '', UNIQUE  (AttP_erson,AttA_rea) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T8_inhabitant (AttP_erson,AttA_rea) VALUES ('Joosten','City'), ('Lemmen','Soho'), ('Mans','West End'), ('Old\' Bevelsborg','City'), ('Pietersen','Soho')");
        DB_doquer("CREATE TABLE T9_area (AttE_mployee varchar(200) NOT NULL default '', AttA_rea varchar(200) NOT NULL default '', UNIQUE  (AttE_mployee,AttA_rea) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T9_area (AttE_mployee,AttA_rea) VALUES ('Mans','Soho'), ('Old\' Bevelsborg','City'), ('Old\' Bevelsborg','Soho'), ('v.d.Knaap','City'), ('v.d.Knaap','West End')");
        DB_doquer("CREATE TABLE T10_leadsto (AttA_pplication varchar(200) NOT NULL default '', AttD_ecision varchar(200) NOT NULL default '', UNIQUE  (AttA_pplication,AttD_ecision) ) TYPE=InnoDB");
        DB_doquer("INSERT IGNORE INTO T10_leadsto (AttA_pplication,AttD_ecision) VALUES ('Q87','D87-2005/04/29'), ('Q88','D88-2005/05/10')");
        DB_doquer("CREATE TABLE C1_P_erson (AttP_erson varchar(200) NOT NULL default '', UNIQUE  (AttP_erson)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C1_P_erson (AttP_erson) SELECT DISTINCT AttP_erson FROM T1_authentic UNION SELECT DISTINCT AttP_erson FROM T8_inhabitant UNION SELECT DISTINCT AttP_erson FROM T2_applicant');
        DB_doquer("CREATE TABLE C2_ID_document (AttID_document varchar(200) NOT NULL default '', UNIQUE  (AttID_document)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C2_ID_document (AttID_document) SELECT DISTINCT AttID_document FROM T1_authentic UNION SELECT DISTINCT AttID_document FROM T3_checked');
        DB_doquer("CREATE TABLE C3_A_pplication (AttA_pplication varchar(200) NOT NULL default '', UNIQUE  (AttA_pplication)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C3_A_pplication (AttA_pplication) SELECT DISTINCT AttA_pplication FROM T2_applicant UNION SELECT DISTINCT AttA_pplication FROM T3_checked UNION SELECT DISTINCT AttA_pplication FROM T4_assigned UNION SELECT DISTINCT AttA_pplication FROM T6_kind UNION SELECT DISTINCT AttA_pplication FROM T10_leadsto');
        DB_doquer("CREATE TABLE C4_E_mployee (AttE_mployee varchar(200) NOT NULL default '', UNIQUE  (AttE_mployee)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C4_E_mployee (AttE_mployee) SELECT DISTINCT AttE_mployee FROM T5_auth UNION SELECT DISTINCT AttE_mployee FROM T9_area UNION SELECT DISTINCT AttE_mployee FROM T4_assigned');
        DB_doquer("CREATE TABLE C5_P_roduct (AttP_roduct varchar(200) NOT NULL default '', UNIQUE  (AttP_roduct)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C5_P_roduct (AttP_roduct) SELECT DISTINCT AttP_roduct FROM T5_auth UNION SELECT DISTINCT AttP_roduct FROM T6_kind UNION SELECT DISTINCT AttP_roduct FROM T7_kind');
        DB_doquer("CREATE TABLE C6_D_ecision (AttD_ecision varchar(200) NOT NULL default '', UNIQUE  (AttD_ecision)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C6_D_ecision (AttD_ecision) SELECT DISTINCT AttD_ecision FROM T7_kind UNION SELECT DISTINCT AttD_ecision FROM T10_leadsto');
        DB_doquer("CREATE TABLE C7_A_rea (AttA_rea varchar(200) NOT NULL default '', UNIQUE  (AttA_rea)) TYPE=InnoDB");
        DB_doquer('INSERT IGNORE INTO C7_A_rea (AttA_rea) SELECT DISTINCT AttA_rea FROM T8_inhabitant UNION SELECT DISTINCT AttA_rea FROM T9_area');
        
        if($DB_errs || !(checkRule1() && checkRule2() && checkRule3() && checkRule4()))
        {  DB_debug( "DB errors, removing database",0);
           mysql_query("DROP DATABASE ".$DB_daba,$DB_link) or die('Could not delete DB');
           die ('Errors creating database');
        }
  }else{
    DB_debug( "Connected to database",4 );
  }
  
  DB_doquer('SET GLOBAL TRANSACTION ISOLATION LEVEL READ COMMITTED');
  
  if($DB_debug>=3){
    checkRule1();
    checkRule2();
    checkRule3();
    checkRule4();
  }
?>