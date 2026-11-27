/*
**********************************************************************************************************
*** Ampersand-v5.15.0 [trial/eissamenvoegbaar-variants:5901e4df1*], build time: 22-Nov-26 08:27:11 CET ***
**********************************************************************************************************
*/
/*
********************************************************************************
***                        Database structure queries                        ***
********************************************************************************
*/
/* ------------------------------------------------------------------- */
/* Plug PrototypeContext.Interface                                     */
/*                                                                     */
/* attributes:                                                         */
/* I[PrototypeContext.Interface]                                       */
/* ifcCanCreate[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ifcCanDelete[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ifcCanUpdate[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ifcConcept[PrototypeContext.Interface*PrototypeContext.ConceptName] */
/* isAPI[PrototypeContext.Interface*PrototypeContext.Interface]        */
/* isPublic[PrototypeContext.Interface*PrototypeContext.Interface]     */
/* label[PrototypeContext.Interface*PrototypeContext.Label]            */
/* ------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.Interface"
     ( "PrototypeContext.Interface" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , "PrototypeContext.ifcCanCreate" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.ifcCanDelete" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.ifcCanUpdate" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.ifcConcept" VARCHAR(255) DEFAULT NULL /* ALPHANUMERIC */
     , "PrototypeContext.isAPI" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.isPublic" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.label" VARCHAR(255) DEFAULT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("PrototypeContext.Interface")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.Interface_0" ON "PrototypeContext.Interface" ("PrototypeContext.ifcCanCreate");
CREATE INDEX "PrototypeContext.Interface_1" ON "PrototypeContext.Interface" ("PrototypeContext.ifcCanDelete");
CREATE INDEX "PrototypeContext.Interface_2" ON "PrototypeContext.Interface" ("PrototypeContext.ifcCanUpdate");
CREATE INDEX "PrototypeContext.Interface_3" ON "PrototypeContext.Interface" ("PrototypeContext.ifcConcept");
CREATE INDEX "PrototypeContext.Interface_4" ON "PrototypeContext.Interface" ("PrototypeContext.isAPI");
CREATE INDEX "PrototypeContext.Interface_5" ON "PrototypeContext.Interface" ("PrototypeContext.isPublic");
CREATE INDEX "PrototypeContext.Interface_6" ON "PrototypeContext.Interface" ("PrototypeContext.label");
/* ---------------------------------------------------------------------- */
/* Plug PrototypeContext.NavMenuItem                                      */
/*                                                                        */
/* attributes:                                                            */
/* I[PrototypeContext.NavMenuItem]                                        */
/* I[PrototypeContext.NavMenu]                                            */
/* ifc[PrototypeContext.NavMenuItem*PrototypeContext.Interface]           */
/* isPartOf[PrototypeContext.NavMenuItem*PrototypeContext.NavMenu]        */
/* isSubItemOf[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem] */
/* isVisible[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]   */
/* label[PrototypeContext.NavMenuItem*PrototypeContext.Label]             */
/* seqNr[PrototypeContext.NavMenuItem*PrototypeContext.SeqNr]             */
/* url[PrototypeContext.NavMenuItem*PrototypeContext.URL]                 */
/* ---------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.NavMenuItem"
     ( "PrototypeContext.NavMenuItem" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , "PrototypeContext.NavMenu" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.ifc" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.isPartOf" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.isSubItemOf" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.isVisible" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , "PrototypeContext.label" VARCHAR(255) DEFAULT NULL /* ALPHANUMERIC */
     , "PrototypeContext.seqNr" BIGINT DEFAULT NULL /* INTEGER */
     , "PrototypeContext.url" TEXT DEFAULT NULL /* BIGALPHANUMERIC */
     , PRIMARY KEY ("PrototypeContext.NavMenuItem")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.NavMenuItem_0" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.NavMenu");
CREATE INDEX "PrototypeContext.NavMenuItem_1" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.ifc");
CREATE INDEX "PrototypeContext.NavMenuItem_2" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.isPartOf");
CREATE INDEX "PrototypeContext.NavMenuItem_3" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.isSubItemOf");
CREATE INDEX "PrototypeContext.NavMenuItem_4" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.isVisible");
CREATE INDEX "PrototypeContext.NavMenuItem_5" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.label");
CREATE INDEX "PrototypeContext.NavMenuItem_6" ON "PrototypeContext.NavMenuItem" ("PrototypeContext.seqNr");
/* --------------------------------------------------- */
/* Plug PrototypeContext.Role                          */
/*                                                     */
/* attributes:                                         */
/* I[PrototypeContext.Role]                            */
/* label[PrototypeContext.Role*PrototypeContext.Label] */
/* --------------------------------------------------- */
CREATE TABLE "PrototypeContext.Role"
     ( "PrototypeContext.Role" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , "PrototypeContext.label" VARCHAR(255) DEFAULT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("PrototypeContext.Role")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.Role_0" ON "PrototypeContext.Role" ("PrototypeContext.label");
/* ------------------------------------------------- */
/* Plug SESSION                                      */
/*                                                   */
/* attributes:                                       */
/* I[SESSION]                                        */
/* lastAccess[SESSION*PrototypeContext.DateTime]     */
/* sessionActiveRoles[SESSION*PrototypeContext.Role] */
/* ------------------------------------------------- */
CREATE TABLE "SESSION"
     ( "SESSION" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , "PrototypeContext.lastAccess" DATETIME DEFAULT NULL /* DATETIME */
     , "PrototypeContext.sessionActiveRoles" VARCHAR(255) DEFAULT NULL /* OBJECT */
     , PRIMARY KEY ("SESSION")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "SESSION_0" ON "SESSION" ("PrototypeContext.lastAccess");
CREATE INDEX "SESSION_1" ON "SESSION" ("PrototypeContext.sessionActiveRoles");
/* ----------------------------------------------------------------------------------------------------------------------------------------------------- */
/* Plug PrototypeContext.ifcRoles                                                                                                                        */
/*                                                                                                                                                       */
/* attributes:                                                                                                                                           */
/* I[PrototypeContext.Interface]/\ifcRoles[PrototypeContext.Interface*PrototypeContext.Role];ifcRoles[PrototypeContext.Interface*PrototypeContext.Role]~ */
/* ifcRoles[PrototypeContext.Interface*PrototypeContext.Role]                                                                                            */
/* ----------------------------------------------------------------------------------------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.ifcRoles"
     ( "PrototypeContext.Interface" VARCHAR(255) NOT NULL /* OBJECT */
     , "PrototypeContext.Role" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("PrototypeContext.Interface", "PrototypeContext.Role")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.ifcRoles_0" ON "PrototypeContext.ifcRoles" ("PrototypeContext.Interface");
CREATE INDEX "PrototypeContext.ifcRoles_1" ON "PrototypeContext.ifcRoles" ("PrototypeContext.Role");
/* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
/* Plug PrototypeContext.isSubItemOfCopy                                                                                                                                                   */
/*                                                                                                                                                                                         */
/* attributes:                                                                                                                                                                             */
/* I[PrototypeContext.NavMenuItem]/\isSubItemOfCopy[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem];isSubItemOfCopy[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]~ */
/* isSubItemOfCopy[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]                                                                                                              */
/* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.isSubItemOfCopy"
     ( "PrototypeContext.SrcNavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , "PrototypeContext.TgtNavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("PrototypeContext.SrcNavMenuItem", "PrototypeContext.TgtNavMenuItem")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.isSubItemOfCopy_0" ON "PrototypeContext.isSubItemOfCopy" ("PrototypeContext.SrcNavMenuItem");
CREATE INDEX "PrototypeContext.isSubItemOfCopy_1" ON "PrototypeContext.isSubItemOfCopy" ("PrototypeContext.TgtNavMenuItem");
/* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
/* Plug PrototypeContext.isSubItemOfPlus                                                                                                                                                   */
/*                                                                                                                                                                                         */
/* attributes:                                                                                                                                                                             */
/* I[PrototypeContext.NavMenuItem]/\isSubItemOfPlus[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem];isSubItemOfPlus[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]~ */
/* isSubItemOfPlus[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]                                                                                                              */
/* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.isSubItemOfPlus"
     ( "PrototypeContext.SrcNavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , "PrototypeContext.TgtNavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("PrototypeContext.SrcNavMenuItem", "PrototypeContext.TgtNavMenuItem")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.isSubItemOfPlus_0" ON "PrototypeContext.isSubItemOfPlus" ("PrototypeContext.SrcNavMenuItem");
CREATE INDEX "PrototypeContext.isSubItemOfPlus_1" ON "PrototypeContext.isSubItemOfPlus" ("PrototypeContext.TgtNavMenuItem");
/* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
/* Plug PrototypeContext.isSubItemOfStar                                                                                                                                                   */
/*                                                                                                                                                                                         */
/* attributes:                                                                                                                                                                             */
/* I[PrototypeContext.NavMenuItem]/\isSubItemOfStar[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem];isSubItemOfStar[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]~ */
/* isSubItemOfStar[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem]                                                                                                              */
/* --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.isSubItemOfStar"
     ( "PrototypeContext.SrcNavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , "PrototypeContext.TgtNavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("PrototypeContext.SrcNavMenuItem", "PrototypeContext.TgtNavMenuItem")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.isSubItemOfStar_0" ON "PrototypeContext.isSubItemOfStar" ("PrototypeContext.SrcNavMenuItem");
CREATE INDEX "PrototypeContext.isSubItemOfStar_1" ON "PrototypeContext.isSubItemOfStar" ("PrototypeContext.TgtNavMenuItem");
/* ------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
/* Plug PrototypeContext.navItemRoles                                                                                                                                  */
/*                                                                                                                                                                     */
/* attributes:                                                                                                                                                         */
/* I[PrototypeContext.NavMenuItem]/\navItemRoles[PrototypeContext.NavMenuItem*PrototypeContext.Role];navItemRoles[PrototypeContext.NavMenuItem*PrototypeContext.Role]~ */
/* navItemRoles[PrototypeContext.NavMenuItem*PrototypeContext.Role]                                                                                                    */
/* ------------------------------------------------------------------------------------------------------------------------------------------------------------------- */
CREATE TABLE "PrototypeContext.navItemRoles"
     ( "PrototypeContext.NavMenuItem" VARCHAR(255) NOT NULL /* OBJECT */
     , "PrototypeContext.Role" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("PrototypeContext.NavMenuItem", "PrototypeContext.Role")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.navItemRoles_0" ON "PrototypeContext.navItemRoles" ("PrototypeContext.NavMenuItem");
CREATE INDEX "PrototypeContext.navItemRoles_1" ON "PrototypeContext.navItemRoles" ("PrototypeContext.Role");
/* ------------------------------------------------------------------------------------------------------------------ */
/* Plug PrototypeContext.sessionAllowedRoles                                                                          */
/*                                                                                                                    */
/* attributes:                                                                                                        */
/* I[SESSION]/\sessionAllowedRoles[SESSION*PrototypeContext.Role];sessionAllowedRoles[SESSION*PrototypeContext.Role]~ */
/* sessionAllowedRoles[SESSION*PrototypeContext.Role]                                                                 */
/* ------------------------------------------------------------------------------------------------------------------ */
CREATE TABLE "PrototypeContext.sessionAllowedRoles"
     ( "SESSION" VARCHAR(255) NOT NULL /* OBJECT */
     , "PrototypeContext.Role" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("SESSION", "PrototypeContext.Role")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "PrototypeContext.sessionAllowedRoles_0" ON "PrototypeContext.sessionAllowedRoles" ("SESSION");
CREATE INDEX "PrototypeContext.sessionAllowedRoles_1" ON "PrototypeContext.sessionAllowedRoles" ("PrototypeContext.Role");
/* ------------------------ */
/* Plug big                 */
/*                          */
/* attributes:              */
/* I[X]/\big[X*X];big[X*X]~ */
/* big[X*X]                 */
/* ------------------------ */
CREATE TABLE "big"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "big_0" ON "big" ("SrcX");
CREATE INDEX "big_1" ON "big" ("TgtX");
/* -------------------------- */
/* Plug bigC                  */
/*                            */
/* attributes:                */
/* I[X]/\bigC[X*X];bigC[X*X]~ */
/* bigC[X*X]                  */
/* -------------------------- */
CREATE TABLE "bigC"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "bigC_0" ON "bigC" ("SrcX");
CREATE INDEX "bigC_1" ON "bigC" ("TgtX");
/* ---------------------------- */
/* Plug bigIU                   */
/*                              */
/* attributes:                  */
/* I[X]/\bigIU[X*X];bigIU[X*X]~ */
/* bigIU[X*X]                   */
/* ---------------------------- */
CREATE TABLE "bigIU"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "bigIU_0" ON "bigIU" ("SrcX");
CREATE INDEX "bigIU_1" ON "bigIU" ("TgtX");
/* -------------------------- */
/* Plug bigU                  */
/*                            */
/* attributes:                */
/* I[X]/\bigU[X*X];bigU[X*X]~ */
/* bigU[X*X]                  */
/* -------------------------- */
CREATE TABLE "bigU"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "bigU_0" ON "bigU" ("SrcX");
CREATE INDEX "bigU_1" ON "bigU" ("TgtX");
/* -------------------- */
/* Plug p               */
/*                      */
/* attributes:          */
/* I[X]/\p[X*X];p[X*X]~ */
/* p[X*X]               */
/* -------------------- */
CREATE TABLE "p"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "p_0" ON "p" ("SrcX");
CREATE INDEX "p_1" ON "p" ("TgtX");
/* -------------------- */
/* Plug q               */
/*                      */
/* attributes:          */
/* I[X]/\q[X*X];q[X*X]~ */
/* q[X*X]               */
/* -------------------- */
CREATE TABLE "q"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "q_0" ON "q" ("SrcX");
CREATE INDEX "q_1" ON "q" ("TgtX");
/* ---------------------------- */
/* Plug small                   */
/*                              */
/* attributes:                  */
/* I[X]/\small[X*X];small[X*X]~ */
/* small[X*X]                   */
/* ---------------------------- */
CREATE TABLE "small"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "small_0" ON "small" ("SrcX");
CREATE INDEX "small_1" ON "small" ("TgtX");
/* ------------------------------ */
/* Plug smallC                    */
/*                                */
/* attributes:                    */
/* I[X]/\smallC[X*X];smallC[X*X]~ */
/* smallC[X*X]                    */
/* ------------------------------ */
CREATE TABLE "smallC"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "smallC_0" ON "smallC" ("SrcX");
CREATE INDEX "smallC_1" ON "smallC" ("TgtX");
/* -------------------------------- */
/* Plug smallIU                     */
/*                                  */
/* attributes:                      */
/* I[X]/\smallIU[X*X];smallIU[X*X]~ */
/* smallIU[X*X]                     */
/* -------------------------------- */
CREATE TABLE "smallIU"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "smallIU_0" ON "smallIU" ("SrcX");
CREATE INDEX "smallIU_1" ON "smallIU" ("TgtX");
/* ------------------------------ */
/* Plug smallU                    */
/*                                */
/* attributes:                    */
/* I[X]/\smallU[X*X];smallU[X*X]~ */
/* smallU[X*X]                    */
/* ------------------------------ */
CREATE TABLE "smallU"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "smallU_0" ON "smallU" ("SrcX");
CREATE INDEX "smallU_1" ON "smallU" ("TgtX");
/* -------------------- */
/* Plug t               */
/*                      */
/* attributes:          */
/* I[X]/\t[X*X];t[X*X]~ */
/* t[X*X]               */
/* -------------------- */
CREATE TABLE "t"
     ( "SrcX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "TgtX" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("SrcX", "TgtX")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "t_0" ON "t" ("SrcX");
CREATE INDEX "t_1" ON "t" ("TgtX");
/* ------------------------------------------------------------------------------------------ */
/* Plug delta_PrototypeContext_ifc_782883                                                     */
/*                                                                                            */
/* attributes:                                                                                */
/* I[PrototypeContext.NavMenuItem]                                                            */
/* delta_PrototypeContext_ifc_782883[PrototypeContext.NavMenuItem*PrototypeContext.Interface] */
/* ------------------------------------------------------------------------------------------ */
CREATE TABLE "delta_PrototypeContext_ifc_782883"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_ifc_782883_0" ON "delta_PrototypeContext_ifc_782883" ("src");
CREATE INDEX "delta_PrototypeContext_ifc_782883_1" ON "delta_PrototypeContext_ifc_782883" ("tgt");
/* ------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_ifcCanCreate_168546                                                   */
/*                                                                                                   */
/* attributes:                                                                                       */
/* I[PrototypeContext.Interface]                                                                     */
/* delta_PrototypeContext_ifcCanCreate_168546[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_ifcCanCreate_168546"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_ifcCanCreate_168546_0" ON "delta_PrototypeContext_ifcCanCreate_168546" ("src");
CREATE INDEX "delta_PrototypeContext_ifcCanCreate_168546_1" ON "delta_PrototypeContext_ifcCanCreate_168546" ("tgt");
/* ------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_ifcCanDelete_213513                                                   */
/*                                                                                                   */
/* attributes:                                                                                       */
/* I[PrototypeContext.Interface]                                                                     */
/* delta_PrototypeContext_ifcCanDelete_213513[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_ifcCanDelete_213513"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_ifcCanDelete_213513_0" ON "delta_PrototypeContext_ifcCanDelete_213513" ("src");
CREATE INDEX "delta_PrototypeContext_ifcCanDelete_213513_1" ON "delta_PrototypeContext_ifcCanDelete_213513" ("tgt");
/* ------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_ifcCanUpdate_814655                                                   */
/*                                                                                                   */
/* attributes:                                                                                       */
/* I[PrototypeContext.Interface]                                                                     */
/* delta_PrototypeContext_ifcCanUpdate_814655[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_ifcCanUpdate_814655"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_ifcCanUpdate_814655_0" ON "delta_PrototypeContext_ifcCanUpdate_814655" ("src");
CREATE INDEX "delta_PrototypeContext_ifcCanUpdate_814655_1" ON "delta_PrototypeContext_ifcCanUpdate_814655" ("tgt");
/* ------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_ifcConcept_444924                                                     */
/*                                                                                                   */
/* attributes:                                                                                       */
/* I[PrototypeContext.Interface]                                                                     */
/* delta_PrototypeContext_ifcConcept_444924[PrototypeContext.Interface*PrototypeContext.ConceptName] */
/* ------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_ifcConcept_444924"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_ifcConcept_444924_0" ON "delta_PrototypeContext_ifcConcept_444924" ("src");
CREATE INDEX "delta_PrototypeContext_ifcConcept_444924_1" ON "delta_PrototypeContext_ifcConcept_444924" ("tgt");
/* ---------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_ifcRoles_818277                                              */
/*                                                                                          */
/* attributes:                                                                              */
/* I[PrototypeContext.Interface]                                                            */
/* delta_PrototypeContext_ifcRoles_818277[PrototypeContext.Interface*PrototypeContext.Role] */
/* ---------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_ifcRoles_818277"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_ifcRoles_818277_0" ON "delta_PrototypeContext_ifcRoles_818277" ("src");
CREATE INDEX "delta_PrototypeContext_ifcRoles_818277_1" ON "delta_PrototypeContext_ifcRoles_818277" ("tgt");
/* ------------------------------------------------------------------------------------------ */
/* Plug delta_PrototypeContext_isAPI_645235                                                   */
/*                                                                                            */
/* attributes:                                                                                */
/* I[PrototypeContext.Interface]                                                              */
/* delta_PrototypeContext_isAPI_645235[PrototypeContext.Interface*PrototypeContext.Interface] */
/* ------------------------------------------------------------------------------------------ */
CREATE TABLE "delta_PrototypeContext_isAPI_645235"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isAPI_645235_0" ON "delta_PrototypeContext_isAPI_645235" ("src");
CREATE INDEX "delta_PrototypeContext_isAPI_645235_1" ON "delta_PrototypeContext_isAPI_645235" ("tgt");
/* --------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isPartOf_629321                                                   */
/*                                                                                               */
/* attributes:                                                                                   */
/* I[PrototypeContext.NavMenuItem]                                                               */
/* delta_PrototypeContext_isPartOf_629321[PrototypeContext.NavMenuItem*PrototypeContext.NavMenu] */
/* --------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isPartOf_629321"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isPartOf_629321_0" ON "delta_PrototypeContext_isPartOf_629321" ("src");
CREATE INDEX "delta_PrototypeContext_isPartOf_629321_1" ON "delta_PrototypeContext_isPartOf_629321" ("tgt");
/* --------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isPublic_174827                                                   */
/*                                                                                               */
/* attributes:                                                                                   */
/* I[PrototypeContext.Interface]                                                                 */
/* delta_PrototypeContext_isPublic_174827[PrototypeContext.Interface*PrototypeContext.Interface] */
/* --------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isPublic_174827"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isPublic_174827_0" ON "delta_PrototypeContext_isPublic_174827" ("src");
CREATE INDEX "delta_PrototypeContext_isPublic_174827_1" ON "delta_PrototypeContext_isPublic_174827" ("tgt");
/* ---------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isSubItemOf_344881                                                       */
/*                                                                                                      */
/* attributes:                                                                                          */
/* I[PrototypeContext.NavMenuItem]                                                                      */
/* delta_PrototypeContext_isSubItemOf_344881[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem] */
/* ---------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isSubItemOf_344881"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isSubItemOf_344881_0" ON "delta_PrototypeContext_isSubItemOf_344881" ("src");
CREATE INDEX "delta_PrototypeContext_isSubItemOf_344881_1" ON "delta_PrototypeContext_isSubItemOf_344881" ("tgt");
/* -------------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isSubItemOfCopy_208468                                                       */
/*                                                                                                          */
/* attributes:                                                                                              */
/* I[PrototypeContext.NavMenuItem]                                                                          */
/* delta_PrototypeContext_isSubItemOfCopy_208468[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem] */
/* -------------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isSubItemOfCopy_208468"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isSubItemOfCopy_208468_0" ON "delta_PrototypeContext_isSubItemOfCopy_208468" ("src");
CREATE INDEX "delta_PrototypeContext_isSubItemOfCopy_208468_1" ON "delta_PrototypeContext_isSubItemOfCopy_208468" ("tgt");
/* ------------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isSubItemOfPlus_82203                                                       */
/*                                                                                                         */
/* attributes:                                                                                             */
/* I[PrototypeContext.NavMenuItem]                                                                         */
/* delta_PrototypeContext_isSubItemOfPlus_82203[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem] */
/* ------------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isSubItemOfPlus_82203"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isSubItemOfPlus_82203_0" ON "delta_PrototypeContext_isSubItemOfPlus_82203" ("src");
CREATE INDEX "delta_PrototypeContext_isSubItemOfPlus_82203_1" ON "delta_PrototypeContext_isSubItemOfPlus_82203" ("tgt");
/* -------------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isSubItemOfStar_829867                                                       */
/*                                                                                                          */
/* attributes:                                                                                              */
/* I[PrototypeContext.NavMenuItem]                                                                          */
/* delta_PrototypeContext_isSubItemOfStar_829867[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem] */
/* -------------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isSubItemOfStar_829867"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isSubItemOfStar_829867_0" ON "delta_PrototypeContext_isSubItemOfStar_829867" ("src");
CREATE INDEX "delta_PrototypeContext_isSubItemOfStar_829867_1" ON "delta_PrototypeContext_isSubItemOfStar_829867" ("tgt");
/* ------------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_isVisible_60059                                                       */
/*                                                                                                   */
/* attributes:                                                                                       */
/* I[PrototypeContext.NavMenuItem]                                                                   */
/* delta_PrototypeContext_isVisible_60059[PrototypeContext.NavMenuItem*PrototypeContext.NavMenuItem] */
/* ------------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_isVisible_60059"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_isVisible_60059_0" ON "delta_PrototypeContext_isVisible_60059" ("src");
CREATE INDEX "delta_PrototypeContext_isVisible_60059_1" ON "delta_PrototypeContext_isVisible_60059" ("tgt");
/* -------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_label_422100                                               */
/*                                                                                        */
/* attributes:                                                                            */
/* I[PrototypeContext.Interface]                                                          */
/* delta_PrototypeContext_label_422100[PrototypeContext.Interface*PrototypeContext.Label] */
/* -------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_label_422100"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_label_422100_0" ON "delta_PrototypeContext_label_422100" ("src");
CREATE INDEX "delta_PrototypeContext_label_422100_1" ON "delta_PrototypeContext_label_422100" ("tgt");
/* ---------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_label_865164                                                 */
/*                                                                                          */
/* attributes:                                                                              */
/* I[PrototypeContext.NavMenuItem]                                                          */
/* delta_PrototypeContext_label_865164[PrototypeContext.NavMenuItem*PrototypeContext.Label] */
/* ---------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_label_865164"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_label_865164_0" ON "delta_PrototypeContext_label_865164" ("src");
CREATE INDEX "delta_PrototypeContext_label_865164_1" ON "delta_PrototypeContext_label_865164" ("tgt");
/* --------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_label_830958                                          */
/*                                                                                   */
/* attributes:                                                                       */
/* I[PrototypeContext.Role]                                                          */
/* delta_PrototypeContext_label_830958[PrototypeContext.Role*PrototypeContext.Label] */
/* --------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_label_830958"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_label_830958_0" ON "delta_PrototypeContext_label_830958" ("src");
CREATE INDEX "delta_PrototypeContext_label_830958_1" ON "delta_PrototypeContext_label_830958" ("tgt");
/* --------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_lastAccess_915476                               */
/*                                                                             */
/* attributes:                                                                 */
/* I[SESSION]                                                                  */
/* delta_PrototypeContext_lastAccess_915476[SESSION*PrototypeContext.DateTime] */
/* --------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_lastAccess_915476"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" DATETIME NOT NULL /* DATETIME */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_lastAccess_915476_0" ON "delta_PrototypeContext_lastAccess_915476" ("src");
CREATE INDEX "delta_PrototypeContext_lastAccess_915476_1" ON "delta_PrototypeContext_lastAccess_915476" ("tgt");
/* ---------------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_navItemRoles_191119                                                */
/*                                                                                                */
/* attributes:                                                                                    */
/* I[PrototypeContext.NavMenuItem]                                                                */
/* delta_PrototypeContext_navItemRoles_191119[PrototypeContext.NavMenuItem*PrototypeContext.Role] */
/* ---------------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_navItemRoles_191119"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_navItemRoles_191119_0" ON "delta_PrototypeContext_navItemRoles_191119" ("src");
CREATE INDEX "delta_PrototypeContext_navItemRoles_191119_1" ON "delta_PrototypeContext_navItemRoles_191119" ("tgt");
/* ---------------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_seqNr_928548                                                 */
/*                                                                                          */
/* attributes:                                                                              */
/* I[PrototypeContext.NavMenuItem]                                                          */
/* delta_PrototypeContext_seqNr_928548[PrototypeContext.NavMenuItem*PrototypeContext.SeqNr] */
/* ---------------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_seqNr_928548"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" BIGINT NOT NULL /* INTEGER */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_seqNr_928548_0" ON "delta_PrototypeContext_seqNr_928548" ("src");
CREATE INDEX "delta_PrototypeContext_seqNr_928548_1" ON "delta_PrototypeContext_seqNr_928548" ("tgt");
/* ------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_sessionActiveRoles_346980                           */
/*                                                                                 */
/* attributes:                                                                     */
/* I[SESSION]                                                                      */
/* delta_PrototypeContext_sessionActiveRoles_346980[SESSION*PrototypeContext.Role] */
/* ------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_sessionActiveRoles_346980"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_sessionActiveRoles_346980_0" ON "delta_PrototypeContext_sessionActiveRoles_346980" ("src");
CREATE INDEX "delta_PrototypeContext_sessionActiveRoles_346980_1" ON "delta_PrototypeContext_sessionActiveRoles_346980" ("tgt");
/* -------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_sessionAllowedRoles_142821                           */
/*                                                                                  */
/* attributes:                                                                      */
/* I[SESSION]                                                                       */
/* delta_PrototypeContext_sessionAllowedRoles_142821[SESSION*PrototypeContext.Role] */
/* -------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_sessionAllowedRoles_142821"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_sessionAllowedRoles_142821_0" ON "delta_PrototypeContext_sessionAllowedRoles_142821" ("src");
CREATE INDEX "delta_PrototypeContext_sessionAllowedRoles_142821_1" ON "delta_PrototypeContext_sessionAllowedRoles_142821" ("tgt");
/* ----------------------------------------------------------------------------------- */
/* Plug delta_PrototypeContext_url_73096                                               */
/*                                                                                     */
/* attributes:                                                                         */
/* I[PrototypeContext.NavMenuItem]                                                     */
/* delta_PrototypeContext_url_73096[PrototypeContext.NavMenuItem*PrototypeContext.URL] */
/* ----------------------------------------------------------------------------------- */
CREATE TABLE "delta_PrototypeContext_url_73096"
     ( "src" VARCHAR(255) NOT NULL /* OBJECT */
     , "tgt" TEXT NOT NULL /* BIGALPHANUMERIC */
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_PrototypeContext_url_73096_0" ON "delta_PrototypeContext_url_73096" ("src");
/* --------------------- */
/* Plug delta_big_143241 */
/*                       */
/* attributes:           */
/* I[X]                  */
/* delta_big_143241[X*X] */
/* --------------------- */
CREATE TABLE "delta_big_143241"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_big_143241_0" ON "delta_big_143241" ("src");
CREATE INDEX "delta_big_143241_1" ON "delta_big_143241" ("tgt");
/* ---------------------- */
/* Plug delta_bigC_229061 */
/*                        */
/* attributes:            */
/* I[X]                   */
/* delta_bigC_229061[X*X] */
/* ---------------------- */
CREATE TABLE "delta_bigC_229061"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_bigC_229061_0" ON "delta_bigC_229061" ("src");
CREATE INDEX "delta_bigC_229061_1" ON "delta_bigC_229061" ("tgt");
/* ----------------------- */
/* Plug delta_bigIU_695981 */
/*                         */
/* attributes:             */
/* I[X]                    */
/* delta_bigIU_695981[X*X] */
/* ----------------------- */
CREATE TABLE "delta_bigIU_695981"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_bigIU_695981_0" ON "delta_bigIU_695981" ("src");
CREATE INDEX "delta_bigIU_695981_1" ON "delta_bigIU_695981" ("tgt");
/* ---------------------- */
/* Plug delta_bigU_398237 */
/*                        */
/* attributes:            */
/* I[X]                   */
/* delta_bigU_398237[X*X] */
/* ---------------------- */
CREATE TABLE "delta_bigU_398237"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_bigU_398237_0" ON "delta_bigU_398237" ("src");
CREATE INDEX "delta_bigU_398237_1" ON "delta_bigU_398237" ("tgt");
/* ------------------- */
/* Plug delta_p_798903 */
/*                     */
/* attributes:         */
/* I[X]                */
/* delta_p_798903[X*X] */
/* ------------------- */
CREATE TABLE "delta_p_798903"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_p_798903_0" ON "delta_p_798903" ("src");
CREATE INDEX "delta_p_798903_1" ON "delta_p_798903" ("tgt");
/* ------------------- */
/* Plug delta_q_409292 */
/*                     */
/* attributes:         */
/* I[X]                */
/* delta_q_409292[X*X] */
/* ------------------- */
CREATE TABLE "delta_q_409292"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_q_409292_0" ON "delta_q_409292" ("src");
CREATE INDEX "delta_q_409292_1" ON "delta_q_409292" ("tgt");
/* ----------------------- */
/* Plug delta_small_250908 */
/*                         */
/* attributes:             */
/* I[X]                    */
/* delta_small_250908[X*X] */
/* ----------------------- */
CREATE TABLE "delta_small_250908"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_small_250908_0" ON "delta_small_250908" ("src");
CREATE INDEX "delta_small_250908_1" ON "delta_small_250908" ("tgt");
/* ------------------------ */
/* Plug delta_smallC_637982 */
/*                          */
/* attributes:              */
/* I[X]                     */
/* delta_smallC_637982[X*X] */
/* ------------------------ */
CREATE TABLE "delta_smallC_637982"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_smallC_637982_0" ON "delta_smallC_637982" ("src");
CREATE INDEX "delta_smallC_637982_1" ON "delta_smallC_637982" ("tgt");
/* ------------------------- */
/* Plug delta_smallIU_104512 */
/*                           */
/* attributes:               */
/* I[X]                      */
/* delta_smallIU_104512[X*X] */
/* ------------------------- */
CREATE TABLE "delta_smallIU_104512"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_smallIU_104512_0" ON "delta_smallIU_104512" ("src");
CREATE INDEX "delta_smallIU_104512_1" ON "delta_smallIU_104512" ("tgt");
/* ------------------------ */
/* Plug delta_smallU_206276 */
/*                          */
/* attributes:              */
/* I[X]                     */
/* delta_smallU_206276[X*X] */
/* ------------------------ */
CREATE TABLE "delta_smallU_206276"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_smallU_206276_0" ON "delta_smallU_206276" ("src");
CREATE INDEX "delta_smallU_206276_1" ON "delta_smallU_206276" ("tgt");
/* ------------------- */
/* Plug delta_t_553157 */
/*                     */
/* attributes:         */
/* I[X]                */
/* delta_t_553157[X*X] */
/* ------------------- */
CREATE TABLE "delta_t_553157"
     ( "src" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , "tgt" VARCHAR(255) NOT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("src", "tgt")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "delta_t_553157_0" ON "delta_t_553157" ("src");
CREATE INDEX "delta_t_553157_1" ON "delta_t_553157" ("tgt");
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;