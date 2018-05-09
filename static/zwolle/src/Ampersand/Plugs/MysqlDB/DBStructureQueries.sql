/* ------------ */
/* Signal table */
/* ------------ */
CREATE TABLE "__all_signals__"
     ( "conjId" VARCHAR(255) NOT NULL
     , "src" VARCHAR(255) NOT NULL
     , "tgt" VARCHAR(255) NOT NULL
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "__all_signals___0" ON "__all_signals__" ("conjId");
CREATE INDEX "__all_signals___1" ON "__all_signals__" ("src");
CREATE INDEX "__all_signals___2" ON "__all_signals__" ("tgt");