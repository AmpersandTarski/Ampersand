/* ------------ */
/* Conjunct violation cache */
/* ------------ */
DROP TABLE IF EXISTS "__conj_violation_cache__";
CREATE TABLE "__conj_violation_cache__"
    ( "conjId" VARCHAR(255) NOT NULL
    , "src" TEXT NOT NULL
    , "tgt" TEXT NOT NULL
    , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
    ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8 COLLATE UTF8_BIN
    , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "__cvc__conjId" ON "__conj_violation_cache__" ("conjId");