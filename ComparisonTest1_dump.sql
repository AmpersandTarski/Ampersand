/*
************************************************************************************************
*** Ampersand-v5.2.5 [Issue-1534-new-attempt:c99354691*], build time: 08-Mar-25 22:52:52 CET ***
************************************************************************************************
*/
/*
********************************************************************************
***                        Database structure queries                        ***
********************************************************************************
*/
/* ----------- */
/* Plug Leeftijd */
/*             */
/* attributes: */
/* I[Leeftijd] */
/* ----------- */
CREATE TABLE "Leeftijd"
     ( "Leeftijd" BIGINT NOT NULL /* INTEGER */
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "Leeftijd_0" ON "Leeftijd" ("Leeftijd");
/* --------------------------- */
/* Plug Persoon                */
/*                             */
/* attributes:                 */
/* I[Persoon]                  */
/* leeftijd [Persoon*Leeftijd] */
/* --------------------------- */
CREATE TABLE "Persoon"
     ( "Persoon" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , "leeftijd" BIGINT DEFAULT NULL /* INTEGER */
     , PRIMARY KEY ("Persoon")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "Persoon_0" ON "Persoon" ("leeftijd");
/* ----------- */
/* Plug ONE    */
/*             */
/* attributes: */
/* I[ONE]      */
/* ----------- */
CREATE TABLE "ONE"
     ( "ONE" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , PRIMARY KEY ("ONE")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
/* ------------------------------------------------------------------------- */
/* Plug groterDan                                                            */
/*                                                                           */
/* attributes:                                                               */
/* I[Leeftijd]/\groterDan [Leeftijd*Leeftijd];groterDan [Leeftijd*Leeftijd]~ */
/* groterDan [Leeftijd*Leeftijd]                                             */
/* ------------------------------------------------------------------------- */
CREATE TABLE "groterDan"
     ( "SrcLeeftijd" BIGINT NOT NULL /* INTEGER */
     , "TgtLeeftijd" BIGINT NOT NULL /* INTEGER */
     , PRIMARY KEY ("SrcLeeftijd", "TgtLeeftijd")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "groterDan_0" ON "groterDan" ("SrcLeeftijd");
CREATE INDEX "groterDan_1" ON "groterDan" ("TgtLeeftijd");
/* ---------------------------------------------------------------------- */
/* Plug isOuderVan                                                        */
/*                                                                        */
/* attributes:                                                            */
/* I[Persoon]/\isOuderVan [Persoon*Persoon];isOuderVan [Persoon*Persoon]~ */
/* isOuderVan [Persoon*Persoon]                                           */
/* ---------------------------------------------------------------------- */
CREATE TABLE "isOuderVan"
     ( "SrcPersoon" VARCHAR(255) NOT NULL /* OBJECT */
     , "TgtPersoon" VARCHAR(255) NOT NULL /* OBJECT */
     , PRIMARY KEY ("SrcPersoon", "TgtPersoon")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "isOuderVan_0" ON "isOuderVan" ("SrcPersoon");
CREATE INDEX "isOuderVan_1" ON "isOuderVan" ("TgtPersoon");
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
/*
********************************************************************************
***                       Inconsistencies in conjuncts                       ***
********************************************************************************
*/
/*
********************************************************************************
***                                  conj_0                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~\/ -I[Persoon]
Rules for this conjunct:
  - UNI_3040012754054565971: RULE UNI_3040012754054565971 LABEL "UNI rule for relation leeftijd[Persoon*Leeftijd]" : leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~ |-  -I[Persoon]
MEANING IN ENGLISH REST
{+ leeftijd\[Persoon\*Leeftijd\] is univalent

+}
MEANING IN DUTCH REST
{+ leeftijd\[Persoon\*Leeftijd\] is univalent

+}
MESSAGE IN ENGLISH REST
{+
Each Persoon may only have one Leeftijd in the relation leeftijd

+}
MESSAGE IN DUTCH REST
{+
Elke Persoon mag slechts één Leeftijd hebben in de relatie leeftijd

+}

*/
select distinct
         someDummyNameBecauseMySQLNeedsOne.src as src,
         someDummyNameBecauseMySQLNeedsOne.tgt as tgt
  from (/* Combination of optimized and non-optimized intersections */
        /*   part1 : I[Persoon]/\I[Persoon] */
        /*   part2 : leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
        /*    Expression: leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~/\I[Persoon] */
        /*    Signature : [Persoon*Persoon] */
        select src as src, tgt as tgt
        from (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (ECpl (EDcI Leeftijd),EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
              /*    Expression: leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
              /*    Signature : [Persoon*Persoon] */
              /* case: (ECps es), with two or more elements in es. */
              /*    Expression: leeftijd [Persoon*Leeftijd]; -I[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
              /*    Signature : [Persoon*Persoon] */
              select fence0.src as src, fence2.tgt as tgt
              from (/* EDcD leeftijd[Persoon*Leeftijd] */
                    /*    Expression: leeftijd [Persoon*Leeftijd] */
                    /*    Signature : [Persoon*Leeftijd] */
                    select "Persoon" as src, "leeftijd" as tgt
                    from "Persoon"
                    where ("Persoon" is not null)
                          and ("leeftijd" is not null))
                   as fence0,
                   (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                    /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                    /*    Signature : [Leeftijd*Persoon] */
                    /* case: EFlp x */
                    /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                    /*    Signature : [Leeftijd*Persoon] */
                    /* EDcD leeftijd[Persoon*Leeftijd] */
                    /*    Expression: leeftijd [Persoon*Leeftijd] */
                    /*    Signature : [Persoon*Leeftijd] */
                    /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                    /*    Signature : [Leeftijd*Persoon] */
                    select "leeftijd" as src, "Persoon" as tgt
                    from "Persoon"
                    where ("Persoon" is not null)
                          and ("leeftijd" is not null))
                   as fence2
              where (fence0.tgt <> fence2.src))
             as part2
        where (src = tgt)
              and (src in (select src
                           from (/* Optimized intersection: */
                                 /*    Expression: I[Persoon]/\I[Persoon] */
                                 select "Persoon" as src, "Persoon" as tgt
                                 from "Persoon"
                                 where ("Persoon" is not null))
                                as part1)))
       as someDummyNameBecauseMySQLNeedsOne
;

/*
********************************************************************************
***                                  conj_1                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -isOuderVan [Persoon*Persoon]~\/leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - kindIsJonger1: RULE kindIsJonger1  : isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~



  - kindIsJonger3: RULE kindIsJonger3  : isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~



*/
select distinct t1.src as src, t1.tgt as tgt
  from (/* EFlp (EDcD isOuderVan[Persoon*Persoon]) */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        /* case: EFlp x */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        /* EDcD isOuderVan[Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon] */
        /*    Signature : [Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        select "TgtPersoon" as src, "SrcPersoon" as tgt
        from "isOuderVan"
        where ("SrcPersoon" is not null)
              and ("TgtPersoon" is not null))
       as t1
       left
       join (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (ECpl (EDcD groterDan[Leeftijd*Leeftijd]),EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
             /*    Expression: leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             /* case: (ECps es), with two or more elements in es. */
             /*    Expression: leeftijd [Persoon*Leeftijd]; -groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             select fence0.src as src, fence2.tgt as tgt
             from (/* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   select "Persoon" as src, "leeftijd" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence0,
                  (/* ECpl (EDcD groterDan[Leeftijd*Leeftijd]) */
                   /*    Expression:  -groterDan [Leeftijd*Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   /* case: ECpl e */
                   /*    Expression:  -groterDan [Leeftijd*Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   select "cartesian product of Leeftijd and Leeftijd".src as src,
                          "cartesian product of Leeftijd and Leeftijd".tgt as tgt
                   from (/* EDcV [Leeftijd*Leeftijd] */
                         /*    Expression: V [Leeftijd*Leeftijd] */
                         /*    Signature : [Leeftijd*Leeftijd] */
                         /* case: (EDcV (Sign s t)) */
                         /*    Expression: V [Leeftijd*Leeftijd] */
                         /*    Signature : [Leeftijd*Leeftijd] */
                         select fst."Leeftijd" as src, snd."Leeftijd" as tgt
                         from "Leeftijd" as fst,
                              "Leeftijd" as snd
                         where (fst."Leeftijd" is not null)
                               and (snd."Leeftijd" is not null))
                        as "cartesian product of Leeftijd and Leeftijd"
                   where NOT exists (select *
                                     from (/* EDcD groterDan[Leeftijd*Leeftijd] */
                                           /*    Expression: groterDan [Leeftijd*Leeftijd] */
                                           /*    Signature : [Leeftijd*Leeftijd] */
                                           select "SrcLeeftijd" as src,
                                                  "TgtLeeftijd" as tgt
                                           from "groterDan"
                                           where ("SrcLeeftijd" is not null)
                                                 and ("TgtLeeftijd" is not null))
                                          as pos
                                     where ("cartesian product of Leeftijd and Leeftijd".src = pos.src)
                                           and ("cartesian product of Leeftijd and Leeftijd".tgt = pos.tgt)))
                  as fence1,
                  (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* case: EFlp x */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   select "leeftijd" as src, "Persoon" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence2
             where (fence0.tgt = fence1.src)
                   and (fence1.tgt = fence2.src))
       as t2
       on (t1.src = t2.src)
       and (t1.tgt = t2.tgt)
  where (t2.src is null)
        or (t2.tgt is null)
;

/*
********************************************************************************
***                                  conj_2                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -isOuderVan [Persoon*Persoon]~\/leeftijd [Persoon*Leeftijd];<[Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - kindIsJonger2: RULE kindIsJonger2  : isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd];<[Leeftijd];leeftijd [Persoon*Leeftijd]~



*/
select distinct t1.src as src, t1.tgt as tgt
  from (/* EFlp (EDcD isOuderVan[Persoon*Persoon]) */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        /* case: EFlp x */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        /* EDcD isOuderVan[Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon] */
        /*    Signature : [Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        select "TgtPersoon" as src, "SrcPersoon" as tgt
        from "isOuderVan"
        where ("SrcPersoon" is not null)
              and ("TgtPersoon" is not null))
       as t1
       left
       join (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (EBin < Leeftijd,EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
             /*    Expression: leeftijd [Persoon*Leeftijd];<[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             /* case: (ECps es), with two or more elements in es. */
             /*    Expression: leeftijd [Persoon*Leeftijd];<[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             select fence0.src as src, fence2.tgt as tgt
             from (/* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   select "Persoon" as src, "leeftijd" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence0,
                  (/* EBin < Leeftijd */
                   /*    Expression: <[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   /* case: EBin oper c  */
                   /*    Expression: <[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   select fst."Leeftijd" as src, snd."Leeftijd" as tgt
                   from "Leeftijd" as fst,
                        "Leeftijd" as snd
                   where (fst."Leeftijd" is not null)
                         and (snd."Leeftijd" is not null)
                         and (fst."Leeftijd" < snd."Leeftijd"))
                  as fence1,
                  (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* case: EFlp x */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   select "leeftijd" as src, "Persoon" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence2
             where (fence0.tgt = fence1.src)
                   and (fence1.tgt = fence2.src))
       as t2
       on (t1.src = t2.src)
       and (t1.tgt = t2.tgt)
  where (t2.src is null)
        or (t2.tgt is null)
;

/*
********************************************************************************
***                                  conj_3                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -isOuderVan [Persoon*Persoon]~\/leeftijd [Persoon*Leeftijd];<=[Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - kindIsJonger4: RULE kindIsJonger4  : isOuderVan [Persoon*Persoon]~ |- leeftijd [Persoon*Leeftijd];<=[Leeftijd];leeftijd [Persoon*Leeftijd]~



*/
select distinct t1.src as src, t1.tgt as tgt
  from (/* EFlp (EDcD isOuderVan[Persoon*Persoon]) */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        /* case: EFlp x */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        /* EDcD isOuderVan[Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon] */
        /*    Signature : [Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon]~ */
        /*    Signature : [Persoon*Persoon] */
        select "TgtPersoon" as src, "SrcPersoon" as tgt
        from "isOuderVan"
        where ("SrcPersoon" is not null)
              and ("TgtPersoon" is not null))
       as t1
       left
       join (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (EBin <= Leeftijd,EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
             /*    Expression: leeftijd [Persoon*Leeftijd];<=[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             /* case: (ECps es), with two or more elements in es. */
             /*    Expression: leeftijd [Persoon*Leeftijd];<=[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             select fence0.src as src, fence2.tgt as tgt
             from (/* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   select "Persoon" as src, "leeftijd" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence0,
                  (/* EBin <= Leeftijd */
                   /*    Expression: <=[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   /* case: EBin oper c  */
                   /*    Expression: <=[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   select fst."Leeftijd" as src, snd."Leeftijd" as tgt
                   from "Leeftijd" as fst,
                        "Leeftijd" as snd
                   where (fst."Leeftijd" is not null)
                         and (snd."Leeftijd" is not null)
                         and (fst."Leeftijd" <= snd."Leeftijd"))
                  as fence1,
                  (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* case: EFlp x */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   select "leeftijd" as src, "Persoon" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence2
             where (fence0.tgt = fence1.src)
                   and (fence1.tgt = fence2.src))
       as t2
       on (t1.src = t2.src)
       and (t1.tgt = t2.tgt)
  where (t2.src is null)
        or (t2.tgt is null)
;

/*
********************************************************************************
***                                  conj_4                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -isOuderVan [Persoon*Persoon]\/leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - ouderIsOuder1: RULE ouderIsOuder1  : isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~



  - ouderIsOuder3: RULE ouderIsOuder3  : isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~



*/
select distinct t1.src as src, t1.tgt as tgt
  from (/* EDcD isOuderVan[Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon] */
        /*    Signature : [Persoon*Persoon] */
        select "SrcPersoon" as src, "TgtPersoon" as tgt
        from "isOuderVan"
        where ("SrcPersoon" is not null)
              and ("TgtPersoon" is not null))
       as t1
       left
       join (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (EDcD groterDan[Leeftijd*Leeftijd],EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
             /*    Expression: leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             /* case: (ECps es), with two or more elements in es. */
             /*    Expression: leeftijd [Persoon*Leeftijd];groterDan [Leeftijd*Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             select fence0.src as src, fence2.tgt as tgt
             from (/* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   select "Persoon" as src, "leeftijd" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence0,
                  (/* EDcD groterDan[Leeftijd*Leeftijd] */
                   /*    Expression: groterDan [Leeftijd*Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   select "SrcLeeftijd" as src, "TgtLeeftijd" as tgt
                   from "groterDan"
                   where ("SrcLeeftijd" is not null)
                         and ("TgtLeeftijd" is not null))
                  as fence1,
                  (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* case: EFlp x */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   select "leeftijd" as src, "Persoon" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence2
             where (fence0.tgt = fence1.src)
                   and (fence1.tgt = fence2.src))
       as t2
       on (t1.src = t2.src)
       and (t1.tgt = t2.tgt)
  where (t2.src is null)
        or (t2.tgt is null)
;

/*
********************************************************************************
***                                  conj_5                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -isOuderVan [Persoon*Persoon]\/leeftijd [Persoon*Leeftijd];>[Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - ouderIsOuder2: RULE ouderIsOuder2  : isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];>[Leeftijd];leeftijd [Persoon*Leeftijd]~



*/
select distinct t1.src as src, t1.tgt as tgt
  from (/* EDcD isOuderVan[Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon] */
        /*    Signature : [Persoon*Persoon] */
        select "SrcPersoon" as src, "TgtPersoon" as tgt
        from "isOuderVan"
        where ("SrcPersoon" is not null)
              and ("TgtPersoon" is not null))
       as t1
       left
       join (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (EBin > Leeftijd,EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
             /*    Expression: leeftijd [Persoon*Leeftijd];>[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             /* case: (ECps es), with two or more elements in es. */
             /*    Expression: leeftijd [Persoon*Leeftijd];>[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             select fence0.src as src, fence2.tgt as tgt
             from (/* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   select "Persoon" as src, "leeftijd" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence0,
                  (/* EBin > Leeftijd */
                   /*    Expression: >[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   /* case: EBin oper c  */
                   /*    Expression: >[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   select fst."Leeftijd" as src, snd."Leeftijd" as tgt
                   from "Leeftijd" as fst,
                        "Leeftijd" as snd
                   where (fst."Leeftijd" is not null)
                         and (snd."Leeftijd" is not null)
                         and (fst."Leeftijd" > snd."Leeftijd"))
                  as fence1,
                  (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* case: EFlp x */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   select "leeftijd" as src, "Persoon" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence2
             where (fence0.tgt = fence1.src)
                   and (fence1.tgt = fence2.src))
       as t2
       on (t1.src = t2.src)
       and (t1.tgt = t2.tgt)
  where (t2.src is null)
        or (t2.tgt is null)
;

/*
********************************************************************************
***                                  conj_6                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -isOuderVan [Persoon*Persoon]\/leeftijd [Persoon*Leeftijd];>=[Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - ouderIsOuder4: RULE ouderIsOuder4  : isOuderVan [Persoon*Persoon] |- leeftijd [Persoon*Leeftijd];>=[Leeftijd];leeftijd [Persoon*Leeftijd]~



*/
select distinct t1.src as src, t1.tgt as tgt
  from (/* EDcD isOuderVan[Persoon*Persoon] */
        /*    Expression: isOuderVan [Persoon*Persoon] */
        /*    Signature : [Persoon*Persoon] */
        select "SrcPersoon" as src, "TgtPersoon" as tgt
        from "isOuderVan"
        where ("SrcPersoon" is not null)
              and ("TgtPersoon" is not null))
       as t1
       left
       join (/* ECps (EDcD leeftijd[Persoon*Leeftijd],ECps (EBin >= Leeftijd,EFlp (EDcD leeftijd[Persoon*Leeftijd]))) */
             /*    Expression: leeftijd [Persoon*Leeftijd];>=[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             /* case: (ECps es), with two or more elements in es. */
             /*    Expression: leeftijd [Persoon*Leeftijd];>=[Leeftijd];leeftijd [Persoon*Leeftijd]~ */
             /*    Signature : [Persoon*Persoon] */
             select fence0.src as src, fence2.tgt as tgt
             from (/* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   select "Persoon" as src, "leeftijd" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence0,
                  (/* EBin >= Leeftijd */
                   /*    Expression: >=[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   /* case: EBin oper c  */
                   /*    Expression: >=[Leeftijd] */
                   /*    Signature : [Leeftijd*Leeftijd] */
                   select fst."Leeftijd" as src, snd."Leeftijd" as tgt
                   from "Leeftijd" as fst,
                        "Leeftijd" as snd
                   where (fst."Leeftijd" is not null)
                         and (snd."Leeftijd" is not null)
                         and (fst."Leeftijd" >= snd."Leeftijd"))
                  as fence1,
                  (/* EFlp (EDcD leeftijd[Persoon*Leeftijd]) */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* case: EFlp x */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   /* EDcD leeftijd[Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd] */
                   /*    Signature : [Persoon*Leeftijd] */
                   /*    Expression: leeftijd [Persoon*Leeftijd]~ */
                   /*    Signature : [Leeftijd*Persoon] */
                   select "leeftijd" as src, "Persoon" as tgt
                   from "Persoon"
                   where ("Persoon" is not null)
                         and ("leeftijd" is not null))
                  as fence2
             where (fence0.tgt = fence1.src)
                   and (fence1.tgt = fence2.src))
       as t2
       on (t1.src = t2.src)
       and (t1.tgt = t2.tgt)
  where (t2.src is null)
        or (t2.tgt is null)
;

/*
********************************************************************************
***                                  conj_7                                  ***
********************************************************************************
*/
/*
Conjunct term:
   -I[Persoon]\/leeftijd [Persoon*Leeftijd];leeftijd [Persoon*Leeftijd]~
Rules for this conjunct:
  - TOT_3040012754054565971: RULE TOT_3040012754054565971 LABEL "TOT rule for relation leeftijd[Persoon*Leeftijd]" : I[Persoon] |- leeftijd [Persoon*Leeftijd];leeftijd [Persoon*Leeftijd]~
MEANING IN ENGLISH REST
{+ leeftijd\[Persoon\*Leeftijd\] is total
 +}
MEANING IN DUTCH REST
{+ leeftijd\[Persoon\*Leeftijd\] is totaal
 +}
MESSAGE IN ENGLISH REST
{+
Every Persoon must have a Leeftijd in the relation leeftijd

+}
MESSAGE IN DUTCH REST
{+
Elke Persoon dient één Leeftijd hebben in de relatie leeftijd

+}

*/
select distinct notIns."Persoon" as src, notIns."Persoon" as tgt
  from "Persoon" as notIns
  where (notIns."Persoon" not in (select src
                                  from (/* EDcD leeftijd[Persoon*Leeftijd] */
                                        /*    Expression: leeftijd [Persoon*Leeftijd] */
                                        /*    Signature : [Persoon*Leeftijd] */
                                        select "Persoon" as src, "leeftijd" as tgt
                                        from "Persoon"
                                        where ("Persoon" is not null)
                                              and ("leeftijd" is not null))
                                       as src))
        and (notIns."Persoon" is not null)
;

/*
********************************************************************************
***                           Queries per relation                           ***
********************************************************************************
*/
/*
********************************************************************************
***                RELATION groterDan [Leeftijd*Leeftijd]                    ***
********************************************************************************
*/
select "SrcLeeftijd" as src, "TgtLeeftijd" as tgt
  from "groterDan"
  where ("SrcLeeftijd" is not null)
        and ("TgtLeeftijd" is not null)
;

/*
********************************************************************************
***                 RELATION isOuderVan [Persoon*Persoon]                    ***
********************************************************************************
*/
select "SrcPersoon" as src, "TgtPersoon" as tgt
  from "isOuderVan"
  where ("SrcPersoon" is not null)
        and ("TgtPersoon" is not null)
;

/*
********************************************************************************
***            RELATION leeftijd [Persoon*Leeftijd]  [UNI, TOT]              ***
********************************************************************************
*/
select "Persoon" as src, "leeftijd" as tgt
  from "Persoon"
  where ("Persoon" is not null)
        and ("leeftijd" is not null)
;

/*
********************************************************************************
***                          Queries of interfaces                           ***
********************************************************************************
*/