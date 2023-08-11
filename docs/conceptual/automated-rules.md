# Automating Rules in Ampersand

This document describes a problem that exists in the Ampersand-compiler. It is introduced for people who have some preliminary knowledge of Ampersand. This document enables the definition of a project, which aims to solve this problem and enhance the Ampersand-compiler with the solution. The introduction gives an informal problem description.

## Introduction

The Ampersand project shows promising results with respect to generating database applications, using a compiler. The purpose of the Ampersand-compiler is to generate a database application from a formal specification, written in a sugared version of relation algebra. Figure 1 is an example of an Ampersand-script that can be compiled and executed directly. The application lets a user create and delete items in a sequence, which is a list of items in a total order. We urge the reader to execute this Ampersand-script and verify that it works.

Figure 1: example of an Ampersand-script
~~~~~~~~~~~~
CONTEXT Sequence IN ENGLISH

PATTERN "Sequences"
    RELATION head[Sequence*Item] [UNI,INJ]
    RELATION succ[Item*Item] [UNI,INJ,IRF]
    RELATION seq[Item*Sequence] [UNI,TOT]

--invariants:
--  RULE "no predecessor of head" : -(head;succ~)
--  RULE "total order" : head;succ* = seq~
--  RULE "transitive succ" : succ*;succ |- succ*
ENDPATTERN

PATTERN "Demo of Sequences"
    VIEW Sequence : Sequence(name)

    RELATION madeIn[Sequence*SESSION] [UNI]
    RELATION insSeq[SESSION*Name] [UNI]
    RULE InsSeq: insSeq |- madeIn~;name
    MEANING "Providing a name in insSeq creates a new sequence with that name."
    VIOLATION ( TXT "{EX} NewStruct;Sequence"
              , TXT ";name;Sequence;_NEW;Name;", TGT I
              , TXT ";madeIn;Sequence;_NEW;SESSION;", SRC I
              , TXT "{EX} DelPair;insSeq;SESSION;", SRC I, TXT ";Name;", TGT I
              )
    ROLE ExecEngine MAINTAINS InsSeq

    REPRESENT Name TYPE ALPHANUMERIC
    REPRESENT Value TYPE ALPHANUMERIC
    RELATION name[Sequence*Name] [UNI]
    MEANING "Every sequence has a name."
    RELATION seqHead[Sequence*Value] [UNI]
    MEANING "Every sequence has a current value."
    RELATION value[Item*Value] [UNI,INJ]
    MEANING "Every item has a value."

    RULE InsItem: seqHead |- head;value
    MEANING "Pushing an item into an empty sequence creates pair in head."
    VIOLATION ( TXT "{EX} NewStruct;Item"
              , TXT ";succ;Item;_NEW;Item;", SRC head
              , TXT ";seq;Item;_NEW;Sequence;", SRC I
              , TXT ";head;Sequence;", SRC I, TXT ";Item;_NEW"
              , TXT ";value;Item;_NEW;Value;", TGT I
              , TXT "{EX} DelPair;seqHead;Sequence;", SRC I, TXT ";Value;", TGT I
              )
    ROLE ExecEngine MAINTAINS InsItem

    RULE delVar : (I/\succ;succ~);succ~;seq |- seq
    VIOLATION ( TXT "{EX} InsPair;succ;Item;", SRC succ~, TXT ";Item;", SRC succ
              , TXT "{EX} DelAtom;Item;", SRC I[Item]
              )
    ROLE ExecEngine MAINTAINS delVar

    RULE delVarLast : (I - succ;succ~);succ~;seq |- seq
    VIOLATION ( TXT "{EX} DelPair;succ;Item;", SRC succ~, TXT ";Item;", SRC I[Item]
              , TXT "{EX} DelAtom;Item;", SRC I[Item]
              )
    ROLE ExecEngine MAINTAINS delVarLast

    RULE delVarHead : (I - succ~;succ);head~ |- seq
    VIOLATION ( TXT "{EX} InsPair;head;Sequence;", TGT I[Sequence], TXT ";Item;", SRC succ
              , TXT "{EX} DelAtom;Item;", SRC I[Item]
              )
    ROLE ExecEngine MAINTAINS delVarHead

    RULE delValue : I[Value] |- value~;value
    VIOLATION ( TXT "{EX} DelAtom;Value;", SRC I[Value] )
    ROLE ExecEngine MAINTAINS delValue
ENDPATTERN

INTERFACE overview(insSeq, seq, seqHead, madeIn) : '_SESSION'
BOX<TABLE>
    [ "Name your new sequence here:" : insSeq
    , Sequences : V[SESSION*Sequence]
      BOX <TABLE>
          [ sequence : name
          , "Name your items here:" : seqHead
          , items : seq~
            BOX <TABLE>
                [ value  : value
                , seq    : seq
                , succ   : succ
                , pred   : succ~
                , stat   : (I /\ head~;head);V;'head'[TEXT] \/ (I - succ;succ~);V;'last'[TEXT] 
                , status : (I -  succ~;succ);V;'head'[TEXT] \/ (I - succ;succ~);V;'last'[TEXT] 
                ]
          ]
    ]
ENDCONTEXT
~~~~~~~~

The Ampersand-compiler can translate binary relations together with multiplicity restrictions into the data model of a MySQL-database. Relation algebra terms are used in rules and services. Rules are constraints on the data stored in the relations. Each service is a “window” on the data set, in which data to be displayed is described in terms of relation algebra terms. The example in Figure 1 is reasonably small: it contains 8 relations with 13 multiplicity restrictions, 6 rules, and one service. Every rule in Ampersand is a constraint, but the enforcement of constraints by the software can be one of the following options:

1.  A rule has to be satisfied at all times. Such rules are called invariants. When violated, the system produces an error message and blocks the transaction in which the violation has occurred.

2.  A rule has to be satisfied by a user. Such rules are called process rules. When violated, the user will get a signal. That signal will not go away until the user fixes the violation.

3.  A rule is restored by an automated action. In fact, all six rules in Figure 1 are automated rules.

4.  A rule is derivable by other rules. If such rules depend only on invariants and automated rules, there is no need to generate code for them.

The Ampersand-script of figure 1 contains six automated rules. The problem is this:

> > How can the code, which is specified in the violation of an automated rule, be derived automatically, if necessary with a little help of the programmer?

## Observations
Experience from practice about automated rules is accumulating. We are learning from writing code manually for the ExecEngine. Some observations inspire to automate that writing process. Negative observations, such as difficult debugging, create a sense of urgency to replace ExecEngine rules by something better. Positive observations, such as the very simple terms that we use in the Exec-Engine code (I occurs frequently), inspires us that it might be quite doable.
In this chapter we have documented some of our observations.
###	Multiplicity rules are simple to automate
Consider the following fragment from the example application:

    RULE delValue : I[Value] |- value~;value
    VIOLATION ( TXT "{EX} DelAtom;Value;", SRC I[Value] )
    ROLE ExecEngine MAINTAINS delValue

This rule describes the surjectivity of relation value. In this particular application, surjectivity can be violated when a user deletes a variable. Needless to say that deleting the corresponding Value-atom is an appropriate reaction to restore surjectivity.
###	Once rules work, terms are often simple
Consider the following fragment from the example application:

    RULE InsSeq: insSeq |- madeIn~;name
    MEANING "Providing a name in insSeq creates a new sequence with that name."
    VIOLATION ( TXT "{EX} NewStruct;Sequence"
              , TXT ";name;Sequence;_NEW;Name;", TGT I
              , TXT ";madeIn;Sequence;_NEW;SESSION;", SRC I
              , TXT "{EX} DelPair;insSeq;SESSION;", SRC I, TXT ";Name;", TGT I
              )
    ROLE ExecEngine MAINTAINS InsSeq

This rule describes how a new Sequence is made. It simply states that a pair in relation insSeq should lead to a new sequence. At the same time, some “attributes” of Sequence are filled. Notice that only I-terms are used in the violations. We consider that a happy circumstance, because its simplicity gives much insight and therefore better predictability as to what happens at runtime.
###	Debugging is hard
In practice, debugging in Ampersand has proven to be hard. We are uncertain why this is. It may be that the level of abstraction works against easy debugging. It may equally well be caused by our own lack of experience, which results in failing intuition when confronted with mistakes. We are in great need of tools and of ways to prevent debugging in the first place.
###	Exec-Engine rules are similar to process algebra rules
Experiments in 2007 showed that process algebra rules can be derived from the constraints in Ampersand. The rules in the Exec-Engine show similarity to these rules.
### To be elaborated further
