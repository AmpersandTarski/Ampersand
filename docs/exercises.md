---
description: Under construction
---

# Exercises

This chapter contains material to help you mature your skills in Ampersand. They are somewhat larger than the exercises that are embedded in the theory. That makes these exercises suitable for classroom projects.

There may be exercises in another language than English, usually for a good reason.

The [VOG \(Verklaring Omtrent Gedrag\)](#VOG). This exercise requires you to read pieces of Dutch legislation, so this exercise is in Dutch.

## Delivery

Consider the following script. You can compile and run it op een RAP server.

```text
CONTEXT Delivery IN ENGLISH

RELATION clientName[Client*Name] [UNI,TOT]
POPULATION clientName CONTAINS
  [ ("Client_1" , "Martijn")
  ; ("Client_2" , "Stef")
  ]

RELATION clientAddress[Client*Address] [UNI,TOT]
POPULATION clientAddress CONTAINS
  [ ("Client_1" , "Kerkstraat")
  ; ("Client_2" , "Dorpsstraat")
  ]

RELATION clientCity[Client*City] [UNI,TOT]
POPULATION clientCity CONTAINS
  [ ("Client_1" , "Utrecht")
  ; ("Client_2" , "Enschede")
  ]

-- Vendor

RELATION vendorName[Vendor*Name] [UNI,TOT]
POPULATION vendorName CONTAINS
  [ ("Vendor_1", "Rubber inc.")
  ; ("Vendor_2", "Mario's pizzas")
  ]

RELATION sells[Vendor*Product]
POPULATION sells CONTAINS
  [ ("Vendor_1", "Product_1")
  ; ("Vendor_1", "Product_2")
  ; ("Vendor_1", "Product_3")
  ; ("Vendor_2", "Product_4")
  ; ("Vendor_2", "Product_5")
  ; ("Vendor_2", "Product_3")
  ]

-- Product

RELATION productName[Product*Name] [UNI,TOT]
POPULATION productName CONTAINS
  [ ("Product_1", "Inner tube")
  ; ("Product_2", "Bouncing ball")
  ; ("Product_3", "Rubber chicken")
  ; ("Product_4", "Pizza Margherita")
  ; ("Product_5", "Broodje Mario")
  ]

RELATION productPrice[Product*Price] [UNI,TOT]
POPULATION productPrice CONTAINS
  [ ("Product_1", "10,00 euro")
  ; ("Product_2", "0,75 euro")
  ; ("Product_3", "6,95 euro")
  ; ("Product_4", "8,50 euro")
  ; ("Product_5", "4,50 euro")
  ]

-- Order
RELATION orderTotal[Order*Price] [UNI]

RELATION orderedBy[Order*Client] [UNI,TOT]
-- POPULATION orderedBy CONTAINS [ ("Order_1", "Client_2") ]
RELATION orderedAt[Order* Vendor] [UNI,TOT]
-- POPULATION orderedAt CONTAINS [ ("Order_1", "Vendor_1") ]
RELATION orderOf[Order* Product] [TOT]
-- POPULATION orderOf CONTAINS [ ("Order_1", "Product_1") ]

-- Rules

RELATION orderAccepted[Order*Vendor] [UNI] -- an order may not be accepted by multiple vendors
-- POPULATION orderAccepted CONTAINS [ ("Order_1", "Vendor_1") ]

RELATION orderReceived[Order*Client] [UNI] -- an order may not be received by multiple clients
-- POPULATION orderReceived CONTAINS [ ("Order_1", "Client_1") ]


RULE orderInAssortment : orderOf |- orderedAt; sells

PURPOSE RULE allAccepted
{+To remind vendors of orders that are not yet accepted, we introduce a process rule.
+}
RULE allAccepted: orderedAt |- (I/\orderAccepted; orderAccepted~); orderedAt -- == TOT extended to allow hyperlinking to vendor in violation
MEANING "All orders have been accepted"
MESSAGE "Not all orders have been accepted"
VIOLATION (TGT I, TXT " has not accepted the order ",SRC I,TXT " by ", SRC orderedBy; clientName)

--RULE allPriced: orderAccepted |- (orderTotal;orderTotal~/\I ) ;orderAccepted
--MEANING "The order's total price must be calculated for each accepted order."
--MESSAGE "Not all accepted orders have been priced."
--VIOLATION (SRC I, TXT " has been accepted by ", TGT I, TXT " but hasn't been priced.")

ROLE Vendor MAINTAINS allAccepted
--ROLE OPA MAINTAINS allPriced


ROLE Client MAINTAINS dummy
RULE dummy: orderedAt |- orderedAt

-- Interfaces
INTERFACE Overview : "_SESSION"[SESSION]
 BOX[ "All clients" : V[SESSION*Client]
    , "All vendors" : V[SESSION*Vendor]
    , "All products" : V[SESSION*Product]
    , "All orders" : V[SESSION*Order]
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    ]

INTERFACE Client FOR Client : I[Client]
BOX [ "Name" : clientName
    , "Street" : clientAddress
    , "City" : clientCity
    , "All orders" : orderedBy~
      BOX [ vendor :orderedAt
          , product : orderOf
          ]
    , "Orders to be accepted by provider" : orderedBy~ /\ -(V; orderAccepted~)
    , "Orders pending delivery" : orderedBy~ /\ (V; orderAccepted~) /\ -orderReceived~
    , "Received orders" : orderReceived~
    ]

INTERFACE ClientInfo FOR Vendor : I[Client]
BOX [ "Name" : clientName
    , "Street" : clientAddress
    , "City" : clientCity
    , "All orders" : orderedBy~
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    , "Orders to be accepted by provider" : orderedBy~ /\ -(V; orderAccepted~)
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    , "Orders pending delivery" : orderedBy~ /\ (V; orderAccepted~) /\ -orderReceived~
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    , "Received orders" : orderReceived~
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    ]

INTERFACE Vendor FOR Vendor: I[Vendor]
BOX [ "Name" : vendorName
    , "Products" : sells
      BOX [ "Name" : productName
          , "Price" : productPrice
          ]
    , "Orders to be accepted" : orderedAt~ /\ -orderAccepted~
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    , "Orders to be delivered" : orderAccepted~ /\ -(orderAccepted~;orderReceived;V)
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]
    , "Past orders" : orderAccepted~ /\ orderAccepted~;orderReceived;V
      BOX [ product : orderOf;productName
          , client : orderedBy;clientName
          , vendor :orderedAt;vendorName
          ]

    ]

INTERFACE Product (productName, productPrice) FOR Vendor : I[Product]
BOX [ "Name" : productName
    , "Price" : productPrice
    , "Vendors" : sells~
    ]

INTERFACE AcceptOrderByVendor (orderAccepted) FOR Vendor : I[Order] /\ -(orderAccepted;orderAccepted~)
BOX [ "Client" : orderedBy
    , "Vendor" : orderedAt
    , "Product" : orderOf
    , "sign here to accept" : orderAccepted
    ]

INTERFACE ViewOrderByVendor FOR Vendor : I[Order]
BOX [ "Client" : orderedBy
    , "Vendor" : orderedAt
    , "Products" : orderOf
    , "sign here to accept" : orderAccepted
    ]

INTERFACE OrdersForClient (orderedBy, orderedAt, orderOf, orderReceived) FOR Client : I[Order]
BOX [ "Client" : orderedBy
    , "Vendor" : orderedAt
    , "Product" : orderOf
    , "accepted by" : orderAccepted
    , "sign here when received" : orderReceived
    ]

ENDCONTEXT
```

### Assignment

This script contains a `RULE` called `orderInAssortment`. By editing fields you can add orders, clients, providers, etc. Find out what `RULE orderInAssortment` means by trying to violate that rule. Why does this rule exist? Describe the meaning and the purpose of this rule. It may help to play with the prototype and discussing this rule with your peers. Add your meaning and purpose to the script and make sure it compiles and runs.

### What have you learned?

1. By playing with the prototype, you have learned the meaning of `RULE orderInAssortment`. 
2. You have learned how to include a purpose and a meaning with a rule. Remember that you can add a purpose not just for any rule, but for relations, concepts, interfaces and contexts as well.


## VOG \(in Dutch\)

In this assignment you will practice the analysis of a legal procedure by using Ampersand. Since the legal procedure used here is in Dutch, this assignment is only given in the Dutch language. The purpose of this assignment is to practice the use of Ampersand in a real-life situation. Before the assignment, your tutor will explain the way of working for this type of analysis. During the assignment, your tutor will occasionally explain features of Ampersand that you might need.

In deze opdracht oefent u met het analyseren van wet- en regelgeving. Dit is een typerend voorbeeld voor de manier waarop Ampersand in de praktijk kan worden gebruikt. Het resultaat van deze opdracht is een conceptuele analyse van het proces van aanvragen en verstrekken van de Verklaring Omtrent Gedrag \(VOG\).

De Verklaring Omtrent Gedrag - ten onrechte door sommigen "verklaring van goed gedrag" genoemd - is het onderwerp van deze casus. Wat de VOG voor burgers betekent beantwoordt overheidsdienst "Justis" in haar [voorlichting op internet](https://www.justis.nl/producten/vog/faq/faq-over-vog-np/). De formele regels hierover staan in de wet en regelgeving, zoals ontsloten op wetten.nl. [Paragraaf 1 van de Beleidsregels VOG-NP-RP 2013](http://wetten.overheid.nl/jci1.3:c:BWBR0032949&paragraaf=1) leidt dit onderwerp in:

> Het Centraal Orgaan Verklaring Omtrent het Gedrag \(COVOG\) geeft op grond van de [Wet justitiële en strafvorderlijke gegevens](http://wetten.overheid.nl/jci1.3:c:BWBR0014194&g=2017-04-25&z=2017-04-25) \(Wjsg\) namens de Minister van Veiligheid en Justitie verklaringen omtrent het gedrag \(VOG\) af aan natuurlijke personen \(VOG-NP\) en rechtspersonen \(VOG-RP\).
>
> Bij een VOG-aanvraag wordt onderzoek gedaan naar het justitiële verleden van een natuurlijke persoon of een rechtspersoon en zijn bestuurders, vennoten, maten of beheerders. Daarbij wordt het belang van de aanvrager afgewogen tegen het risico voor de samenleving in het licht van het doel van de aanvraag. Naar aanleiding hiervan wordt verklaard of al dan niet is gebleken van bezwaren tegen die natuurlijke persoon of rechtspersoon en wordt de VOG geweigerd respectievelijk verstrekt.

In deze casus wordt in tweetallen gewerkt. Je maakt een Ampersand-script die de regels rond het aanvragen en afgeven van de VOG weergeven.

Scope 1: de administratieve afhandeling. Dit betreft hetgeen beschreven is in [paragraaf 4 van de Beleidsregels VOG-NP-RP 2013](http://wetten.overheid.nl/jci1.3:c:BWBR0032949&paragraaf=4). We beperken ons tot de elektronische procedure voor natuurlijke personen.

Scope 2: beoordeling van de aanvraag. Dit betreft hetgeen beschreven is in [paragraaf 3 van de Beleidsregels VOG-NP-RP 2013](http://wetten.overheid.nl/jci1.3:c:BWBR0032949&paragraaf=3). We beperken ons tot aanvragen van natuurlijke personen.

### Opdracht

Bestudeer \(voor zover relevant en voor zover de tijd het toelaat\) de wettelijke bronnen en verwerk deze in een Ampersand-script. Dit werkstuk kun je te zijner tijd laten uitgroeien tot een volwaardig prototype voor een systeem dat VOG-aanvragen registreert en afhandelt.

### Beoordeling

Indien de docent deze opdracht beoordeelt, zullen de volgende criteria worden toegepast:

1. Is het ingeleverde conceptueel model compileerbaar in Ampersand?
2. Zijn er concepten, die in een adminstratie voor het uitreiken van verklaringen omtrent gedrag noodzakelijk zijn en die niet in het script aanwezig zijn?
3. Zijn er relaties, die in een adminstratie voor het uitreiken van verklaringen omtrent gedrag noodzakelijk zijn en die niet in het script aanwezig zijn?
4. Zijn er concepten in het script aanwezig, die in een adminstratie voor het uitreiken van verklaringen omtrent gedrag overbodig zijn?
5. Zijn er relaties in het script aanwezig, die in een adminstratie voor het uitreiken van verklaringen omtrent gedrag overbodig zijn?
6. Zijn er regels in  [Beleidsregels VOG-NP-RP 2013](http://wetten.overheid.nl/jci1.3:c:BWBR0032949), die niet in het conceptuele model voorkomen?
7. Zijn er regels, die niet traceerbaar zijn naar een regel in [Beleidsregels VOG-NP-RP 2013](http://wetten.overheid.nl/jci1.3:c:BWBR0032949)
8. Is van elke relatie en van elke regel de betekenis vastgelegd in een `MEANING`?
9. Is van elke relatie en van elke regel vastgelegd waartoe hij bestaat, d.m.v. een `PURPOSE`?
