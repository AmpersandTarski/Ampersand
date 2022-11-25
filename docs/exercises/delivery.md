# Delivery

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

## Assignment

This script contains a `RULE` called `orderInAssortment`. By editing fields you can add orders, clients, providers, etc. Find out what `RULE orderInAssortment` means by trying to violate that rule. Why does this rule exist? Describe the meaning and the purpose of this rule. It may help to play with the prototype and discussing this rule with your peers. Add your meaning and purpose to the script and make sure it compiles and runs.

## What have you learned?

1. By playing with the prototype, you have learned the meaning of `RULE orderInAssortment`. 
2. You have learned how to include a purpose and a meaning with a rule. Remember that you can add a purpose not just for any rule, but for relations, concepts, services and contexts as well.

