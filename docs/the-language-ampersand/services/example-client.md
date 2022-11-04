# Example: Client

## A Client service

Suppose we have a delivery-hub that distributes orders over vendors and registers the subsequent deliveries. Let us define a service for clients, to allow clients to change their name and address and display their orders.

```text
INTERFACE ClientInfo FOR Client,Vendor : I[Client]
BOX [ "Name" : clientName
    , "Street" : clientAddress
    , "City" : clientCity
    , "All orders" : orderedBy~
      BOX [ vendor :orderedAt
          , product : orderOf
          ]
    , "Orders to be accepted by provider" : orderedBy~ - V;orderAccepted~
    , "Orders pending delivery" : orderedBy~ /\ (V;orderAccepted~ - orderReceived~)
    , "Received orders" : orderReceived~
    ]
```

## Structure

The service has a **header**, which is the first line in this example:

```text
INTERFACE ClientInfo FOR Client,Vendor : I[Client]
```

The word `ClientInfo` is the **name** of this service. This name identifies the service , so it must be unique throughout the entire context.

This service is shown only to users with roles `Client` or `Vendor`. That is indicated by the restriction `FOR Client,Vendor`. Without that restriction, the service is available for every user in any role.

The term to the right of the colon \(`:`\) symbol is called the **interface term**. A service is called from an atom which must be in the domain of this term. Let, for example, `Peter` be a `Client`. As `Peter` is an element of the domain of `I[Client]`, the service can be called from that atom.

The same term, `I[Client]`, is also used as **box term** for the box that follows the header. For every element in the codomain of the box term, a container \(in HTML: `<div>`\) will be drawn on the user screen. That box serves as a subinterface, which is called with precisely one atom. With `I[Client]` as box term, the codomain will contain just one atom, which is precisely the atom from which the service was called.

In this example, the outermost box contains seven **box items** and the innermost box two. Each box item has a label and an term. For example the box item `"Name" : clientName` has `"Name"` as its label and `clientName` as term. The atom `a` from which the box was called is used to select the pairs \(`a`,`x`\) from the term. All `x`-es for which \(`a`,`x`\) is in `clientName` will be displayed. Supposing that the relation `clientName` associates only one name to a client, this specific box item displays just one name. However, in the fifth box item, the term `orderedBy~ - V; orderAccepted~` may contain an arbitrary number of orders to be accepted by provider, all of which are shown.

