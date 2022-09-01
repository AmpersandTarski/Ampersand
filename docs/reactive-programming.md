---
description: >-
  Ampersand adheres to the reactive programming paradigm. This chapter tells the
  story...
---

# Reactive programming

The easiest way to think about Ampersand is to envisage an information system as a set of structured data that changes over time (just like a database), together with a set of constraints (the rules) on that data that must remain satisfied throughout their lifetime.

Abstract? Here is a simple example:

1. Think of your example system as an empty set of data with just one rule: "Every parcel must have an owner."
2. Suppose that there is an event that inserts into the data set: "Parcel with property identifier 074.0-0000-0084.0 is 0.54 acres large."
3. The system will now signal that property 074.0-0000-0084.0 doesn't have an owner.
4. Either you (as a user) or a computer (as automaton) can satisfy the rule by inserting into the data that the owner of property 074.0-0000-0084.0 is Grafton Town of Highway Department, 30 Providence Rd, Grafton, MA 01519-1186. Hence the rule is satisfied.
5. bla bla

