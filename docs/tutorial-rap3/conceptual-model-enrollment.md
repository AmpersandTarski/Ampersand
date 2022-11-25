# Conceptual Model: Enrollment

In the code you can find the MEANING of each relation in natural language. In the model, each relation represents a set of pairs. The relation `takes` is filled with `(Student, Course)` -pairs that each specify a specific course that that specific student is taking. The same student can appear in more pairs and the same course can appear in more pairs. But each combination is unique, a specific pair (x,y) can only appear in the set once.

![conceptual model of Enrollment](<../.gitbook/assets/br-course-pagina-1 (1).png>)

You have seen the web application Enrollment in action. You have seen the code that defines the system in such a way that Ampersand can generate the web application. Now we will have a look at the conceptual model that is defined in that code. In this tutorial we will only describe the example to make you more familiar with the terminology. Just try to recognize what is described in the code and in the working system.

We have three ingredients:

* CONCEPT
* RELATION
* RULE

Before we discuss these three main ingredients, we will discuss the other keywords you see in the code.

* INTERFACE is not crucial for the conceptual model, but still crucial for the web application. It uses the conceptual model to define the tabs and fields displayed. We will come back to this later.
* The text after MEANING and PURPOSE is printed in the documentation that RAP4 can generate.&#x20;
* MESSAGE and VIOLATION are used to display messages on screen to the user about rule violations.&#x20;
* POPULATION provides the web application with actual data to test the rules with. Adding data to the system can also be done with an excel sheet. The data specifies elements that populate the concepts and whether or not these elements are connected to each other in a specific relation.&#x20;

## The core of the model

The goal of the model is to define rules that will govern the behavior of the system. Rules are about relations and relations link elements of one concept with elements of another concept. So we need all three ingredients (concept, relation and rule) to define the model. Deciding about these ingredients, their name and their attributes is exactly the modeling that you will learn in this course.

A conceptual model of Enrollment can be represented with a diagram:

![](<../.gitbook/assets/br-course-pagina-1 (1) (1).png>)

Watch [this clip](https://player.ou.nl/wowzaportlets/#!production/Cq0M1nv) to learn how we use the words atom, concept, and relation.

In the code you can find the MEANING of each relation in natural English. In the model, each relation represents a set of pairs. The relation `takes` is filled with `(Student, Course)` -pairs that each specify a specific course that that specific student is taking. The same student can appear in more pairs and the same course can appear in more pairs. But each combination is unique, a specific pair (x,y) can only appear in the set once.

In the code you see the keyword `[TOT]` in the definition of the relation `takes`. It is called a multiplicity. The multiplicity 'total' means that each student must take at least one course.

### Assignment

**Question 1**: given the initial dataset in the source-code of Enrollment, what pairs are in the relation `isPartOf`?

**Question 2**: given the initial dataset in the source-code of Enrollment, which students are enrolled for which modules?

Check your answer in the web application Enrollment.

## The rule

So let’s finally go to the one rule that governs this information system: `isEnrolledFor |- takes ; isPartOf~`

The rule consists of two parts with `|-` as separator. On each side of the separator you find a relation. On the left-hand side we have the relation `isEnrolledFor` and on the right-hand side you see a relation that is not explicitly defined in the model. This relation is constructed with two relations that are in the model: `takes` and `isPartOf~` (pronounced as “isPartOf-flip”, indicating the relation in opposite direction). This constructed relation consists of (`Student`, `Module`)-pairs with **a specific student** that is taking **a course** that contains (among others) **this specific module**.\
Try to trace this last description in the model and note that although the result is a pair of two elements, there are actually three concepts involved.\
Let’s call this new relation `canEnrollFor`.

Now we can pronounce the rule in more or less natural language: "isEnrolledFor implies canEnrollFor". If the first is true than the second must also be true.

Everytime the user of the system tries to enroll a student for a module, the rule checks whether this student can be enrolled for this module based on the course the student is taking. If this enrollement is not allowed, the rule produces a violation message. In fact, with each ‘save’ in the database, all information in the database is checked against this rule.

### Assignment

Try to reason about the answers to the following questions based on the conceptual model and the rule. After that, try it out in the system.

**Question 1**: Suppose the user adds to the database that student Peter is enrolled for the module IT-Governance. Will this cause a violation message?

**Question 2**: Next, what will happen when the module IT-Governance is no longer a part of the course Business IT. Why?

**Question 3**: Think of an entry you can do in the system to generate a violation message and try it out in the system.

**Question 4**: John is a hard working student and he wants to take a second course, Management. Will the system allow this? Why?

**Question 5**: The multiplicity \[TOT] seem to work as a rule. What is the difference between a multiplicity, like `TOT`, and a rule, like `isEnrolledFor |- takes ; isPartOf~`?

## What have you learned?

* an information system may be seen as a system of relations and rules that governs data, supplemented by user interfaces that give access to that data.
* a rule engine executes the rules on all data regularly

## What's next?

This concludes your first steps to get acquainted with Ampersand script en RAP4. The Open University Coursebook contains all theoretical background you need to start understanding and creating your own model. This Gitbook offers more practical and up to date information about the language Ampersand script. For the syntax and meaning of Ampersand constructs, consult chapter [The language Ampersand](../the-language-ampersand/). For more in depth information about modeling in Ampersand [click here](../modeling/). For the rest...have a look around and feel free to ask questions.
