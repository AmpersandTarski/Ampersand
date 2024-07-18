---
numbersections: true
codeBlockCaptions: True
figureTitle: |
  Figure #
lofTitle: |
  ## List of Figures
lotTitle: |
  ## List of Tables
lolTitle: |
  ## List of Listings
tableTemplate: |
  *$tableTitle$ $i$*$titleDelim$ $t$
autoSectionLabels: True
title: pandoc-crossref demo document
---
# Relations
This second lesson explains relations that let developers define the data model of their information system.

Key takeaways:
1. Relations record facts the organization needs to memorize.
2. Relations serve to build useful data models, knowledge models, and conceptual models.
3. Relations have a useful mathematical structure, which serves to automate building information systems.

*Keywords*: Specifications, Relational Calculus, Design patterns,
Conceptual modelling.

## Introduction 

We use relations to record facts.
Among other things, an information system records facts that an organization chooses to remember.
Workers, customers, suppliers, managers, and other stakeholders rely on this "shared memory"
to do their work.
So, the purpose of recording facts is to provide services and products consistently and reliably.
Would that data be inconsistent, workers might contradict themselves and each other in the face of their clients or other working partners.
With too many inconsistencies, the organization as a whole starts behaving erratically and may eventually fail as a result.
We call this "organizational dementia".

Relations are the basic building blocks of information systems.
Underneath the surface, they are stored in databases on persistent storage devices.
In this tutorial, we abstract away from such implementation details.

## Refresher: Sets

In order to define relations, you need to be familiar with sets.
Just in case you need to refresh the notations, this section summarizes that.
Otherwise, just skip to the next section.

The following conventions are meant to ease your reading effort:
For sets, we use names starting with a capital letter, such as *Book*, *A*, and *PDC*.
We use names (e.g. *Book*) that provide readers with a clue as to which kind of elements are in the set.
Variables, such as $b$, $a$, and $p$, denote elements of these sets.
These elements are denoted as strings, numbers, or anything else because an organization uses them to represent anything, such as a book, a person, or a course.

The expression $b\in$ *Book* states the fact that there is a
book, which is represented by $b$. We pronounce: "$b$ is an element
(instance, member) of *Book*".

Given a set $X$ and a property $P$, the set of all elements of $X$ that
satisfy $P$ is denoted by:
$$
\{x\in X|\ P(x)\}\hspace{1cm} or \hspace{1cm}\{x| x\in X\ and \ P(x)\}
$$
For example, if property $P(x)$ means that $x$ is written by the author Ludlum, and $X$ is the set of all books,
then $\{x\in X|\ P(x)\}$ is the set of all books written by Ludlum.
The property $P$ is a predicate, i.e. a constraint that is either satisfied or not for a particular *x*.

We use the following relations and operations on sets throughout this paper:
-   *equality:* $X=Y$ if $X$ and $Y$ contain the same elements,
-   *inclusion:* $X\subseteq Y$ if every element of $X$ is also an
    element of $Y$,
-   *union:* $X\cup Y$ is the set of elements in $X$ or $Y$,
-   *intersection:* $X\cap Y$ is the set of elements in $X$ and $Y$,

These notations allow us to manipulate with sets in a formal way, using
the well-established rules.

### Set laws
There are some properties of sets that are used very often in practice.
These properties have well-known names, which you might just as well know.
We express them as laws, i.e. statements that are always true, no matter the value of the variables in it.
For any set $X$, $Y$, and $Z$:
|   |   |
|---|---|
$X\subseteq X$ | ($\subseteq$ is reflexive)
$X\subseteq Y$ and $Y\subseteq Z$ imply $X\subseteq Z$ | ($\subseteq$ is transitive)
$X\subseteq Y$ and $Y\subseteq X$ imply $X=Y$ | ($\subseteq$ is antisymmetric)
$X\cup Y=Y\cup X$ | ($\cup$ is commutative)
$X\cap Y=Y\cap X$ | ($\cap$ is commutative)
$(X\cup Y)\cup Z=X\cup (Y\cup Z)$ | ($\cup$ is associative)
$(X\cap Y)\cap Z=X\cap (Y\cap Z)$ | ($\cap$ is associative)
$(X\cap Y)\cup Z=(X\cup Z)\cap(Y\cup Z)$ | ($\cup$ distributes over $\cap$)
$(X\cup Y)\cap Z=(X\cap Z)\cup(Y\cap Z)$ | ($\cap$ distributes over $\cup$)

These laws are useful by themselves, because they allow you to
manipulate with sets without knowing their contents.
This ends the refresher on sets.

## Population
Relations are basic building blocks of information systems specifications.
Think of the following table as an example of a relation.
It shows which client has paid which invoice.
Let us call this relation *paid*:

  | *Client*      |   *Invoice* |
  | ------------- | ----------- |
  | *Applegate*   |     *5362a* |
  | *Brown*       |      *721i* |
  | *Conway*      |     *9443a* |

It actually stands for three facts: Client Applegate has paid
invoice number 5362a, Brown has paid invoice 721i, and Conway has paid
invoice 9443a. With a contents of only three facts, this relation can be shown in its entirety.
But quoting an entire telephone directory in this way would be impractical.
Yet, telephone directories, account administrations, land
registries etc. comprise the real relations we deal with in practice.
That is why we want to refer to relations without quoting their contents.

In Ampersand you may refer to this relation as `paid[Client*Invoice]`,
or just as `paid` if the compiler can infer the part `[Client,Invoice]` from the context.
We call `[Client,Invoice]` the signature,
the concept on the left (`Client`) the *source*,
and the concept on the right (`Invoice`) the *target* of this relation.
By convention, names of relations start with a lowercase
letter and names of concepts start with an uppercase letter.
Ampersand requires relations to be declared, e.g.:
```Ampersand
RELATION paid[Client*Invoice]
```
This  means that there exists a relation $paid$ with source $Client$ and target
$Invoice$. If a relation is not declared, each reference to it yields a compiler error.

The notation $\langle a,b\rangle$ shapes $a$ and $b$ into a pair.
We call $a$ and $b$ the *left* and *right* atoms of the pair.
If pair $\langle a,b\rangle$ is in relation $r$, we write $a\ r\ b$.
So, stating that $Applegate\ paid\ 5362a$ means that pair $\langle Applegate,5362a\rangle$ is in relation $paid$, and that $Applegate$ is a $Client$, and that $5362a$ is an $Invoice$.


### Modeling with relations
How do you model the business of an organization?
In practice, you get many different relations, each representing a different aspect of the business.
It helps to draw a conceptual diagram.
It contains concepts as dots and relations as arcs, e.g.:

![conceptual model of trading](../assets/trading.png)

This example represents ten relations in one diagram.
It represents the following declarations:

```Ampersand
RELATION paid[Client*Invoice]
RELATION sent[Invoice*Client]
RELATION deliveredto[Delivery*Client]
RELATION for[Invoice*Delivery]
RELATION from[Order*Client]
RELATION from[Invoice*Provider]
RELATION provided[Delivery*Provider]
RELATION of[Delivery*Order]
RELATION accepted[Provider*Order]
RELATION issued[Order*Provider]
```
In the conceptual diagrams,
we write the name of the relation near the arc.
A small arrowhead in the middle of the arc points from the source to the target concept,
to disambiguate the diagram.
Thus, each arc corresponds to precisely one declaration,
which gives us a perfect visualization of all relations.

### Multiplicity constraints
Relations can be constrained in terms of the number of elements they contain.
We distinguish between the following constraints, which are called *multiplicity constraints* or just *multiplicities*:

|     |     |
| --- | --- |
| univalent | For each element in the source, there is at most one element in the target. |
| total | For each element in the source, there is at least one element in the target. |
| function | For each element in the source, there is exactly one element in the target. Note that a function is the same as a relation that is univalent and total. |
| injective | For each element in the target, there is at most one element in the source. |
| surjective | For each element in the target, there is at least one element in the source. |
| bijective | For each element in the source, there is exactly one element in the target, and vice versa.  Note that a bijection (aka bijective function) is the same as a relation that is univalent, total, injective, and surjective. |

We add these constraints to the relation declarations, as in the following example.
```Ampersand
RELATION from[Order*Client] [UNI, TOT]
RELATION of[Delivery*Order] [TOT]
RELATION issuedTo[Order*Provider]
RELATION accepted[Provider*Order] [INJ]
RELATION deliveredto[Delivery*Client] [UNI]
RELATION from[Invoice*Provider] [UNI, TOT]
RELATION for[Invoice*Delivery] [INJ, SUR]
RELATION sent[Invoice*Client] [UNI]
RELATION paid[Client*Invoice]
RELATION provided[Delivery*Provider] [TOT]
```


This vocabulary needed to write rules consists of relations and
operators. You will use relations as your basic building bricks and
operators as mortar to combine relations into rules that describe
accurately whatever you want to specify.

Set operators are already a part of the vocabulary of relations, since
relations themselves are sets. So the expression $r\vdash s$ means that
every pair in $r$ is also a pair in $s$. Similarly, the expression
$r\cup s$ is the set of pairs in $r$ or in $s$ and $r\cap s$ is the set
of pairs in $r$ and in $s$.

For example, if
*candyprice*$=\{\langle {\tt Jelly},1.05\rangle,\langle {\tt Chocolate},0.80\rangle,\langle {\tt Apples},2.50\rangle\}$
and
*groceryprice*$=\{\langle {\tt Chocolate},0.80\rangle,\langle {\tt Apples},2.95\rangle\}$,
then

|     |
| --- |
| *candyprice* $\cup$ *groceryprice* $=\{\langle {\tt Jelly},1.05\rangle, \langle {\tt Chocolate},0.80\rangle, \langle {\tt Apples},2.50\rangle, \langle {\tt Apples},2.95\rangle\}$ |
| *candyprice* $\cap$ *groceryprice* $=\{\langle {\tt Chocolate},0.80\rangle\}$ |

Set operators alone are not enough. We also need operators from
relational calculus as well, most notably the *converse* of a relation,
the *composition* of two relations, and the *identity relation*. These
three operators are standard in relational calculus, although their
notations may vary across different publications.

### Converse
Converse merely swaps the left hand side and the right hand side:

-   *converse:* Let $r[A*B]$, then relation $r^\smallsmile[B*A]$
    is defined by: ${r}^\smallsmile = \{\langle b,a\rangle |\ a\ r\ b\}$

The converse of a relation swaps the left and the right atoms.
Let us look at an example:

*candyprice*$\smallsmile=\{\langle 1.05,{\tt Jelly},\rangle,\langle 0.80,{\tt Chocolate}\rangle,\langle 2.50,{\tt Apples}\rangle\}$

### Composition
$r;s$ represents the composition of two relations $r$ and $s$.
If $\langle a,c\rangle$ is in $r;s$, then there is a $b$ such that $\langle a,b\rangle$ is in $r$ and $\langle b,c\rangle$ is in $s$.
In other words, if some intermediate element $b$ exists, that relates $a$ to $c$ in $r;s$.

Here is an example. 
let the relation *provider*$[Candy\times Provider]$ contain the following population:

 *provider*$=\{\langle {\tt Jelly},{\tt \#4921}\rangle,\langle {\tt Chocolate},{\tt \#0382}\rangle,\langle {\tt Apples},{\tt \#1993}\rangle\}$.

Now *provider*$\smallsmile$;*candyprice* is the relation that contains the pairs $\langle c,p\rangle$ for which there is a $b$ such that $\langle c,b\rangle$ is in *provider* and $\langle b,p\rangle$ is in *candyprice*.
based on *provided* (relation
[\[exm:provided\]](#exm:provided){reference-type="ref"
reference="exm:provided"} on page ), *paid* (relation
[1] on page ), and
the relation $for[Invoice*Delivery]$, which is defined by:
$$\begin{array}{rcl}
*for*&:&*Invoice*\times*Delivery*

*for*&=&\{\begin{array}[t]{@{}l@{}l}\langle`721i`
, `Cookies #0382`
\rangle&,
 \langle`5362a`
, `Jelly beans #4921`
\rangle&,
 \langle`9443a`
, `Peanut butter #1993`
\rangle&\}\end{array}
\end{array}
$$ The converse of *for* is: $$\begin{array}{rcl}
{*for*}^\smallsmile&:&*Delivery*\times*Invoice*

{*for*}^\smallsmile&=&\{\begin{array}[t]{@{}l@{}l}\langle`Cookies #0382`
, `721i`
\rangle&,
 \langle`Jelly beans #4921`
, `5362a`
\rangle&,
 \langle`Peanut butter #1993`
, `9443a`
\rangle&\}\end{array}
\end{array}$$ Composition makes one relation from its two arguments. If,
for example, $name[String*Person]$ defines which names belong
to which person, and $age[Person*Int]$ defines the age of
each person, then $*name*;*age*$ is a relation
that contains names and corresponding ages. Here is the definition:

-   *composition :* Let $r[A*B]$ and $s[B*C]$,
    then $(r;s)$ is a relation, which is defined by: $$\begin{aligned}
        (r;s)&:&A\times C\nonumber

        r;s&=&$\{\langle a,c\rangle |\ $ there exist $b$ such that $a\ r\ b$ and $b\ s\ c\}$ 
    \end{aligned}

If you know about relational databases, you might like to know that
composition corresponds to the natural join operator. Here is an
example: The composition of *paid* and *for* is obtained by applying
definition [\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}. Figure
[3](#fig:paid;for){reference-type="ref" reference="fig:paid;for"}
illustrates this composition. $$\begin{array}{rcl}
*paid*;*for*&:&*Client*\times*Delivery*

*paid*;*for*&=&\{\begin{array}[t]{@{}l@{}l}\langle`Brown`
, `Cookies #0382`
\rangle&,
 \langle`Applegate`
, `Jelly beans #4921`
\rangle&,
 \langle`Conway`
, `Peanut butter #1993`
\rangle&\}\end{array}
\end{array}
{#exm:paid;for}$$

::: center
![composition of *paid* and *for*
($*paid*;*for*$)]({../assets/paidfor.png}){#fig:paid;for}
:::

You cannot compose just any relation. The relation
$*provided*;*for*$ is not defined, because
definition [\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"} requires the right attribute of *provided*
to be the same as the left attribute of *for*. *Delivery*, being the
right attribute of *provided*, is clearly not the same as *Invoice*,
which is the left attribute of *for*. Nevertheless, composing *provided*
with ${*for*}^\smallsmile$ is defined, and the result can be
obtained by applying definition
[\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}: $$\begin{array}{rcl}
*provided*;{*for*}^\smallsmile&:&*Provider*\times*Invoice*

*provided*;{*for*}^\smallsmile&=&\{\begin{array}[t]{@{}l@{}l}\langle`Brown`
, `Cookies #0382`
\rangle&,
 \langle`Applegate`
, `Jelly beans #4921`
\rangle&,
 \langle`Conway`
, `Peanut butter #1993`
\rangle&\}\end{array}
\end{array}
{#exm:provided;flipFor}$$

The identity relation is implicitly defined for every concept $C$. We
use it for example to specify equality in rules.

-   *identity relation:* For every concept $C$, the identity relation
    $\it{id}_{C}$ is defined by: $$\it{id}_{C} : C\times C

        \it{id}_{C} = \{\langle c,c\rangle |\ c\in C\}{#def:identity relation}$$

Having introduced relations and defined operators on them, let us turn
to rules.

## Rules 

Rules are used to specify your business, whatever that business might
be. Useful rules allow you to talk to various stakeholders in their own
language. In practice, any stakeholder may propose rules. As a designer,
you will make rule proposals concrete and propose new rules that others
have not yet thought of. The CC-technique lets you describe each rule
specifically enough to expose violations. It also lets you discover new
rules to propose to the business. Any rule proposal can be submitted to
a body that is authorized to impose new rules. As a designer, you rarely
have that authority yourself. It is however your responsibility as a
designer to make rules explicit, to expose possible inconsistencies, and
to present rules in a way that stakeholders in the business can
understand.

Let us elaborate some rules in the example of figure
[2]. In that
situation, clients and providers are trading according to a simple
pattern of orders, deliveries and invoices. The figure proper does not
contain any rules; it is merely a picture in which ten different
relations are declared. This section proposes six rules. Each rule is
represented in relational calculus, emphasizing the line of reasoning
that brings us from informal text to a precise formula.

For starters, how do we express that a provider does not accept any
order issued to someone other than himself? Relation
$issued[Order*Provider]$ in figure
[2]
apparently tells us which orders were issued to which providers.
Relation $accepted[Provider*Order]$ states which providers
have accepted which orders. Our first rule states that providers need
not accept all orders; they are supposed to accept only those orders
that were issued to them. So the set of orders that have been accepted
by a provider is a subset of the orders that were issued to him. In
other words, every element $\langle p,o\rangle$ of the relation
*accepted* is also an element of ${*issued*}^\smallsmile$.
Recalling the definition of inclusion on page , we can write this rule
as: $$*accepted*\ \vdash\ {*issued*}^\smallsmile
{#eqn:RA accepted issued}$$ Rule
[\[eqn:RA accepted issued\]](#eqn:RA accepted issued){reference-type="ref"
reference="eqn:RA accepted issued"} uses relations to represent that a
provider does not accept an order issued to someone other than himself.
This is exactly what we meant to represent, so here we have a first
example of a rule that represents business knowledge.

Having successfully represented a first rule, let us try another one.
How can we state that no client should ever pay an invoice that was not
addressed to him? First, we establish that this rule involves the
relations $*sent*$ and $*paid*$. Then we
reformulate the rule, stating that invoices paid by a client can be only
those invoices that are sent to that client. In other words, every
element $\langle i,c\rangle$ of the relation *paid* is also an element
of ${*sent*}^\smallsmile$. This is represented by:
$$*paid*\ \vdash\ {*sent*}^\smallsmile
{#eqn:RA paid sent}$$ This rule is quite similar to rule
[\[eqn:RA accepted issued\]](#eqn:RA accepted issued){reference-type="ref"
reference="eqn:RA accepted issued"}, because the reasoning that leads to
this rule is quite similar. An important use of these rules is to
identify violations explicitly. For instance, any invoice that is paid
by a client but not sent to that client constitutes a violation of rule
[\[eqn:RA paid sent\]](#eqn:RA paid sent){reference-type="ref"
reference="eqn:RA paid sent"}. Any invoice sent to one client but paid
by another client also constitutes a violation. However, an invoice that
was sent but not paid does not yield a violation. This is precisely what
we want, because the invoice might be still in the mail or waiting on a
desk.

Let us see if other rules can be identified. How can we describe that no
delivery is provided without an order? First, let us establish that this
rule involves the relations *provided*, *accepted*, and *of*. The
relation *provided* states which provider has provided which delivery.
Relation *accepted* says which provider has accepted which order, and
*of* relates a delivery to an order. If an element $\langle p,d\rangle$
is an element of the relation *provided*, then it must also be an
element of the relation
$*accepted*;{*of*}^\smallsmile$ that says that
there is an order, which was accepted by the provider and upon which the
delivery was made. This rule is written as:
$$*provided*\ \vdash\ *accepted*;{*of*}^\smallsmile
{#eqn:RA provided}$$ Let us verify that this is exactly the rule we
want. The second part of this rule,
$*accepted*;{*of*}^\smallsmile$, represents the
relation that contains pairs $\langle p,d\rangle$ of
$*Provider*\times*Delivery*$ for which there exist
$o\in*Order*$ such that $p\ *accepted*\ o$ and
$o\ ({*of*}^\smallsmile)\ d$. You may verify this by checking
on definition
[\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}. Note that
$o\ ({*of*}^\smallsmile)\ d$ is equivalent to
$d\ *of*\ o$ (by definition
[\[def:converse\]](#def:converse){reference-type="ref"
reference="def:converse"}). A violation of rule
[\[eqn:RA provided\]](#eqn:RA provided){reference-type="ref"
reference="eqn:RA provided"} occurs every time a delivery is made
without an order. So, this third rule represents precisely what we
intended.

The client however, will not accept invoices for orders undelivered. Can
we represent that in a fourth rule? Apparently, this involves the
relations *sent*, *for*, and *deliveredto*. Relation *sent* tells us
which invoice was sent to which client; relation *for* tells us for
which delivery the invoice was made; and relation *deliveredto*
specifies which delivery was delivered to which client. If there is an
invoice for delivery $d$, which was sent to client $c$, then
$\langle d,c\rangle$ must be in *deliveredto*. Therefore, we can write:
$${*for*}^\smallsmile;*sent*\ \vdash\ *deliveredto*
{#eqn:RA flipFor sent}$$ This rule is useful to expose violations
just like the previous ones. If an invoice is sent to a client different
from the one who received the delivery, we have a violation. If an
invoice is about a delivery that was never delivered, that too
constitutes a violation. If there is no invoice, there is no violation
of this particular rule. It might still be in the mail, after all.

After discussing four rules, let us look back at figure
[2]. You may
have noticed that every time we discussed a rule, the relations involved
in that rule form a cycle in figure
[2]. This is
no coincidence. Each rule has a relation in the left hand side and a
relation on the right hand side. These two relations form two different
paths in figure [2](#fig:trading){reference-type="ref"
reference="fig:trading"} between the same concepts. Hence, the relations
involved form a cycle, which encloses an area in figure
[2].

By turning this principle around, we can use it to discover new rules.
For instance, let us investigate the relations
$from[Invoice*Provider]$, *for*, and *provided*. These three
relations form a cycle in figure [2](#fig:trading){reference-type="ref"
reference="fig:trading"}, which has not been covered by any of the
previous rules. Can we propose a rule, using these three relations? If
there is an invoice for delivery $d$ that comes from provider $p$, then
$\langle p,d\rangle$ must be in *provided*. One part of this rule, which
says whether there is an invoice for delivery $d$ that comes from
provider $p$, is represented by
${*from*}^\smallsmile;*for*$ (by definition
[\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}). In relational terms, we are saying that
any pair $\langle p,d\rangle$ that is in
${*from*}^\smallsmile;*for*$ is also in
*provided*. However, the other way around is also useful: for any pair
$\langle p,d\rangle$ that is in *provided* there must be an invoice for
delivery $d$ that comes from provider $p$. The rule works in two
directions: the relations *provided* and
${*from*}^\smallsmile;*for*$ are a subset of one
another. Using property
[\[eqn:antisymmetric subset\]](#eqn:antisymmetric subset){reference-type="ref"
reference="eqn:antisymmetric subset"}, we get:
$$*provided*\ =\ {*from*}^\smallsmile;*for*
{#eqn:DEF provided}$$ Reflecting on this result, you may have
noticed that this rule was discovered merely by following the procedure
of inspecting relations in a cycle. There was no flash of ingenuity, no
spectacular discovery involved. Also notice that equation
[\[eqn:DEF provided\]](#eqn:DEF provided){reference-type="ref"
reference="eqn:DEF provided"} can be used to define the relation
*provided*. If relations *from* and *for* are known, the
contents of *provided* can be computed from their contents.

Each of the previous rules
[\[eqn:RA accepted issued\]](#eqn:RA accepted issued){reference-type="ref"
reference="eqn:RA accepted issued"} through
[\[eqn:RA flipFor sent\]](#eqn:RA flipFor sent){reference-type="ref"
reference="eqn:RA flipFor sent"} might have been discovered in a similar
way, systematically inspecting the cycles in figure
[2]. In
fact, most areas in figure [2](#fig:trading){reference-type="ref"
reference="fig:trading"} are enclosed by the cycles discussed so far.
Only the area enclosed by relations $from[Order*Client]$, *of*
and *deliveredto* remains to be analyzed. So these three relations are a
candidate for a rule. By similar reasoning as in the previous rule, our
proposal will be:
$$*deliveredto*\ =\ *of*;*from*
{#eqn:DEF deliveredto}$$ This rule says: if a pair
$\langle d,c\rangle$ is in *deliveredto*, there must be an order for
delivery $d$ that comes from provider $p$. Reversely, if there is an
order for delivery $d$ that comes from provider $p$,
$\langle d,c\rangle$ must be in *deliveredto*. So this rule too is a
proper candidate for agreement among stakeholders in the business.

After studying the six rules presented so far, you will have acquired
the idea of inventing rules by chasing cycles. It is a skill, basic to
the CC-technique. This skill is fundamental in analyzing the rule
proposals of various stakeholders and making these proposals concrete.

## Combining rules

Let us pursue the following idea: if larger cycles are made up from
smaller ones, and if cycles correspond to rules, can we use that to
create new rules? The answer is yes, but why take the trouble? After
all, if we can derive a new rule from existing ones, the new rule is not
something that stakeholders should have to approve. If stakeholders have
approved the existing rules, the newly assembled rule is merely a
consequence of their own choosing.

Still, it is useful to combine rules, precisely for the purpose to
confront the consequences of chosen rules. Very often, knowing about the
consequences can convince people to decide pro or con. So let us see how
it works, and take two rules that correspond to adjacent areas and
combine them to form a new rule?

::: center
![conceptual model of
trading](../assets/derivationOrderInvoice.png)
:::

For example, the cycles that correspond to rules
[\[eqn:RA flipFor sent\]](#eqn:RA flipFor sent){reference-type="ref"
reference="eqn:RA flipFor sent"} and
[\[eqn:DEF deliveredto\]](#eqn:DEF deliveredto){reference-type="ref"
reference="eqn:DEF deliveredto"}, drawn again in figure
[4](#fig:derivationOrderInvoice){reference-type="ref"
reference="fig:derivationOrderInvoice"}, form adjacent areas. The
relation *deliveredto* is shared between them. This observation inspires
the following derivation: $$\begin{array}{cl}
&{*for*}^\smallsmile;*sent*

\vdash&\hspace{1in}\{\hbox{rule \ref{eqn:RA flipFor sent}}\}

&*deliveredto*

=&\hspace{1in}\{\hbox{rule \ref{eqn:DEF deliveredto}}\}

&*of*;*from*
\end{array}$$ As a result, we may conclude:
$${*for*}^\smallsmile;*sent*\ \vdash\ *of*;*from*
{#eqn:flipfor;sent of;from}$$ This result says: if there is an
invoice for a delivery, which has been sent to some client $c$, then
there is an order for that delivery, which has come from client $c$. So
the idea has worked: adjacent areas in a conceptual diagram may inspire
new rules. The new rule
[\[eqn:flipfor;sent of;from\]](#eqn:flipfor;sent of;from){reference-type="ref"
reference="eqn:flipfor;sent of;from"} is a logical consequence of the
existing rules
[\[eqn:RA flipFor sent\]](#eqn:RA flipFor sent){reference-type="ref"
reference="eqn:RA flipFor sent"} and
[\[eqn:DEF deliveredto\]](#eqn:DEF deliveredto){reference-type="ref"
reference="eqn:DEF deliveredto"}.

In a similar fashion we can explore other parts of figure 2, and do some
more reasoning with rules: $$\begin{array}{cl}
&{*from*}^\smallsmile;*for*

=&\hspace{1in}\{\hbox{rule \ref{eqn:DEF provided}}\}

&*provided*

\vdash&\hspace{1in}\{\hbox{rule \ref{eqn:RA provided}}\}

&*accepted*;{*of*}^\smallsmile

\vdash&\hspace{1in}\{\hbox{rule \ref{eqn:RA accepted issued}}\}

&{*issued*}^\smallsmile;{*of*}^\smallsmile
\end{array}$$ This derivation says: if there is an invoice for a
delivery, that came from some provider $p$, then there is an order for
that delivery, which was issued to provider $p$. Or, in relational
terms:
$${*from*}^\smallsmile;*for*\ \vdash\ {*issued*}^\smallsmile;{*of*}^\smallsmile$$

## Multiplicities 

On many occasions, relations are qualified in terms of phrases like
\"one-to-many\", \"surjective\", \"1..n\", and the like. Such properties
are called *multiplicities*, because they provide bits of information
about the number of items in a relation. Multiplicities occur so often,
that each of them has been given a name of their own: univalent, total,
function, injective, surjective and bijective.

In this section we will give their definitions in relational calculus.
For now, it is sufficient to know that multiplicities can be expressed
as rules, just like any other rule. Later on in this book, we will
calculate with rules, using these multiplicity definitions frequently.

-   **univalent**\
    A relation $r[A*B]$ is *univalent* if each element of $A$
    corresponds to at most one element of $B$. This property is defined
    by: $${r}^\smallsmile;r\ \vdash\ \it{id}_{B}$$
    The definition says that $a\ r\ b$ and $a\ r\ b'$ imply $b=b'$ for
    all $a$, $b$, and $b'$. To denote that relation $r$ is univalent, we
    write $*univalent*(r)$.

-   **total**\
    A relation $r[A*B]$ is *total* if each element of $A$
    corresponds to at least one element of $B$. This property is defined
    by: $$\it{id}_{A}\ \vdash\ r;{r}^\smallsmile$$ The
    definition says that for all $a\in A$ there exists $b\in B$ such
    that $a\ r\ b$. To denote that relation $r$ is univalent, we write
    $*univalent*(r)$.

-   **function**\
    A relation is a *function* if it is both univalent and total. That
    is: a relation $r[A*B]$ is a *function* if every element
    of $A$ corresponds to precisely one element of $B$. If $r$ is
    univalent but not total then it is called a *partial function*.

-   **injective**\
    A relation $r[A*B]$ is *injective* if each element of $B$
    corresponds to at most one element of $A$. This property is defined
    by: $$r;{r}^\smallsmile\ \vdash\ \it{id}_{A}$$
    The definition says that $b\ r\ a$ and $b\ r\ a'$ imply $a=a'$ for
    all $a$, $a'$, and $b$. Notice that the property injective is
    defined conversely to the property univalent. To denote that
    relation $r$ is injective, we write $*injective*(r)$.

-   **surjective**\
    A relation $r[A*B]$ is *surjective* if each element of
    $B$ corresponds to at least one element of $A$. This property is
    defined by:
    $$\it{id}_{B}\ \vdash\ {r}^\smallsmile;r$$ The
    definition says that for all $b\in B$ there exists $a\in A$ such
    that $a\ r\ b$. Notice that the property surjective is defined
    conversely to the property total. To denote that relation $r$ is
    surjective, we write $*surjective*(r)$.

-   **bijective**\
    A relation is *bijective* if it is a function that is both injective
    and surjective. That is: a relation $r[A*B]$ is a
    *bijective* if every element of $A$ corresponds to precisely one
    element of $B$ and every element of $B$ corresponds to precisely one
    element of $A$.

::: center
![Graphical notations of
multiplicities](../assets/multiplicities.png)
:::

Database designers are familiar with multiplicities, because these
properties have significant impact on implementation choices. Figure
[5](#fig:multiplicities){reference-type="ref"
reference="fig:multiplicities"} shows two popular ways in which
multiplicities are shown in database models. The left hand side shows
the convention in UML [@UML]. On the right, you can see the \"crow's
foot\" notation, which has become popular in entity relationship
modelling (e.g. James Martin). Multiplicity notations may differ from
one graphical modelling technique to another. In some graphical modeling
techniques it may not be clear which side of a relation is source and
which is target. In those techniques, you may want to write the name of
a relation near its target, so you can easily remember the source and
target of that relation. In conceptual diagrams (e.g. in figure
[2]) the
little arrowhead gives direction to every relation.

## Functions 

Functions are relations that are total and univalent. This allows for a
convenient notation, which makes functions special. If
$r[A*B]$ is a function, we write $r:{A}\rightarrow{B}$. We
will normally use the symbols $f$, $g$, $h$ instead of relational
symbols, and the notation $b=f(a)$ instead of $a\ f\ b$.

::: center
![multiplicities in conceptual
diagrams](../assets/functional.png)
:::

In a conceptual diagram functions are depicted by an arrow ($f$ in
figure [6](#fig:functional){reference-type="ref"
reference="fig:functional"}). Every other relation is depicted with the
little arrowhead ($r$ in figure
[6]).

For example, we may write the function
$*provided*:{*Provider*}\rightarrow{*Delivery*}$
(example [\[exm:provided\]](#exm:provided){reference-type="ref"
reference="exm:provided"} on page ) in this way: $$\begin{array}{rcl}
 *provided*(`Cookies #0382`
)&=&`Candy's candy`


 *provided*(`Jelly beans #4921`
)&=&`Carter`


 *provided*(`Peanut butter #1993`
)&=&`Carter`

\end{array}$$ If, however, we would allow a delivery to have more than
one provider (e.g. the cookies with delivery number 0382 were delivered
both by Walmart and by Candy's candy, then *provided* would no longer be
a function, and therefore we would use the relational notation
([\[def:relational notation\]](#def:relational notation){reference-type="ref"
reference="def:relational notation"}) rather than the functional
notation.

[^1]: In this introduction, the reader may recognize some of the
    skillfully crafted sentences and formulas from Jipsen, Brink, and
    Schmidt, whose work is gratefully acknowledged.

[^2]: The entire example, including the rules in section
    [3.1], was
    communicated to the author by Rieks Joosten.
