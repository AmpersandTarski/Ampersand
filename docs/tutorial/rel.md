This second lesson explains relations that let developers define the
data model of their information system.

Key takeaways:
1. Relations record facts the organization needs to memorize.
2. Relations serve to build useful data models, knowledge models, and conceptual models.
3. Relations have a useful mathematical structure, which serves to automate building information systems.

*Keywords*: Specifications, Relational Calculus, Design patterns,
Conceptual modelling.

# Introduction {#sec:Motivation}
Among other things, an information system records facts that an organization needs to remember.
Workers, customers, suppliers, managers, and other stakeholders rely on this "shared memory"
to do their work without contradicting each other.
Would that data be inconsistent, workers might start contradicting each other and the operation is at risk,
depending on the precise impact of those inconsistencies.
The purpose of recording facts is to provide services and products consistently and reliably and avoid such risks.

We use relations to record facts.
Relations are the basic building blocks of information systems.
Underneath the surface, they are stored in databases on persistent storage devices.
However, for understanding information systems, we use the most simple notion of relation
and abstract away from their implementation.

# Preliminaries {#sec:Preliminaries}
In order to define relations, you need to be familiar with sets. This
section merely defines the notations used in the sequel, assuming that
you have previously learned about sets. The following typographic
conventions are meant to ease your reading effort: Sets are written with
names starting with a capital letter, such as *Book*, *A*, and *PDC*.
The name itself (e.g. *Book*) may provide you with a clue as to which
kind of elements are in the set. Lowercase letters, possibly suffixed by
apostrophes, denote these elements. Where appropriate, such a lowercase
letter will match the first letter of the set.

The expression $b\in$ *Book* states the fact that there is a book, which
is represented by $b$. We pronounce: "$b$ is an instance (element,
member) of *Book*".

Given a set $X$ and a property $P$, the set of all elements of $X$ that
satisfy $P$ is denoted by:
$\{x\in X|\ P(x)\}\hspace{1cm} or \hspace{1cm}\{x| x\in X\ and \ P(x)\}$
The property $P$ is often described informally, but is understood to be
an abbreviation for a precise expression in some formal language.

The following relations and operations on sets are used throughout this
paper:

-   *equality:* $X=Y$ if $X$ and $Y$ contain the same elements,

-   *inclusion:* $X\subseteq Y$ if every element of $X$ is also an
    element of $Y$,{#def:inclusion}

-   *union:* $X\cup Y$ is the set of elements in $X$ or $Y$,

-   *intersection:* $X\cap Y$ is the set of elements in $X$ and $Y$,

These notations allow us to manipulate with sets in a formal way, using
the well-established rules. The main ones are captured by equations
[\[eqn:set first\]](#eqn:set%20first){reference-type="ref"
reference="eqn:set first"} through [\[eqn:set
last\]](#eqn:set%20last){reference-type="ref" reference="eqn:set last"}.
For any set $X$, $Y$, and $Z$ \| \| \| \|---\|---\| $X\dashv X$ \|
($\dashv$ is reflexive){#eqn:set first} $X\dashv Y$ and $Y\dashv Z$
imply $X\dashv Z$ \| ($\dashv$ is transitive $)$X`\dashv `{=tex}Y\$ and
$Y\dashv X$ imply $X=Y$ \| ($\dashv$ is antisymmetric
$){#eqn:antisymmetric subset}$X`\cup `{=tex}Y=Y`\cup `{=tex}X\$ \|
($\cup$ is commutative $)$X`\cap `{=tex}Y=Y`\cap `{=tex}X\$ \| ($\cap$
is commutative
$)$(X`\cup `{=tex}Y)`\cup `{=tex}Z=X`\cup `{=tex}(Y`\cup `{=tex}Z)\$ \|
($\cup$ is associative
$)$(X`\cap `{=tex}Y)`\cap `{=tex}Z=X`\cap `{=tex}(Y`\cap `{=tex}Z)\$ \|
($\cap$ is associative
$)$(X`\cap `{=tex}Y)`\cup `{=tex}Z=(X`\cup `{=tex}Z)`\cap`{=tex}(Y`\cup `{=tex}Z)\$
\| ($\cup$ distributes over $\cap$)
$(X\cup Y)\cap Z=(X\cap Z)\cup(Y\cap Z)$ \| ($\cap$ distributes over
$\cup$){#eqn:set last}

Such rules are useful, because they allow you to manipulate with sets
without any reference to their contents.

# Relations {#sec:relations}

Relations are the basic entity out of which you build specifications.
Think of table [1](#exm:paid) as an example of a relation that shows
which client has paid which invoice. Let us call this relation *paid*.

:::: center
::: {#exm:paid}

| *Client*      | *Invoice* |
| ------------- | ----------- |
| *Applegate*   | *5362a* |
| *Brown*       | *721i* |
| *Conway*      | *9443a* |

  : Contents of *paid*
:::
::::

It actually stands for three facts: Client Applegate has paid
invoice number 5362a, Brown has paid invoice 721i, and Conway has paid
invoice 9443a. With a contents of only three facts, this relation can be shown in its entirety.
But quoting an entire telephone directory in this way would be very impractical.
Yet, telephone directories, account administrations, land
registries etc. comprise the real relations we deal with in practice.
That is why we want to refer to relations without quoting their contents.
In Ampersand you may refer to this relation as `paid[Client,Invoice]`,
or just as `paid` if the compiler can infer the part `[Client,Invoice]` from the context.
We call `[Client,Invoice]` the signature.
the concept on the left (`Client`) the *source*,
and the concept on the right (`Invoice`) the *target* of this relation.
By convention, names of relations start with a lowercase
letter and names of concepts start with an uppercase letter.

In order to define a relation (e.g. *paid*), we write:
$paid[Client*Invoice]$
{#def:relational notation}$ This is called a *declaration*, which means
that there exists a relation *paid* with source *Client* and target
*Invoice*. Together, the name, source and target determine the relation
uniquely. \[\]{#signature identifies relation label="signature
identifies relation"} That is: if two relations have identical names,
left- and right attributes, they have the same contents. As a
consequence, different relations may have identical names, provided one
or both attributes are different. So name and attributes determine a
relation uniquely. When confusion is impossible, the name alone is
sufficient reference to the relation. In all cases, you may refer to the
relation as $r[A*B]$.

Actually, the notation $A\times B$ means the *cartesian product* of $A$
and $B$. That is: $A\times B$ is the set of all pairs of which the left
element is an element of $A$ and the right element is an element of $B$:
$A\times B\ =\ \{\langle a,b\rangle |\ a\in A\wedge b\in B\}
{#def:cartesian product}$ The symbols $\langle a,b\rangle$ denote a
*pair* with the characteristic property
$\langle a,b\rangle=\langle a',b'\rangle$ if and only if $a=a'$ and
$b=b'$. As a consequence of definition [\[def:cartesian
product\]](#def:cartesian%20product){reference-type="ref"
reference="def:cartesian product"}, every relation *rel* that has left
attribute $A$ and right attribute $B$, is a subset of $A\times B$:
$rel[A*B]\ \dashv\ A\times B$ Instead of
$\langle x,y\rangle\in*rel*$, the shorter notation $x\ *rel*\ y$ is
commonly used. For example, the fact that *Applegate* and *5362a* are
related in the relation *paid*, is written as: *Applegate* *paid* 5362a
{#def:relational notation example}

A relation can be represented in many different ways, such as
*mathematical* forms, *tabular* forms, in *matrix* forms, and various
*graphical* forms. Let us look at an example. The mathematical notation
of relation $provided[Provider*Delivery]$ reads as follows:
\$\$
\begin{array}{rcl}
*provided*&:&*Provider*\times*Delivery*

*provided*&=&\{\begin{array}[t]{@{}l@{}l}\langle`Candy's candy`
, `Cookies #0382`
\rangle&,
 \langle`Carter`
, `Jelly beans #4921`
\rangle&,
 \langle`Carter`
, `Peanut butter #1993`
\rangle&\}\end{array}
\end{array}
{#exm:provided}$ This relation means that a delivery of cookies,
marked with delivery number 0382, has come from Candy's candy, whereas
Carter has provided both the jelly beans (delivery number 4921) and the
peanut butter (#1993) in two deliveries. Both attributes are sets, which
might for instance be defined by: $
\begin{array}{rcl}
*Provider*&=&\{`Candy's candy`
, `Carter`
, `Walmart`
\}

*Delivery*&=&\{\begin{array}[t]{@{}l@{}l}`Cookies #0382`
&,
`Jelly beans #4921`
&,
`Peanut butter #1993`
&\}\end{array}
\end{array}
{#exm:Provider and Delivery}$ Example
([\[exm:provided\]](#exm:provided){reference-type="ref"
reference="exm:provided"}) represents the relation *provided* in a
mathematical form. A tabular representation of the same relation looks
like this: $%%
\begin{tabular}{|l|l|}
%%\hline
%%{\bf Person}&{\bf City}

%%\hline
%%{\tt Peter}&{\tt Amsterdam}

%%{\tt Jill}&{\tt Beverly\ Hills}

%%{\tt Jack}&{\tt Beverly\ Hills}

%%\hline
%%\end{tabular}
  \begin{tabular}{|l|l|}
  \hline
  {\bf Provider}&{\bf Delivery}

  \hline
  `Candy's candy`
&`Cookies #0382`


  `Carter`
&`Jelly beans #4921`


  `Carter`
&`Peanut butter #1993`


  \hline
  \end{tabular}
$$ A matrix representation looks like this:
$$
\begin{tabular}{|l|c|c|c|}
\hline
&`Candy's candy`
&`Carter`
&`Walmart`


\hline
`Cookies #0382`
&$\times$&&

\hline
`Jelly beans #4921`
&&$\times$&

\hline
`Peanut butter #1993`
&&$\times$&

\hline
\end{tabular}

\$\$ A Venn-diagram is a graphical form of representing a relation,
which is shown in figure [1](#fig:graphical%20form){reference-type="ref"
reference="fig:graphical form"}.

::: center
![Graphical representation of
*provided*](../assets/VennProvider.png){#fig:graphical form}
:::

Venn-diagrams show the contents of a relation and the left attribute and
right attributes.

In practice, people write relations in many different forms. A telephone
directory relates names and addresses to telephone numbers in a tabular
manner and a dictionary does the same with words and their meanings.
Still, the representations of telephone directories and dictonaries may
vary, even though both are in essence tabular forms. The effort people
spend in communicating the contents of such relations underlines the
importance of carefully choosing your representations. As a designer,
you too will spend that effort once you expose the contents of a
relation to the public. Before that time, however, you will work with
relations without reference to their contents.

By abstracting away from the contents, you can oversee the structure of
many relations at the same time. For that purpose, you may find it
useful to draw *conceptual diagrams*. Figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"} contains
an example[^2],

::: center
![Figure \# 1: conceptual model of
trading](../assets/trading.png){#fig:trading}
:::

which represents ten relations in one diagram. In conceptual diagrams,
arcs represent relations. Each dot in the diagram represents a concept,
which is used as source or target of relations. The name of a relation
is written near the arc, located such that it is clear which name
belongs to which arc. A little arrowhead in the middle of an arc points
from left attribute to right attribute. Thus, each arc corresponds to
precisely one declaration. The little arrowhead in the middle of an arc
helps you to remember the order of attributes. Note that each arc
determines a corresponding relation uniquely, because name, source, and
target are known for each line. These three items determine the relation
uniquely as explained on page .

Figure [2](#fig:trading){reference-type="ref" reference="fig:trading"}
represents the following declarations: \$\$
\begin{aligned}
  *paid*&:&*Client* \times *Invoice*{#sgn:paidClientInvoice}

  *sent*&:&*Invoice* \times *Client*

  *deliveredto*&:&*Delivery* \times *Client*

  *for*&:&*Invoice* \times *Delivery*

  *from*&:&*Order* \times *Client*

  *from*&:&*Invoice* \times *Provider*

  *provided*&:&*Delivery* \times *Provider*

  *of*&:&*Delivery* \times *Order*

  *accepted*&:&*Provider* \times *Order*{#sgn:acceptedProviderOrder}

  *issued*&:&*Order* \times *Provider*{#sgn:issuedOrderProvider}
\end{aligned}

\$\$

This vocabulary needed to write rules consists of relations and
operators. You will use relations as your basic building bricks and
operators as mortar to combine relations into rules that describe
accurately whatever you want to specify.

Set operators are already a part of the vocabulary of relations, since
relations themselves are sets. So the expression $r\dashv s$ means that
every pair in $r$ is also a pair in $s$. Similarly, the expression
$r\cup s$ is the set of pairs in $r$ or in $s$ and $r\cap s$ is the set
of pairs in $r$ and in $s$.

For example, if
$*candyprice*=\{\langle {\tt Jelly},1.05\rangle,\langle {\tt Chocolate},0.80\rangle,\langle {\tt Apples},2.50\rangle\}$
and
$*groceryprice*=\{\langle {\tt Chocolate},0.80\rangle,\langle {\tt Apples},2.95\rangle\}$,
then \$\$
\begin{aligned}
*candyprice*\cup *groceryprice*&=&\begin{array}[t]{@{}l}\{\langle {\tt Jelly},1.05\rangle, \langle {\tt Chocolate},0.80\rangle,
 \langle {\tt Apples},2.50\rangle, \langle {\tt Apples},2.95\rangle\}\end{array}

*candyprice*\cap *groceryprice*&=&\{\langle {\tt Chocolate},0.80\rangle\}
\end{aligned}

\$\$

Set operators alone are not enough. We also need operators from
relational calculus as well, most notably the *converse* of a relation,
the *composition* of two relations, and the *identity relation*. These
three operators are standard in relational calculus, although their
notations may vary across different publications.

Converse merely swaps the left hand side and the right hand side:

-   *converse:* Let $r[A*B]$, then ${r}^\smallsmile$ is a
    relation, which is defined by:
    \$\declare{${r}\^`\smallsmile`{=tex}\$}{A}{B}

        {r}^\smallsmile = \{\langle b,a\rangle |\ a\ r\ b\}{#def:converse}$

The converse of a relation swaps the left column and the right column in
the tabular form. It mirrors the matrix form of a relation along its
diagonal. Let us look at some examples, based on *provided* (relation
[\[exm:provided\]](#exm:provided){reference-type="ref"
reference="exm:provided"} on page ), *paid* (relation
[1](#exm:paid){reference-type="ref" reference="exm:paid"} on page ), and
the relation $for[Invoice*Delivery]$, which is defined by:
\$\$
\begin{array}{rcl}
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
{#exm:for}$ The converse of *for* is: $
\begin{array}{rcl}
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
\end{array}

\$\$ Composition makes one relation from its two arguments. If, for
example, $name[String*Person]$ defines which names belong to
which person, and $age[Person*Int]$ defines the age of each
person, then $*name*;*age*$ is a relation that contains names and
corresponding ages. Here is the definition:

-   *composition :* Let $r[A*B]$ and $s[B*C]$,
    then $(r;s)$ is a relation, which is defined by: \$\$
    \begin{aligned}
        (r;s)&:&A\times C\nonumber

        r;s&=&$\{\langle a,c\rangle |\ $ there exist $b$ such that $a\ r\ b$ and $b\ s\ c\}$ {#def:composition}
    \end{aligned}

If you know about relational databases, you might like to know that
composition corresponds to the natural join operator. Here is an
example: The composition of *paid* and *for* is obtained by applying
definition [\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}. Figure
[3](#fig:paid;for){reference-type="ref" reference="fig:paid;for"}
illustrates this composition. \$\$
\begin{array}{rcl}
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

{#exm:paid;for}\$\$

::: center
![composition of *paid* and *for*
($*paid*;*for*$)](%7B../assets/paidfor.png%7D){#fig:paid;for}
:::

You cannot compose just any relation. The relation $*provided*;*for*$ is
not defined, because definition
[\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"} requires the right attribute of *provided*
to be the same as the left attribute of *for*. *Delivery*, being the
right attribute of *provided*, is clearly not the same as *Invoice*,
which is the left attribute of *for*. Nevertheless, composing *provided*
with ${*for*}^\smallsmile$ is defined, and the result can be obtained by
applying definition
[\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}: \$\$
\begin{array}{rcl}
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

{#exm:provided;flipFor}\$\$

The identity relation is implicitly defined for every concept $C$. We
use it for example to specify equality in rules.

-   *identity relation:* For every concept $C$, the identity relation
    $\it{id}_{C}$ is defined by: \$\$`\it{id}_{C} : C\times C`{=tex}

        \it{id}_{C} = \{\langle c,c\rangle |\ c\in C\}{#def:identity relation}$

Having introduced relations and defined operators on them, let us turn
to rules.

## Rules {#sec:Rules}

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
[2](#fig:trading){reference-type="ref" reference="fig:trading"}. In that
situation, clients and providers are trading according to a simple
pattern of orders, deliveries and invoices. The figure proper does not
contain any rules; it is merely a picture in which ten different
relations are declared. This section proposes six rules. Each rule is
represented in relational calculus, emphasizing the line of reasoning
that brings us from informal text to a precise formula.

For starters, how do we express that a provider does not accept any
order issued to someone other than himself? Relation
$issued[Order*Provider]$ in figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"}
apparently tells us which orders were issued to which providers.
Relation $accepted[Provider*Order]$ states which providers
have accepted which orders. Our first rule states that providers need
not accept all orders; they are supposed to accept only those orders
that were issued to them. So the set of orders that have been accepted
by a provider is a subset of the orders that were issued to him. In
other words, every element $\langle p,o\rangle$ of the relation
*accepted* is also an element of ${*issued*}^\smallsmile$. Recalling the
definition of inclusion on page , we can write this rule as:
$*accepted*\ \dashv\ {*issued*}^\smallsmile
{#eqn:RA accepted issued}$ Rule [\[eqn:RA accepted
issued\]](#eqn:RA%20accepted%20issued){reference-type="ref"
reference="eqn:RA accepted issued"} uses relations to represent that a
provider does not accept an order issued to someone other than himself.
This is exactly what we meant to represent, so here we have a first
example of a rule that represents business knowledge.

Having successfully represented a first rule, let us try another one.
How can we state that no client should ever pay an invoice that was not
addressed to him? First, we establish that this rule involves the
relations $*sent*$ and $*paid*$. Then we reformulate the rule, stating
that invoices paid by a client can be only those invoices that are sent
to that client. In other words, every element $\langle i,c\rangle$ of
the relation *paid* is also an element of ${*sent*}^\smallsmile$. This
is represented by: $*paid*\ \dashv\ {*sent*}^\smallsmile
{#eqn:RA paid sent}$ This rule is quite similar to rule [\[eqn:RA
accepted issued\]](#eqn:RA%20accepted%20issued){reference-type="ref"
reference="eqn:RA accepted issued"}, because the reasoning that leads to
this rule is quite similar. An important use of these rules is to
identify violations explicitly. For instance, any invoice that is paid
by a client but not sent to that client constitutes a violation of rule
[\[eqn:RA paid sent\]](#eqn:RA%20paid%20sent){reference-type="ref"
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
element of the relation $*accepted*;{*of*}^\smallsmile$ that says that
there is an order, which was accepted by the provider and upon which the
delivery was made. This rule is written as:
$*provided*\ \dashv\ *accepted*;{*of*}^\smallsmile
{#eqn:RA provided}$ Let us verify that this is exactly the rule we
want. The second part of this rule, $*accepted*;{*of*}^\smallsmile$,
represents the relation that contains pairs $\langle p,d\rangle$ of
$*Provider*\times*Delivery*$ for which there exist $o\in*Order*$ such
that $p\ *accepted*\ o$ and $o\ ({*of*}^\smallsmile)\ d$. You may verify
this by checking on definition
[\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}. Note that $o\ ({*of*}^\smallsmile)\ d$ is
equivalent to $d\ *of*\ o$ (by definition
[\[def:converse\]](#def:converse){reference-type="ref"
reference="def:converse"}). A violation of rule [\[eqn:RA
provided\]](#eqn:RA%20provided){reference-type="ref"
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
${*for*}^\smallsmile;*sent*\ \dashv\ *deliveredto*
{#eqn:RA flipFor sent}$ This rule is useful to expose violations just
like the previous ones. If an invoice is sent to a client different from
the one who received the delivery, we have a violation. If an invoice is
about a delivery that was never delivered, that too constitutes a
violation. If there is no invoice, there is no violation of this
particular rule. It might still be in the mail, after all.

After discussing four rules, let us look back at figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"}. You may
have noticed that every time we discussed a rule, the relations involved
in that rule form a cycle in figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"}. This is
no coincidence. Each rule has a relation in the left hand side and a
relation on the right hand side. These two relations form two different
paths in figure [2](#fig:trading){reference-type="ref"
reference="fig:trading"} between the same concepts. Hence, the relations
involved form a cycle, which encloses an area in figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"}.

By turning this principle around, we can use it to discover new rules.
For instance, let us investigate the relations
$from[Invoice*Provider]$, *for*, and *provided*. These three
relations form a cycle in figure [2](#fig:trading){reference-type="ref"
reference="fig:trading"}, which has not been covered by any of the
previous rules. Can we propose a rule, using these three relations? If
there is an invoice for delivery $d$ that comes from provider $p$, then
$\langle p,d\rangle$ must be in *provided*. One part of this rule, which
says whether there is an invoice for delivery $d$ that comes from
provider $p$, is represented by ${*from*}^\smallsmile;*for*$ (by
definition [\[def:composition\]](#def:composition){reference-type="ref"
reference="def:composition"}). In relational terms, we are saying that
any pair $\langle p,d\rangle$ that is in ${*from*}^\smallsmile;*for*$ is
also in *provided*. However, the other way around is also useful: for
any pair $\langle p,d\rangle$ that is in *provided* there must be an
invoice for delivery $d$ that comes from provider $p$. The rule works in
two directions: the relations *provided* and
${*from*}^\smallsmile;*for*$ are a subset of one another. Using property
[\[eqn:antisymmetric
subset\]](#eqn:antisymmetric%20subset){reference-type="ref"
reference="eqn:antisymmetric subset"}, we get:
$*provided*\ =\ {*from*}^\smallsmile;*for*
{#eqn:DEF provided}$ Reflecting on this result, you may have noticed
that this rule was discovered merely by following the procedure of
inspecting relations in a cycle. There was no flash of ingenuity, no
spectacular discovery involved. Also notice that equation [\[eqn:DEF
provided\]](#eqn:DEF%20provided){reference-type="ref"
reference="eqn:DEF provided"} can be used to define the relation
*provided*. If relations *from* and *for* are known, the contents of
*provided* can be computed from their contents.

Each of the previous rules [\[eqn:RA accepted
issued\]](#eqn:RA%20accepted%20issued){reference-type="ref"
reference="eqn:RA accepted issued"} through [\[eqn:RA flipFor
sent\]](#eqn:RA%20flipFor%20sent){reference-type="ref"
reference="eqn:RA flipFor sent"} might have been discovered in a similar
way, systematically inspecting the cycles in figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"}. In
fact, most areas in figure [2](#fig:trading){reference-type="ref"
reference="fig:trading"} are enclosed by the cycles discussed so far.
Only the area enclosed by relations $from[Order*Client]$, *of*
and *deliveredto* remains to be analyzed. So these three relations are a
candidate for a rule. By similar reasoning as in the previous rule, our
proposal will be: $*deliveredto*\ =\ *of*;*from*
{#eqn:DEF deliveredto}$ This rule says: if a pair $\langle d,c\rangle$
is in *deliveredto*, there must be an order for delivery $d$ that comes
from provider $p$. Reversely, if there is an order for delivery $d$ that
comes from provider $p$, $\langle d,c\rangle$ must be in *deliveredto*.
So this rule too is a proper candidate for agreement among stakeholders
in the business.

After studying the six rules presented so far, you will have acquired
the idea of inventing rules by chasing cycles. It is a skill, basic to
the CC-technique. This skill is fundamental in analyzing the rule
proposals of various stakeholders and making these proposals concrete.

## Combining rules {#sec:combining-rules}

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
![Figure \# 2: conceptual model of
trading](../assets/derivationOrderInvoice.png){#fig:derivationOrderInvoice}
:::

For example, the cycles that correspond to rules [\[eqn:RA flipFor
sent\]](#eqn:RA%20flipFor%20sent){reference-type="ref"
reference="eqn:RA flipFor sent"} and [\[eqn:DEF
deliveredto\]](#eqn:DEF%20deliveredto){reference-type="ref"
reference="eqn:DEF deliveredto"}, drawn again in figure
[4](#fig:derivationOrderInvoice){reference-type="ref"
reference="fig:derivationOrderInvoice"}, form adjacent areas. The
relation *deliveredto* is shared between them. This observation inspires
the following derivation: \$\$
\begin{array}{cl}
&{*for*}^\smallsmile;*sent*

\dashv&\hspace{1in}\{\hbox{rule \ref{eqn:RA flipFor sent}}\}

&*deliveredto*

=&\hspace{1in}\{\hbox{rule \ref{eqn:DEF deliveredto}}\}

&*of*;*from*
\end{array}

$ As a result, we may conclude:
${*for*}\^`\smallsmile`{=tex};*sent* `\dashv`{=tex} *of*;*from*
{#eqn:flipfor;sent of;from}\$\$ This result says: if there is an invoice
for a delivery, which has been sent to some client $c$, then there is an
order for that delivery, which has come from client $c$. So the idea has
worked: adjacent areas in a conceptual diagram may inspire new rules.
The new rule [\[eqn:flipfor;sent
of;from\]](#eqn:flipfor;sent%20of;from){reference-type="ref"
reference="eqn:flipfor;sent of;from"} is a logical consequence of the
existing rules [\[eqn:RA flipFor
sent\]](#eqn:RA%20flipFor%20sent){reference-type="ref"
reference="eqn:RA flipFor sent"} and [\[eqn:DEF
deliveredto\]](#eqn:DEF%20deliveredto){reference-type="ref"
reference="eqn:DEF deliveredto"}.

In a similar fashion we can explore other parts of figure 2, and do some
more reasoning with rules: \$\$
\begin{array}{cl}
&{*from*}^\smallsmile;*for*

=&\hspace{1in}\{\hbox{rule \ref{eqn:DEF provided}}\}

&*provided*

\dashv&\hspace{1in}\{\hbox{rule \ref{eqn:RA provided}}\}

&*accepted*;{*of*}^\smallsmile

\dashv&\hspace{1in}\{\hbox{rule \ref{eqn:RA accepted issued}}\}

&{*issued*}^\smallsmile;{*of*}^\smallsmile
\end{array}

$ This derivation says: if there is an invoice for a
delivery, that came from some provider $p$, then there is an order for
that delivery, which was issued to provider $p$. Or, in relational
terms:
${*from*}^`\smallsmile`{=tex};*for* `\dashv`{=tex} {*issued*}^`\smallsmile`{=tex};{*of*}\^`\smallsmile`{=tex}\$\$

## Multiplicities {#sec:Multiplicities}

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
    by: ${r}^\smallsmile;r\ \dashv\ \it{id}_{B}{#def:univalent}$ The
    definition says that $a\ r\ b$ and $a\ r\ b'$ imply $b=b'$ for all
    $a$, $b$, and $b'$. To denote that relation $r$ is univalent, we
    write $*univalent*(r)$.

-   **total**\
    A relation $r[A*B]$ is *total* if each element of $A$
    corresponds to at least one element of $B$. This property is defined
    by: $\it{id}_{A}\ \dashv\ r;{r}^\smallsmile{#def:total}$ The
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
    by: $r;{r}^\smallsmile\ \dashv\ \it{id}_{A}{#def:injective}$ The
    definition says that $b\ r\ a$ and $b\ r\ a'$ imply $a=a'$ for all
    $a$, $a'$, and $b$. Notice that the property injective is defined
    conversely to the property univalent. To denote that relation $r$ is
    injective, we write $*injective*(r)$.

-   **surjective**\
    A relation $r[A*B]$ is *surjective* if each element of
    $B$ corresponds to at least one element of $A$. This property is
    defined by:
    $\it{id}_{B}\ \dashv\ {r}^\smallsmile;r{#def:surjective}$ The
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
![Figure \# 3: Graphical notations of
multiplicities](../assets/multiplicities.png){#fig:multiplicities}
:::

Database designers are familiar with multiplicities, because these
properties have significant impact on implementation choices. Figure
[5](#fig:multiplicities){reference-type="ref"
reference="fig:multiplicities"} shows two popular ways in which
multiplicities are shown in database models. The left hand side shows
the convention in UML [@UML]. On the right, you can see the \"crow's
foot\" notation, which has become popular in entity relationship
modelling (e.g. James Martin). Multiplicity notations may differ from
one graphical modelling technique to another. In some graphical modeling
techniques it may not be clear which side of a relation is source and
which is target. In those techniques, you may want to write the name of
a relation near its target, so you can easily remember the source and
target of that relation. In conceptual diagrams (e.g. in figure
[2](#fig:trading){reference-type="ref" reference="fig:trading"}) the
little arrowhead gives direction to every relation.

## Functions {#sec:Functions}

Functions are relations that are total and univalent. This allows for a
convenient notation, which makes functions special. If
$r[A*B]$ is a function, we write $r:{A}\rightarrow{B}$. We
will normally use the symbols $f$, $g$, $h$ instead of relational
symbols, and the notation $b=f(a)$ instead of $a\ f\ b$.

::: center
![Figure \# 4: multiplicities in conceptual
diagrams](../assets/functional.png){#fig:functional}
:::

In a conceptual diagram functions are depicted by an arrow ($f$ in
figure [6](#fig:functional){reference-type="ref"
reference="fig:functional"}). Every other relation is depicted with the
little arrowhead ($r$ in figure
[6](#fig:functional){reference-type="ref" reference="fig:functional"}).

For example, we may write the function
$*provided*:{*Provider*}\rightarrow{*Delivery*}$ (example
[\[exm:provided\]](#exm:provided){reference-type="ref"
reference="exm:provided"} on page ) in this way: \$\$
\begin{array}{rcl}
 *provided*(`Cookies #0382`
)&=&`Candy's candy`


 *provided*(`Jelly beans #4921`
)&=&`Carter`


 *provided*(`Peanut butter #1993`
)&=&`Carter`

\end{array}

\$\$ If, however, we would allow a delivery to have more than one
provider (e.g. the cookies with delivery number 0382 were delivered both
by Walmart and by Candy's candy, then *provided* would no longer be a
function, and therefore we would use the relational notation
([\[def:relational
notation\]](#def:relational%20notation){reference-type="ref"
reference="def:relational notation"}) rather than the functional
notation.

[^1]: In this introduction, the reader may recognize some of the
    skillfully crafted sentences and formulas from Jipsen, Brink, and
    Schmidt, whose work is gratefully acknowledged.

[^2]: The entire example, including the rules in section
    [3.1](#sec:Rules){reference-type="ref" reference="sct:Rules"}, was
    communicated to the author by Rieks Joosten.
