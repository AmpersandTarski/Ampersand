# Can dougnuts be declarative?

What is a doughnut? If we first make some dough, then shape it into a circle, and then fry it, that gives us a doughnut. I can also just say that a doughnut is a toroid-shaped, fried piece of dough. The former is called a procedural or imperative description, which we know from cooking \(e.g. a recipe for making apple pie\) or in the process industry \(a method for extracting nitrogen from the air\), for example. The latter is called declarative, examples of which are [a blueprint of the Tower Bridge](https://en.wikipedia.org/wiki/File:Tower_bridge_schm020.png) in London, a [city map of Antwerp](https://en.wikipedia.org/wiki/City_map#/media/File:Antwerp,_Belgium,_Braun_and_Hogenberg,_1572-79.jpg) in 1572, [the law that defines the national registry of persons](http://wetten.overheid.nl/jci1.3:c:BWBR0033715) in the Netherlands \(called BRP\), and a [conceptual model](http://www.iso-architecture.org/ieee-1471/cm/Conceptual-Realm.png). The distinction between defining \(by constraints\) and making \(by giving steps\) is known in patent law, e.g. for a chemical substance as opposed to the process of making that substance.

This distinction works for information systems too. We can define an information system by describing the steps that lead to that system. Or we can define that system by defining its components and their relations. 

Ampersand is a declarative language and it is meant for specifing information systems. It defines a system by stating its components and their relations. Other examples of declarative languages are YAML, JSON, SQL, Haskell, and Terraform.

Examples of procedural or imperative languages are Perl, make, Java, and C\#. These languages define a desired result by specifying the steps needed to produce it. In a procedure, the order of steps is relevant. For example, if we would fry the dough first and then shape it into a circle, who would expect to get a proper doughnut from that? In a definition, the order of constraints is much less relevant. We can say that a doughnut is toroid shaped and that it is fried and that it is made of dough. And we can mention these constraints in any order.

We made Ampersand declarative on purpose, because Ampersand is meant to define things. So if you want to define an information system, Ampersand lets you specify it in terms of constraints \(also known as rules\).

Still wondering whether doughnuts can be declarative? Let's dismiss the question and say that a dougnut is best defined in a declarative way, best produced in a procedural way, and best enjoyed with lots of sugar.

