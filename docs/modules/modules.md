# Modules

An Ampersand Module is a coherent set of Ampersand scripts that:

* provide a specific functionality, and 
* are set up in such a way that re-using them in different prototypes is easy.

An example is the SIAM module \(SIAM = Session Identity and Access Management Module\). This module provides login functionality and extendable registrations for e.g. persons, organizations and accounts. Also, it provides functionality that allows for access control \(of INTERFACEs\) based on roles, as well as on account attributes.

The SIAM module is set up in such a way that it is easy to re-use. Developers may INCLUDE a script \(for which a template is provided\) that imports the appropriate functionalities into the prototype. By tailoring this INCLUDE-script, a developer may turn specific features on or off. This is much easier than having to develop the functionality all by himself.

The current set of modules include:

* Messaging - sending messages to people, roles and other recipients by variious means, e.g. email.
* Sets - perhaps the simplest module of all, implementing set- and subset functionality.
* Sequences - functions for creating sequences of items, adding items at different ends
* SIAM - i.e. Session, Identity and Access Management.

