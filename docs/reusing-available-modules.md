# Reusing Available Modules

## Sessions, Identity and Access Management \(SIAM version 3\) 

You might want to know who is using your application. Or you might want to offer each one of your users some privacy when using your application. Logging in, passwords, authorizations, are typically used for such purposes. This field, also known as Identity and Access Management is covered in [this chapter](#the-siam-module).

The [SIAMv3 module](https://github.com/AmpersandTarski/ampersand-models/tree/master/SIAMv3) gives you a selection of ways to organise that, ready for you to use.This module is located in the ampersand-models repository on [Github](https://github.com/AmpersandTarski). It is maintained by [Rieks Joosten](https://github.com/orgs/AmpersandTarski/people/RieksJ) from a closed repository at TNO \(git@ci.tno.nl:ampersand/models.git\).

## Modules

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

## Security

Even though Ampersand was designed for prototyping, we are taking applications such as RAP4 and Semantic Treehouse to production. As the development of Ampersand is going in the direction of production software, we must be prepared for questions. Currently, I can hardly answer any questions about security. This page is for the purpose to discuss security questions, such that we are at least on the same ground wrt security. If any changes to Ampersand emerge from that, we should create separate issues for them. Here is a list of questions.

### Access control

How is access control arranged in an Ampersand application? _Answer_: An Ampersand programmer can arrange this by using the SIAM modules, which provide role-based access controls, login possibilities and password security. Yet, we advise to let the deployment platform (e.g.\ Kubernetes) take care of access control, keeping the Ampersand code clean and independent of any access control mechanism.

### Logging

How does an Ampersand application log critical activities, and transactions that deal with sensitive data? _Answer_: The runtime system used the Monolog-framework to support all runtime logging. It allows selective logging of precisely the right actions, at the control of the designer.

### Encryption of data

How does Ampersand encrypt data? _Answer_: It doesn't. All data is stored as-is in the database. We advise to put the database in a separate container, which is not connected to the outside world, to minimize the chance of illegal access to the database.

### Access control for specific purposes

How does an Ampersand application control access for purposes that are known only to the Ampersand application designer? _Answer_: The designer can use the ROLE mechanism to provide different interfaces to different roles. Also, the designer can model these purposes in the Ampersand-script and take it from there. In the latter case, every possible access control scheme can be built.

### Injection

How does an Ampersand application prevent injection flaws, by which hostile data can trick the interpreter into executing unintended commands or accessing data without proper authorization? _Answer_: Currently, the Ampersand application is not secured against injection flaws.

### Broken Authentication and Session Management

How can we know that application functions related to authentication and session management are implemented correctly? Is there a chance that attackers can compromise passwords, keys, or session tokens, or exploit other implementation flaws to assume other users’ identities? _Answer_:
 * In the browser, Ampersand runs a PHP session strict mode. This prevents a user defined session ID that is never generated
 * The authentication and session management are described in the SIAM-module in Ampersand itself. So we have a mathematical handle on this, which gives more certainty than hand-crafted code. We have no security assessment on the SIAM-code for this risk. Of course, when this is embedded in for example a SSO-service, we would have to look for specific risks incurred by that.
 * At the moment I have no idea how good or bad an Ampersand-application will do on this risk.

### Cross-Site Scripting

Can an attacker execute scripts in the browser in which the Ampersand-session is running, and hijack user sessions, deface web sites, or redirect the user to malicious sites? _Answer_: There are currently no protections against Cross-Site Scripting.

### Insecure Direct Object References

Can an attacker get hold of a reference to an internal implementation object, such as a file, directory, or database key? _Answer_: The Ampersand language offers facilities to hide direct object references from the user. This is not the case in the API. A user with API-access can find direct object references.

### Security Misconfiguration

How do we know that a secure configuration is defined and deployed, and that security settings are defined, implemented and maintained? _Answer_: The security configuration concerns more than the Ampersand-application alone. It touches on frameworks, application server, web server, database server and platform, and all components in the landscape in which it performs its duty. The Ampersand-configuration items are described in the architecture. Their parameters have been set to defaults that are "generally secure" without jeopardizing their functioning. Hardening of the database is left to the person who installs the Ampersand-application.

### Sensitive Data Exposure

Does Ampersand have extra protection for sensitive data, such as credit cards, tax IDs, and authentication credentials? _Answer_: No.
We have a policy not to publish secrets in the Ampersand repository. GitHub (scans for secrets)[https://github.com/AmpersandTarski/Ampersand/security/secret-scanning], so we get notified of any violations of this policy.

### Missing Function Level Access Control

Does an Ampersand application re-verify access rights in every API-call? _Answer_: No

### Cross-Site Request Forgery \(CSRF\)

Suppose the browser has been taken over maliciously. Can an Ampersand application be tempted to send a forged HTTP-request, including the session cookie and any other automatically included authentication information? _Answer_: We don't know

### Using Known Vulnerable Components

How well are libraries, frameworks, and other software modules protected? Are their privileges restrained? How? _Answer_: We don't know

### Unvalidated Redirects and Forwards

Is it conceivable that an Ampersand application forwards its user to other pages and websites? _Answer_: The Ampersand-programmer uses the \(technical\) data type `URL` for hyperlinking the user to other sites. She must ensure the trustworthiness of the URL's used in order to build a trustworthy web-application.

## The SIAM Module

The SIAM (Sessions, Identity and Access Management) Module implements functionalities for managing Sessions, Identies and authentication \(login\). Also, it implements Accounts, Roles and a primitive yet extendable registration for people and organizations.

This chapter informs developers about:

* how to include the SIAM module into a prototype;
* what configuration/tinkering options he has;
* what he should or should not do.

### Where to find the Module

### Acquainting yourself with the  Module's functionality

Playing with the module's functionality allows you to assess whether or not the implemented functionality is valid for your purposes. This section tells you how to do this.

### Including the Module into your prototype

