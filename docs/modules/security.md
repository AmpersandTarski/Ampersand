# Security

Even though Ampersand was designed for prototyping, we are taking applications such as RAP2 and RAP3 to production. As the development of Ampersand is going in the direction of production software, we must be prepared for questions. Currently, I can hardly answer any questions about security. This page is for the purpose to discuss security questions, such that we are at least on the same ground wrt security. If any changes to Ampersand emerge from that, we should create separate issues for them. Here is a list of questions.

## Access control

How is access control arranged in an Ampersand application? _Answer_: This can be arranged by using the SIAM modules, which provide role-based access controls, login possibilities and password security.

## Logging

How does an Ampersand application log critical activities, and transactions that deal with sensitive data? _Answer_: The runtime system used the Monolog-framework to support all runtime logging. It allows selective logging of precisely the right actions, at the control of the designer.

## Encryption of data

How does Ampersand encrypt data? _Answer_: It doesn't. All data is stored as-is in the database.

## Access control for specific purposes

How does an Ampersand application control access for purposes that are known only to the Ampersand application designer? _Answer_: The designer can use the ROLE mechanism to provide different services to different roles. Also, the designer can model these purposes in the Ampersand-script and take it from there. In the latter case, every possible access control scheme can be built.

## Injection

How does an Ampersand application prevent injection flaws, by which hostile data can trick the interpreter into executing unintended commands or accessing data without proper authorization? _Answer_: Currently, the Ampersand application is not secured against injection flaws.

## Broken Authentication and Session Management

How can we know that application functions related to authentication and session management are implemented correctly? Is there a chance that attackers can compromise passwords, keys, or session tokens, or exploit other implementation flaws to assume other users’ identities? _Answer_: The authentication and session management are described in the SIAM-module in Ampersand itself. So we have a mathematical handle on this, which gives more certainty than hand-crafted code. We have no security assessment on the SIAM-code for this risk. Of course, when this is embedded in for example a SSO-service, we would have to look for specific risks incurred by that. At the moment I have no idea how good or bad an Ampersand-application will do on this risk.

## Cross-Site Scripting

Can an attacker execute scripts in the browser in which the Ampersand-session is running, and hijack user sessions, deface web sites, or redirect the user to malicious sites? _Answer_: There are currently no protections against Cross-Site Scripting.

## Insecure Direct Object References

Can an attacker get hold of a reference to an internal implementation object, such as a file, directory, or database key? _Answer_: The Ampersand language offers facilities to hide direct object references from the user. This is not the case in the API. A user with API-access can find direct object references.

## Security Misconfiguration

How do we know that a secure configuration is defined and deployed, and that security settings are defined, implemented and maintained? _Answer_: The security configuration concerns more than the Ampersand-application alone. It touches on frameworks, application server, web server, database server and platform, and all components in the landscape in which it performs its duty. The Ampersand-configuration items are described in the architecture. Their parameters have been set to defaults that are "generally secure" without jeopardizing their functioning. Hardening of the database is left to the person who installs the Ampersand-application.

## Sensitive Data Exposure

Does Ampersand have extra protection for sensitive data, such as credit cards, tax IDs, and authentication credentials? _Answer_: No.

## Missing Function Level Access Control

Does an Ampersand application re-verify access rights in every API-call? _Answer_: No

## Cross-Site Request Forgery \(CSRF\)

Suppose the browser has been taken over maliciously. Can an Ampersand application be tempted to send a forged HTTP-request, including the session cookie and any other automatically included authentication information? _Answer_: We don't know

## Using Known Vulnerable Components

How well are libraries, frameworks, and other software modules protected? Are their privileges restrained? How? _Answer_: We don't know

## Unvalidated Redirects and Forwards

Is it conceivable that an Ampersand application forwards its user to other pages and websites? _Answer_: The Ampersand-programmer uses the \(technical\) data type `URL` for hyperlinking the user to other sites. She must ensure the trustworthiness of the URL's used in order to build a trustworthy web-application.

