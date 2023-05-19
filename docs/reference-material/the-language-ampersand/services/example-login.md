---
description: 'TODO: This example is subject to bitrot. It has to be redone.'
---

# Example: Login

This example defines a login/logout service,  
because it is familiar. We show this example to demonstrate how to get different interface structures under varying conditions.

## Preliminaries

The compiler uses templates to adapt an interface to specific needs regarding its HTML structure. Please read the [documentation of templates](https://github.com/AmpersandTarski/prototype/tree/master/templates) first for details.

To link system activities to a person or organisation, we use the notion of `Account`. To log in means to associate a session with the `Account` of the user. This association is made in the relation `sessionAccount`. To log out means to break that link, i.e. to remove the session/account pair from relation `sessionAccount`. When logging in, it is customary that the user identifies herself. In this example we do this with a `UserID` and `Password`.

A `UserId` is used to identify the user by a unique name. In this way, the \(system generated\) key of the user in the database is kept within the database.

To make it more difficult to use an other person's `Account`, the system registers passwords. A `Password` is a string of characters known to the user only. For this reason, the login service must not expose the password while the user is typing it.

To isolate a data space for one specific user, we use the notion of session. A `SESSION` corresponds with the notion of session as used in browsers. Ampersand links the session called `'_SESSION'` to the current browser session, which results in the behaviour one would expect of a browser session.

## How the service works

A login service allows a user to log in and log out of the system. Here is what it looks like in a browser:

![login service in browser](../../.gitbook/assets/ssh1.png)

Wonder what the 25al1rdkdfvmapkkqvuf5sroc5 means? Well this is the session number of the actual browser session. It is the value for which the atom

`"_SESSION"`

stands in your script.

When you type your name, it shows up in the field Userid, but when you type in the password it is obscured by dots

\(

as we would expect

\)

:

![passwd remains invisible](../../.gitbook/assets/ssh2.png)

When we then type

`<enter>`

, the login functionality disappears and the logout functionality appears:

![interface swap](../../.gitbook/assets/ssh3.png)

When you click the checkbox, you have logged out and will return to the first screen

## What the Ampersand code looks like

To understand how it all works, let us discuss the code for this service:

```text
INTERFACE Login : '_SESSION'[SESSION] cRud BOX <HROWS>
   [ "Login": I - sessionAccount;sessionAccount~ cRud BOX <HCOLS>
      [ "Userid"   : loginUserid cRUd
      , "Password": loginPassword crUd -- crUd is needed for Passwords
      ]
   , "Logout": I /\ sessionAccount;sessionAccount~ cRud BOX <ROWSNL>
      [ "Logout": I cRud BOX <HCOLS>
         [ "Logout?": logoutRequest cRUd
         , "UserID": sessionUserid cRud
         ]
      ]
   ]
```

If you analyse this code, notice the nested structure of `BOX`-es. The service is a box on the top level with two sub-boxes labeled `"Login"` and `"Logout"`.

The top-level box has `"_SESSION"[SESSION]` as its box-term. What you must remember is that the every atom of the codomain of that term causes one contain \(HTML: `<div>`\). In this example, the codomain of `"_SESSION"[SESSION]` is just one atom, which is the session identifier. That is shown in the title of the outmost box in the browser.

### Selectively showing subboxes by `<HROWS>`

We would expect to see two subboxes, one labeled `"Login"` and another labeled `"Logout"`. However, the outmost box was annotated with `<HROWS>`. The `H` stands for hidden. It means that empty subboxes will be hidden from the user. Now look at the box-terms of the `"Login"`- and `"Logout"` subboxes. Which elements are in the codomain of the box-term of the `"Login"` subbox? It says: "All sessions without an account associated with it". Since there is only one session \(i.e. the browser-session\) this comes down to "the current session, provided there is no account associated with it." So if nobody is logged in in the current browser session, the session atom is the only atom. Otherwise there is no atom and the `"Login"` subbox is not shown. Similarly, which elements are there to make the the `"Logout"` subbox appear? That box-term shows all sessions associated with an account. Which would be the account of the current session, provided someone is logged in.

That explains why the `"Login"` subbox is shown when nobody is logged in and the `"Logout"` subbox is shown when someone is logged in.

So let us do the following experiment: change the

`<HROWS>`

annotation to

`<ROWS>`

. Then we will see both boxes:

![without hiding](../../.gitbook/assets/ssh4.png)

Notice that both subboxes have the

`H`

in their annotations, so in the screeshot above the

`"Logout"`

subbox remains empty. However, when logged in, the other subbox remains empty:

![without hiding](../../.gitbook/assets/ssh5.png)

## 

