# Prototype framework

## Backend implementation \(PHP\)

### Structure of project folder

```text
|- 
|- 
|-
```

### Errors and exceptions

* In the code of the prototype framework \(php\), around 250 places exist where an exception is thrown. Exceptions are thrown with a message and code. For the code we reuse the [HTTP status codes](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes), to have clear semantics and to determine which error is returned to the user.
* Codes 4xx are bad request by the user, and result in an error which is always shown in full details to the user. E.g.
  * "404 Resource not found" when a user requests an atom which does not exist.
  * "403 You do not have access to this page" when a user doesn't have the required ROLE
  * "400 Data entry too long"
* Codes 5xx are server side errors, and indicate a programming error which is \(in most cases\) not caused by user input. These exceptions can give away how the code is structured, which is why depending on the `debugMode` settings, the error message is shown to the user or not. In case `debugMode` is off, a generic message is shown: 'An error occured \(debug information in server log files\)'. When debugMode is on, the user can click on the error message and see the stack trace.
* The log files always contain details about the exception.
* All exceptions are caught by the API framework we use, and transformed into a json structure for the frontend.
* As php code is compiled at runtime, the prototype framework can contain errors that can't be caught and/or handled by the API framework. This especially holds for errors early in the execution, when initializing the API framework or AmpersandApp object.
* We use a [static analyzer](https://github.com/phan/phan) to check the code during development. This prevents us from introducing bugs that can be detected by such an analyzer \(which is actually a lot\).
  * At most class methods we specify input and return type definitions. This improves the static analyses.

## Frontend implementation \(AngularJS\)

### Building and packaging the application

TODO explain: Bower, NPM, Gulp

