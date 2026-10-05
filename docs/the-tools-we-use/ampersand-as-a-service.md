---
description: >-
  How to run the Ampersand compiler as an HTTP/JSON service with `ampersand serve`,
  which questions the service answers, and what a caller needs to know to rely on it.
---

# Running the compiler as a service

The command `ampersand serve` keeps the compiler running
and lets other programs ask it questions over HTTP.
We built it for callers that hold a script in memory, such as an editor in a browser,
and for applications such as RAP,
which want the compiler's answer without installing the compiler themselves.
This page is for the developer who writes such a caller.

## Starting the service

```bash
ampersand serve
```

The service listens on port 8080 on all network interfaces.
Set the environment variable `AMPERSAND_SERVE_PORT` to choose another port.
With Docker, the published image starts the service like this:

```bash
docker run -p 8080:8080 ampersandtarski/ampersand serve
```

To see that it runs, ask for its health:

```bash
curl localhost:8080/health
```

```json
{"status":"ok"}
```

## Asking a question

Every question except `/health` is a `POST` with a JSON object as its body,
and every answer is a JSON object.
For instance, this request asks whether a script is correct:

```bash
curl -X POST localhost:8080/check \
  -d '{"script":"CONTEXT Library IN ENGLISH\nRELATION owner[Book*Person]\nRULE r : owner;owner |- owner\nENDCONTEXT"}'
```

The script composes `owner` with itself, which does not type-check, so the service answers:

```json
{
  "ok": false,
  "diagnostics": [
    {
      "severity": "error",
      "file": "/tmp/ampersand-serve-1443903077053229793/script.adl",
      "line": 3,
      "col": 15,
      "endLine": 3,
      "endCol": 15,
      "message": [
        "/tmp/ampersand-serve-1443903077053229793/script.adl:3:15 error:",
        "  Cannot match the signatures on the left and right of the composition.",
        "    The target of owner, which is Person, should match the source of owner, which is Book."
      ]
    }
  ]
}
```

So, an answer has a field `ok`, which is `true` when the compiler found no errors,
and a list `diagnostics` with what it found.
A warning appears in `diagnostics` with severity `warning` and leaves `ok` at `true`.

## The questions

| Request | Body | Answer |
| --- | --- | --- |
| `GET /health` | none | `{"status":"ok"}` as long as the service runs. |
| `POST /check` | `{"script": text}` | Whether the script parses and type-checks, with the diagnostics. |
| `POST /translate` | `{"script": text, "term": text}` | Whether the term type-checks in the context of the script, with the diagnostics and the term itself. |
| `POST /fspec` | `{"dump": text}` | Whether the dump is a valid Atlas population, with the diagnostics. |
| `POST /import` | `{"dump": text}` | The Ampersand script that the Atlas population describes, in the field `adl`. |
| `POST /population` | `{"script": text, "name": text}` | The population of the script in terms of FormalAmpersand. The field `name` is optional. |

A dump is the JSON text of an Atlas population, passed as one string.
The Atlas is the part of RAP that shows a script as a population of FormalAmpersand,
the metamodel of Ampersand.

### /translate

The purpose of `/translate` is to check a term while a user is typing it,
before it is part of the script.
The service appends a rule to the script that uses the term on both sides,
and type-checks the result.
Two consequences follow for the caller.
A diagnostic about the term carries a line number in the extended script,
which is a line the user never wrote.
And an error in the term may be reported twice, once for each side of the rule.

### /population

The answer of `/population` is the JSON document that the command `ampersand population --build-recipe Grind --output-format json` writes to a file: an object with the lists `atoms` and `links`.
When the script contains errors, the answer is `{"ok": false, "diagnostics": [...]}` instead.

The compiler records where each element of a script is defined,
and it uses that origin in the names it generates.
For instance, a relation `owner` with the property `UNI` yields an atom called `PropertyRule for UNI_owner[Book*Person] which is defined at /tmp/ampersand-serve-8507833470083743211/script.adl:2:1`.
So, the population depends on the file name and on the directory in which the service keeps the script.
The service derives that directory from the content of the script,
so the same script yields the same population on every request to the same service.
A caller that passes `"name": "Library.adl"` gets `Library.adl` in those names instead of `script.adl`.
The service uses only the last part of the name,
so a name cannot point outside the directory of the request.

## What a caller can rely on

The status code tells whether the service understood the request,
and the field `ok` tells what the compiler found.
A script with errors is therefore answered with status 200 and `"ok": false`.
Status 400 means that the body is not the JSON object that the question expects,
and status 404 that the path or the method is unknown.

The diagnostics have two shapes.
The questions `/check`, `/translate` and `/fspec` answer with structured diagnostics,
as in the example above.
The questions `/import` and `/population` answer with a list of strings,
each holding one error message as the compiler prints it.

The service keeps each script in a directory of its own under the temporary directory of the system, and removes that directory after the request.
The path of that directory appears in the `file` field and in the messages of the diagnostics.
Two equal requests share a directory, so the second one waits until the first one has finished.

## Where to run it

The service has no authentication, and a script can name any file with an `INCLUDE` statement.
The compiler then reads that file from the machine on which the service runs,
and reports in a diagnostic what it found there.
So, run the service on a network that only its callers can reach,
in a container that holds nothing but the compiler.
