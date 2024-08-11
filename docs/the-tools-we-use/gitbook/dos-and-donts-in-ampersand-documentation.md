# Do's and Dont's in Ampersand documentation

## Do's and don'ts

## Don't mess up the left sidebars
If you edit the file `sidebar.js`, you had better know what you do. This is very error-prone and will break the build of the documentation.
The right sidebars are generated from the document. It follows the structure of headers, starting with the headers at level 2 (two hashes at the beginning of a line).

## Referencing is done to relative locations.

If you want to refer to another article, that can be done by using a relative path. This makes moving the text to another location easier.

## Test before publishing
You can deploy the documentation locally. This is a good idea to do before you push your changes to the docs dicrectory, so you can prevent your mistakes from being deployed. For local testing, deploy as usual by running `docker compose up -d --build` in the root of the repository `Ampersandtarski.github.io`, except that you must use `Dockerfile.testlocal` instead of the default `Dockerfile`. (you can temporarily edit `docker-compose.yml` for that purpose) Then navigate to `http://localhost` in your browser.
