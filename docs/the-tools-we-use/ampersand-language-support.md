---
description: This page describes tooling and workflow in relation to our VSCode extention
---

# Ampersand language support

As we start fiddling around with getting a first version of VSCode extention, it turns out all kind of tooling needs to be in place. The vision of the extention is that it will eventually give ampersand modellers a rich integrated development environment, including syntax colouring, prettyprint, syntax supported editing, script execution etc etc. 

At first, I had a look at [language-haskell](https://github.com/JustusAdam/language-haskell). They have some IDE support in place, so it seems a reasonable first start. I don't like to invent the wheel myself.



### [npm](https://docs.npmjs.com/)

There is a lot of javascript going on, so we need a package manager for it. 

### Releasing of this vs-code extension

We have a release pipeline in place using travis-ci. See the travis.yml file in the root directory. Documentation of extentions can be found [here](https://code.visualstudio.com/api/working-with-extensions/publishing-extension).

The personal access token in use has an expiry date on it. An email is sent to the owner of that token \(currently Han\), in advance of the expiry. It can be renewed or the expire date [can be extended](https://dev.azure.com/hanjoostenhan/_usersSettings/tokens). 

