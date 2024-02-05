---
description: This page describes tooling and workflow in relation to our VSCode extension
---

# Ampersand dev tools

Ampersand has various tools that help development of ampersand models.

## Ampersand language support

There is a VSCode extension named Ampersand language support (ampersandtarski.language-ampersand), which adds support for creating ampersand models.

### Settings

The extension has the following settings.

#### Main script name

When using one of the generate commands, this file is used as an entrypoint for the ampersand compiler. This file needs to located inside the project folder.

**Default**: main.adl

#### Project folder name

When using one of the generate commands, this is the folder that is used to pass on to the ampersand compiler.

**Default**: project

### Commands

Listed below are the commands made available by this extension. For these commands to work you also need to have ampersand in PATH.

#### Ampersand Version

Will show you which version of ampersand you have.

#### Ampersand Daemon

Starts the ampersand daemon. This tool will check the files listed in `.ampersand` for syntax errors.

#### Ampersand Generate Functional Specifications

This will take the project folder and create a word document with the functional specification of the model.

#### Ampersand Generate Atlas

As of this writing, this will generate a json file which describes the relations within the model.

#### Ampersand Generate Protoype

This command generates a Kubernetes manifest file containing a deployment and service. It will then try to apply this file and port-forward the service. To apply the file, a running Kubernetes cluster and kubectl are required. Once port-forwarded, the prototype is available at localhost:8000.

As we start fiddling around with getting a first version of VSCode extension, it turns out all kind of tooling needs to be in place. The vision of the extension is that it will eventually give ampersand modellers a rich integrated development environment, including syntax colouring, prettyprint, syntax supported editing, script execution etc etc.

At first, I had a look at [language-haskell](https://github.com/JustusAdam/language-haskell). They have some IDE support in place, so it seems a reasonable first start. I don't like to invent the wheel myself.

### [npm](https://docs.npmjs.com/)

There is a lot of javascript going on, so we need a package manager for it.

### Releasing of this vs-code extension

We have a release pipeline in place using travis-ci. See the travis.yml file in the root directory. Documentation of extentions can be found [here](https://code.visualstudio.com/api/working-with-extensions/publishing-extension).

The personal access token in use has an expiry date on it. An email is sent to the owner of that token \(currently Han\), in advance of the expiry. It can be renewed or the expire date [can be extended](https://dev.azure.com/hanjoostenhan/_usersSettings/tokens). 
