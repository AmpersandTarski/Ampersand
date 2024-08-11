# Frequently Asked Questions

This page tries to help you by anticipating on the questions you might have.
The more questions you ask us, the better the chance that this question will end up in this page...
So don't hesitate to ask.

Some questions in this page do not have an answer yet. Please consider that as ongoing work and bear with us...

##  Documentation
* How can I contribute to the documentation of Ampersand?

  *Answer*

##  Maintenance
* How can I inspect the database underneath an Ampersand application?

  *Answer*: You can deploy PhpMyAdmin alongside your Ampersand application, to inspect the database underneath your Ampersand application. Do a full-text search to `PhpMyAdmin` in the [Ampersand documentation](https://ampersandtarski.github.io) to find out how to do this.

## Deployment
* [How can I deploy an Ampersand application on my laptop?](installing-ampersand.md)
* [How can I deploy an Ampersand application on a virual machine (i.e. a server)](installing-ampersand.md)
* How can I deploy an Ampersand application on Kubernetes?

##  Data
* How can I get bulk data into an Ampersand application at runtime?

   You can [import data from Excel files](../the-excel-importer.md) by calling the Excel Import Extension from the menu bar in your Ampersand application. The importer is part of the standard runtime environment, so you can use it without any specific configuration.

##  Learning
* Where can I learn about Ampersand?

##  Contributing to Ampersand
* How can I change the documentation of Ampersand?

##  General interest
* Why is "declarative" a useful property?

  *Answer*: Ampersand is a [declarative](../conceptual/why-declarative.md) language, to enable mathematical reasoning with the terms in the language.

* How can I monitor an Ampersand application in production?

  *Answer*: RAP is an Ampersand application, which is monitored by Grafana and Prometheus on a Kubernetes cluster. You can take it as an example and copy the setup from the manifest files in the [RAP repository](https://github.com/AmpersandTarski/RAP).