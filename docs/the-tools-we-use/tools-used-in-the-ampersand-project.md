---
description: >-
  This page lists the tools that are used and for which purpose they are used.
  This list is intended for reference, so it is full of hyperlinks that can
  point you to the right location.
---

# Tools used in the Ampersand project

## Specific tools used in the Ampersand project

| Tool                  | Purpose          | 
|-----------------------|-----------------------------|
| [Ampersand image](https://hub.docker.com/r/ampersandtarski/ampersand-prototype/) | A docker-image in which we keep the  latest version of the Ampersand compiler in executable form. It resides in [docker-hub](https://hub.docker.com/r/ampersandtarski/ampersand/). 
| Ampersand compiler    | An executable that is used to generate software, documentation, and analyses from Ampersand-scripts. It is embedded into the Ampersand image and into RAP3. There exist multiple instances, so we cannot hyperlink to it.     
| [Ampersand repository](https://github.com/AmpersandTarski/Ampersand/) | A repositoryin which we keep Ampersand source codeand manage the issues wrt Ampersand. 
| [Docker Hub](https://hub.docker.com/u/ampersandtarski/) | A repository in which  we keep [Ampersand images](../the-tools-we-use/making-docker-images.md)
| [Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design) | A course at the Open Universiteit.
| [GitHub](https://github.com/AmpersandTarski/) | An organisation in which we keep all Ampersand git-repos.
| [RAP3](http://ampersand.tarski.nl/RAP3/)   | A [web-based application, generated with Ampersand](functionality-of-rap3/) for the public at large to use Ampersand. This instance is also used as acceptation environment for [the RAP instance for students](http://rap.cs.ou.nl/RAP3) in the course [Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design).
| [RAP](http://rap.cs.ou.nl) | A [web-based application, generated with Ampersand](functionality-of-rap3/) that is used as production environment for students in the course [Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design).


## Generic tools used in the Ampersand project

| Tool                  | Purpose              
|-----------------------|----------------------|
| [ACE](https://ace.c9.io/) | A web-editor that is used within RAP. It has been integrated in the RAP-application code.
| [Docker](https://www.docker.com/)          | The platform for technology agnostic, fully automated deployment of services, which we use to deploy the Ampersand compiler and applications developed in Ampersand.        
| [Docker-compose](https://docs.docker.com/compose/) | A platform for configuring services in a full-fledged application such as RAP.
| [Git](https://git-scm.com/community) | The versioning system in which all source code is kept. It enables us to work collaboratively on multiple features in parallel, without interfering each other\'s work. 
| [Graphviz](https://www.graphviz.org/)  | A system for generating diagrams, which is used to generate conceptual models and data models as part of the documentation generator.           
| [Haskell](https://www.haskell.org/) | The programming language in which the Ampersand compiler is built and maintained.
| [Kubernetes](https://kubernetes.io/) | A platform for configuring and managing deployed services (such as RAP) in an operational environment. Kubernetes is not being used yet in production instances of Ampersand.       
| [MariaDB](https://mariadb.org/) | The database that is used by Ampersand for persistency. This used to be MySQL, until MariaDB took over in the Open Source community.
| [Markdown](https://www.markdownguide.org/) | The language in which Ampersand documentation is written. We use it for maximal portability of text over different platforms.
| [Node.js](https://nodejs.org/) | The JavaScript framework in which an Ampersand Prototype is generated.
| [Pandoc](https://pandoc.org/)  | A system for document translation and markup, which we use to create a host of different document formats.
| [Stack](https://www.haskellstack.org/)     | A build automation environment for Haskell that we use to build the Ampersand compiler with. It guarantees consistency of module dependencies within the Haskell world.
| [VS-code](https://code.visualstudio.com/)  | An editor for development of the Ampersand compiler and  Ampersand-projects. A VScode extension for Ampersand exists that offers syntax coloring and other language specific features. 

