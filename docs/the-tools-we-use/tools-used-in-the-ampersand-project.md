---
description: >-
  This page lists the tools that are used and for which purpose they are used.
  This list is intended for reference, so it is full of hyperlinks that can
  point you to the right location.
---

# Tools used in the Ampersand project

## Specific tools used in the Ampersand project

+-----------------------+-----------------------+-----------------------+
| Tool                  | Purpose (the          | Knowledge holder      |
|                       | hyperlink navigates   |                       |
|                       | to the section in     |                       |
|                       | this book)            |                       |
+:======================+:======================+:======================+
| [Ampersand image](https://hub.docker.com/r/ampersandtarski/ampersand-prototype/) | a docker-image in     | all                   |
|                       | which we keep the     |                       |
|                       | latest version of the |                       |
|                       | Ampersand compiler in |                       |
|                       | executable form. It   |                       |
|                       | resides in            |                       |
|                       | [docker-hub](https://hub.docker.com/r/ampersandtarski/ampersand/). |                       |
+-----------------------+-----------------------+-----------------------+
| Ampersand compiler    | An executable that is |                       |
|                       | used to generate      |                       |
|                       | software,             |                       |
|                       | documentation, and    |                       |
|                       | analyses from         |                       |
|                       | Ampersand-scripts. It |                       |
|                       | is embedded into the  |                       |
|                       | Ampersand image and   |                       |
|                       | into RAP3. There      |                       |
|                       | exist multiple        |                       |
|                       | instances, so we      |                       |
|                       | cannot hyperlink to   |                       |
|                       | it.                   |                       |
+-----------------------+-----------------------+-----------------------+
| [Ampersand repository](https://github.com/AmpersandTarski/Ampersand/) | a                     | all                   |
|       | [repository](gitbook/getting-started-with-gitbook.md) |                       |
|                       | in which we keep      |                       |
|                       | Ampersand source code |                       |
|                       | and manage the issues |                       |
|                       | wrt Ampersand. It     |                       |
|                       | resides on            |                       |
|                       | [GitHub](https://github.com/AmpersandTarski/Ampersand). |                       |
+-----------------------+-----------------------+-----------------------+
| [Appveyor](https://ci.appveyor.com/project/hanjoosten/ampersand) | a service that        | [Han](https://github.com/hanjoosten)       |
|  | generates executable  |  |
|  | files for Windows     |                       |
|  | automatically, each   |                       |
|                       | time a new release of |                       |
|                       | Ampersand appears. It |                       |
|                       | [releases the Ampersand compiler](releasing-ampersand-and-workflow-details.md) |                       |
|                       | for Windows automatically, provided        |                       |
|                       | all automated tests have passed. |                       |
+-----------------------+-----------------------+-----------------------+
| [Docker Hub](https://hub.docker.com/u/ampersandtarski/) | a repository in which | [Stef](https://github.com/stefjoosten) |
|                  | we keep [Ampersand images](installation-of-rap/making-docker-images.md) |                       |
+-----------------------+-----------------------+-----------------------+
| [Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design) | a course at the Open Universiteit | [Stef](https://github.com/stefjoosten),      |
|                       |  | [Lloyd](https://github.com/LloydRutledge), |
+-----------------------+-----------------------+-----------------------+
| [GitHub](https://github.com/AmpersandTarski/) | an organisation in which we keep all Ampersand git-repos. | all                   |
+-----------------------+-----------------------+-----------------------+
| [RAP3](http://ampersand.tarski.nl/RAP3/)   | a [web-based application](functionality-of-rap3/)         | [Lloyd](https://github.com/LloydRutledge), |
|                       |  | [Stef](https://github.com/stefjoosten)     |
|                       | for the public at large to use Ampersand.         |                       |
|                       | This instance is also used as acceptation environment for |                       |
|                       | [the RAP instance for students](http://rap.cs.ou.nl/RAP3) |                       |
|                       | in the course [Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design). |                       |
+-----------------------+-----------------------+-----------------------+
| [RAP](http://rap.cs.ou.nl) | a [web-based application](functionality-of-rap3/)         | [Lloyd](https://github.com/LloydRutledge), |
|                       | that is used as production      | [Stef](https://github.com/stefjoosten) |
|                       | environment for students in the course     |      |
|                       | [Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design).   |                       |
+-----------------------+-----------------------+-----------------------+
| [RAP-repo](https://github.com/AmpersandTarski/RAP/)             | a Github repository   | [Stef](https://github.com/stefjoosten)     |
|  | in which we keep the  |  |
|  | [source code of RAP](installation-of-rap/).      |                       |
+-----------------------+-----------------------+-----------------------+
| [Travis](https://travis-ci.org/AmpersandTarski/Ampersand)       | a service, which runs automated tests on | [Han](https://github.com/hanjoosten)       |
|  | every commit of the Ampersand repository  |                       |
| ![](.gitbook/assets/travisci-full-color-1.png)                  | on Github. Only the successfully tested  |                       |
|  | commits on the releases branch are released      |                       |
+-----------------------+-----------------------+-----------------------+

\## Generic tools used in the Ampersand project

+-----------------------+-----------------------+-----------------------+
| Tool                  | Purpose               | Knowledge holder      |
+:======================+:======================+:======================+
| ACE                   | a web-editor that is  | [Michiel](https://github.com/Michiel-s)    |
|                       | used within RAP. It   |                       |
|                       | has been integrated   |                       |
|                       | in the                |                       |
|                       | RAP-application code. |                       |
+-----------------------+-----------------------+-----------------------+
| EditPad               | a text editor that    | [Rieks](https://github.com/RieksJ)         |
|                       | has long been used in |                       |
|                       | the Ampersand         |                       |
|                       | project, but not any  |                       |
|                       | more. A syntax        |                       |
|                       | coloring mechanism    |                       |
|                       | for Ampersand still   |                       |
|                       | exists. This editor   |                       |
|                       | has been superseded   |                       |
|                       | by VScode, because it |                       |
|                       | is being supported by |                       |
|                       | a much larger         |                       |
|                       | community.            |                       |
+-----------------------+-----------------------+-----------------------+
| [Docker](https://www.docker.com/)          | the platform for technology agnostic,  | [Stef](https://github.com/stefjoosten) |
|                       | fully automated       |                       |
|                       | deployment of         |                       |
|                       | services, which we    |                       |
|                       | use to deploy the     |                       |
|                       | Ampersand compiler    |                       |
|                       | and applications      |                       |
|                       | developed in          |                       |
|                       | Ampersand.            |                       |
+-----------------------+-----------------------+-----------------------+
| [Docker-compose](https://docs.docker.com/compose/) | a platform for configuring services       | [Stef](https://github.com/stefjoosten), |
|                       | in a full-fledged application such as RAP.                  |                       |
+-----------------------+-----------------------+-----------------------+
| [Git](https://git-scm.com/community)       | the versioning system | [Han](https://github.com/hanjoosten),      |
|                       | in which all source code is kept. It      | [Rieks](https://github.com/RieksJ),        |
| ![](.gitbook/assets/logo-2x-1.png) | enables us to work collaboratively on multiple features in    | [Michiel](https://github.com/Michiel-s),   |
|                       | parallel, without interfering each other\'s work.        | [Martijn](https://github.com/Oblosys),     |
|                       |                       | [Stef](https://github.com/stefjoosten),    |
|                       |                       | [Sebastiaan](https://github.com/sjcjoosten) |
+-----------------------+-----------------------+-----------------------+
| [Graphviz](https://www.graphviz.org/)  | a system for generating diagrams,         | [Stef](https://github.com/stefjoosten),    |
|                       | which is used to      | [Han](https://github.com/hanjoosten)       |
|                       | generate conceptual   |                       |
|                       | models and data       |                       |
|                       | models as part of the |                       |
|                       | documentation         |                       |
|                       | generator.            |                       |
+-----------------------+-----------------------+-----------------------+
| [Haskell](https://www.haskell.org/) | the programming language in which the      | [Stef](https://github.com/stefjoosten),    |
|                       | Ampersand compiler is built and maintained. | [Sebastiaan](https://github.com/sjcjoosten),                    |
|                       |                       | [Han](https://github.com/hanjoosten) |
+-----------------------+-----------------------+-----------------------+
| [JSON](https://www.json.org/)              | a simple, standardized, format            | [Han](https://github.com/hanjoosten),      |
|                       | for communicating data structures. It    | [Michiel](https://github.com/Michiel-s)    |
|                       | is used to communicate data from the          |                       |
|                       | Ampersand-compiler to the generated |                       |
|                       | application.          |                       |
+-----------------------+-----------------------+-----------------------+
| [Kubernetes](https://kubernetes.io/) | a platform for        | [Stef](https://github.com/stefjoosten)     |
|                       | configuring and       |                       |
|                       | managing deployed     |                       |
|                       | services (such as     |                       |
|                       | RAP) in an            |                       |
|                       | operational           |                       |
|                       | environment.          |                       |
|                       | Kubernetes is not     |                       |
|                       | being used yet in     |                       |
|                       | production instances  |                       |
|                       | of Ampersand.         |                       |
+-----------------------+-----------------------+-----------------------+
| LaTeX                 | a typesetting system, which we use to | [Stef](https://github.com/stefjoosten),    |
|                       |       |  |
|                       | generate PDF\'s with. At the moment | [Han](https://github.com/hanjoosten)       |
|                       | MarkDown is the preferred markup language,     |                       |
|                       | so LaTeX\'s role in the Ampersand |                       |
|                       | project is decreasing.           |                       |
+-----------------------+-----------------------+-----------------------+
| [MariaDB](https://mariadb.org/)            | the database that is used by Ampersand for | [Han](https://github.com/hanjoosten),      |
|                       | persistency. This used to be MySQL,    | [Michiel](https://github.com/Michiel-s),   |
|                       | until MariaDB took over in the Open   | [Stef](https://github.com/stefjoosten),    |
|                       | Source community.     | [Sebastiaan](https://github.com/sjcjoosten) |
+-----------------------+-----------------------+-----------------------+
| [Markdown](https://www.markdownguide.org/) | the language in which | all                   |
|                       | Ampersand documentation is written.      |                       |
|                       | We use it for maximal portability of           |                       |
|                       | text over different platforms.            |                       |
+-----------------------+-----------------------+-----------------------+
| [Node.js](https://nodejs.org/)             | the JavaScript        | [Michiel](https://github.com/Michiel-s)    |
|                       | framework in which an |  |
|                       | Ampersand Prototype is generated.          |                       |
+-----------------------+-----------------------+-----------------------+
| [npm](https://www.npmjs.com/)              | the package manager   | [Michiel](https://github.com/Michiel-s)    |
|                       | for JavaScript, which |  |
|                       | guarantees consistency of module |                       |
|                       | dependencies in the JavaScript world.     |                       |
+-----------------------+-----------------------+-----------------------+
| [Pandoc](https://pandoc.org/)              | a system for document | [Han](https://github.com/hanjoosten)       |
|                       | translation and       |  |
|                       | markup, which we use to create a host of   |                       |
|                       | different document formats               |                       |
+-----------------------+-----------------------+-----------------------+
| [Stack](https://www.haskellstack.org/)     | a build automation environment for    | [Han](https://github.com/hanjoosten),      |
|                       | Haskell that we use to build the  | [Stef](https://github.com/stefjoosten)     |
|                       | Ampersand compiler with. It guarantees   |                       |
|                       | consistency of module dependencies within |                       |
|                       | the Haskell world.    |                       |
+-----------------------+-----------------------+-----------------------+
| [VS-code](https://code.visualstudio.com/)  | an editor for development of the        | [Han](https://github.com/hanjoosten),      |
|                       | Ampersand compiler and  Ampersand-projects. | [Rieks](https://github.com/RieksJ)         |
|                       | A VScode extension for Ampersand exists that |                       |
|                       | offers syntax coloring.             |                       |
+-----------------------+-----------------------+-----------------------+
