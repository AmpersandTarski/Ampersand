---
description: >-
  If you want to test your application on your own laptop, you need to configure
  localhost to let it behave like a regular top-level domain.
---
# Testing with Docker on your own laptop
## Testing with Docker on your own laptop

### Requirement

The proper use of Docker ensures that your application will run on any location on the internet. Yet, when testing your application on your laptop, you may discover that your laptop is not configured as a domain on the internet. To run any Ampersand application locally without changes, your laptop must behave like an ordinary top-level domain: `localhost`. Your computer must believe that `localhost` is like `com`, or `edu`, or `nl`. All requests from your browser(s) to `localhost` must be routed to IP-address `127.0.0.1`, which is your laptop.

### The desired result

:::tip

When the following experiments are successful on your computer, you may consider this requirement to be fulfilled

:::

First demonstrate that an arbitrary internet domain (here: `nu.nl`) is visible.

`> ping -c 1 nu.nl`\
`PING nu.nl (99.86.122.115): 56 data bytes 64 bytes from 99.86.122.115: icmp_seq=0 ttl=232 time=24.367 ms`\
`--- nu.nl ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 24.367/24.367/24.367/0.000 ms`

Now demonstrate that `localhost` is routed to IP-address `127.0.0.1`.

`> ping -c 1 localhost`\
`PING localhost (127.0.0.1): 56 data bytes 64 bytes from 127.0.0.1: icmp_seq=0 ttl=64 time=0.040 ms`\
`--- localhost ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 0.040/0.040/0.040/0.000 ms`

Then demonstrate that an arbitrary subdomain of `localhost` is routed to IP-address `127.0.0.1`.

`> ping -c 1 rap.localhost`\
`PING rap.localhost (127.0.0.1): 56 data bytes 64 bytes from 127.0.0.1: icmp_seq=0 ttl=64 time=0.029 ms`\
`--- rap.localhost ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 0.029/0.029/0.029/0.000 ms`

Finally, show that this works recursively on an arbitrary sub-subdomain.

`> ping -c 1 foo.rap.localhost`\
`PING foo.rap.localhost (127.0.0.1): 56 data bytes 64 bytes from 127.0.0.1: icmp_seq=0 ttl=64 time=0.032 ms`\
`--- foo.rap.localhost ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 0.032/0.032/0.032/0.000 ms`

### Complications

So why should this NOT work?

1. Your machine might not be connected to the internet. And if it is connected, it needs access to a domain name server to find locations on the internet. For most laptops this is the case, so you have no problems here. Some corporate laptops are connected only to the local corporate network. This means that you cannot let your application communicate with the outside world, or you have to organize that your laptop gains access (e.g. through a proxy).
2. You cannot run your application locally if `localhost` is not configured. On some laptops there is a file called `hosts` or `.hosts`, which contains the redirection from `localhost` to IP-address `127.0.0.1`. However, this redirection does not cater for subdomains. Consider this to partially fulfill the requirement, which is sufficient if your application works without subdomains.
3. If your application works with subdomains, `localhost` must behave like a top-level domain. For this purpose you must install and configure a domain name server on your laptop (which is typically not there).

### Solutions

- Turn to [http://passingcuriosity.com/2013/dnsmasq-dev-osx/](http://passingcuriosity.com/2013/dnsmasq-dev-osx/) to install a local DNS-server on your Macbook.
- Turn to ??? to install a local DNS-server on your Windows laptop.
- Turn to [https://support.microsoft.com/nl-nl/help/972034/how-to-reset-the-hosts-file-back-to-the-default](https://support.microsoft.com/nl-nl/help/972034/how-to-reset-the-hosts-file-back-to-the-default) for setting the hosts-file on your Windows laptop. (Don't do this if you are installing a domain name server)
- Turn to [https://www.techjunkie.com/edit-hosts-file-mac-os-x/](https://www.techjunkie.com/edit-hosts-file-mac-os-x/) for setting the hosts-file on your Macbook. (Don't do this if you are installing a domain name server)

### Workarounds

If you cannot get your laptop configured, you might test your application on a separate test-server that runs Linux.

<p>&nbsp;</p>
<p>&nbsp;</p>

## Testing with SpecFlow

### Introduction
This piece of documentation explains what you need to do to extend or modify the existing SpecFlow tests for RAP4. 
First, there is an explanation of what you need to install, followed by an explanation of the SpecFlow 
tests themselves. This document is set up in August '24.

### Getting Started with SpecFlow
The existing tests are created with `VS2022` and `Docker Desktop` (version 4.25.0). It is also possible to 
work with `Visual Studio Code`, but this documentation does not cover that. 
In VS2022, you need support for C# and the following NuGet packages:
 - SpecFlow
 - SpecFlow.NUnit
 - FluentAssertions
 - Microsoft.Extensions.DependencyInjection
 - Microsoft.NET.Test.Sdk
 - nunit
 - NUnit3TestAdapter
 - SpecFlow.Plus.LivingDocPlugin
 - Newtonsoft.Json

Next, Docker Desktop must be installed. Then, RAP must be run in Docker Desktop. To get RAP4 up and running you can look [here](rap/deployment-guide.md) for the 
explanation in the documentation.

Here you will also find the explanation of where to find a clone of the RAP repository.

Once RAP is running in Docker Desktop, you can load `SpecFlowRAP.sln` in Visual Studio 
and run all the tests with `Ctrl+R, V`.

### RAP SpecFlow Test Files
The specFlow-solution can be found in the RAP4-repository in `\RAP\Testing\SpecFlow\SpecFlowRAP`.
SpecFlow works with `*.feature` and `*StepDefinition` files. Currently, there are 3 features developed: 
MyAccount, Logout_login and Atlas.

Additionally, the Data folder contains all the definitions of the data classes. Here you will find 
the data structure of the responses to the various API calls.

There are also some general files that include code for composing requests, creating the URL, etc.

### Control Flow of the SpecFlow Tests
The different features are generally executed in a random order. For this reason, before executing 
each feature, the application is reinstalled, an account is created, and a script is loaded 
(loading a script is only necessary for the Atlas tests). This happens in the `[BeforeFeature]` hook in `Hooks.cs`.

To compose the API calls for the different tests, various items are needed that must be extracted 
from the responses of previous API calls. This information (id and labels) is stored in the `_featureContext` (a dictionary 
with key-value pairs). For example, the `PHPsessid` is extracted from the response in 
`GivenINeedASessionIdOfRAP()` and later used in `GivenINeedToSeeTheRegisterButton()`.

### Exploring RAP4 with inspect-window

To expand the existing tests with new features, it will be necessary to identify the URL of an API call and examine the content of the response. 
The following steps will help you get started quickly.

- Start Docker (or Kubernetes) and check that RAP4 is running in a container
- Start RAP4 in a browser via: `http://localhost/#/page/home`
- Watch the video or click `Login` and then `Register` to create an account
- Load the script `NewScript.adl` in `\RAP\Testing\SpecFlow\SpecFlowRAP\scripts` and compile the script
- Go to inspect via `Ctrl+right mouse click`
- Select `Network` and `Fetch/XHR`
- If you now click anywhere in the left screen, e.g. `Atlas`, you will see `Atlas` also appear under `Name` in the 
inspect screen. Click on it and then on `Headers`, where you will find the URL used, 
which you should also use in the SpecFlow test.
- If you click `Response` you will find the complete response from the API-call.
