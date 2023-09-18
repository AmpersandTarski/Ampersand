---
description: >-
  If you want to test your application on your own laptop, you need to configure
  localhost to let it behave like a regular top-level domain.
---

# Testing with Docker on your own laptop

## Requirement

The proper use of Docker ensures that your application will run on any location on the internet. Yet, when testing your application on your laptop, you may discover that your laptop is not configured as a domain on the internet. To run any Ampersand application locally without changes, your laptop must behave like an ordinary top-level domain: `localhost`. Your computer must believe that `localhost` is like `com`, or `edu`, or `nl`. All requests from your browser(s) to `localhost` must be routed to IP-address `127.0.0.1`,  which is your laptop.

## The desired result

{% hint style="success" %}
When the following experiments are successful on your computer, you may consider this requirement to be fulfilled
{% endhint %}

First demonstrate that an arbitrary internet domain (here: `nu.nl`) is visible.

`> ping -c 1 nu.nl`\
`PING nu.nl (99.86.122.115): 56 data bytes 64 bytes from 99.86.122.115: icmp_seq=0 ttl=232 time=24.367 ms`\
`--- nu.nl ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 24.367/24.367/24.367/0.000 ms`

Now demonstrate that `localhost` is routed to IP-address `127.0.0.1`.&#x20;

`> ping -c 1 localhost`\
`PING localhost (127.0.0.1): 56 data bytes 64 bytes from 127.0.0.1: icmp_seq=0 ttl=64 time=0.040 ms`\
`--- localhost ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 0.040/0.040/0.040/0.000 ms`

Then demonstrate that an arbitrary subdomain of `localhost` is routed to IP-address `127.0.0.1`.&#x20;

`> ping -c 1 rap.localhost`\
`PING rap.localhost (127.0.0.1): 56 data bytes 64 bytes from 127.0.0.1: icmp_seq=0 ttl=64 time=0.029 ms`\
`--- rap.localhost ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 0.029/0.029/0.029/0.000 ms`

Finally, show that this works recursively on an arbitrary sub-subdomain.

`> ping -c 1 foo.rap.localhost`\
`PING foo.rap.localhost (127.0.0.1): 56 data bytes 64 bytes from 127.0.0.1: icmp_seq=0 ttl=64 time=0.032 ms`\
`--- foo.rap.localhost ping statistics --- 1 packets transmitted, 1 packets received, 0.0% packet loss round-trip min/avg/max/stddev = 0.032/0.032/0.032/0.000 ms`

## Complications

So why should this NOT work?

1. Your machine might not be connected to the internet. And if it is connected, it needs access to a domain name server to find locations on the internet. For most laptops this is the case, so you have no problems here. Some corporate laptops are connected only to the local corporate network. This means that you cannot let your application communicate with the outside world, or you have to organize that your laptop gains access (e.g. through a proxy).
2. You cannot run your application locally if `localhost` is not configured. On some laptops there is a file called `hosts` or `.hosts`, which contains the redirection from `localhost` to IP-address `127.0.0.1`. However, this redirection does not cater for subdomains. Consider this to partially fulfill the requirement, which is sufficient if your application works without subdomains.
3. If your application works with subdomains, `localhost` must behave like a top-level domain. For this purpose you must install and configure a domain name server on your laptop (which is typically not there).

## Solutions

* Turn to [http://passingcuriosity.com/2013/dnsmasq-dev-osx/](http://passingcuriosity.com/2013/dnsmasq-dev-osx/) to install a local DNS-server on your Macbook.
* Turn to ??? to install a local DNS-server on your Windows laptop.
* Turn to [https://support.microsoft.com/nl-nl/help/972034/how-to-reset-the-hosts-file-back-to-the-default](https://support.microsoft.com/nl-nl/help/972034/how-to-reset-the-hosts-file-back-to-the-default) for setting the hosts-file on your Windows laptop. (Don't do this if you are installing a domain name server)
* Turn to [https://www.techjunkie.com/edit-hosts-file-mac-os-x/](https://www.techjunkie.com/edit-hosts-file-mac-os-x/) for setting the hosts-file on your Macbook. (Don't do this if you are installing a domain name server)

## Workarounds

If you cannot get your laptop configured, you might test your application on a separate test-server that runs Linux.
