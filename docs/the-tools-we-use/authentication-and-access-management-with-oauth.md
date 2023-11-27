# Authentication and access management with OAuth

An Ampersand application may grant access to named users. We have done this before using the [open standard OAuth ](https://oauth.net)\(Open Authorization\). This allows access to specific private data on a different website, without requiring the user to hand over their credentials \(e.g. username/password\).

## Setting up with Github as access provider

I made a separate Github organization, RAP-OUNL, to serve as access provider for RAP4.

callback URL: [http://example.com/AmpersandPrototypes/RAP4/api/v1/oauthlogin/callback/github](http://example.com/AmpersandPrototypes/RAP3/api/v1/oauthlogin/callback/github)

## Experimenting from a laptop

For experimentation purposes, I added the following line to `C:\Windows\System32\drivers\etc` on my Windows 7 laptop.

```text
127.0.0.1 example.com
```

Then I flushed the local name server in a command line window.

```text
C:\> ipconfig /flushdns
```

Then I brought up a browser window, to see that `example.com` is a valid URL. Now I can experiment with OUauth from the laptop, because the OAuth server can return messages to `example.com`.

