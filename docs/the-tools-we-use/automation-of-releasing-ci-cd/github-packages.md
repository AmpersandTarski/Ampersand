# Github packages

## Querying packages using Github's GraphQL API

* The query below returns a list of all packages and package versions pushed to github repo of Ampersand
* The packages api is still in development, therefore you must include a HTTP Accept header to indicate you want to use this feature
* Make sure the right access right are set for your personal access token. Inluding read repo + read/write packages

  > `POST https://api.github.com/graphql`

headers: &gt;

```text
Content-Type: application/json
Authorization: bearer [put your personal access token here]
Accept: application/vnd.github.packages-preview+json
```

body \(grapql query\):

> ```javascript
> {
>     "query": "query {
>         viewer { login }
>         repository(name: \"ampersand\", owner: \"ampersandtarski\") {
>             id
>             packages (first: 10) {
>                 nodes {
>                     id
>                     name
>                     versions (first: 100) {
>                         nodes {
>                             id version
>                         }
>                     }
>                 }
>             }
>         }
>     }"
> }
> ```

## Deleting specific packages

**NOTE! Doesn't work with public packages, like we have**

> `POST https://api.github.com/graphql`

headers: &gt;

```text
Content-Type: application/json
Authorization: bearer [put your personal access token here]
Accept: application/vnd.github.package-deletes-preview+json
```

body \(graphql query\) &gt;

```javascript
{ "query" : "mutation { deletePackageVersion(input:{packageVersionId:\"[package-version-id]==\"}) { success }}" }
```

