# Do's and Dont's in Ampersand documentation

## Do's and don'ts

## Every commit in the origin/master branch is built.

If you prefer the web editor, know that each time the master branch is committed, the book is generated. While we are in draft, it is recommended to use a separate branch to do the writing. This prevents too many commits in the github repo master, as each time you save your work in the editor, a commit is done. If you use the [desktop client](https://www.gitbook.com/editor/), this is far less a problem, because you only synchronize when you made some more changes.

## A reference can only be made to an article.

As far as I know, a reference can only be made to something written down in an article, i.e. a separate file. Therefore, it is a good habit to focus on exactly one thing you want to explain/write about in every file.

## Referencing is done to relative locations.

If you want to refer to another article, that can be done by using a relative path. Like I have done with this link to [Getting started with gitbook.](getting-started-with-gitbook.md) It is probably a good habit to check that the links work, by checking the book as it is generated.

