# Installing Ampersand

Ampersand is great for rapid prototyping. We advise you to do that on the web. But sometimes you want to run your prototypes on your own computer\(s\). For different purposes there are different ways of doing that. This chapter shows you how.

## How to edit Ampersand scripts

You can use any text editor to create Ampersand scripts. However, for those that use the [Visual Studio Code \(vscode\)](https://code.visualstudio.com/) editor, there is language support. All you have to do is search for the vscode extension "Ampersand \(ADL\) language support" and install it, and then choose the coloring theme called "Ampersand".

## How to use Ampersand on your own laptop

Using Ampersand offline does not require you to install Ampersand. Ampersand runs in Docker so you can use it independently and on almost any platform. [Here is an explanation of how to do this \(don't mind the title of that page\)](deploying-your-prototype.md). It can be summarized as follows:

1. Make sure Docker runs on your laptop or install it if it doesn't.
2. Copy the files `Dockerfile` and `docker-compose.yml` and adapt them for your own Ampersand prototype. [Read this](deploying-your-prototype.md) if you don't know where to find them.
3. Run your `.adl`-file on the Docker platform.

### How to compile Ampersand programs manually

DIY-engineers will find instructions in section [Installing the tools manually](installing-the-tools-manually.md). You need a webserver that can run javascript, PHP7, the PHP composer, and a \(My\)SQL or MariaDB database server. For generating functional specifications, you might use LaTeX, Markdown, Word .docx and other formats. This chapter gives an overview of the Ampersand production line for whoever needs to circumvent the automated process.

### How to install your own copy of RAP4 on a server of your own choosing

RAP4 is an Ampersand repository, in which multiple users can store and use their Ampersand scripts. Consult [the tools we use at Ampersand](https://ampersandtarski.gitbooks.io/the-tools-we-use-for-ampersand/content/installation_of_rap.html). This is work in progress.

### How to change Ampersand itself

If you want to change the Ampersand compiler for your own purposes, you need access to the source files, and a Haskell development environment. This section still has to be written. It will describe the software process for developing Ampersand itself.

The remainder of this chapter explains in detail all the things you need to get you up and running with Ampersand. The instructions presume that you are familiar with your own computers.

