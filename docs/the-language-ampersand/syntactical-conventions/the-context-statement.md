# The CONTEXT statement

RJ/20161220: the CONTEXT statement is currently being revised. It is expected that this will lead to changes in syntax in the course of 2017.

## Purpose

The data contained in a business system represents a view of \(a very small part of\) the real world. Ideally, this view must be consistent, meaning that there may be no contradictions within that view. Since different business systems have different ways of viewing the real world, and/or look at different parts of the real world, we need to be able to distinguish between such views. We use the term 'Context' to refer to an individual view. Thus, a Context is defined in terms of concepts, relations and rules, and it consists of atoms and links to populate them.

## Semantics

Any Ampersand model has one context.  
The model is true within its context and there is no knowledge in a model about other contexts.

## Syntax

The model is specified between the keywords CONTEXT and ENDCONTEXT. A context has a name.

```text
CONTEXT MyModel
INCLUDE*

<all kind of elements in the model>

ENDCONTEXT
```

Other models included with the INCLUDE statement become part of the context they are included in.

### Optional parts

#### Language definition

To tell Ampersand what language your context is in, you can append a language directive to your context. Currently English and Dutch are supported. To do so, directly following the name of your context, you can specify

```text
IN <language>
```

Where  can be `ENGLISH` or `DUTCH`.

#### Markup format

Directly following the optional language definition, you can optionally specify the format of your texts \(see PURPOSE statement\). Ampersand understands some different markup styles. The default style is REST \(Restructured Text\)

```text
<markupStyle>
```

where  can be one of

`REST`,

`HTML`,

`LATEX`,

`MARKDOWN`.

\(For details on these formats, see [pandoc.org](http://pandoc.org/)\).

