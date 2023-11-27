# Best practices For Ampersand Modellers

## Use `PURPOSE`-statements abundantly

Curious people want to know the purpose of everything.
Give purpose and people will make sense of what you do.

For this reason, we tell Ampersand users to provide as many `PURPOSE`-statements as possible, preferrably one for every definition in their script.

The purpose of a `PURPOSE`-statement is to explain why something exists. The Ampersand compiler uses `PURPOSE`-statements only for generating documentation.
It has no consequence in the generated prototype.

The ideal moment for writing a `PURPOSE`-statement is now, i.e. when you are in the flow of writing Ampersand statements.
At that moment you have your reasons in the top of your mind.

## Distinguish between requirements and specifications

Managing requirements can be difficult when different stakeholders have different concerns, individual users have strong opinions of their own, requirements are changing over time, or users are unable to articulate requirements when you most need it.

Now, let us assume that hurdle is taken and you have an approved list of requirements. How does this differ from specifications? In Ampersand, you state things as they will be built. A statement in Ampersand specifies your information system precisely. If you generate a system directly from your Ampersand script, the resulting system will help users to keep all rules satisfied. If software engineers build the system to specification, they must guarantee nothing less.

Requirements tell what users say they want. Specifications define unambiguously what to build. So write prose in your requirements and write Ampersand as your specification.

The pitfall is to mistakenly use the list of requirements as specifications. This is not uncommon, so be warned. Well-known consequences are confusion among software engineers, scope creep, and project overruns. Failure to make this distinction is an early warning for project failure.

A working Ampersand prototype and an understandable specification document are compelling evidence of the completeness of your specification. Especially if both have been generated from the same Ampersand script. Fortunately, requirements articulation takes a while so in many cases you can have your specification done by the time the list of requirements is approved. This means you can make the specifications as requirements are developing, not causing any delays other than needed for the requirements.

## Work in pairs

Making a good Ampersand script is difficult. Working in pairs increases your speed for a number of reasons:

* you learn from each other. Switch pairs every now and then, if possible.
* Developing thoughts works better and faster if you work together. Develop thoughts by discussing, challenging, and trying things in practice.

## Change your ontology frequently

An ontology consists of relations and rules. It serves to codify language. Don't hesitate to change it as insights develop.

If you have a history in database engineering, you might see the resemblance between the ontology (in Ampersand) and a data model.
Don't let this trick you into hesitating to change the ontology.
If relations change, you might need to change some rules and interfaces.
The type checker will guide you through the process.

## Iterate and test frequently

Start with the tiniest thing that works. As you expand it, see to it that it stays working. Taking tiny steps, compile and run each one before taking the next step. Keep an eye on your data model as your model grows. And run a diagnosis every now and then.

## Orthogonal design

Work towards things that work in every situation without exception. Leave out frills and fringe, because they will hinder future reuse. Do not hesitate to start over \(refactor\), because your design usually gets better.

## Concern with maintenance

Consider each choice and every decision you make carefully from a maintainer's perspective. Will your code be adaptable in five years' time, when you, your team, and all other stakeholders have been replaced by others?

## Careful with generalization

More general solutions are more widely applicable, but also more abstract. More specific solutions are easier to understand because they are more concrete. Choose the right abstraction level, keeping your audience in mind.

## Avoid quick fixes

Doing things right the first time pays off.

## Choose meaningful names

Names should remind the reader of the intended meaning. For that to happen, describe meanings concisely and precisely and choose short names that will remind readers of that meaning.

## Avoid rules without a purpose

Rules without purpose may block transactions without proper cause, frustrating your user. Or they may produce avoidable work for them. With rules, less is better. Focus on rules that are strictly necessary.

