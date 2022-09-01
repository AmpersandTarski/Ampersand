# Residual operators

[Residual operators](https://en.wikipedia.org/wiki/Residuated_Boolean_algebra) are used when "[material implication](https://en.wikipedia.org/wiki/Material_implication_%28rule_of_inference%29)" is involved.

* right residual : $$a\ (r\backslash s)\ b\ \Leftrightarrow\ \forall x: x\ r\ a\rightarrow x\ s\ b$$ . In other words:  $$(a,b)$$ is in the right residual of $$r$$ and $$s$$ means that for every $$x$$, pair $$(x,a)$$ is in relation $$r$$ implies that pair $$(x,b)$$ is in $$s$$.
* left residual : $$a\ (s/r)\ b\ \Leftrightarrow\ \forall x: b\ r\ x\rightarrow a\ s\ x$$ . In words: $$(a,b)$$ is in the left residual of $$s$$ and $$r$$

  means that for every $$x$$ pair$$(b,x)$$ is in relation $$r$$ implies that pair $$(a,x)$$ is in $$s$$.

* diamond: $$a (r♢s) b\ \Leftrightarrow\ \forall x: a\ r\ x\ =\ x\ s\ b$$. In words: For every $$x$$, both $$a\ r\ x$$ and $$x\ s\ b$$ are true or both are false.

## How to type boolean operators in your script

[This page](../#notation-on-the-keyboard) shows how you can type boolean \(and other\) operators in your Ampersand script.

## Other explanation

Would you like a different explanation of the residual operators? [This page](../semantics-in-natural-language/residual-operators.md) explains them in natural language. [Click here](../semantics-visualized/semantics-visualized.md) for visualized examples about residual operators.

