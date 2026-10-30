theory AnchoredRewrite
  imports Main
begin

(* Machine-checked support for the anchored-complement rewrite
   (Ampersand.ADL1.Expression.anchorComplements, issue #562).

   Dictionary  Ampersand  <->  Isabelle/HOL:
     r /\ s  =  r Int s     intersection
     r \/ s  =  r Un  s     union
     r - s   =  r - s       difference
     -r      =  V - r       complement w.r.t. the closed world V[A*B]
     V[A*B]  =  V           an arbitrary but fixed set of pairs

   The typing invariant of Ampersand's type checker (and of the generated
   database: relation contents lie within the concept tables) is that every
   well-typed term of signature [A*B] denotes a subset of V[A*B]. Each rule
   below therefore assumes containment in V only where it is needed. *)

(* ---- R1 (absorb): an anchor g absorbs a complement into a difference ---- *)

lemma R1_absorb:
  fixes g e V :: "('a \<times> 'b) set"
  assumes anchored: "g \<subseteq> V"
  shows "g \<inter> (V - e) = g - e"
  using anchored by blast

(* ---- R2 (distribute): an anchor distributes over a union ---- *)

lemma R2_distribute:
  fixes g p q :: "('a \<times> 'b) set"
  shows "g \<inter> (p \<union> q) = (g \<inter> p) \<union> (g \<inter> q)"
  by blast

(* ---- R3 (push through difference) ---- *)

lemma R3_push:
  fixes g p q :: "('a \<times> 'b) set"
  shows "g \<inter> (p - q) = (g \<inter> p) - q"
  by blast

(* ---- The difference desugaring used by the SQL generator ----
   selectExpr compiles  l - r  as  l /\ -r  (and maybeSpecialCase turns that
   shape into an anti-join). This is the same identity as R1, read in the
   other direction. *)

lemma dif_is_anchored_complement:
  fixes l r V :: "('a \<times> 'b) set"
  assumes anchored: "l \<subseteq> V"
  shows "l - r = l \<inter> (V - r)"
  using anchored by blast

(* ---- The worked example of issue #562 ----
   x /\ ((-a \/ -b) - c)  =  ((x - a) \/ (x - b)) - c
   i.e. the violation term of the reproducer's ENFORCE rule, rewritten by
   R3, R2 and R1 (in that order), no longer contains a complement. *)

lemma worked_example:
  fixes x a b c V :: "('a \<times> 'b) set"
  assumes anchored: "x \<subseteq> V"
  shows "x \<inter> (((V - a) \<union> (V - b)) - c) = ((x - a) \<union> (x - b)) - c"
proof -
  have "x \<inter> (((V - a) \<union> (V - b)) - c)
      = (x \<inter> ((V - a) \<union> (V - b))) - c" by (rule R3_push)
  also have "\<dots> = ((x \<inter> (V - a)) \<union> (x \<inter> (V - b))) - c"
    by (simp add: R2_distribute)
  also have "\<dots> = ((x - a) \<union> (x - b)) - c"
    by (simp add: R1_absorb[OF anchored])
  finally show ?thesis .
qed

(* ---- Anchor folding ----
   anchorComplements folds one anchor through several needy members in
   sequence: the result of anchoring the first member is used as the anchor
   for the next ((g /\ m1) /\ m2 = g /\ m1 /\ m2). The fold preserves the
   typing invariant, because the anchored result is contained in the anchor. *)

lemma fold_step_sound:
  fixes g m1 m2 :: "('a \<times> 'b) set"
  shows "(g \<inter> m1) \<inter> m2 = g \<inter> (m1 \<inter> m2)"
  by blast

lemma anchored_result_still_anchored:
  fixes g e V :: "('a \<times> 'b) set"
  assumes "g \<subseteq> V"
  shows "g - e \<subseteq> V"
  using assms by blast

end
