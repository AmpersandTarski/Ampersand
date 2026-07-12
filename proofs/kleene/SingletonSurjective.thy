theory SingletonSurjective
  imports Main
begin

(* ------------------------------------------------------------------ *)
(* Background: the compiler bug in Ampersand.Classes.Relational.       *)
(*                                                                     *)
(* isTotSur claimed that a singleton EMp1 a c (in Ampersand: "a"[c],   *)
(* the relation {(a,a)} of type [c*c]) is BOTH total and surjective:   *)
(*     EMp1 {} -> True                                                 *)
(* Via isTrue (ECps (l,r)) | isSur r -> isTrue l, this made            *)
(*     isTrue (V ; "a")  ==  isTrue V  ==  True,                       *)
(* i.e. the normaliser believed  V[A*B];"a" = V[A*B].  Then            *)
(*     -(V;"a") \/ s  =  -V \/ s  =  s,                                *)
(* so the restriction ";a" was silently dropped.                       *)
(*                                                                     *)
(* Dictionary Ampersand <-> Isabelle/HOL:                              *)
(*   r ; s        = r O s          (relcomp)                           *)
(*   V[A*B]       = A \<times> B          (full relation between carriers)      *)
(*   "a"[C*C]     = {(a,a)}         (a singleton, EMp1)                 *)
(*   r surjective onto C : every c in C is hit by r                    *)
(*   r total on A        : every a in A has an image under r           *)
(* ------------------------------------------------------------------ *)

definition surj_on :: "'b set \<Rightarrow> ('a \<times> 'b) set \<Rightarrow> bool" where
  "surj_on C r \<longleftrightarrow> (\<forall>c\<in>C. \<exists>a. (a,c) \<in> r)"

definition total_on' :: "'a set \<Rightarrow> ('a \<times> 'b) set \<Rightarrow> bool" where
  "total_on' A r \<longleftrightarrow> (\<forall>a\<in>A. \<exists>b. (a,b) \<in> r)"

(* ================================================================== *)
(* PART 1 - the OLD rule is WRONG: a singleton need not be surjective  *)
(*          nor total.  We prove the counterexamples constructively.   *)
(* ================================================================== *)

(* nitpick immediately refutes the universally-quantified claim. *)
lemma singleton_surj_is_false:
  "\<not> (\<forall>(C::nat set) (a::nat). surj_on C {(a,a)})"
proof
  assume "\<forall>(C::nat set) (a::nat). surj_on C {(a,a)}"
  hence "surj_on {0,1::nat} {((0::nat),0)}" by blast
  thus False unfolding surj_on_def by auto
qed

(* Concrete witness: on the two-element carrier {0,1} the singleton
   {(0,0)} is NOT surjective (1 is never hit). *)
lemma singleton_not_surj_witness:
  "\<not> surj_on {0,1::nat} {((0::nat),0)}"
  unfolding surj_on_def by auto

lemma singleton_not_total_witness:
  "\<not> total_on' {0,1::nat} {((0::nat),0)}"
  unfolding total_on'_def by auto

(* The concrete consequence the compiler relied on is also false:
   V ; singleton  is NOT equal to V in general. *)
lemma V_comp_singleton_not_V:
  fixes a b :: 'a
  assumes ab: "a \<noteq> b"
  shows "(({a} \<times> {a,b}) O {(a,a)}) \<noteq> {a} \<times> {a,b}"
proof
  assume eq: "(({a} \<times> {a,b}) O {(a,a)}) = {a} \<times> {a,b}"
  have "(a,b) \<in> {a} \<times> {a,b}" by simp
  with eq have "(a,b) \<in> (({a} \<times> {a,b}) O {(a,a)})" by simp
  then obtain c where "(c,b) \<in> {(a,a)}" by (auto simp: relcomp_unfold)
  hence "b = a" by simp
  with ab show False by simp
qed

(* ================================================================== *)
(* PART 2 - the properties the compiler DOES keep for a singleton are  *)
(*          sound: {(a,a)} is univalent, injective, symmetric,         *)
(*          antisymmetric and transitive.                              *)
(* ================================================================== *)

lemma singleton_univalent: "\<forall>x y z. (x,y) \<in> {(a,a)} \<and> (x,z) \<in> {(a,a)} \<longrightarrow> y = z"
  by auto

lemma singleton_injective: "\<forall>x y z. (x,z) \<in> {(a,a)} \<and> (y,z) \<in> {(a,a)} \<longrightarrow> x = y"
  by auto

lemma singleton_symmetric: "sym {(a,a)}"
  by (auto intro: symI)

lemma singleton_antisymmetric: "antisym {(a,a)}"
  by (auto intro: antisymI)

lemma singleton_transitive: "trans {(a,a)}"
  by (auto intro: transI)

(* ================================================================== *)
(* PART 3 - the sound law the fix PRESERVES:                           *)
(*          if r really is surjective onto C, then V;r = V.            *)
(*   This is the isTrue (ECps (l,r)) | isSur r -> isTrue l branch;     *)
(*   it stays valid, we only stop applying it to non-surjective        *)
(*   singletons.                                                       *)
(* ================================================================== *)

lemma V_comp_surj_is_V:
  assumes sub: "r \<subseteq> B \<times> C"
      and sur: "surj_on C r"
  shows "(A \<times> B) O r = A \<times> C"
proof
  show "(A \<times> B) O r \<subseteq> A \<times> C"
    using sub by (auto simp: relcomp_unfold)
next
  show "A \<times> C \<subseteq> (A \<times> B) O r"
  proof
    fix p assume "p \<in> A \<times> C"
    then obtain a c where p: "p = (a,c)" and a: "a \<in> A" and c: "c \<in> C" by auto
    from c sur obtain b where "(b,c) \<in> r" unfolding surj_on_def by blast
    with sub have "b \<in> B" by auto
    with a \<open>(b,c) \<in> r\<close> p show "p \<in> (A \<times> B) O r" by (auto simp: relcomp_unfold)
  qed
qed

(* Sanity: with the corrected under-approximation, when we CANNOT derive
   surjectivity of r we do not conclude V;r = V, so no restriction is lost. *)

end
