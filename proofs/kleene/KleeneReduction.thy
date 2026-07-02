theory KleeneReduction
  imports Main
begin

(* Dictionary  Ampersand  <->  Isabelle/HOL:
     r ; s   =  r O s     relation composition (relcomp)
     I       =  Id        identity relation
     r+      =  r\<^sup>+       transitive closure (trancl)
     r*      =  r\<^sup>*       reflexive-transitive closure (rtrancl)
     r - s   =  r - s     set difference
     r%      =  red r     transitive reduction, defined below
   In Isabelle  r O s = {(a,c). EX b. (a,b):r & (b,c):s}, matching Ampersand r;s. *)

definition red :: "'a rel \<Rightarrow> 'a rel" where
  "red r = r - r O r\<^sup>+"

(* ---- reusable monotonicity lemmas (checked separately) ---- *)

lemma trancl_mono': "r \<subseteq> s \<Longrightarrow> r\<^sup>+ \<subseteq> s\<^sup>+"
  by (meson subsetI trancl_mono)

lemma trancl_idemp': "(r\<^sup>+)\<^sup>+ = r\<^sup>+"
  by (metis trancl_id trans_trancl)

lemma r_subset_trancl: "r \<subseteq> r\<^sup>+"
proof
  fix p assume "p \<in> r"
  then obtain a b where "p = (a, b)" by (cases p) auto
  with \<open>p \<in> r\<close> show "p \<in> r\<^sup>+" by (simp add: r_into_trancl)
qed

(* ---- PO-1: the Ampersand maintenance equations, proved as lemmas ---- *)

(* rPlus = r \<union> (r ; rPlus) *)
lemma rPlus_def_law: "r\<^sup>+ = r \<union> r O r\<^sup>+"
  by (auto elim: converse_tranclE relcompEpair
           intro: r_into_trancl trancl_into_trancl2 relcompI)

(* rPlus = r \<union> (rPlus ; r) *)
lemma rPlus_def_law_right: "r\<^sup>+ = r \<union> r\<^sup>+ O r"
  by (rule trancl_unfold)

(* rStar = I \<union> (r ; rStar)   -- the SOUND law (cf. issue #1651) *)
lemma rStar_def_law: "r\<^sup>* = Id \<union> r O r\<^sup>*"
  by (auto elim: converse_rtranclE relcompEpair
           intro: converse_rtrancl_into_rtrancl relcompI)

(* rStar = I \<union> rPlus *)
lemma rStar_eq_Id_un_rPlus: "r\<^sup>* = Id \<union> r\<^sup>+"
  by (auto simp: rtrancl_eq_or_trancl)

(* rStar = I \<union> (rStar ; r)  -- PO-8, right-recursive mirror of rStar_def_law.
   Needed for the sound replacement of the law  r* = r*;r  in tceDerivRules. *)
lemma rStar_def_law_right: "r\<^sup>* = Id \<union> r\<^sup>* O r"
  by (auto elim: rtranclE relcompEpair
           intro: rtrancl_into_rtrancl relcompI)

(* The UNSOUND law from issue #1651, "r* = r;r*", really fails:
   at r = {} we have r;r* = {} but r* = Id. So the identity part is essential. *)
lemma rStar_naive_law_unsound:
  "\<exists>r :: (nat \<times> nat) set. r\<^sup>* \<noteq> r O r\<^sup>*"
proof (intro exI[of _ "{}"])
  have "(0::nat, 0) \<in> ({}::(nat \<times> nat) set)\<^sup>*" by simp
  moreover have "(0::nat, 0) \<notin> {} O ({}::(nat \<times> nat) set)\<^sup>*" by simp
  ultimately show "({}::(nat \<times> nat) set)\<^sup>* \<noteq> {} O ({}::(nat \<times> nat) set)\<^sup>*" by blast
qed

(* rRed = r - (r ; rPlus) *)
lemma rRed_def_law: "red r = r - r O r\<^sup>+"
  by (simp add: red_def)

(* r ; r+ = r+ ; r  -- paths of length \<ge> 2, from either end *)
lemma comp_trancl_comm: "r O r\<^sup>+ = r\<^sup>+ O r"
  by (metis O_assoc trancl_unfold_left trancl_unfold_right)

(* PO-6: reduction commutes with converse, red (r~) = (red r)~.
   Justifies  flp (PKl2 o a) = PKl2 o (flp a)  in the Ampersand parser. *)
lemma red_converse: "red (r\<inverse>) = (red r)\<inverse>"
proof -
  have "red (r\<inverse>) = r\<inverse> - r\<inverse> O (r\<^sup>+)\<inverse>"
    by (simp add: red_def trancl_converse)
  also have "\<dots> = r\<inverse> - (r\<^sup>+ O r)\<inverse>"
    by (simp add: converse_relcomp)
  also have "\<dots> = r\<inverse> - (r O r\<^sup>+)\<inverse>"
    by (simp add: comp_trancl_comm)
  also have "\<dots> = (r - r O r\<^sup>+)\<inverse>"
    by auto
  finally show ?thesis by (simp add: red_def)
qed

(* ---- PO-5: fixpoint view -- r+ is the least fixpoint of the ENFORCE operator ---- *)

definition Fplus :: "'a rel \<Rightarrow> 'a rel \<Rightarrow> 'a rel" where
  "Fplus r X = r \<union> X O r"

lemma Fplus_mono: "mono (Fplus r)"
  by (auto simp: mono_def Fplus_def)

lemma rPlus_is_fixpoint: "Fplus r (r\<^sup>+) = r\<^sup>+"
  unfolding Fplus_def by (metis rPlus_def_law_right)

lemma rPlus_is_lfp: "lfp (Fplus r) = r\<^sup>+"
proof (rule antisym)
  show "lfp (Fplus r) \<subseteq> r\<^sup>+"
    by (rule lfp_lowerbound) (simp add: rPlus_is_fixpoint)
next
  have Fbase: "r \<subseteq> lfp (Fplus r)"
    by (subst lfp_unfold[OF Fplus_mono]) (auto simp: Fplus_def)
  have Fstep: "\<And>a b c. (a, b) \<in> lfp (Fplus r) \<Longrightarrow> (b, c) \<in> r \<Longrightarrow> (a, c) \<in> lfp (Fplus r)"
    by (subst lfp_unfold[OF Fplus_mono]) (auto simp: Fplus_def)
  show "r\<^sup>+ \<subseteq> lfp (Fplus r)"
  proof
    fix p assume "p \<in> r\<^sup>+"
    then obtain a b where p: "p = (a, b)" and ab: "(a, b) \<in> r\<^sup>+" by (cases p) auto
    have "(a, b) \<in> lfp (Fplus r)" using ab
    proof (induct rule: trancl_induct)
      case (base y) then show ?case using Fbase by blast
    next
      case (step y z) then show ?case using Fstep by blast
    qed
    with p show "p \<in> lfp (Fplus r)" by simp
  qed
qed

(* ---- PO-3: incremental growth (insertion) is sound ---- *)

lemma trancl_absorb_insert: "(r\<^sup>+ \<union> e)\<^sup>+ = (r \<union> e)\<^sup>+"
proof (rule antisym)
  have "r \<union> e \<subseteq> r\<^sup>+ \<union> e" using r_subset_trancl by blast
  then show "(r \<union> e)\<^sup>+ \<subseteq> (r\<^sup>+ \<union> e)\<^sup>+" by (rule trancl_mono')
next
  have "r\<^sup>+ \<subseteq> (r \<union> e)\<^sup>+" by (meson sup_ge1 trancl_mono')
  moreover have "e \<subseteq> (r \<union> e)\<^sup>+" by (meson sup_ge2 r_subset_trancl subset_trans)
  ultimately have "r\<^sup>+ \<union> e \<subseteq> (r \<union> e)\<^sup>+" by blast
  then have "(r\<^sup>+ \<union> e)\<^sup>+ \<subseteq> ((r \<union> e)\<^sup>+)\<^sup>+" by (rule trancl_mono')
  then show "(r\<^sup>+ \<union> e)\<^sup>+ \<subseteq> (r \<union> e)\<^sup>+" by (simp add: trancl_idemp')
qed

(* ---- PO-2: transitive reduction preserves reachability ---- *)

lemma red_subset: "red r \<subseteq> r"
  by (simp add: red_def)

lemma red_trancl_subset: "(red r)\<^sup>+ \<subseteq> r\<^sup>+"
  using red_subset by (rule trancl_mono')

(* Inner induction (separate lemma to avoid nested-case name clashes):
   given the outer hypothesis IHo, every base edge (a,c) is reachable in red r.
   Well-founded on the target c via r+ (finite acyclic => wf (r+)). *)
lemma red_inner:
  assumes wfrp: "wf (r\<^sup>+)"
    and IHo: "\<And>c d. (a, c) \<in> r \<Longrightarrow> (c, d) \<in> r\<^sup>+ \<Longrightarrow> (c, d) \<in> (red r)\<^sup>+"
  shows "(a, c) \<in> r \<longrightarrow> (a, c) \<in> (red r)\<^sup>+"
proof (induct c rule: wf_induct_rule[OF wfrp])
  fix c
  assume IH: "\<And>d. (d, c) \<in> r\<^sup>+ \<Longrightarrow> ((a, d) \<in> r \<longrightarrow> (a, d) \<in> (red r)\<^sup>+)"
  show "(a, c) \<in> r \<longrightarrow> (a, c) \<in> (red r)\<^sup>+"
  proof
    assume ac: "(a, c) \<in> r"
    show "(a, c) \<in> (red r)\<^sup>+"
    proof (cases "(a, c) \<in> r O r\<^sup>+")
      case False
      with ac have "(a, c) \<in> red r" by (simp add: red_def)
      then show ?thesis by (rule r_into_trancl)
    next
      case True
      then obtain d where ad: "(a, d) \<in> r" and dc: "(d, c) \<in> r\<^sup>+"
        by (auto elim: relcompEpair)
      from IH[OF dc] ad have "(a, d) \<in> (red r)\<^sup>+" by simp
      moreover from IHo[OF ad dc] have "(d, c) \<in> (red r)\<^sup>+" .
      ultimately show ?thesis by (rule trancl_trans)
    qed
  qed
qed

(* Hard inclusion; needs r finite and acyclic.
   Outer induction on the source a via wf (r-inverse); the two shortcut halves
   are discharged by IHo (tail) and red_inner (head). *)
lemma red_reaches_edges:
  assumes fin: "finite r" and acy: "acyclic r"
  shows "r \<subseteq> (red r)\<^sup>+"
proof
  have "wf r" using fin acy by (rule finite_acyclic_wf)
  then have wfrp: "wf (r\<^sup>+)" by (rule wf_trancl)
  have "acyclic (r\<inverse>)" using acy by (simp add: acyclic_converse)
  moreover have "finite (r\<inverse>)" using fin by (simp add: finite_converse)
  ultimately have wfri: "wf (r\<inverse>)"
    using finite_acyclic_wf by blast
  have K: "\<forall>b. (a, b) \<in> r\<^sup>+ \<longrightarrow> (a, b) \<in> (red r)\<^sup>+" for a
  proof (induct a rule: wf_induct_rule[OF wfri])
    fix a
    assume IHwf: "\<And>y. (y, a) \<in> r\<inverse> \<Longrightarrow> (\<forall>b. (y, b) \<in> r\<^sup>+ \<longrightarrow> (y, b) \<in> (red r)\<^sup>+)"
    have IHo: "\<And>c d. (a, c) \<in> r \<Longrightarrow> (c, d) \<in> r\<^sup>+ \<Longrightarrow> (c, d) \<in> (red r)\<^sup>+"
    proof -
      fix c d assume ac: "(a, c) \<in> r" and cd: "(c, d) \<in> r\<^sup>+"
      from ac have "(c, a) \<in> r\<inverse>" by simp
      from IHwf[OF this] cd show "(c, d) \<in> (red r)\<^sup>+" by blast
    qed
    have inner: "(a, c) \<in> r \<longrightarrow> (a, c) \<in> (red r)\<^sup>+" for c
      using red_inner[OF wfrp IHo] .
    show "\<forall>b. (a, b) \<in> r\<^sup>+ \<longrightarrow> (a, b) \<in> (red r)\<^sup>+"
    proof (intro allI impI)
      fix b assume ab: "(a, b) \<in> r\<^sup>+"
      then obtain c where ac: "(a, c) \<in> r" and cb: "(c, b) \<in> r\<^sup>*"
        by (meson tranclD)
      from inner ac have acR: "(a, c) \<in> (red r)\<^sup>+" by simp
      show "(a, b) \<in> (red r)\<^sup>+"
      proof (cases "c = b")
        case True with acR show ?thesis by simp
      next
        case False
        with cb have "(c, b) \<in> r\<^sup>+" by (metis rtrancl_eq_or_trancl)
        from IHo[OF ac this] acR show ?thesis by (metis trancl_trans)
      qed
    qed
  qed
  fix p assume "p \<in> r"
  then obtain a b where p: "p = (a, b)" and ab: "(a, b) \<in> r" by (cases p) auto
  from ab have "(a, b) \<in> r\<^sup>+" by (rule r_into_trancl)
  with K p show "p \<in> (red r)\<^sup>+" by blast
qed

theorem red_trancl_eq:
  assumes "finite r" and "acyclic r"
  shows "(red r)\<^sup>+ = r\<^sup>+"
proof (rule antisym)
  show "(red r)\<^sup>+ \<subseteq> r\<^sup>+" by (rule red_trancl_subset)
next
  have "r \<subseteq> (red r)\<^sup>+" using assms by (rule red_reaches_edges)
  then have "r\<^sup>+ \<subseteq> ((red r)\<^sup>+)\<^sup>+" by (rule trancl_mono')
  then show "r\<^sup>+ \<subseteq> (red r)\<^sup>+" by (simp add: trancl_idemp')
qed

(* Minimality: red r is contained in EVERY subgraph of r that has the same
   closure. So red r is the least (hence minimum) generating subgraph -- the
   defining property of a transitive reduction. Note: needs neither finiteness
   nor acyclicity. Together with red_trancl_eq (which needs both) this makes
   red r THE transitive reduction of a finite acyclic r. *)
lemma red_minimal:
  assumes sub: "s \<subseteq> r" and eq: "s\<^sup>+ = r\<^sup>+"
  shows "red r \<subseteq> s"
proof
  fix p assume "p \<in> red r"
  then obtain u v where p: "p = (u, v)" and uvr: "(u, v) \<in> r"
      and notred: "(u, v) \<notin> r O r\<^sup>+"
    by (cases p) (auto simp: red_def)
  have inSplus: "(u, v) \<in> s\<^sup>+" using uvr eq r_subset_trancl by blast
  have "(u, v) \<in> s"
  proof (rule ccontr)
    assume notin: "(u, v) \<notin> s"
    from inSplus obtain w where uw: "(u, w) \<in> s" and wv: "(w, v) \<in> s\<^sup>*"
      by (meson tranclD)
    have "(w, v) \<in> s\<^sup>+"
    proof (cases "w = v")
      case True with uw notin show ?thesis by blast
    next
      case False with wv show ?thesis by (metis rtrancl_eq_or_trancl)
    qed
    then have "(w, v) \<in> r\<^sup>+" using sub trancl_mono' by blast
    moreover from uw sub have "(u, w) \<in> r" by blast
    ultimately have "(u, v) \<in> r O r\<^sup>+" by (blast intro: relcompI)
    with notred show False by simp
  qed
  then show "p \<in> s" using p by simp
qed

(* ---- PO-4: deletion (shrinking) ---- *)

(* Safe direction: deletion never adds reachability. *)
lemma delete_mono: "(r - d)\<^sup>+ \<subseteq> r\<^sup>+"
  by (simp add: Diff_subset trancl_mono')

(* Unsoundness of NAIVE deletion: subtracting the deleted edges from the stored
   closure does not yield the new closure.  This is exactly why a shrinking
   maintenance must re-derive (DRed) or recompute; cf. issue #1651. *)
lemma naive_delete_unsound:
  "\<exists>r d :: (nat \<times> nat) set. (r - d)\<^sup>+ \<noteq> r\<^sup>+ - d"
proof (intro exI)
  let ?r = "{(0::nat, 1), (1, 2)}"  and  ?d = "{(1::nat, 2)}"
  have r02: "(0::nat, 2) \<in> ?r\<^sup>+"
    by (meson insertCI r_into_trancl trancl_trans)
  have diff: "?r - ?d = {(0::nat, 1)}" by auto
  have notin: "(0::nat, 2) \<notin> {(0::nat, 1)}\<^sup>+" by (auto elim: tranclE)
  have "(0::nat, 2) \<in> ?r\<^sup>+ - ?d" using r02 by simp
  moreover have "(0::nat, 2) \<notin> (?r - ?d)\<^sup>+" by (subst diff) (rule notin)
  ultimately show "(?r - ?d)\<^sup>+ \<noteq> ?r\<^sup>+ - ?d" by blast
qed

end
