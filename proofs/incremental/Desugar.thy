theory Desugar
  imports Main
begin

section \<open>The desugaring obligations S1..S5 (set level)\<close>

text \<open>
  Obligations from memorybank/incremental-evaluation/delta-calculus.md,
  sections 2 and 5. Modelling copied from proofs/spike/Ampersand_RA.thy
  (house style): one universe type @{typ 'a}, concepts as sets \<open>A B C\<close>,
  every relation as an @{typ "('a \<times> 'a) set"}, and the complement TYPED,
  i.e. relative to the \<open>V\<close> of the signature.

  The adequacy condition of delta-calculus.md section 2 appears here as the
  typing premises \<open>l \<subseteq> A \<times> C\<close> etc.: every relation is contained in the \<open>V\<close> of
  its signature because the concept populations contain every atom occurring
  in any relation (\<open>atomValuesOf\<close>, Populated.hs:30).
\<close>

definition cpl :: "'a set \<Rightarrow> 'a set \<Rightarrow> ('a \<times> 'a) set \<Rightarrow> ('a \<times> 'a) set" where
  "cpl A B r = A \<times> B - r"

subsection \<open>S1: left residual\<close>

(* S1 (delta-calculus.md section 5): l/r = -(-l ; r~).
   Pointwise: x (l/r) y \<longleftrightarrow> \<forall>z: y r z \<longrightarrow> x l z \<longleftrightarrow> (x,y) \<notin> (-l ; r~). *)
lemma S1_lrs_as_antijoin:
  assumes "l \<subseteq> A \<times> C" and "r \<subseteq> B \<times> C"
  shows "{(x, y) \<in> A \<times> B. \<forall>z\<in>C. (y, z) \<in> r \<longrightarrow> (x, z) \<in> l}
         = cpl A B (cpl A C l O converse r)"
  using assms unfolding cpl_def by auto

text \<open>
  Why the quantifier domain \<open>z \<in> C\<close> agrees with \<open>fullContents\<close>'s row-based
  quantification (which lets z range over the rows of r only): the adequacy
  condition \<open>r \<subseteq> B \<times> C\<close> puts every row atom of r inside C, so restricting the
  universal quantifier to C excludes no z for which the premise \<open>(y,z) \<in> r\<close>
  could hold. Machine-checked:
\<close>

lemma S1_quantifier_domain:
  assumes "r \<subseteq> B \<times> C"
  shows "{(x, y) \<in> A \<times> B. \<forall>z\<in>C. (y, z) \<in> r \<longrightarrow> (x, z) \<in> l}
       = {(x, y) \<in> A \<times> B. \<forall>z. (y, z) \<in> r \<longrightarrow> (x, z) \<in> l}"
  using assms by auto

subsection \<open>S2: right residual\<close>

(* S2 (delta-calculus.md section 5): l\r = -(l~ ; -r).
   Pointwise: x (l\r) y \<longleftrightarrow> \<forall>z: z l x \<longrightarrow> z r y \<longleftrightarrow> (x,y) \<notin> (l~ ; -r). *)
lemma S2_rrs_as_antijoin:
  assumes "l \<subseteq> C \<times> A" and "r \<subseteq> C \<times> B"
  shows "{(x, y) \<in> A \<times> B. \<forall>z\<in>C. (z, x) \<in> l \<longrightarrow> (z, y) \<in> r}
         = cpl A B (converse l O cpl C B r)"
  using assms unfolding cpl_def by auto

lemma S2_quantifier_domain:
  assumes "l \<subseteq> C \<times> A"
  shows "{(x, y) \<in> A \<times> B. \<forall>z\<in>C. (z, x) \<in> l \<longrightarrow> (z, y) \<in> r}
       = {(x, y) \<in> A \<times> B. \<forall>z. (z, x) \<in> l \<longrightarrow> (z, y) \<in> r}"
  using assms by auto

subsection \<open>S3: diamond\<close>

(* S3 (delta-calculus.md section 5): l<>r is the intersection of the two
   residual directions: \<forall>z: x l z \<longrightarrow> z r y  gives  -(l ; -r),  and
   \<forall>z: z r y \<longrightarrow> x l z  gives  -(-l ; r). *)
lemma S3_dia_as_conjunction:
  assumes "l \<subseteq> A \<times> C" and "r \<subseteq> C \<times> B"
  shows "{(x, y) \<in> A \<times> B. \<forall>z\<in>C. ((x, z) \<in> l) = ((z, y) \<in> r)}
         = cpl A B (l O cpl C B r) \<inter> cpl A B (cpl A C l O r)"
  using assms unfolding cpl_def by (auto; blast)

subsection \<open>S4: relative addition (dagger)\<close>

(* S4 (delta-calculus.md section 5): l!r = -(-l ; -r).
   Pointwise: x (l!r) y \<longleftrightarrow> \<forall>z\<in>C: x l z \<or> z r y. Both complements are typed,
   so no typing premise on l or r is needed for this identity. *)
lemma S4_rad_as_double_complement:
  "{(x, y) \<in> A \<times> B. \<forall>z\<in>C. (x, z) \<in> l \<or> (z, y) \<in> r}
   = cpl A B (cpl A C l O cpl C B r)"
  unfolding cpl_def by auto

subsection \<open>S5: typed complement\<close>

(* S5 (delta-calculus.md section 5): complement as difference from V (definitional). *)
lemma S5_cpl_as_difference: "cpl A B r = A \<times> B - r"
  by (simp add: cpl_def)

(* S5: double complement restores every well-typed relation. *)
lemma S5_cpl_involution: "r \<subseteq> A \<times> B \<Longrightarrow> cpl A B (cpl A B r) = r"
  unfolding cpl_def by auto

(* S5: V absorbs every well-typed relation. *)
lemma S5_V_absorbs_isc: "r \<subseteq> A \<times> B \<Longrightarrow> r \<inter> A \<times> B = r"
  by auto

lemma S5_V_absorbs_uni: "r \<subseteq> A \<times> B \<Longrightarrow> r \<union> A \<times> B = A \<times> B"
  by auto

end
