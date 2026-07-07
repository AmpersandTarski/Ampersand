theory IncrementalDelete
  imports KleeneReduction
begin

section \<open>Incremental deletion of the transitive closure, with cycles\<close>

(*
  Setting: a materialised closure  T = r^+  is kept in sync with r. We delete a
  set of edges  del  from r, giving  r - del, and want the new closure
  (r - del)^+  without recomputing from scratch. The hard case is a graph WITH
  cycles, where a pair can be supported by a derivation that runs through a
  cycle; naive deletion (subtracting the deleted tuples from T) is unsound
  (see naive_delete_unsound).

  Idea (delete-and-rederive, relational form):
  * "affected r del" over-approximates the pairs that MIGHT be removed:
    those with SOME path through a deleted edge.
  * "safe r del" is the rest of the old closure. Every safe pair
    survives (lemma safe_survives) -- this is where cycles are handled:
    a pair whose only support ran through a deleted edge is NOT safe, so it is
    never wrongly kept alive by a cycle.
  * The new closure is obtained by re-closing the safe pairs together with the
    surviving edges (theorem incr_delete_correct). No acyclicity needed.

  Proof style: membership in "affected" is always established by the explicit
  introduction rule affectedI. blast/meson on affected_iff + rtrancl_refl
  diverges (the rtrancl intro rules give the search infinite paths), so we
  avoid that combination.
*)

definition affected :: "'a rel \<Rightarrow> 'a rel \<Rightarrow> 'a rel" where
  "affected r del = r\<^sup>* O del O r\<^sup>*"

lemma affected_iff:
  "(a, b) \<in> affected r del \<longleftrightarrow> (\<exists>u v. (a, u) \<in> r\<^sup>* \<and> (u, v) \<in> del \<and> (v, b) \<in> r\<^sup>*)"
  by (auto simp: affected_def relcomp_unfold)

lemma affectedI:
  assumes "(a, u) \<in> r\<^sup>*" and "(u, v) \<in> del" and "(v, b) \<in> r\<^sup>*"
  shows "(a, b) \<in> affected r del"
  unfolding affected_def
  by (rule relcompI[OF assms(1) relcompI[OF assms(2) assms(3)]])

lemma affectedE:
  assumes "(a, b) \<in> affected r del"
  obtains u v where "(a, u) \<in> r\<^sup>*" and "(u, v) \<in> del" and "(v, b) \<in> r\<^sup>*"
  using assms unfolding affected_def by (elim relcompE) auto

definition safe :: "'a rel \<Rightarrow> 'a rel \<Rightarrow> 'a rel" where
  "safe r del = r\<^sup>+ - affected r del"

(*The key lemma: a pair with no path through a deleted edge survives.
  Proof by induction on the derivation of (a,b) : r^+.*)

lemma safe_survives_aux:
  assumes "(a, b) \<in> r\<^sup>+"
  shows "(a, b) \<notin> affected r del \<longrightarrow> (a, b) \<in> (r - del)\<^sup>+"
  using assms
proof (induct rule: trancl_induct)
  fix y assume ay: "(a, y) \<in> r"
  show "(a, y) \<notin> affected r del \<longrightarrow> (a, y) \<in> (r - del)\<^sup>+"
  proof
    assume notaff: "(a, y) \<notin> affected r del"
    have "(a, y) \<notin> del"
    proof
      assume "(a, y) \<in> del"
      then have "(a, y) \<in> affected r del"
        by (rule affectedI[OF rtrancl_refl _ rtrancl_refl])
      with notaff show False by simp
    qed
    with ay show "(a, y) \<in> (r - del)\<^sup>+" by (simp add: r_into_trancl)
  qed
next
  fix y z
  assume ir: "(a, y) \<in> r\<^sup>+" and yz: "(y, z) \<in> r"
     and IH: "(a, y) \<notin> affected r del \<longrightarrow> (a, y) \<in> (r - del)\<^sup>+"
  show "(a, z) \<notin> affected r del \<longrightarrow> (a, z) \<in> (r - del)\<^sup>+"
  proof
    assume notaff: "(a, z) \<notin> affected r del"
    have ays: "(a, y) \<in> r\<^sup>*" using ir by (simp add: trancl_into_rtrancl)
    have yznd: "(y, z) \<notin> del"
    proof
      assume "(y, z) \<in> del"
      then have "(a, z) \<in> affected r del"
        by (rule affectedI[OF ays _ rtrancl_refl])
      with notaff show False by simp
    qed
    have aynd: "(a, y) \<notin> affected r del"
    proof
      assume "(a, y) \<in> affected r del"
      then obtain u v where au: "(a, u) \<in> r\<^sup>*" and uv: "(u, v) \<in> del"
                        and vy: "(v, y) \<in> r\<^sup>*"
        by (rule affectedE)
      have vz: "(v, z) \<in> r\<^sup>*" using vy yz by (rule rtrancl_into_rtrancl)
      have "(a, z) \<in> affected r del" by (rule affectedI[OF au uv vz])
      with notaff show False by simp
    qed
    from aynd IH have "(a, y) \<in> (r - del)\<^sup>+" by simp
    moreover from yznd yz have "(y, z) \<in> r - del" by simp
    ultimately show "(a, z) \<in> (r - del)\<^sup>+" by (rule trancl_into_trancl)
  qed
qed

lemma safe_survives: "safe r del \<subseteq> (r - del)\<^sup>+"
proof
  fix p assume "p \<in> safe r del"
  then obtain a b where p: "p = (a, b)" and ab: "(a, b) \<in> r\<^sup>+"
                    and naff: "(a, b) \<notin> affected r del"
    by (cases p) (auto simp: safe_def)
  from safe_survives_aux[OF ab] naff have "(a, b) \<in> (r - del)\<^sup>+" by (rule mp)
  with p show "p \<in> (r - del)\<^sup>+" by simp
qed

(*Over-deletion is sound: every pair that really disappears was affected.*)

lemma overdelete_covers: "r\<^sup>+ - (r - del)\<^sup>+ \<subseteq> affected r del"
proof
  fix p assume "p \<in> r\<^sup>+ - (r - del)\<^sup>+"
  then obtain a b where p: "p = (a, b)" and ab: "(a, b) \<in> r\<^sup>+"
                    and nsur: "(a, b) \<notin> (r - del)\<^sup>+"
    by (cases p) auto
  have "(a, b) \<in> affected r del"
  proof (rule ccontr)
    assume "(a, b) \<notin> affected r del"
    with safe_survives_aux[OF ab] have "(a, b) \<in> (r - del)\<^sup>+" by (rule mp)
    with nsur show False by simp
  qed
  with p show "p \<in> affected r del" by simp
qed

(*Main theorem: the new closure is the closure of the safe remnant together
  with the surviving edges. Holds for arbitrary r -- cycles included.*)

theorem incr_delete_correct:
  "(r - del)\<^sup>+ = (safe r del \<union> (r - del))\<^sup>+"
proof (rule antisym)
  have "safe r del \<union> (r - del) \<subseteq> (r - del)\<^sup>+"
    by (rule Un_least[OF safe_survives r_subset_trancl])
  then have "(safe r del \<union> (r - del))\<^sup>+ \<subseteq> ((r - del)\<^sup>+)\<^sup>+" by (rule trancl_mono')
  then show "(safe r del \<union> (r - del))\<^sup>+ \<subseteq> (r - del)\<^sup>+" by (simp add: trancl_idemp')
next
  show "(r - del)\<^sup>+ \<subseteq> (safe r del \<union> (r - del))\<^sup>+"
    by (rule trancl_mono'[OF sup_ge2])
qed

(*The affected region bounds the recomputation: outside it the old closure
  is reused verbatim.*)

lemma reuse_outside_affected: "(r - del)\<^sup>+ - affected r del = r\<^sup>+ - affected r del"
proof -
  have "safe r del \<subseteq> (r - del)\<^sup>+" by (rule safe_survives)
  moreover have "(r - del)\<^sup>+ \<subseteq> r\<^sup>+" by (rule delete_mono)
  ultimately show ?thesis by (auto simp: safe_def)
qed

section \<open>Consequences for rStar and rRed under deletion\<close>

corollary incr_delete_rStar:
  "(r - del)\<^sup>* = Id \<union> (safe r del \<union> (r - del))\<^sup>+"
  by (simp only: rStar_eq_Id_un_rPlus incr_delete_correct)

corollary incr_delete_rRed:
  "red (r - del) = (r - del) - (r - del) O (safe r del \<union> (r - del))\<^sup>+"
  by (simp add: red_def incr_delete_correct)

end
