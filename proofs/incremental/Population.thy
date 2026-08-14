theory Population
  imports Circuit
begin

section \<open>The population mirror: occurrence integrals vs. \<open>atomValuesOf\<close>\<close>

text \<open>
  The engine derives concept populations from relation occurrences
  (delta-calculus.md section 3, "Concept populations"): for concept \<open>c\<close>, the
  occurrence integral sums the row weights of every relation whose source
  concept lies in \<open>c\<close>'s cone (the concept and its specializations), the column
  weights of every relation whose target concept does, and the explicitly
  populated atoms up the same cone; the maintained population set is its
  @{const zdistinct}.  The reference is \<open>atomValuesOf\<close>
  (src/Ampersand/FSpec/ToFSpec/Populated.hs:30), the engine state is
  \<open>ieCptOcc\<close>/\<open>ieCptSet\<close> and the per-transaction derivation \<open>occDelta\<close>/
  \<open>cptSetDelta\<close> (src/Ampersand/FSpec/Incremental.hs, \<open>applyTx\<close>).

  This theory proves the three facts that make that bookkeeping correct:
  \<^item> P1: the occurrence integral is linear in the environment, so maintaining
    it by adding \<open>occZ\<close> of the transaction delta (\<open>occDelta\<close>) is exact — rule
    D4 for populations.
  \<^item> P2: on set-valued stores, the integral's carried set is exactly the set
    \<open>atomValuesOf\<close> computes.
  \<^item> P3: the population-set delta is the zero-crossing \<open>H\<close> of the integral and
    its delta (\<open>cptSetDelta\<close> = \<open>bagH\<close>), by Z5.
  The same shape covers the relation front-end: a term relation's pre-distinct
  integral is the linear sum of its feeding populated relations (P4), and its
  carried set is the union \<open>pairsOf\<close> computes (P5).

  The set-discipline premise of P2/P5 — every raw store holds weights 0/1 —
  is the engine's lockstep invariant: transaction weights are \<open>\<plusminus>1\<close> and the
  raw-pair sets and integrals are updated together (\<open>engineInit\<close> comment).
  The cone function \<open>bel\<close> is abstract; the implementation instantiates it with
  \<open>c : smallerConcepts c\<close>, and the QuickCheck bridge exercises concrete ISA
  hierarchies against the same oracle.
\<close>

subsection \<open>Sums of nonnegative Z-sets carry unions\<close>

lemma setof_add_nonneg:
  assumes f: "\<And>x. 0 \<le> f x" and g: "\<And>x. 0 \<le> g x"
  shows "setof (\<lambda>x. f x + g x) = setof f \<union> setof g"
proof (rule set_eqI)
  fix x
  have "(0 < f x + g x) \<longleftrightarrow> (0 < f x \<or> 0 < g x)"
    using f[of x] g[of x] by linarith
  then show "x \<in> setof (\<lambda>x. f x + g x) \<longleftrightarrow> x \<in> setof f \<union> setof g"
    by (simp add: setof_def)
qed

lemma setof_sum_nonneg:
  "finite I \<Longrightarrow> (\<And>i x. i \<in> I \<Longrightarrow> 0 \<le> f i x) \<Longrightarrow>
   setof (\<lambda>x. \<Sum>i\<in>I. f i x) = (\<Union>i\<in>I. setof (f i))"
proof (induction I rule: finite_induct)
  case empty
  then show ?case by (simp add: setof_def)
next
  case (insert i I)
  have nn_i: "\<And>x. 0 \<le> f i x" using insert.prems by simp
  have nn_sum: "\<And>x. 0 \<le> (\<Sum>j\<in>I. f j x)"
    by (rule sum_nonneg) (use insert.prems in auto)
  have "setof (\<lambda>x. \<Sum>j\<in>insert i I. f j x) = setof (\<lambda>x. f i x + (\<Sum>j\<in>I. f j x))"
    using insert.hyps by simp
  also have "\<dots> = setof (f i) \<union> setof (\<lambda>x. \<Sum>j\<in>I. f j x)"
    by (rule setof_add_nonneg[OF nn_i nn_sum])
  also have "setof (\<lambda>x. \<Sum>j\<in>I. f j x) = (\<Union>j\<in>I. setof (f j))"
    by (rule insert.IH) (use insert.prems in auto)
  finally show ?case by simp
qed

lemma zcol_nonneg: "isSet a \<Longrightarrow> 0 \<le> zcol a x"
  unfolding zcol_def by (rule zrow_nonneg[OF isSet_zflip])

subsection \<open>The model\<close>

locale population_model =
  fixes R :: "'r set"                \<comment> \<open>the populated declarations\<close>
    and rsrc rtgt :: "'r \<Rightarrow> 'c"      \<comment> \<open>their signature concepts\<close>
    and C :: "'c set"                \<comment> \<open>concepts with explicit population\<close>
    and bel :: "'c \<Rightarrow> 'c set"        \<comment> \<open>the cone: a concept and its specializations\<close>
    and fdr :: "'r \<Rightarrow> 'r set"        \<comment> \<open>term relation \<Rightarrow> feeding declarations\<close>
  assumes finR: "finite R"
      and finC: "finite C"
      and finFdr: "\<And>r. finite (fdr r)"
begin

lemma finR1: "finite {d \<in> R. P d}"
  by (rule finite_subset[of _ R]) (auto simp: finR)

lemma finC1: "finite {c' \<in> C. P c'}"
  by (rule finite_subset[of _ C]) (auto simp: finC)

text \<open>The occurrence integral (engine: \<open>ieCptOcc\<close>).\<close>

definition occZ :: "('r, 'c, 'a) env \<Rightarrow> 'c \<Rightarrow> 'a \<Rightarrow> int" where
  "occZ E c = (\<lambda>x.
       (\<Sum>d\<in>{d \<in> R. rsrc d \<in> bel c}. zrow (re E d) x)
     + (\<Sum>d\<in>{d \<in> R. rtgt d \<in> bel c}. zcol (re E d) x)
     + (\<Sum>c'\<in>{c' \<in> C. c' \<in> bel c}. ce E c' x))"

text \<open>The set \<open>atomValuesOf\<close> computes (Populated.hs, \<open>PlainConcept\<close> case).\<close>

definition AV :: "('r, 'c, 'a) env \<Rightarrow> 'c \<Rightarrow> 'a set" where
  "AV E c = (\<Union>d\<in>{d \<in> R. rsrc d \<in> bel c}. fst ` setof (re E d))
          \<union> (\<Union>d\<in>{d \<in> R. rtgt d \<in> bel c}. snd ` setof (re E d))
          \<union> (\<Union>c'\<in>{c' \<in> C. c' \<in> bel c}. setof (ce E c'))"

text \<open>The relation front-end (engine: the \<open>KRel\<close> integral; reference: \<open>pairsOf\<close>).\<close>

definition feedZ :: "('r, 'c, 'a) env \<Rightarrow> 'r \<Rightarrow> 'a \<times> 'a \<Rightarrow> int" where
  "feedZ E r = (\<lambda>p. \<Sum>d\<in>fdr r. re E d p)"

subsection \<open>P1: the occurrence integral is linear (D4 for populations)\<close>

theorem P1_occ_linear:
  assumes E: "wf_env E" and D: "wf_env D"
  shows "occZ (eadd E D) c = zplus (occZ E c) (occZ D c)"
proof (rule ext)
  fix x
  let ?S1 = "{d \<in> R. rsrc d \<in> bel c}"
  let ?S2 = "{d \<in> R. rtgt d \<in> bel c}"
  let ?S3 = "{c' \<in> C. c' \<in> bel c}"
  have r: "\<And>d. zrow (zplus (re E d) (re D d)) x = zrow (re E d) x + zrow (re D d) x"
  proof -
    fix d
    have "zrow (zplus (re E d) (re D d)) = zplus (zrow (re E d)) (zrow (re D d))"
      by (rule zrow_add[OF wf_env_re[OF E] wf_env_re[OF D]])
    then have "zrow (zplus (re E d) (re D d)) x = zplus (zrow (re E d)) (zrow (re D d)) x"
      by (rule fun_cong)
    then show "zrow (zplus (re E d) (re D d)) x = zrow (re E d) x + zrow (re D d) x"
      by (simp add: zplus_apply)
  qed
  have c: "\<And>d. zcol (zplus (re E d) (re D d)) x = zcol (re E d) x + zcol (re D d) x"
  proof -
    fix d
    have "zcol (zplus (re E d) (re D d)) = zplus (zcol (re E d)) (zcol (re D d))"
      by (rule zcol_add[OF wf_env_re[OF E] wf_env_re[OF D]])
    then have "zcol (zplus (re E d) (re D d)) x = zplus (zcol (re E d)) (zcol (re D d)) x"
      by (rule fun_cong)
    then show "zcol (zplus (re E d) (re D d)) x = zcol (re E d) x + zcol (re D d) x"
      by (simp add: zplus_apply)
  qed
  have e1: "(\<Sum>d\<in>?S1. zrow (zplus (re E d) (re D d)) x)
          = (\<Sum>d\<in>?S1. zrow (re E d) x) + (\<Sum>d\<in>?S1. zrow (re D d) x)"
    by (simp add: r sum.distrib)
  have e2: "(\<Sum>d\<in>?S2. zcol (zplus (re E d) (re D d)) x)
          = (\<Sum>d\<in>?S2. zcol (re E d) x) + (\<Sum>d\<in>?S2. zcol (re D d) x)"
    by (simp add: c sum.distrib)
  have e3: "(\<Sum>c'\<in>?S3. zplus (ce E c') (ce D c') x)
          = (\<Sum>c'\<in>?S3. ce E c' x) + (\<Sum>c'\<in>?S3. ce D c' x)"
    by (simp add: zplus_apply sum.distrib)
  have expand: "occZ (eadd E D) c x
              = (\<Sum>d\<in>?S1. zrow (zplus (re E d) (re D d)) x)
              + (\<Sum>d\<in>?S2. zcol (zplus (re E d) (re D d)) x)
              + (\<Sum>c'\<in>?S3. zplus (ce E c') (ce D c') x)"
    by (simp add: occZ_def)
  have appE: "occZ E c x
            = (\<Sum>d\<in>?S1. zrow (re E d) x) + (\<Sum>d\<in>?S2. zcol (re E d) x)
            + (\<Sum>c'\<in>?S3. ce E c' x)"
    by (simp add: occZ_def)
  have appD: "occZ D c x
            = (\<Sum>d\<in>?S1. zrow (re D d) x) + (\<Sum>d\<in>?S2. zcol (re D d) x)
            + (\<Sum>c'\<in>?S3. ce D c' x)"
    by (simp add: occZ_def)
  have rhs: "zplus (occZ E c) (occZ D c) x = occZ E c x + occZ D c x"
    by (simp add: zplus_apply)
  show "occZ (eadd E D) c x = zplus (occZ E c) (occZ D c) x"
    using e1 e2 e3 expand appE appD rhs by linarith
qed

subsection \<open>P2: the carried set is the \<open>atomValuesOf\<close> set\<close>

theorem P2_occ_mirrors_atomValuesOf:
  assumes E: "wf_env E"
    and rels: "\<And>d. isSet (re E d)" and cpts: "\<And>c'. isSet (ce E c')"
  shows "setof (occZ E c) = AV E c"
proof -
  let ?S1 = "{d \<in> R. rsrc d \<in> bel c}"
  let ?S2 = "{d \<in> R. rtgt d \<in> bel c}"
  let ?S3 = "{c' \<in> C. c' \<in> bel c}"
  let ?f1 = "\<lambda>x. \<Sum>d\<in>?S1. zrow (re E d) x"
  let ?f2 = "\<lambda>x. \<Sum>d\<in>?S2. zcol (re E d) x"
  let ?f3 = "\<lambda>x. \<Sum>c'\<in>?S3. ce E c' x"
  have nn1: "\<And>x. 0 \<le> ?f1 x"
    by (rule sum_nonneg) (auto intro: zrow_nonneg[OF rels])
  have nn2: "\<And>x. 0 \<le> ?f2 x"
    by (rule sum_nonneg) (auto intro: zcol_nonneg[OF rels])
  have nn3: "\<And>x. 0 \<le> ?f3 x"
    by (rule sum_nonneg) (auto intro: isSet_nonneg[OF cpts])
  have split: "setof (occZ E c) = setof ?f1 \<union> setof ?f2 \<union> setof ?f3"
  proof -
    have "setof (occZ E c) = setof (\<lambda>x. (?f1 x + ?f2 x) + ?f3 x)"
      by (simp add: occZ_def)
    also have "\<dots> = setof (\<lambda>x. ?f1 x + ?f2 x) \<union> setof ?f3"
      by (rule setof_add_nonneg) (auto intro: add_nonneg_nonneg nn1 nn2 nn3)
    also have "setof (\<lambda>x. ?f1 x + ?f2 x) = setof ?f1 \<union> setof ?f2"
      by (rule setof_add_nonneg[OF nn1 nn2])
    finally show ?thesis by simp
  qed
  have s1: "setof ?f1 = (\<Union>d\<in>?S1. fst ` setof (re E d))"
  proof -
    have "setof ?f1 = (\<Union>d\<in>?S1. setof (zrow (re E d)))"
      by (rule setof_sum_nonneg[OF finR1]) (auto intro: zrow_nonneg[OF rels])
    moreover have "\<And>d. d \<in> ?S1 \<Longrightarrow> setof (zrow (re E d)) = fst ` setof (re E d)"
      by (rule zrow_setof[OF rels wf_env_re[OF E]])
    ultimately show ?thesis by auto
  qed
  have s2: "setof ?f2 = (\<Union>d\<in>?S2. snd ` setof (re E d))"
  proof -
    have "setof ?f2 = (\<Union>d\<in>?S2. setof (zcol (re E d)))"
      by (rule setof_sum_nonneg[OF finR1]) (auto intro: zcol_nonneg[OF rels])
    moreover have "\<And>d. d \<in> ?S2 \<Longrightarrow> setof (zcol (re E d)) = snd ` setof (re E d)"
      by (rule zcol_setof[OF rels wf_env_re[OF E]])
    ultimately show ?thesis by auto
  qed
  have s3: "setof ?f3 = (\<Union>c'\<in>?S3. setof (ce E c'))"
    by (rule setof_sum_nonneg[OF finC1]) (auto intro: isSet_nonneg[OF cpts])
  show ?thesis by (simp add: split s1 s2 s3 AV_def)
qed

subsection \<open>P3: the population-set delta is the zero-crossing of the integral\<close>

corollary P3_popset_delta:
  assumes E: "wf_env E" and D: "wf_env D"
  shows "zdistinct (occZ (eadd E D) c)
       = zplus (zdistinct (occZ E c)) (H (occZ E c) (occZ D c))"
  by (simp add: P1_occ_linear[OF E D] Z5_distinct_delta)

subsection \<open>P4/P5: the relation front-end\<close>

theorem P4_feed_linear:
  "feedZ (eadd E D) r = zplus (feedZ E r) (feedZ D r)"
proof (rule ext)
  fix p
  show "feedZ (eadd E D) r p = zplus (feedZ E r) (feedZ D r) p"
    unfolding feedZ_def by (simp add: zplus_apply sum.distrib)
qed

theorem P5_feed_contents:
  assumes rels: "\<And>d. isSet (re E d)"
  shows "setof (feedZ E r) = (\<Union>d\<in>fdr r. setof (re E d))"
  unfolding feedZ_def
  by (rule setof_sum_nonneg[OF finFdr]) (auto intro: isSet_nonneg[OF rels])

end

end
