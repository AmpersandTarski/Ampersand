theory Circuit
  imports Delta Bridge
begin

section \<open>The circuit: deep embedding, step function, whole-circuit induction\<close>

text \<open>
  Deep embedding of the circuit language of the incremental evaluator
  (src/Ampersand/FSpec/Incremental.hs, @{text "data Kind"}), with the per-node
  step function and the theorem that a step preserves the invariant
  "every node's clipped output equals the set semantics of its term under the
  current population" (delta-calculus.md, section 4; the structural-induction
  glue that section 5 lists as the stretch goal).

  The node inventory and the state shapes mirror the implementation exactly:
  \<^item> @{text CRel}, @{text CUni}, @{text CDif}, @{text CCps} carry a pre-distinct
    integral; @{text CCps} additionally the flipped copy of the left child's
    output; @{text CPrd} the four projection integrals.
  \<^item> @{text CI}/@{text CV} read the globally maintained concept populations
    (the @{text ce} component of the environment holds the occurrence
    integrals; their @{const zdistinct} is the maintained population set).
  \<^item> Every construct without a proven delta rule — fallback terms, @{text Mp1},
    @{text Bin}, @{text Kl0}, @{text Kl1} — is a specification node
    @{text CSpec}: its step re-evaluates the stored semantics function on the
    new environment and emits @{text "new - old"} (rule D7, correct by
    construction). Modelling them as such keeps the theorem's scope honest:
    it covers every circuit the compiler builds.  For the Kleene nodes the
    stored function is "closure of the child term's semantics"; the
    implementation computes the closure of the child's maintained output,
    which by this very invariant equals its semantics.

  The environment update is linear (@{const zplus} per relation and concept),
  which is what @{text applyTx} does.  The backfill is the first step from the
  all-zero base state under the all-zero environment (matching
  @{text engineInit}, where the initial population — including ONE's singleton
  — travels through @{text tx0} like any other delta); theorem
  @{text C4_backfill_base} shows the base state is well-formed, and
  @{text C5_run_correct} concludes correctness of every reachable state.
\<close>

subsection \<open>Additional Z-set material\<close>

text \<open>Clipping is the indicator of the carried set.\<close>

lemma zdistinct_as_indic: "zdistinct f = indic (setof f)"
  by (rule ext) (simp add: setof_def)

lemma indic_isSet_id: "isSet f \<Longrightarrow> indic (setof f) = f"
proof (rule ext)
  fix p assume "isSet f"
  then have "f p = 0 \<or> f p = 1" by (simp add: isSet_def)
  then show "indic (setof f) p = f p" by (auto simp: setof_def)
qed

lemma finite_setof: "finsupp f \<Longrightarrow> finite (setof f)"
  unfolding finsupp_def
  by (rule finite_subset[of _ "support f"]) (auto simp: setof_def support_def)

lemma finsupp_indic: "finite S \<Longrightarrow> finsupp (indic S)"
  unfolding finsupp_def
  by (rule finite_subset[of _ S]) (auto simp: support_def)

lemma isSet_zzero: "isSet zzero"
  by (simp add: isSet_def)

lemma zdistinct_zzero: "zdistinct zzero = zzero"
  by (rule ext) simp

lemma zplus_minus_cancel: "zplus S (zminus N S) = N"
  by (rule ext) simp

lemma zplus_ac: "zplus (zplus p q) (zplus r s) = zplus (zplus p r) (zplus q s)"
  by (rule ext) simp

lemma zplus_minus_ac: "zplus (zminus p q) (zminus r s) = zminus (zplus p r) (zplus q s)"
  by (rule ext) simp

lemma zmul_zzero_left: "zmul zzero v = zzero"
  by (rule ext) simp

lemma zplus_zzero_zzero: "zplus zzero zzero = zzero"
  by (rule ext) simp

lemma zminus_zzero_zzero: "zminus zzero zzero = zzero"
  by (rule ext) simp

text \<open>The diagonal embedding of a unary Z-set (the @{text I} node, D4/D6).\<close>

definition zdiag :: "('a \<Rightarrow> int) \<Rightarrow> 'a \<times> 'a \<Rightarrow> int" where
  "zdiag u = (\<lambda>(x, y). if x = y then u x else 0)"

lemma zdiag_apply [simp]: "zdiag u (x, y) = (if x = y then u x else 0)"
  by (simp add: zdiag_def)

lemma zdiag_add: "zdiag (zplus u v) = zplus (zdiag u) (zdiag v)"
proof (rule ext)
  fix p show "zdiag (zplus u v) p = zplus (zdiag u) (zdiag v) p"
    by (cases p) simp
qed

lemma zdiag_zzero: "zdiag zzero = zzero"
proof (rule ext)
  fix p show "zdiag zzero p = zzero p" by (cases p) simp
qed

lemma zdiag_indic: "zdiag (indic S) = indic (Id_on S)"
proof (rule ext)
  fix p show "zdiag (indic S) p = indic (Id_on S) p"
    by (cases p) (auto simp: Id_on_iff)
qed

lemma support_zdiag: "support (zdiag u) \<subseteq> (\<lambda>x. (x, x)) ` support u"
proof
  fix p assume "p \<in> support (zdiag u)"
  then have nz: "zdiag u p \<noteq> 0" by (simp add: support_def)
  show "p \<in> (\<lambda>x. (x, x)) ` support u"
  proof (cases p)
    case (Pair x y)
    with nz have "x = y" and "u x \<noteq> 0" by (auto split: if_splits)
    then show ?thesis
      by (intro rev_image_eqI[of x]) (simp_all add: Pair support_def)
  qed
qed

lemma finsupp_zdiag: "finsupp u \<Longrightarrow> finsupp (zdiag u)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zdiag]) (intro finite_imageI; assumption)

text \<open>Products of indicators.\<close>

lemma zprod_indic: "zprod (indic A) (indic B) = indic (A \<times> B)"
proof (rule ext)
  fix p show "zprod (indic A) (indic B) p = indic (A \<times> B) p"
    by (cases p) (auto simp: mem_Times_iff)
qed

lemma zprod_zzero_left: "zprod zzero v = zzero"
proof (rule ext)
  fix p show "zprod zzero v p = zzero p" by (cases p) simp
qed

lemma zflip_indic: "zflip (indic R) = indic (R\<inverse>)"
proof (rule ext)
  fix p show "zflip (indic R) p = indic (R\<inverse>) p"
    by (cases p) simp
qed

lemma isSet_zflip: "isSet a \<Longrightarrow> isSet (zflip a)"
  unfolding isSet_def
proof
  fix p assume A: "\<forall>p. a p = 0 \<or> a p = 1"
  show "zflip a p = 0 \<or> zflip a p = 1"
  proof (cases p)
    case (Pair y x)
    with A show ?thesis by simp
  qed
qed

subsection \<open>Canonical composition with its bilinear delta (D5/Z3)\<close>

text \<open>
  @{const zcomp} takes an explicit finite middle set.  The implementation sums
  over the keys of its source-indexed maps, i.e. over (a superset of) the
  middle support.  @{text zc} fixes the canonical cover
  @{term "snd ` support a"}; by @{thm [source] zcomp_cover_indep} any finite
  cover gives the same function.
\<close>

definition zc :: "('a \<times> 'a \<Rightarrow> int) \<Rightarrow> ('a \<times> 'a \<Rightarrow> int) \<Rightarrow> ('a \<times> 'a \<Rightarrow> int)" where
  "zc a b = zcomp (snd ` support a) a b"

lemma zc_eq_zcomp:
  assumes fa: "finsupp a" and M: "finite M" and cov: "midsupp a b \<subseteq> M"
  shows "zc a b = zcomp M a b"
  unfolding zc_def
proof (rule zcomp_cover_indep)
  show "finite (snd ` support a)"
    using fa unfolding finsupp_def by (intro finite_imageI)
  show "finite M" by (rule M)
  show "midsupp a b \<subseteq> snd ` support a" by (rule midsupp_subset)
  show "midsupp a b \<subseteq> M" by (rule cov)
qed

lemma finsupp_zc: "finsupp a \<Longrightarrow> finsupp b \<Longrightarrow> finsupp (zc a b)"
  unfolding zc_def by (rule finsupp_zcomp)

lemma zc_zero_left: "zc zzero b = zzero"
proof (rule ext)
  fix p show "zc zzero b p = zzero p"
    by (cases p) (simp add: zc_def)
qed

text \<open>The asymmetric bilinear expansion for @{text zc}, from Z3.\<close>

lemma zc_bilinear_delta:
  assumes fa: "finsupp a" and fda: "finsupp da"
  shows "zc (zplus a da) (zplus b db)
       = zplus (zc a b) (zplus (zc da b) (zc (zplus a da) db))"
proof -
  let ?M = "snd ` support a \<union> snd ` support da"
  have finM: "finite ?M"
    using fa fda unfolding finsupp_def by (intro finite_UnI finite_imageI)
  have covsum: "snd ` support (zplus a da) \<subseteq> ?M"
  proof -
    have "snd ` support (zplus a da) \<subseteq> snd ` (support a \<union> support da)"
      by (rule image_mono[OF support_zplus])
    also have "\<dots> = ?M" by (rule image_Un)
    finally show ?thesis .
  qed
  have c1: "zc (zplus a da) (zplus b db) = zcomp ?M (zplus a da) (zplus b db)"
    by (rule zc_eq_zcomp[OF finsupp_zplus[OF fa fda] finM])
       (rule subset_trans[OF midsupp_subset covsum])
  have c2: "zc a b = zcomp ?M a b"
    by (rule zc_eq_zcomp[OF fa finM])
       (rule subset_trans[OF midsupp_subset], blast)
  have c3: "zc da b = zcomp ?M da b"
    by (rule zc_eq_zcomp[OF fda finM])
       (rule subset_trans[OF midsupp_subset], blast)
  have c4: "zc (zplus a da) db = zcomp ?M (zplus a da) db"
    by (rule zc_eq_zcomp[OF finsupp_zplus[OF fa fda] finM])
       (rule subset_trans[OF midsupp_subset covsum])
  show ?thesis
    by (simp only: c1 c2 c3 c4 Z3_zcomp_bilinear_delta)
qed

text \<open>B4 restated for @{text zc}.\<close>

lemma zc_distinct_relcomp:
  assumes a: "isSet a" and b: "isSet b" and fa: "finsupp a"
  shows "setof (zdistinct (zc a b)) = setof a O setof b"
  unfolding zc_def
proof (rule B4_distinct_zcomp_is_relcomp[OF a b])
  show "finite (snd ` support a)"
    using fa unfolding finsupp_def by (intro finite_imageI)
  show "midsupp a b \<subseteq> snd ` support a" by (rule midsupp_subset)
qed

subsection \<open>Row and column projections (the @{text Prd} node, D5/Z4)\<close>

definition zrow :: "('a \<times> 'a \<Rightarrow> int) \<Rightarrow> 'a \<Rightarrow> int" where
  "zrow a = (\<lambda>x. \<Sum>y \<in> snd ` support a. a (x, y))"

lemma zrow_cover:
  assumes N: "finite N" and cov: "snd ` support a \<subseteq> N"
  shows "zrow a x = (\<Sum>y\<in>N. a (x, y))"
proof -
  have z: "\<forall>y\<in>N - snd ` support a. a (x, y) = 0"
  proof
    fix y assume yN: "y \<in> N - snd ` support a"
    show "a (x, y) = 0"
    proof (rule ccontr)
      assume "a (x, y) \<noteq> 0"
      then have "(x, y) \<in> support a" by (simp add: support_def)
      then have "y \<in> snd ` support a"
        by (intro rev_image_eqI[of "(x, y)"]) simp_all
      with yN show False by blast
    qed
  qed
  have "(\<Sum>y\<in>N. a (x, y)) = (\<Sum>y\<in>snd ` support a. a (x, y))"
    by (rule sum.mono_neutral_right[OF N cov z])
  then show ?thesis by (simp add: zrow_def)
qed

lemma zrow_add:
  assumes fa: "finsupp a" and fb: "finsupp b"
  shows "zrow (zplus a b) = zplus (zrow a) (zrow b)"
proof (rule ext)
  fix x
  let ?N = "snd ` support a \<union> snd ` support b"
  have finN: "finite ?N"
    using fa fb unfolding finsupp_def by (intro finite_UnI finite_imageI)
  have covab: "snd ` support (zplus a b) \<subseteq> ?N"
  proof -
    have "snd ` support (zplus a b) \<subseteq> snd ` (support a \<union> support b)"
      by (rule image_mono[OF support_zplus])
    also have "\<dots> = ?N" by (rule image_Un)
    finally show ?thesis .
  qed
  have ca: "zrow a x = (\<Sum>y\<in>?N. a (x, y))"
    by (rule zrow_cover[OF finN]) blast
  have cb: "zrow b x = (\<Sum>y\<in>?N. b (x, y))"
    by (rule zrow_cover[OF finN]) blast
  have cab: "zrow (zplus a b) x = (\<Sum>y\<in>?N. zplus a b (x, y))"
    by (rule zrow_cover[OF finN covab])
  show "zrow (zplus a b) x = zplus (zrow a) (zrow b) x"
    by (simp add: cab ca cb sum.distrib)
qed

lemma zrow_zzero: "zrow zzero = zzero"
  by (rule ext) (simp add: zrow_def)

lemma zrow_support: "support (zrow a) \<subseteq> fst ` support a"
proof
  fix x assume "x \<in> support (zrow a)"
  then have nz: "zrow a x \<noteq> 0" by (simp add: support_def)
  have "\<exists>y \<in> snd ` support a. a (x, y) \<noteq> 0"
  proof (rule ccontr)
    assume n: "\<not> (\<exists>y \<in> snd ` support a. a (x, y) \<noteq> 0)"
    have "(\<Sum>y\<in>snd ` support a. a (x, y)) = 0"
    proof (rule sum.neutral)
      show "\<forall>y\<in>snd ` support a. a (x, y) = 0" using n by auto
    qed
    with nz show False by (simp add: zrow_def)
  qed
  then obtain y where "a (x, y) \<noteq> 0" by blast
  then have "(x, y) \<in> support a" by (simp add: support_def)
  then show "x \<in> fst ` support a"
    by (intro rev_image_eqI[of "(x, y)"]) simp_all
qed

lemma zrow_nonneg: "isSet a \<Longrightarrow> 0 \<le> zrow a x"
  unfolding zrow_def by (rule sum_nonneg) (auto intro: isSet_nonneg)

lemma finsupp_zrow: "finsupp a \<Longrightarrow> finsupp (zrow a)"
  unfolding finsupp_def
  by (rule finite_subset[OF zrow_support]) (intro finite_imageI; assumption)

lemma zrow_setof:
  assumes S: "isSet a" and F: "finsupp a"
  shows "setof (zrow a) = fst ` setof a"
proof (rule set_eqI)
  fix x
  have finS: "finite (snd ` support a)"
    using F unfolding finsupp_def by (intro finite_imageI)
  show "x \<in> setof (zrow a) \<longleftrightarrow> x \<in> fst ` setof a"
  proof
    assume "x \<in> setof (zrow a)"
    then have pos: "0 < zrow a x" by (simp add: setof_def)
    have "\<exists>y \<in> snd ` support a. a (x, y) \<noteq> 0"
    proof (rule ccontr)
      assume n: "\<not> (\<exists>y \<in> snd ` support a. a (x, y) \<noteq> 0)"
      have "(\<Sum>y\<in>snd ` support a. a (x, y)) = 0"
      proof (rule sum.neutral)
        show "\<forall>y\<in>snd ` support a. a (x, y) = 0" using n by auto
      qed
      with pos show False by (simp add: zrow_def)
    qed
    then obtain y where "a (x, y) \<noteq> 0" by blast
    with S have "a (x, y) = 1" using isSet_cases[of a "(x, y)"] by auto
    then have "(x, y) \<in> setof a" by (simp add: setof_def)
    then show "x \<in> fst ` setof a"
      by (intro rev_image_eqI[of "(x, y)"]) simp_all
  next
    assume "x \<in> fst ` setof a"
    then obtain p where pin: "p \<in> setof a" and px: "x = fst p" by (rule imageE)
    obtain y where py: "p = (x, y)" using px by (cases p) auto
    have ay1: "0 < a (x, y)" using pin by (simp add: py setof_def)
    then have "(x, y) \<in> support a" by (simp add: support_def)
    then have ymem: "y \<in> snd ` support a"
      by (intro rev_image_eqI[of "(x, y)"]) simp_all
    have nn: "\<And>z. 0 \<le> a (x, z)" by (rule isSet_nonneg[OF S])
    have "(\<Sum>z\<in>snd ` support a. a (x, z))
        = a (x, y) + (\<Sum>z\<in>snd ` support a - {y}. a (x, z))"
      by (rule sum.remove[OF finS ymem])
    moreover have "0 \<le> (\<Sum>z\<in>snd ` support a - {y}. a (x, z))"
      by (rule sum_nonneg) (use nn in auto)
    ultimately have "0 < zrow a x" using ay1 by (simp add: zrow_def)
    then show "x \<in> setof (zrow a)" by (simp add: setof_def)
  qed
qed

definition zcol :: "('a \<times> 'a \<Rightarrow> int) \<Rightarrow> 'a \<Rightarrow> int" where
  "zcol a = zrow (zflip a)"

lemma zcol_add:
  assumes "finsupp a" and "finsupp b"
  shows "zcol (zplus a b) = zplus (zcol a) (zcol b)"
  unfolding zcol_def
  by (simp add: Z1_flip_additive zrow_add finsupp_zflip assms)

lemma zcol_zzero: "zcol zzero = zzero"
  unfolding zcol_def by (simp add: Z1_flip_zero zrow_zzero)

lemma finsupp_zcol: "finsupp a \<Longrightarrow> finsupp (zcol a)"
  unfolding zcol_def by (intro finsupp_zrow finsupp_zflip)

lemma fst_converse: "fst ` (R\<inverse>) = snd ` R"
proof (rule set_eqI)
  fix x
  show "x \<in> fst ` (R\<inverse>) \<longleftrightarrow> x \<in> snd ` R"
  proof
    assume "x \<in> fst ` (R\<inverse>)"
    then obtain p where pin: "p \<in> R\<inverse>" and px: "x = fst p" by (rule imageE)
    obtain y where py: "p = (x, y)" using px by (cases p) auto
    have "(y, x) \<in> R" using pin by (simp add: py)
    then show "x \<in> snd ` R" by (intro rev_image_eqI[of "(y, x)"]) simp_all
  next
    assume "x \<in> snd ` R"
    then obtain p where pin: "p \<in> R" and px: "x = snd p" by (rule imageE)
    obtain y where py: "p = (y, x)" using px by (cases p) auto
    have "(x, y) \<in> R\<inverse>" using pin by (simp add: py)
    then show "x \<in> fst ` (R\<inverse>)" by (intro rev_image_eqI[of "(x, y)"]) simp_all
  qed
qed

lemma zcol_setof:
  assumes S: "isSet a" and F: "finsupp a"
  shows "setof (zcol a) = snd ` setof a"
proof -
  have "setof (zcol a) = fst ` setof (zflip a)"
    unfolding zcol_def
    by (rule zrow_setof[OF isSet_zflip[OF S] finsupp_zflip[OF F]])
  also have "setof (zflip a) = (setof a)\<inverse>" by (rule B6_flip_is_converse)
  also have "fst ` ((setof a)\<inverse>) = snd ` setof a" by (rule fst_converse)
  finally show ?thesis .
qed

text \<open>
  From here on, reasoning is at the level of whole Z-set values, never
  pointwise.  The pointwise @{text "*_apply"} rules would make simp unfold
  operators under @{const finsupp}/@{const isSet} into raw lambdas (simp
  eta-expands them to rewrite partial applications), after which the
  compositional lemmas no longer match — so they leave the simp set here.
\<close>

declare zzero_apply [simp del] zplus_apply [simp del] zneg_apply [simp del]
  zminus_apply [simp del] zmul_apply [simp del] zdistinct_apply [simp del]
  indic_apply [simp del] zflip_apply [simp del] zprod_apply [simp del]
  zcomp_apply [simp del] zdiag_apply [simp del]

subsection \<open>The environment: relation integrals and concept-occurrence integrals\<close>

datatype ('r, 'c, 'a) env =
  Env (re: "'r \<Rightarrow> 'a \<times> 'a \<Rightarrow> int") (ce: "'c \<Rightarrow> 'a \<Rightarrow> int")

definition eadd :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) env" where
  "eadd E D = Env (\<lambda>r. zplus (re E r) (re D r)) (\<lambda>c. zplus (ce E c) (ce D c))"

lemma re_eadd [simp]: "re (eadd E D) r = zplus (re E r) (re D r)"
  by (simp add: eadd_def)

lemma ce_eadd [simp]: "ce (eadd E D) c = zplus (ce E c) (ce D c)"
  by (simp add: eadd_def)

definition ezero :: "('r, 'c, 'a) env" where
  "ezero = Env (\<lambda>_. zzero) (\<lambda>_. zzero)"

lemma re_ezero [simp]: "re ezero r = zzero"
  by (simp add: ezero_def)

lemma ce_ezero [simp]: "ce ezero c = zzero"
  by (simp add: ezero_def)

definition wf_env :: "('r, 'c, 'a) env \<Rightarrow> bool" where
  "wf_env E \<longleftrightarrow> (\<forall>r. finsupp (re E r)) \<and> (\<forall>c. finsupp (ce E c))"

lemma wf_env_re: "wf_env E \<Longrightarrow> finsupp (re E r)"
  by (simp add: wf_env_def)

lemma wf_env_ce: "wf_env E \<Longrightarrow> finsupp (ce E c)"
  by (simp add: wf_env_def)

lemma wf_env_ezero: "wf_env ezero"
  by (simp add: wf_env_def finsupp_zzero)

lemma wf_env_eadd: "wf_env E \<Longrightarrow> wf_env D \<Longrightarrow> wf_env (eadd E D)"
  by (simp add: wf_env_def finsupp_zplus)

subsection \<open>The circuit datatype\<close>

text \<open>
  Each constructor carries its state inline, exactly like the Haskell
  @{text Circuit}/@{text Kind} pair: the last field is always the node's output
  set @{text S}; internal fields per the Kind table of delta-calculus.md
  section 3.  A specification node stores its semantics as a function of the
  environment.
\<close>

definition spec_ok :: "(('r, 'c, 'a) env \<Rightarrow> ('a \<times> 'a) set) \<Rightarrow> bool" where
  "spec_ok F \<longleftrightarrow> (\<forall>E. wf_env E \<longrightarrow> finite (F E))"

datatype ('r, 'c, 'a) circuit =
    CRel 'r "'a \<times> 'a \<Rightarrow> int" "'a \<times> 'a \<Rightarrow> int"
  | CI 'c "'a \<times> 'a \<Rightarrow> int"
  | CV 'c 'c "'a \<times> 'a \<Rightarrow> int"
  | CSpec "('r, 'c, 'a) env \<Rightarrow> ('a \<times> 'a) set" "'a \<times> 'a \<Rightarrow> int"
  | CFlp "('r, 'c, 'a) circuit" "'a \<times> 'a \<Rightarrow> int"
  | CUni "('r, 'c, 'a) circuit" "('r, 'c, 'a) circuit" "'a \<times> 'a \<Rightarrow> int" "'a \<times> 'a \<Rightarrow> int"
  | CDif "('r, 'c, 'a) circuit" "('r, 'c, 'a) circuit" "'a \<times> 'a \<Rightarrow> int" "'a \<times> 'a \<Rightarrow> int"
  | CIsc "('r, 'c, 'a) circuit" "('r, 'c, 'a) circuit" "'a \<times> 'a \<Rightarrow> int"
  | CCps "('r, 'c, 'a) circuit" "('r, 'c, 'a) circuit" "'a \<times> 'a \<Rightarrow> int" "'a \<times> 'a \<Rightarrow> int" "'a \<times> 'a \<Rightarrow> int"
  | CPrd "('r, 'c, 'a) circuit" "('r, 'c, 'a) circuit" "'a \<Rightarrow> int" "'a \<Rightarrow> int" "'a \<Rightarrow> int" "'a \<Rightarrow> int" "'a \<times> 'a \<Rightarrow> int"

primrec "out" :: "('r, 'c, 'a) circuit \<Rightarrow> 'a \<times> 'a \<Rightarrow> int" where
  "out (CRel r Z S) = S"
| "out (CI c S) = S"
| "out (CV c d S) = S"
| "out (CSpec F S) = S"
| "out (CFlp a S) = S"
| "out (CUni a b Z S) = S"
| "out (CDif a b Z S) = S"
| "out (CIsc a b S) = S"
| "out (CCps a b Z Fl S) = S"
| "out (CPrd a b DW DS CW CS S) = S"

subsection \<open>Set semantics\<close>

primrec sem :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> ('a \<times> 'a) set" where
  "sem E (CRel r Z S) = setof (re E r)"
| "sem E (CI c S) = Id_on (setof (ce E c))"
| "sem E (CV c d S) = setof (ce E c) \<times> setof (ce E d)"
| "sem E (CSpec F S) = F E"
| "sem E (CFlp a S) = (sem E a)\<inverse>"
| "sem E (CUni a b Z S) = sem E a \<union> sem E b"
| "sem E (CDif a b Z S) = sem E a - sem E b"
| "sem E (CIsc a b S) = sem E a \<inter> sem E b"
| "sem E (CCps a b Z Fl S) = sem E a O sem E b"
| "sem E (CPrd a b DW DS CW CS S) = (fst ` sem E a) \<times> (snd ` sem E b)"

subsection \<open>The step: emitted delta and next state\<close>

text \<open>
  @{text "dlt E D c"} is the output delta node @{text c} emits when transaction
  delta @{text D} arrives with old environment @{text E}; @{text "nxt E D c"}
  is the stepped circuit.  Every equation is the delta rule of
  delta-calculus.md section 4 in the exact asymmetric form the implementation
  computes (D1-D6), and D7 for specification nodes.
\<close>

primrec dlt :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> 'a \<times> 'a \<Rightarrow> int" where
  \<comment> \<open>D4+D6: linear feed into the pre-distinct integral, emit through H\<close>
  "dlt E D (CRel r Z S) = H Z (re D r)"
  \<comment> \<open>D4: the diagonal of the concept's population-set delta (linear)\<close>
| "dlt E D (CI c S) = zdiag (H (ce E c) (ce D c))"
  \<comment> \<open>D5 (Z4) at set level: \<open>\<Delta>u \<otimes> old(v) + new(u) \<otimes> \<Delta>v\<close>\<close>
| "dlt E D (CV c d S) =
     zplus (zprod (H (ce E c) (ce D c)) (zdistinct (ce E d)))
           (zprod (zdistinct (zplus (ce E c) (ce D c))) (H (ce E d) (ce D d)))"
  \<comment> \<open>D7: re-evaluate the specification, emit new - old\<close>
| "dlt E D (CSpec F S) = zminus (indic (F (eadd E D))) S"
  \<comment> \<open>D1: transpose is linear\<close>
| "dlt E D (CFlp a S) = zflip (dlt E D a)"
  \<comment> \<open>D2+D6\<close>
| "dlt E D (CUni a b Z S) = H Z (zplus (dlt E D a) (dlt E D b))"
  \<comment> \<open>D3+D6\<close>
| "dlt E D (CDif a b Z S) = H Z (zminus (dlt E D a) (dlt E D b))"
  \<comment> \<open>D5 (Z2): \<open>\<Delta>a \<odot> old(b) + new(a) \<odot> \<Delta>b\<close>, sets stay sets\<close>
| "dlt E D (CIsc a b S) =
     zplus (zmul (dlt E D a) (out b))
           (zmul (zplus (out a) (dlt E D a)) (dlt E D b))"
  \<comment> \<open>D5 (Z3) + D6: \<open>\<Delta>a ; old(b) + new(a) ; \<Delta>b\<close> into the integral, emit through H\<close>
| "dlt E D (CCps a b Z Fl S) =
     H Z (zplus (zc (dlt E D a) (out b))
                (zc (zplus (out a) (dlt E D a)) (dlt E D b)))"
  \<comment> \<open>D5 (Z4) + D6 over the projection integrals\<close>
| "dlt E D (CPrd a b DW DS CW CS S) =
     zplus (zprod (H DW (zrow (dlt E D a))) CS)
           (zprod (zplus DS (H DW (zrow (dlt E D a)))) (H CW (zcol (dlt E D b))))"

primrec nxt :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> ('r, 'c, 'a) circuit" where
  "nxt E D (CRel r Z S) = CRel r (zplus Z (re D r)) (zplus S (dlt E D (CRel r Z S)))"
| "nxt E D (CI c S) = CI c (zplus S (dlt E D (CI c S)))"
| "nxt E D (CV c d S) = CV c d (zplus S (dlt E D (CV c d S)))"
| "nxt E D (CSpec F S) = CSpec F (indic (F (eadd E D)))"
| "nxt E D (CFlp a S) = CFlp (nxt E D a) (zplus S (dlt E D (CFlp a S)))"
| "nxt E D (CUni a b Z S) =
     CUni (nxt E D a) (nxt E D b)
          (zplus Z (zplus (dlt E D a) (dlt E D b)))
          (zplus S (dlt E D (CUni a b Z S)))"
| "nxt E D (CDif a b Z S) =
     CDif (nxt E D a) (nxt E D b)
          (zplus Z (zminus (dlt E D a) (dlt E D b)))
          (zplus S (dlt E D (CDif a b Z S)))"
| "nxt E D (CIsc a b S) =
     CIsc (nxt E D a) (nxt E D b) (zplus S (dlt E D (CIsc a b S)))"
| "nxt E D (CCps a b Z Fl S) =
     CCps (nxt E D a) (nxt E D b)
          (zplus Z (zplus (zc (dlt E D a) (out b))
                          (zc (zplus (out a) (dlt E D a)) (dlt E D b))))
          (zplus Fl (zflip (dlt E D a)))
          (zplus S (dlt E D (CCps a b Z Fl S)))"
| "nxt E D (CPrd a b DW DS CW CS S) =
     CPrd (nxt E D a) (nxt E D b)
          (zplus DW (zrow (dlt E D a)))
          (zplus DS (H DW (zrow (dlt E D a))))
          (zplus CW (zcol (dlt E D b)))
          (zplus CS (H CW (zcol (dlt E D b))))
          (zplus S (dlt E D (CPrd a b DW DS CW CS S)))"

text \<open>Every node's emitted delta is exact: new output = old output + delta.\<close>

lemma out_nxt: "out (nxt E D c) = zplus (out c) (dlt E D c)"
  by (cases c) (simp_all add: zplus_minus_cancel)

subsection \<open>The invariant\<close>

text \<open>
  @{text wfs} is the state invariant: every internal state field is the correct
  function of the children's current outputs (resp. of the environment for the
  leaves), and every clipped field is the clip of its integral.  Specification
  nodes only promise a set-valued, finitely supported output and a finite
  semantics — their output content is the separate invariant @{text sholds},
  which one step establishes unconditionally (their step recomputes the
  specification).  This split is what makes the backfill work: the all-zero
  base state satisfies @{text wfs} outright, and the first step — the backfill
  transaction of @{text engineInit} — establishes @{text sholds}.
\<close>

primrec wfs :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> bool" where
  "wfs E (CRel r Z S) \<longleftrightarrow> Z = re E r \<and> S = zdistinct Z"
| "wfs E (CI c S) \<longleftrightarrow> S = zdiag (zdistinct (ce E c))"
| "wfs E (CV c d S) \<longleftrightarrow> S = zprod (zdistinct (ce E c)) (zdistinct (ce E d))"
| "wfs E (CSpec F S) \<longleftrightarrow> isSet S \<and> finsupp S \<and> spec_ok F"
| "wfs E (CFlp a S) \<longleftrightarrow> S = zflip (out a) \<and> wfs E a"
| "wfs E (CUni a b Z S) \<longleftrightarrow>
     Z = zplus (out a) (out b) \<and> S = zdistinct Z \<and> wfs E a \<and> wfs E b"
| "wfs E (CDif a b Z S) \<longleftrightarrow>
     Z = zminus (out a) (out b) \<and> S = zdistinct Z \<and> wfs E a \<and> wfs E b"
| "wfs E (CIsc a b S) \<longleftrightarrow> S = zmul (out a) (out b) \<and> wfs E a \<and> wfs E b"
| "wfs E (CCps a b Z Fl S) \<longleftrightarrow>
     Z = zc (out a) (out b) \<and> Fl = zflip (out a) \<and> S = zdistinct Z \<and> wfs E a \<and> wfs E b"
| "wfs E (CPrd a b DW DS CW CS S) \<longleftrightarrow>
     DW = zrow (out a) \<and> DS = zdistinct DW \<and>
     CW = zcol (out b) \<and> CS = zdistinct CW \<and>
     S = zprod DS CS \<and> wfs E a \<and> wfs E b"

primrec sholds :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> bool" where
  "sholds E (CRel r Z S) \<longleftrightarrow> True"
| "sholds E (CI c S) \<longleftrightarrow> True"
| "sholds E (CV c d S) \<longleftrightarrow> True"
| "sholds E (CSpec F S) \<longleftrightarrow> S = indic (F E)"
| "sholds E (CFlp a S) \<longleftrightarrow> sholds E a"
| "sholds E (CUni a b Z S) \<longleftrightarrow> sholds E a \<and> sholds E b"
| "sholds E (CDif a b Z S) \<longleftrightarrow> sholds E a \<and> sholds E b"
| "sholds E (CIsc a b S) \<longleftrightarrow> sholds E a \<and> sholds E b"
| "sholds E (CCps a b Z Fl S) \<longleftrightarrow> sholds E a \<and> sholds E b"
| "sholds E (CPrd a b DW DS CW CS S) \<longleftrightarrow> sholds E a \<and> sholds E b"

definition wf_circuit :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> bool" where
  "wf_circuit E c \<longleftrightarrow> wfs E c \<and> sholds E c"

subsection \<open>Finite support of outputs and deltas\<close>

lemma out_finsupp:
  assumes E: "wf_env E"
  shows "wfs E c \<Longrightarrow> finsupp (out c)"
proof (induction c)
  case (CRel r Z S)
  then show ?case by (simp add: finsupp_zdistinct wf_env_re[OF E])
next
  case (CI c S)
  then show ?case
    by (simp add: finsupp_zdiag finsupp_zdistinct wf_env_ce[OF E])
next
  case (CV c d S)
  then show ?case
    by (simp add: finsupp_zprod finsupp_zdistinct wf_env_ce[OF E])
next
  case (CSpec F S)
  then show ?case by simp
next
  case (CFlp a S)
  then show ?case by (simp add: finsupp_zflip)
next
  case (CUni a b Z S)
  then show ?case by (simp add: finsupp_zdistinct finsupp_zplus)
next
  case (CDif a b Z S)
  then show ?case by (simp add: finsupp_zdistinct finsupp_zminus)
next
  case (CIsc a b S)
  then show ?case by (simp add: finsupp_zmul)
next
  case (CCps a b Z Fl S)
  then show ?case by (simp add: finsupp_zdistinct finsupp_zc)
next
  case (CPrd a b DW DS CW CS S)
  then show ?case
    by (simp add: finsupp_zprod finsupp_zdistinct finsupp_zrow finsupp_zcol)
qed

lemma dlt_finsupp:
  assumes E: "wf_env E" and D: "wf_env D"
  shows "wfs E c \<Longrightarrow> finsupp (dlt E D c)"
proof (induction c)
  case (CRel r Z S)
  then show ?case by (simp add: Z5_H_finsupp wf_env_re[OF D])
next
  case (CI c S)
  then show ?case by (simp add: finsupp_zdiag Z5_H_finsupp wf_env_ce[OF D])
next
  case (CV c d S)
  then show ?case
    by (simp add: finsupp_zplus finsupp_zprod Z5_H_finsupp
        finsupp_zdistinct wf_env_ce[OF D] wf_env_ce[OF E])
next
  case (CSpec F S)
  then have "finite (F (eadd E D))"
    by (auto simp: spec_ok_def wf_env_eadd[OF E D])
  with CSpec show ?case by (simp add: finsupp_zminus finsupp_indic)
next
  case (CFlp a S)
  then show ?case by (simp add: finsupp_zflip)
next
  case (CUni a b Z S)
  then show ?case by (simp add: Z5_H_finsupp finsupp_zplus)
next
  case (CDif a b Z S)
  then show ?case by (simp add: Z5_H_finsupp finsupp_zminus)
next
  case (CIsc a b S)
  then show ?case
    by (simp add: finsupp_zplus finsupp_zmul out_finsupp[OF E])
next
  case (CCps a b Z Fl S)
  then show ?case
    by (simp add: Z5_H_finsupp finsupp_zplus finsupp_zc out_finsupp[OF E])
next
  case (CPrd a b DW DS CW CS S)
  then show ?case
    by (simp add: finsupp_zplus finsupp_zprod Z5_H_finsupp finsupp_zrow
        finsupp_zcol finsupp_zdistinct out_finsupp[OF E])
qed

subsection \<open>C1: a step preserves the state invariant\<close>

text \<open>
  Obligation C1 (the structural-induction glue of delta-calculus.md section 5):
  one step keeps every state field the correct function of the children's new
  outputs and the new environment.  Each case is discharged by the
  corresponding proven delta rule: Z5 for the distinct states, Z2/Z3/Z4 in
  their asymmetric form for the bilinear nodes, Z1 linearity for flip.
\<close>

theorem C1_step_preserves_state:
  assumes E: "wf_env E" and D: "wf_env D"
  shows "wfs E c \<Longrightarrow> wfs (eadd E D) (nxt E D c)"
proof (induction c)
  case (CRel r Z S)
  then have zeq: "Z = re E r" and seq: "S = zdistinct Z" by simp_all
  show ?case
    by (simp add: zeq seq Z5_distinct_delta[symmetric])
next
  case (CI c S)
  then have seq: "S = zdiag (zdistinct (ce E c))" by simp
  have "zplus S (zdiag (H (ce E c) (ce D c)))
      = zdiag (zplus (zdistinct (ce E c)) (H (ce E c) (ce D c)))"
    by (simp add: seq zdiag_add)
  also have "\<dots> = zdiag (zdistinct (zplus (ce E c) (ce D c)))"
    by (simp add: Z5_distinct_delta[symmetric])
  finally show ?case by simp
next
  case (CV c d S)
  then have seq: "S = zprod (zdistinct (ce E c)) (zdistinct (ce E d))" by simp
  let ?u = "zdistinct (ce E c)" and ?du = "H (ce E c) (ce D c)"
  let ?v = "zdistinct (ce E d)" and ?dv = "H (ce E d) (ce D d)"
  have nu: "zdistinct (zplus (ce E c) (ce D c)) = zplus ?u ?du"
    by (rule Z5_distinct_delta)
  have nv: "zdistinct (zplus (ce E d) (ce D d)) = zplus ?v ?dv"
    by (rule Z5_distinct_delta)
  have "zplus S (zplus (zprod ?du ?v) (zprod (zdistinct (zplus (ce E c) (ce D c))) ?dv))
      = zplus (zprod ?u ?v) (zplus (zprod ?du ?v) (zprod (zplus ?u ?du) ?dv))"
    by (simp add: seq nu)
  also have "\<dots> = zprod (zplus ?u ?du) (zplus ?v ?dv)"
    by (simp add: Z4_zprod_bilinear_delta[symmetric])
  finally show ?case by (simp add: nu nv)
next
  case (CSpec F S)
  then have so: "spec_ok F" by simp
  have "finite (F (eadd E D))" using so by (auto simp: spec_ok_def wf_env_eadd[OF E D])
  then show ?case
    by (simp add: so isSet_indic finsupp_indic)
next
  case (CFlp a S)
  then have seq: "S = zflip (out a)" and wa: "wfs E a" by simp_all
  have "zplus S (zflip (dlt E D a)) = zflip (out (nxt E D a))"
    by (simp add: seq out_nxt Z1_flip_additive)
  then show ?case by (simp add: CFlp.IH[OF wa])
next
  case (CUni a b Z S)
  then have zeq: "Z = zplus (out a) (out b)" and seq: "S = zdistinct Z"
    and wa: "wfs E a" and wb: "wfs E b" by simp_all
  have newZ: "zplus Z (zplus (dlt E D a) (dlt E D b))
            = zplus (out (nxt E D a)) (out (nxt E D b))"
    by (simp add: zeq out_nxt zplus_ac)
  have newS: "zplus S (H Z (zplus (dlt E D a) (dlt E D b)))
            = zdistinct (zplus Z (zplus (dlt E D a) (dlt E D b)))"
    by (simp add: seq Z5_distinct_delta[symmetric])
  show ?case
    by (simp add: newZ newS CUni.IH(1)[OF wa] CUni.IH(2)[OF wb])
next
  case (CDif a b Z S)
  then have zeq: "Z = zminus (out a) (out b)" and seq: "S = zdistinct Z"
    and wa: "wfs E a" and wb: "wfs E b" by simp_all
  have newZ: "zplus Z (zminus (dlt E D a) (dlt E D b))
            = zminus (out (nxt E D a)) (out (nxt E D b))"
    by (simp add: zeq out_nxt zplus_minus_ac)
  have newS: "zplus S (H Z (zminus (dlt E D a) (dlt E D b)))
            = zdistinct (zplus Z (zminus (dlt E D a) (dlt E D b)))"
    by (simp add: seq Z5_distinct_delta[symmetric])
  show ?case
    by (simp add: newZ newS CDif.IH(1)[OF wa] CDif.IH(2)[OF wb])
next
  case (CIsc a b S)
  then have seq: "S = zmul (out a) (out b)"
    and wa: "wfs E a" and wb: "wfs E b" by simp_all
  have "zplus S (zplus (zmul (dlt E D a) (out b))
                       (zmul (zplus (out a) (dlt E D a)) (dlt E D b)))
      = zmul (zplus (out a) (dlt E D a)) (zplus (out b) (dlt E D b))"
    by (simp add: seq Z2_pointwise_bilinear_delta[symmetric])
  then have newS: "zplus S (zplus (zmul (dlt E D a) (out b))
                       (zmul (zplus (out a) (dlt E D a)) (dlt E D b)))
      = zmul (out (nxt E D a)) (out (nxt E D b))"
    by (simp add: out_nxt)
  show ?case
    by (simp add: newS CIsc.IH(1)[OF wa] CIsc.IH(2)[OF wb])
next
  case (CCps a b Z Fl S)
  then have zeq: "Z = zc (out a) (out b)" and fleq: "Fl = zflip (out a)"
    and seq: "S = zdistinct Z" and wa: "wfs E a" and wb: "wfs E b" by simp_all
  have fa: "finsupp (out a)" by (rule out_finsupp[OF E wa])
  have fda: "finsupp (dlt E D a)" by (rule dlt_finsupp[OF E D wa])
  have newFl: "zplus Fl (zflip (dlt E D a)) = zflip (out (nxt E D a))"
    by (simp add: fleq out_nxt Z1_flip_additive)
  have newZ: "zplus Z (zplus (zc (dlt E D a) (out b))
                             (zc (zplus (out a) (dlt E D a)) (dlt E D b)))
            = zc (out (nxt E D a)) (out (nxt E D b))"
  proof -
    have "zc (zplus (out a) (dlt E D a)) (zplus (out b) (dlt E D b))
        = zplus (zc (out a) (out b))
                (zplus (zc (dlt E D a) (out b))
                       (zc (zplus (out a) (dlt E D a)) (dlt E D b)))"
      by (rule zc_bilinear_delta[OF fa fda])
    then show ?thesis by (simp add: zeq out_nxt)
  qed
  have newS: "zplus S (H Z (zplus (zc (dlt E D a) (out b))
                                  (zc (zplus (out a) (dlt E D a)) (dlt E D b))))
            = zdistinct (zplus Z (zplus (zc (dlt E D a) (out b))
                                        (zc (zplus (out a) (dlt E D a)) (dlt E D b))))"
    by (simp add: seq Z5_distinct_delta[symmetric])
  show ?case
    by (simp add: newZ newFl newS CCps.IH(1)[OF wa] CCps.IH(2)[OF wb])
next
  case (CPrd a b DW DS CW CS S)
  then have dweq: "DW = zrow (out a)" and dseq: "DS = zdistinct DW"
    and cweq: "CW = zcol (out b)" and cseq: "CS = zdistinct CW"
    and seq: "S = zprod DS CS"
    and wa: "wfs E a" and wb: "wfs E b" by simp_all
  have fa: "finsupp (out a)" by (rule out_finsupp[OF E wa])
  have fb: "finsupp (out b)" by (rule out_finsupp[OF E wb])
  have fda: "finsupp (dlt E D a)" by (rule dlt_finsupp[OF E D wa])
  have fdb: "finsupp (dlt E D b)" by (rule dlt_finsupp[OF E D wb])
  have newDW: "zplus DW (zrow (dlt E D a)) = zrow (out (nxt E D a))"
    by (simp add: dweq out_nxt zrow_add[OF fa fda])
  have newDS: "zplus DS (H DW (zrow (dlt E D a)))
             = zdistinct (zplus DW (zrow (dlt E D a)))"
    by (simp add: dseq Z5_distinct_delta[symmetric])
  have newCW: "zplus CW (zcol (dlt E D b)) = zcol (out (nxt E D b))"
    by (simp add: cweq out_nxt zcol_add[OF fb fdb])
  have newCS: "zplus CS (H CW (zcol (dlt E D b)))
             = zdistinct (zplus CW (zcol (dlt E D b)))"
    by (simp add: cseq Z5_distinct_delta[symmetric])
  have newS: "zplus S (zplus (zprod (H DW (zrow (dlt E D a))) CS)
                             (zprod (zplus DS (H DW (zrow (dlt E D a))))
                                    (H CW (zcol (dlt E D b)))))
            = zprod (zplus DS (H DW (zrow (dlt E D a))))
                    (zplus CS (H CW (zcol (dlt E D b))))"
    by (simp add: seq Z4_zprod_bilinear_delta[symmetric])
  show ?case
    unfolding nxt.simps dlt.simps wfs.simps
    by (intro conjI newDW newDS newCW newCS newS
        CPrd.IH(1)[OF wa] CPrd.IH(2)[OF wb])
qed

subsection \<open>C2: one step establishes the specification-node invariant\<close>

text \<open>
  Specification nodes recompute; their output is correct after every step, with
  no assumption on the state before it.  This is the formal counterpart of
  "backfill is the first transaction": the base state need not know the
  constants (@{text Mp1} content, @{text "I[ONE]"}) — the first step emits
  them.
\<close>

theorem C2_step_establishes_spec: "sholds (eadd E D) (nxt E D c)"
  by (induction c) simp_all

subsection \<open>C3: a well-formed circuit's outputs are its semantics\<close>

lemma sem_finite:
  assumes E: "wf_env E"
  shows "wfs E c \<Longrightarrow> finite (sem E c)"
proof (induction c)
  case (CRel r Z S)
  show ?case by (simp add: finite_setof wf_env_re[OF E])
next
  case (CI c S)
  have "Id_on (setof (ce E c)) \<subseteq> setof (ce E c) \<times> setof (ce E c)"
    by (rule Id_on_subset_Times)
  then show ?case
    using finite_setof[OF wf_env_ce[OF E]]
    by (simp add: finite_subset finite_cartesian_product)
next
  case (CV c d S)
  show ?case
    by (simp add: finite_cartesian_product finite_setof wf_env_ce[OF E])
next
  case (CSpec F S)
  then show ?case by (auto simp: spec_ok_def E)
next
  case (CFlp a S)
  then show ?case by (simp add: finite_converse)
next
  case (CUni a b Z S)
  then show ?case by simp
next
  case (CDif a b Z S)
  then show ?case by (simp add: finite_Diff)
next
  case (CIsc a b S)
  then show ?case by (simp add: finite_Int)
next
  case (CCps a b Z Fl S)
  then have fa: "finite (sem E a)" and fb: "finite (sem E b)" by simp_all
  have sub: "sem E a O sem E b \<subseteq> (fst ` sem E a) \<times> (snd ` sem E b)"
  proof
    fix p assume "p \<in> sem E a O sem E b"
    then obtain x y z where p: "p = (x, z)"
      and xy: "(x, y) \<in> sem E a" and yz: "(y, z) \<in> sem E b"
      by (blast elim: relcompE)
    have "x \<in> fst ` sem E a"
      using xy by (intro rev_image_eqI[of "(x, y)"]) simp_all
    moreover have "z \<in> snd ` sem E b"
      using yz by (intro rev_image_eqI[of "(y, z)"]) simp_all
    ultimately show "p \<in> (fst ` sem E a) \<times> (snd ` sem E b)"
      by (simp add: p mem_Times_iff)
  qed
  have "finite ((fst ` sem E a) \<times> (snd ` sem E b))"
    by (intro finite_cartesian_product finite_imageI fa fb)
  then show ?case by (simp add: finite_subset[OF sub])
next
  case (CPrd a b DW DS CW CS S)
  then show ?case
    by (simp add: finite_cartesian_product finite_imageI)
qed

theorem C3_output_is_semantics:
  assumes E: "wf_env E"
  shows "wfs E c \<Longrightarrow> sholds E c \<Longrightarrow> out c = indic (sem E c)"
proof (induction c)
  case (CRel r Z S)
  then show ?case by (simp add: zdistinct_as_indic)
next
  case (CI c S)
  then show ?case by (simp add: zdistinct_as_indic zdiag_indic)
next
  case (CV c d S)
  then show ?case by (simp add: zdistinct_as_indic zprod_indic)
next
  case (CSpec F S)
  then show ?case by simp
next
  case (CFlp a S)
  then show ?case by (simp add: zflip_indic)
next
  case (CUni a b Z S)
  then have ia: "out a = indic (sem E a)" and ib: "out b = indic (sem E b)"
    by simp_all
  have u: "zdistinct (zplus (indic (sem E a)) (indic (sem E b)))
         = indic (sem E a \<union> sem E b)"
    using B1_distinct_plus_is_union[OF isSet_indic isSet_indic,
        of "sem E a" "sem E b"]
    by simp
  show ?case using CUni.prems by (simp add: ia ib u)
next
  case (CDif a b Z S)
  then have ia: "out a = indic (sem E a)" and ib: "out b = indic (sem E b)"
    by simp_all
  have u: "zdistinct (zminus (indic (sem E a)) (indic (sem E b)))
         = indic (sem E a - sem E b)"
    using B2_distinct_minus_is_difference[OF isSet_indic isSet_indic,
        of "sem E a" "sem E b"]
    by simp
  show ?case using CDif.prems by (simp add: ia ib u)
next
  case (CIsc a b S)
  then have ia: "out a = indic (sem E a)" and ib: "out b = indic (sem E b)"
    by simp_all
  have u: "zmul (indic (sem E a)) (indic (sem E b))
         = indic (sem E a \<inter> sem E b)"
    using B3_mul_is_intersection[OF isSet_indic isSet_indic,
        of "sem E a" "sem E b"]
    by simp
  show ?case using CIsc.prems by (simp add: ia ib u)
next
  case (CCps a b Z Fl S)
  then have ia: "out a = indic (sem E a)" and ib: "out b = indic (sem E b)"
    and wa: "wfs E a" by simp_all
  have fina: "finite (sem E a)" by (rule sem_finite[OF E wa])
  have st: "setof (zc (out a) (out b)) = sem E a O sem E b"
    using zc_distinct_relcomp[of "out a" "out b"]
    by (simp add: ia ib isSet_indic finsupp_indic fina)
  show ?case
    using CCps.prems by (simp add: zdistinct_as_indic st)
next
  case (CPrd a b DW DS CW CS S)
  then have ia: "out a = indic (sem E a)" and ib: "out b = indic (sem E b)"
    and wa: "wfs E a" and wb: "wfs E b" by simp_all
  have fina: "finite (sem E a)" by (rule sem_finite[OF E wa])
  have finb: "finite (sem E b)" by (rule sem_finite[OF E wb])
  have dom: "setof (zrow (out a)) = fst ` sem E a"
    by (simp add: ia zrow_setof isSet_indic finsupp_indic fina)
  have cod: "setof (zcol (out b)) = snd ` sem E b"
    by (simp add: ib zcol_setof isSet_indic finsupp_indic finb)
  have pr: "zprod (zdistinct (zrow (out a))) (zdistinct (zcol (out b)))
      = indic ((fst ` sem E a) \<times> (snd ` sem E b))"
    by (simp add: zdistinct_as_indic dom cod zprod_indic)
  show ?case using CPrd.prems by (simp add: pr)
qed

corollary C3_setof_output:
  assumes "wf_env E" "wfs E c" "sholds E c"
  shows "setof (out c) = sem E c"
  by (simp add: C3_output_is_semantics[OF assms] setof_indic)

subsection \<open>C4: the base state\<close>

text \<open>
  The all-zero circuit under the all-zero environment satisfies the state
  invariant.  Together with C1/C2 this makes the backfill — the initial
  population arriving as the first transaction, @{text engineInit} — an
  ordinary step: after it, the full invariant holds.
\<close>

primrec fresh :: "('r, 'c, 'a) circuit \<Rightarrow> bool" where
  "fresh (CRel r Z S) \<longleftrightarrow> Z = zzero \<and> S = zzero"
| "fresh (CI c S) \<longleftrightarrow> S = zzero"
| "fresh (CV c d S) \<longleftrightarrow> S = zzero"
| "fresh (CSpec F S) \<longleftrightarrow> S = zzero"
| "fresh (CFlp a S) \<longleftrightarrow> S = zzero \<and> fresh a"
| "fresh (CUni a b Z S) \<longleftrightarrow> Z = zzero \<and> S = zzero \<and> fresh a \<and> fresh b"
| "fresh (CDif a b Z S) \<longleftrightarrow> Z = zzero \<and> S = zzero \<and> fresh a \<and> fresh b"
| "fresh (CIsc a b S) \<longleftrightarrow> S = zzero \<and> fresh a \<and> fresh b"
| "fresh (CCps a b Z Fl S) \<longleftrightarrow> Z = zzero \<and> Fl = zzero \<and> S = zzero \<and> fresh a \<and> fresh b"
| "fresh (CPrd a b DW DS CW CS S) \<longleftrightarrow>
     DW = zzero \<and> DS = zzero \<and> CW = zzero \<and> CS = zzero \<and> S = zzero \<and> fresh a \<and> fresh b"

primrec specs_ok :: "('r, 'c, 'a) circuit \<Rightarrow> bool" where
  "specs_ok (CRel r Z S) \<longleftrightarrow> True"
| "specs_ok (CI c S) \<longleftrightarrow> True"
| "specs_ok (CV c d S) \<longleftrightarrow> True"
| "specs_ok (CSpec F S) \<longleftrightarrow> spec_ok F"
| "specs_ok (CFlp a S) \<longleftrightarrow> specs_ok a"
| "specs_ok (CUni a b Z S) \<longleftrightarrow> specs_ok a \<and> specs_ok b"
| "specs_ok (CDif a b Z S) \<longleftrightarrow> specs_ok a \<and> specs_ok b"
| "specs_ok (CIsc a b S) \<longleftrightarrow> specs_ok a \<and> specs_ok b"
| "specs_ok (CCps a b Z Fl S) \<longleftrightarrow> specs_ok a \<and> specs_ok b"
| "specs_ok (CPrd a b DW DS CW CS S) \<longleftrightarrow> specs_ok a \<and> specs_ok b"

lemma fresh_out: "fresh c \<Longrightarrow> out c = zzero"
  by (cases c) simp_all

theorem C4_backfill_base:
  "specs_ok c \<Longrightarrow> fresh c \<Longrightarrow> wfs ezero c"
  by (induction c)
     (simp_all add: fresh_out zdistinct_zzero zdiag_zzero zprod_zzero_left
        zmul_zzero_left zplus_zzero_zzero zminus_zzero_zzero zrow_zzero
        zcol_zzero zc_zero_left Z1_flip_zero isSet_zzero finsupp_zzero)

subsection \<open>C5: every reachable state is correct\<close>

text \<open>
  A run folds a list of transaction deltas over the circuit; the environment
  accumulates linearly, as in @{text applyTx}.  Starting from the all-zero base
  and applying at least one delta (the backfill), every node's clipped output
  is the set semantics of its term under the accumulated population — for every
  transaction sequence.
\<close>

fun run :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) env list \<Rightarrow> ('r, 'c, 'a) circuit \<Rightarrow> ('r, 'c, 'a) circuit" where
  "run E [] c = c"
| "run E (D # Ds) c = run (eadd E D) Ds (nxt E D c)"

fun runenv :: "('r, 'c, 'a) env \<Rightarrow> ('r, 'c, 'a) env list \<Rightarrow> ('r, 'c, 'a) env" where
  "runenv E [] = E"
| "runenv E (D # Ds) = runenv (eadd E D) Ds"

text \<open>A step changes state, not structure: the semantics reading is stable.\<close>

lemma sem_nxt: "sem X (nxt E D c) = sem X c"
  by (induction c) simp_all

lemma sem_run: "sem X (run E Ds c) = sem X c"
  by (induction Ds arbitrary: E c) (simp_all add: sem_nxt)

lemma runenv_wf_env:
  "wf_env E \<Longrightarrow> \<forall>D\<in>set Ds. wf_env D \<Longrightarrow> wf_env (runenv E Ds)"
  by (induction Ds arbitrary: E) (simp_all add: wf_env_eadd)

lemma run_wf:
  "wf_env E \<Longrightarrow> \<forall>D\<in>set Ds. wf_env D \<Longrightarrow> wfs E c \<Longrightarrow> Ds \<noteq> [] \<Longrightarrow>
   wf_circuit (runenv E Ds) (run E Ds c)"
proof (induction Ds arbitrary: E c)
  case Nil
  then show ?case by simp
next
  case (Cons D Ds)
  have wE: "wf_env E" and wD: "wf_env D" using Cons.prems by simp_all
  have step_wfs: "wfs (eadd E D) (nxt E D c)"
    by (rule C1_step_preserves_state[OF wE wD]) (rule Cons.prems(3))
  show ?case
  proof (cases Ds)
    case Nil
    have "sholds (eadd E D) (nxt E D c)" by (rule C2_step_establishes_spec)
    with step_wfs Nil show ?thesis by (simp add: wf_circuit_def)
  next
    case (Cons D' Ds')
    then have ne: "Ds \<noteq> []" by simp
    show ?thesis
      using Cons.prems(2)
      by (simp add: Cons.IH[OF wf_env_eadd[OF wE wD] _ step_wfs ne])
  qed
qed

theorem C5_run_correct:
  assumes ok: "specs_ok c" and base: "fresh c"
    and ne: "Ds \<noteq> []" and D: "\<forall>D\<in>set Ds. wf_env D"
  shows "setof (out (run ezero Ds c)) = sem (runenv ezero Ds) c"
proof -
  have w0: "wfs ezero c" by (rule C4_backfill_base[OF ok base])
  have wf: "wf_circuit (runenv ezero Ds) (run ezero Ds c)"
    by (rule run_wf[OF wf_env_ezero D w0 ne])
  have wE: "wf_env (runenv ezero Ds)"
    by (rule runenv_wf_env[OF wf_env_ezero D])
  have "setof (out (run ezero Ds c)) = sem (runenv ezero Ds) (run ezero Ds c)"
    using wf by (intro C3_setof_output[OF wE]) (simp_all add: wf_circuit_def)
  then show ?thesis by (simp add: sem_run)
qed

end
