theory ZSet
  imports Main
begin

section \<open>Z-sets: integer-weighted sets with finite support\<close>

text \<open>
  Model for memorybank/incremental-evaluation/delta-calculus.md, section 1.
  A Z-set over a domain @{typ 'p} is a map @{typ "'p \<Rightarrow> int"}; the implementation
  invariant "no key maps to 0" corresponds to finite support here. Binary Z-sets
  are Z-sets over a pair type @{typ "'x \<times> 'y"}, matching the house style of
  proofs/spike/Ampersand_RA.thy (one universe, relations as pair sets).
\<close>

definition support :: "('p \<Rightarrow> int) \<Rightarrow> 'p set" where
  "support f = {p. f p \<noteq> 0}"

definition finsupp :: "('p \<Rightarrow> int) \<Rightarrow> bool" where
  "finsupp f \<longleftrightarrow> finite (support f)"

subsection \<open>The operations\<close>

definition zzero :: "'p \<Rightarrow> int" where
  "zzero = (\<lambda>p. 0)"

definition zplus :: "('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int)" where
  "zplus f g = (\<lambda>p. f p + g p)"

definition zneg :: "('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int)" where
  "zneg f = (\<lambda>p. - f p)"

definition zminus :: "('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int)" where
  "zminus f g = (\<lambda>p. f p - g p)"

text \<open>\<open>distinct\<close> of the delta calculus (name \<open>zdistinct\<close> to avoid HOL's list @{const distinct}).\<close>

definition zdistinct :: "('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int)" where
  "zdistinct f = (\<lambda>p. if f p > 0 then 1 else 0)"

text \<open>A Z-set is a *set* when every weight is 0 or 1.\<close>

definition isSet :: "('p \<Rightarrow> int) \<Rightarrow> bool" where
  "isSet f \<longleftrightarrow> (\<forall>p. f p = 0 \<or> f p = 1)"

text \<open>The underlying set of a Z-set, and the indicator of a set.\<close>

definition setof :: "('p \<Rightarrow> int) \<Rightarrow> 'p set" where
  "setof f = {p. f p > 0}"

definition indic :: "'p set \<Rightarrow> ('p \<Rightarrow> int)" where
  "indic S = (\<lambda>p. if p \<in> S then 1 else 0)"

text \<open>Pointwise product (the \<open>Isc\<close> node).\<close>

definition zmul :: "('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int)" where
  "zmul f g = (\<lambda>p. f p * g p)"

text \<open>Transpose (the \<open>Flp\<close> node).\<close>

definition zflip :: "('x \<times> 'y \<Rightarrow> int) \<Rightarrow> ('y \<times> 'x \<Rightarrow> int)" where
  "zflip a = (\<lambda>(y, x). a (x, y))"

text \<open>
  Composition (the \<open>Cps\<close> node): \<open>(a ; b)(x,z) = \<Sum>\<^sub>y a(x,y) \<cdot> b(y,z)\<close>.
  The sum is parameterized by a finite middle set \<open>M\<close>; the result does not
  depend on the choice of \<open>M\<close> as long as \<open>M\<close> covers @{term "midsupp a b"}
  (lemma \<open>zcomp_cover_indep\<close> below). This mirrors the implementation, which
  sums over the finitely many keys of the source-indexed maps.
\<close>

definition zcomp :: "'y set \<Rightarrow> ('x \<times> 'y \<Rightarrow> int) \<Rightarrow> ('y \<times> 'z \<Rightarrow> int) \<Rightarrow> ('x \<times> 'z \<Rightarrow> int)" where
  "zcomp M a b = (\<lambda>(x, z). \<Sum>y\<in>M. a (x, y) * b (y, z))"

text \<open>The middle atoms that can contribute to some composition entry.\<close>

definition midsupp :: "('x \<times> 'y \<Rightarrow> int) \<Rightarrow> ('y \<times> 'z \<Rightarrow> int) \<Rightarrow> 'y set" where
  "midsupp a b = {y. (\<exists>x. a (x, y) \<noteq> 0) \<and> (\<exists>z. b (y, z) \<noteq> 0)}"

text \<open>Unary weighted cartesian product (the \<open>Prd\<close> and \<open>V\<close> nodes).\<close>

definition zprod :: "('x \<Rightarrow> int) \<Rightarrow> ('y \<Rightarrow> int) \<Rightarrow> ('x \<times> 'y \<Rightarrow> int)" where
  "zprod u v = (\<lambda>(x, y). u x * v y)"

subsection \<open>Application rules\<close>

lemma zzero_apply [simp]: "zzero p = 0" by (simp add: zzero_def)
lemma zplus_apply [simp]: "zplus f g p = f p + g p" by (simp add: zplus_def)
lemma zneg_apply [simp]: "zneg f p = - f p" by (simp add: zneg_def)
lemma zminus_apply [simp]: "zminus f g p = f p - g p" by (simp add: zminus_def)
lemma zmul_apply [simp]: "zmul f g p = f p * g p" by (simp add: zmul_def)
lemma zdistinct_apply [simp]: "zdistinct f p = (if f p > 0 then 1 else 0)"
  by (simp add: zdistinct_def)
lemma indic_apply [simp]: "indic S p = (if p \<in> S then 1 else 0)" by (simp add: indic_def)
lemma zflip_apply [simp]: "zflip a (y, x) = a (x, y)" by (simp add: zflip_def)
lemma zprod_apply [simp]: "zprod u v (x, y) = u x * v y" by (simp add: zprod_def)
lemma zcomp_apply [simp]: "zcomp M a b (x, z) = (\<Sum>y\<in>M. a (x, y) * b (y, z))"
  by (simp add: zcomp_def)

subsection \<open>isSet characterisations\<close>

lemma isSet_cases: "isSet f \<Longrightarrow> f p = 0 \<or> f p = 1"
  by (simp add: isSet_def)

lemma isSet_nonneg: "isSet f \<Longrightarrow> 0 \<le> f p"
  using isSet_cases[of f p] by auto

text \<open>"all weights in {0,1}" is the same as "@{const zdistinct} is the identity".\<close>

lemma isSet_alt: "isSet f \<longleftrightarrow> zdistinct f = f"
proof
  assume A: "isSet f"
  show "zdistinct f = f"
  proof (rule ext)
    fix p
    from A have "f p = 0 \<or> f p = 1" by (simp add: isSet_def)
    then show "zdistinct f p = f p" by auto
  qed
next
  assume B: "zdistinct f = f"
  show "isSet f"
    unfolding isSet_def
  proof
    fix p
    from B have "zdistinct f p = f p" by simp
    then show "f p = 0 \<or> f p = 1" by (auto split: if_splits)
  qed
qed

subsection \<open>Support lemmas (closure of the finite-support carrier)\<close>

lemma support_zzero [simp]: "support zzero = {}"
  by (simp add: support_def)

lemma support_zplus: "support (zplus f g) \<subseteq> support f \<union> support g"
  by (auto simp: support_def)

lemma support_zneg [simp]: "support (zneg f) = support f"
  by (auto simp: support_def)

lemma support_zminus: "support (zminus f g) \<subseteq> support f \<union> support g"
  by (auto simp: support_def)

lemma support_zmul: "support (zmul f g) \<subseteq> support f \<inter> support g"
  by (auto simp: support_def)

lemma support_zmul_left: "support (zmul f g) \<subseteq> support f"
  by (auto simp: support_def)

lemma support_zdistinct: "support (zdistinct f) \<subseteq> support f"
  by (auto simp: support_def)

lemma support_zprod: "support (zprod u v) \<subseteq> support u \<times> support v"
  by (auto simp: support_def zprod_def case_prod_beta mem_Times_iff)

lemma support_zflip: "support (zflip a) = (support a)\<inverse>"
  by (auto simp: support_def zflip_def split: prod.splits)

lemma finsupp_zzero: "finsupp zzero"
  by (simp add: finsupp_def)

lemma finsupp_zplus: "finsupp f \<Longrightarrow> finsupp g \<Longrightarrow> finsupp (zplus f g)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zplus]) simp

lemma finsupp_zneg: "finsupp f \<Longrightarrow> finsupp (zneg f)"
  by (simp add: finsupp_def)

lemma finsupp_zminus: "finsupp f \<Longrightarrow> finsupp g \<Longrightarrow> finsupp (zminus f g)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zminus]) simp

lemma finsupp_zmul: "finsupp f \<Longrightarrow> finsupp (zmul f g)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zmul_left]) assumption

lemma finsupp_zdistinct: "finsupp f \<Longrightarrow> finsupp (zdistinct f)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zdistinct]) assumption

lemma finsupp_zprod: "finsupp u \<Longrightarrow> finsupp v \<Longrightarrow> finsupp (zprod u v)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zprod])
     (intro finite_cartesian_product; assumption)

lemma finsupp_zflip: "finsupp a \<Longrightarrow> finsupp (zflip a)"
  unfolding finsupp_def by (simp add: support_zflip)

subsection \<open>Composition: witnesses, support, cover independence\<close>

lemma zcomp_nonzero_witness:
  assumes "zcomp M a b (x, z) \<noteq> 0"
  shows "\<exists>y\<in>M. a (x, y) \<noteq> 0 \<and> b (y, z) \<noteq> 0"
proof (rule ccontr)
  assume n: "\<not> (\<exists>y\<in>M. a (x, y) \<noteq> 0 \<and> b (y, z) \<noteq> 0)"
  have "(\<Sum>y\<in>M. a (x, y) * b (y, z)) = 0"
  proof (rule sum.neutral)
    show "\<forall>y\<in>M. a (x, y) * b (y, z) = 0" using n by auto
  qed
  with assms show False by simp
qed

lemma support_zcomp: "support (zcomp M a b) \<subseteq> (fst ` support a) \<times> (snd ` support b)"
proof
  fix p assume "p \<in> support (zcomp M a b)"
  moreover obtain x z where p: "p = (x, z)" by fastforce
  ultimately have nz: "zcomp M a b (x, z) \<noteq> 0" by (simp add: support_def)
  from zcomp_nonzero_witness[OF nz]
  obtain y where "y \<in> M" and ay: "a (x, y) \<noteq> 0" and byz: "b (y, z) \<noteq> 0"
    by blast
  from ay have x_in: "x \<in> fst ` support a"
    by (intro rev_image_eqI[of "(x, y)"]) (simp_all add: support_def)
  from byz have z_in: "z \<in> snd ` support b"
    by (intro rev_image_eqI[of "(y, z)"]) (simp_all add: support_def)
  show "p \<in> (fst ` support a) \<times> (snd ` support b)"
    by (simp add: p mem_Times_iff x_in z_in)
qed

lemma finsupp_zcomp: "finsupp a \<Longrightarrow> finsupp b \<Longrightarrow> finsupp (zcomp M a b)"
  unfolding finsupp_def
  by (rule finite_subset[OF support_zcomp])
     (intro finite_cartesian_product finite_imageI; assumption)

lemma midsupp_subset: "midsupp a b \<subseteq> snd ` support a"
proof
  fix y assume "y \<in> midsupp a b"
  then obtain x where "a (x, y) \<noteq> 0" by (auto simp: midsupp_def)
  then show "y \<in> snd ` support a"
    by (intro rev_image_eqI[of "(x, y)"]) (simp_all add: support_def)
qed

lemma finite_midsupp: "finsupp a \<Longrightarrow> finite (midsupp a b)"
  unfolding finsupp_def
  by (rule finite_subset[OF midsupp_subset])
     (intro finite_imageI; assumption)

text \<open>The composition does not depend on the choice of the finite covering set.\<close>

lemma zcomp_cover_indep:
  assumes "finite M" "finite M'" "midsupp a b \<subseteq> M" "midsupp a b \<subseteq> M'"
  shows "zcomp M a b = zcomp M' a b"
proof (rule ext)
  fix p
  show "zcomp M a b p = zcomp M' a b p"
  proof (cases p)
    case (Pair x z)
    have drop: "(\<Sum>y\<in>N. a (x, y) * b (y, z)) = (\<Sum>y\<in>midsupp a b. a (x, y) * b (y, z))"
      if "finite N" and "midsupp a b \<subseteq> N" for N
      using that by (intro sum.mono_neutral_right) (auto simp: midsupp_def)
    from drop[OF assms(1) assms(3)] drop[OF assms(2) assms(4)]
    show ?thesis by (simp add: Pair)
  qed
qed

subsection \<open>setof and zdistinct\<close>

lemma setof_zdistinct [simp]: "setof (zdistinct f) = setof f"
  by (auto simp: setof_def)

lemma setof_indic [simp]: "setof (indic S) = S"
  by (auto simp: setof_def)

lemma isSet_indic: "isSet (indic S)"
  by (simp add: isSet_def)

lemma isSet_zdistinct: "isSet (zdistinct f)"
  by (simp add: isSet_def)

end
