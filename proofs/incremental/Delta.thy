theory Delta
  imports ZSet
begin

section \<open>The delta obligations Z1..Z5\<close>

text \<open>
  Obligations from memorybank/incremental-evaluation/delta-calculus.md, section 5
  (Z-set level). The linear D-rules of section 4 are instances:
  D1 is \<open>Z1_flip_additive\<close>/\<open>Z1_flip_minus\<close>, D2/D3/D4 are the group laws of Z1,
  D5 is Z2/Z3/Z4, D6 is Z5.
\<close>

subsection \<open>Z1: abelian group; +, -, flip are linear (their own delta)\<close>

(* Z1 (delta-calculus.md section 5): Z[p] is an abelian group under pointwise +. *)
lemma Z1_zplus_assoc: "zplus (zplus f g) h = zplus f (zplus g h)"
  by (rule ext) simp

lemma Z1_zplus_comm: "zplus f g = zplus g f"
  by (rule ext) simp

lemma Z1_zplus_zero_left: "zplus zzero f = f"
  by (rule ext) simp

lemma Z1_zplus_zero_right: "zplus f zzero = f"
  by (rule ext) simp

lemma Z1_zplus_inverse: "zplus f (zneg f) = zzero"
  by (rule ext) simp

lemma Z1_zminus_is_plus_neg: "zminus f g = zplus f (zneg g)"
  by (rule ext) simp

(* Z1: the finite-support carrier is closed under the group operations
   (proved in ZSet.thy: finsupp_zzero, finsupp_zplus, finsupp_zneg, finsupp_zminus). *)
lemmas Z1_finsupp_closure =
  finsupp_zzero finsupp_zplus finsupp_zneg finsupp_zminus finsupp_zflip

(* Z1: flip is linear, hence its own delta rule (D1): flip commutes with +, -, 0. *)
lemma Z1_flip_additive: "zflip (zplus a b) = zplus (zflip a) (zflip b)"
proof (rule ext)
  fix p show "zflip (zplus a b) p = zplus (zflip a) (zflip b) p"
    by (cases p) simp
qed

lemma Z1_flip_neg: "zflip (zneg a) = zneg (zflip a)"
proof (rule ext)
  fix p show "zflip (zneg a) p = zneg (zflip a) p"
    by (cases p) simp
qed

lemma Z1_flip_minus: "zflip (zminus a b) = zminus (zflip a) (zflip b)"
proof (rule ext)
  fix p show "zflip (zminus a b) p = zminus (zflip a) (zflip b) p"
    by (cases p) simp
qed

lemma Z1_flip_zero: "zflip zzero = zzero"
proof (rule ext)
  fix p show "zflip zzero p = zzero p"
    by (cases p) simp
qed

lemma Z1_flip_involution: "zflip (zflip a) = a"
proof (rule ext)
  fix p show "zflip (zflip a) p = a p"
    by (cases p) simp
qed

subsection \<open>Z2: pointwise product, asymmetric bilinear delta expansion (D5)\<close>

(* Z2 (delta-calculus.md section 5): (a+da) \<odot> (b+db) = a\<odot>b + da\<odot>b + (a+da)\<odot>db.
   Note the absence of a separate da\<odot>db term: it is absorbed by (a+da)\<odot>db. *)
lemma Z2_pointwise_bilinear_delta:
  "zmul (zplus a da) (zplus b db)
     = zplus (zmul a b) (zplus (zmul da b) (zmul (zplus a da) db))"
  by (rule ext) (simp add: algebra_simps)

subsection \<open>Z3: composition, same expansion (finite middle cover)\<close>

(* Z3 (delta-calculus.md section 5): (a+da);(b+db) = a;b + da;b + (a+da);db.
   Stated for one fixed middle set M; ZSet.zcomp_cover_indep shows the choice of
   a finite cover of midsupp does not matter, and ZSet.finite_midsupp shows a
   finite cover exists whenever the inputs have finite support. *)

lemma Z3_zcomp_linear_left:
  "zcomp M (zplus a a') b = zplus (zcomp M a b) (zcomp M a' b)"
proof (rule ext)
  fix p
  show "zcomp M (zplus a a') b p = zplus (zcomp M a b) (zcomp M a' b) p"
  proof (cases p)
    case (Pair x z)
    have "(\<Sum>y\<in>M. (a (x, y) + a' (x, y)) * b (y, z))
        = (\<Sum>y\<in>M. a (x, y) * b (y, z) + a' (x, y) * b (y, z))"
      by (rule sum.cong) (auto simp: algebra_simps)
    also have "\<dots> = (\<Sum>y\<in>M. a (x, y) * b (y, z)) + (\<Sum>y\<in>M. a' (x, y) * b (y, z))"
      by (simp add: sum.distrib)
    finally show ?thesis by (simp add: Pair)
  qed
qed

lemma Z3_zcomp_linear_right:
  "zcomp M a (zplus b b') = zplus (zcomp M a b) (zcomp M a b')"
proof (rule ext)
  fix p
  show "zcomp M a (zplus b b') p = zplus (zcomp M a b) (zcomp M a b') p"
  proof (cases p)
    case (Pair x z)
    have "(\<Sum>y\<in>M. a (x, y) * (b (y, z) + b' (y, z)))
        = (\<Sum>y\<in>M. a (x, y) * b (y, z) + a (x, y) * b' (y, z))"
      by (rule sum.cong) (auto simp: algebra_simps)
    also have "\<dots> = (\<Sum>y\<in>M. a (x, y) * b (y, z)) + (\<Sum>y\<in>M. a (x, y) * b' (y, z))"
      by (simp add: sum.distrib)
    finally show ?thesis by (simp add: Pair)
  qed
qed

lemma Z3_zcomp_bilinear_delta:
  "zcomp M (zplus a da) (zplus b db)
     = zplus (zcomp M a b) (zplus (zcomp M da b) (zcomp M (zplus a da) db))"
proof (rule ext)
  fix p
  show "zcomp M (zplus a da) (zplus b db) p
      = zplus (zcomp M a b) (zplus (zcomp M da b) (zcomp M (zplus a da) db)) p"
  proof (cases p)
    case (Pair x z)
    have "(\<Sum>y\<in>M. (a (x, y) + da (x, y)) * (b (y, z) + db (y, z)))
        = (\<Sum>y\<in>M. a (x, y) * b (y, z)
                + (da (x, y) * b (y, z) + (a (x, y) + da (x, y)) * db (y, z)))"
      by (rule sum.cong) (auto simp: algebra_simps)
    also have "\<dots> = (\<Sum>y\<in>M. a (x, y) * b (y, z))
                + ((\<Sum>y\<in>M. da (x, y) * b (y, z))
                +  (\<Sum>y\<in>M. (a (x, y) + da (x, y)) * db (y, z)))"
      by (simp add: sum.distrib)
    finally show ?thesis by (simp add: Pair)
  qed
qed

subsection \<open>Z4: weighted cartesian product, same expansion\<close>

(* Z4 (delta-calculus.md section 5): the same asymmetric bilinear expansion for
   the weighted cartesian product of unary Z-sets (the Prd and V nodes). *)
lemma Z4_zprod_bilinear_delta:
  "zprod (zplus u du) (zplus v dv)
     = zplus (zprod u v) (zplus (zprod du v) (zprod (zplus u du) dv))"
proof (rule ext)
  fix p show "zprod (zplus u du) (zplus v dv) p
      = zplus (zprod u v) (zplus (zprod du v) (zprod (zplus u du) dv)) p"
    by (cases p) (simp add: algebra_simps)
qed

subsection \<open>Z5: the zero-crossing delta of distinct (D6)\<close>

text \<open>
  \<open>H Z dZ\<close> is defined as the difference of the clipped values, so
  \<open>distinct(Z+dZ) = distinct Z + H Z dZ\<close> holds definitionally. The two real
  claims are (a) \<open>H\<close> is computable from sign changes alone (the explicit case
  form used by the implementation), and (b) its support is bounded by the
  incoming delta, so the work is bounded by the delta.
\<close>

definition H :: "('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int) \<Rightarrow> ('p \<Rightarrow> int)" where
  "H Z dZ = (\<lambda>x. zdistinct (zplus Z dZ) x - zdistinct Z x)"

(* Z5 (delta-calculus.md section 5): distinct(Z+\<Delta>Z) = distinct(Z) + H(Z,\<Delta>Z). *)
lemma Z5_distinct_delta:
  "zdistinct (zplus Z dZ) = zplus (zdistinct Z) (H Z dZ)"
  by (rule ext) (simp add: H_def)

(* Z5(a): H equals the zero-crossing case form of D6. *)
lemma Z5_H_zero_crossing:
  "H Z dZ x = (if Z x \<le> 0 \<and> 0 < Z x + dZ x then 1
               else if 0 < Z x \<and> Z x + dZ x \<le> 0 then - 1
               else 0)"
  by (auto simp: H_def)

(* Z5(b): support(H(Z,\<Delta>Z)) \<subseteq> support(\<Delta>Z). *)
lemma Z5_H_support: "support (H Z dZ) \<subseteq> support dZ"
  by (auto simp: support_def H_def)

lemma Z5_H_finsupp: "finsupp dZ \<Longrightarrow> finsupp (H Z dZ)"
  unfolding finsupp_def
  by (rule finite_subset[OF Z5_H_support]) assumption

end
