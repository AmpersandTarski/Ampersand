theory Bridge
  imports ZSet
begin

section \<open>The bridge obligations B1..B6: Z-set circuit vs. set semantics\<close>

text \<open>
  Obligations from memorybank/incremental-evaluation/delta-calculus.md, section 5
  (bridge level): when the inputs are sets (all weights 0/1), the Z-set
  operations followed by @{const zdistinct} compute exactly the set operations
  that \<open>fullContents\<close> uses. This justifies reading each circuit node's clipped
  output as the node's set of pairs.
\<close>

(* B1 (delta-calculus.md section 5): distinct(a+b) is the indicator of the union. *)
lemma B1_distinct_plus_is_union:
  assumes "isSet a" and "isSet b"
  shows "zdistinct (zplus a b) = indic (setof a \<union> setof b)"
proof (rule ext)
  fix p
  from assms have "a p = 0 \<or> a p = 1" and "b p = 0 \<or> b p = 1"
    by (auto simp: isSet_def)
  then show "zdistinct (zplus a b) p = indic (setof a \<union> setof b) p"
    by (auto simp: setof_def)
qed

(* B2 (delta-calculus.md section 5): distinct(a-b) is the indicator of the difference. *)
lemma B2_distinct_minus_is_difference:
  assumes "isSet a" and "isSet b"
  shows "zdistinct (zminus a b) = indic (setof a - setof b)"
proof (rule ext)
  fix p
  from assms have "a p = 0 \<or> a p = 1" and "b p = 0 \<or> b p = 1"
    by (auto simp: isSet_def)
  then show "zdistinct (zminus a b) p = indic (setof a - setof b) p"
    by (auto simp: setof_def)
qed

(* B3 (delta-calculus.md section 5): a\<odot>b is the indicator of the intersection;
   0/1 times 0/1 stays in {0,1}, so no distinct is needed. *)
lemma B3_mul_is_intersection:
  assumes "isSet a" and "isSet b"
  shows "zmul a b = indic (setof a \<inter> setof b)"
proof (rule ext)
  fix p
  from assms have "a p = 0 \<or> a p = 1" and "b p = 0 \<or> b p = 1"
    by (auto simp: isSet_def)
  then show "zmul a b p = indic (setof a \<inter> setof b) p"
    by (auto simp: setof_def)
qed

(* B4 (delta-calculus.md section 5): distinct(a;b) is relational composition
   (Isabelle's O), for any finite middle set M covering midsupp a b. *)
lemma B4_distinct_zcomp_is_relcomp:
  assumes a: "isSet a" and b: "isSet b"
      and M: "finite M" and cov: "midsupp a b \<subseteq> M"
  shows "setof (zdistinct (zcomp M a b)) = setof a O setof b"
proof (rule set_eqI)
  fix p
  show "p \<in> setof (zdistinct (zcomp M a b)) \<longleftrightarrow> p \<in> setof a O setof b"
  proof (cases p)
    case (Pair x z)
    have nn: "\<And>w. 0 \<le> a (x, w) * b (w, z)"
      using isSet_nonneg[OF a] isSet_nonneg[OF b] by (simp add: mult_nonneg_nonneg)
    have "p \<in> setof (zcomp M a b) \<longleftrightarrow> (x, z) \<in> setof a O setof b"
    proof
      assume "p \<in> setof (zcomp M a b)"
      then have nz: "zcomp M a b (x, z) \<noteq> 0" by (auto simp: Pair setof_def)
      from zcomp_nonzero_witness[OF nz]
      obtain y where y: "y \<in> M" "a (x, y) \<noteq> 0" "b (y, z) \<noteq> 0"
        by blast
      then have "0 < a (x, y)" and "0 < b (y, z)"
        using isSet_cases[OF a] isSet_cases[OF b] by force+
      then show "(x, z) \<in> setof a O setof b"
        by (auto simp: setof_def intro: relcompI)
    next
      assume "(x, z) \<in> setof a O setof b"
      then obtain y where y: "(x, y) \<in> setof a" "(y, z) \<in> setof b" by auto
      then have ay: "0 < a (x, y)" and byz: "0 < b (y, z)" by (auto simp: setof_def)
      then have pos: "0 < a (x, y) * b (y, z)" by (simp add: mult_pos_pos)
      have "a (x, y) \<noteq> 0" and "b (y, z) \<noteq> 0" using ay byz by simp_all
      then have "y \<in> midsupp a b" unfolding midsupp_def by blast
      then have yM: "y \<in> M" using cov by blast
      have "(\<Sum>w\<in>M. a (x, w) * b (w, z))
          = a (x, y) * b (y, z) + (\<Sum>w\<in>M - {y}. a (x, w) * b (w, z))"
        by (rule sum.remove[OF M yM])
      moreover have "0 \<le> (\<Sum>w\<in>M - {y}. a (x, w) * b (w, z))"
        by (rule sum_nonneg) (use nn in auto)
      ultimately have "0 < (\<Sum>w\<in>M. a (x, w) * b (w, z))" using pos by linarith
      then show "p \<in> setof (zcomp M a b)" by (simp add: Pair setof_def)
    qed
    then show ?thesis by (simp add: Pair)
  qed
qed

(* B5 (delta-calculus.md section 5): unary weighted product + distinct is the
   cartesian product of the underlying sets. *)
lemma B5_distinct_zprod_is_cartesian:
  assumes "isSet u" and "isSet v"
  shows "setof (zdistinct (zprod u v)) = setof u \<times> setof v"
proof (rule set_eqI)
  fix p
  show "p \<in> setof (zdistinct (zprod u v)) \<longleftrightarrow> p \<in> setof u \<times> setof v"
  proof (cases p)
    case (Pair x y)
    from assms have "u x = 0 \<or> u x = 1" and "v y = 0 \<or> v y = 1"
      by (auto simp: isSet_def)
    then show ?thesis unfolding Pair by (auto simp: setof_def)
  qed
qed

(* B6 (delta-calculus.md section 5): flip is converse. *)
lemma B6_flip_is_converse: "setof (zflip a) = (setof a)\<inverse>"
proof (rule set_eqI)
  fix p
  show "p \<in> setof (zflip a) \<longleftrightarrow> p \<in> (setof a)\<inverse>"
  proof (cases p)
    case (Pair y x)
    show ?thesis unfolding Pair by (simp add: setof_def)
  qed
qed

text \<open>Auxiliary: clipping keeps the sethood invariant of every node's output.\<close>

lemma bridge_output_isSet: "isSet (zdistinct f)"
  by (rule isSet_zdistinct)

end
