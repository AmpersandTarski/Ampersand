theory Candidates
  imports Desugar
begin

section \<open>The candidate calculus (K-obligations, candidate completeness)\<close>

text \<open>
  Obligations from memorybank/incremental-evaluation/delta-calculus.md,
  section 7 (branch delta-sql): the candidate calculus for delta-scoped
  re-evaluation, implemented by
  src/Ampersand/FSpec/Incremental/DeltaTerms.hs.

  The design (DC-8) maintains the violation cache by re-running the existing
  violation predicate on a small CANDIDATE set of pairs. Correctness therefore
  needs one property only: candidate COMPLETENESS — every pair whose
  membership in a rule term differs between the old and the new database
  state lies in the candidate set. A too-large candidate set costs time,
  never correctness.

  The obligation series was originally labelled C1..C6; it is renamed to
  K1..K6 (K for "kandidaat") because C1..C5 already name the whole-circuit
  obligations of @{file Circuit.thy}. Register claim: PRF-7 in
  docs/proofs/README.md.

  Modelling, in the set-level house style of @{file Desugar.thy}: one
  universe type @{typ 'a}, relations as @{typ "('a \<times> 'a) set"}, the
  complement TYPED (@{const cpl}). The term language below is the supported
  class of \<open>deltaSupported\<close>: union, intersection, difference, composition,
  converse and typed complement over relation leaves and state-independent
  leaves. The constructor \<open>Cst\<close> covers \<open>EDcI\<close>, \<open>EDcV\<close>, \<open>EMp1\<close> and \<open>EBin\<close>:
  within the supported class the concept populations are unchanged (a
  transaction that touches a concept the conjunct mentions falls back to full
  re-evaluation, DC-9), so these leaves denote the same set in the old and
  the new state — as does the bounding rectangle \<open>A \<times> B\<close> of every typed
  complement.
\<close>

abbreviation symdiff :: "'p set \<Rightarrow> 'p set \<Rightarrow> 'p set" where
  "symdiff s t \<equiv> (s - t) \<union> (t - s)"

datatype ('a, 'r) trm =
    Rel 'r
  | Cst "('a \<times> 'a) set"
  | Uni "('a, 'r) trm" "('a, 'r) trm"
  | Isc "('a, 'r) trm" "('a, 'r) trm"
  | Dif "('a, 'r) trm" "('a, 'r) trm"
  | Cps "('a, 'r) trm" "('a, 'r) trm"
  | Flp "('a, 'r) trm"
  | Cpl "'a set" "'a set" "('a, 'r) trm"

primrec sem :: "('r \<Rightarrow> ('a \<times> 'a) set) \<Rightarrow> ('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where
  "sem db (Rel r) = db r"
| "sem db (Cst s) = s"
| "sem db (Uni a b) = sem db a \<union> sem db b"
| "sem db (Isc a b) = sem db a \<inter> sem db b"
| "sem db (Dif a b) = sem db a - sem db b"
| "sem db (Cps a b) = sem db a O sem db b"
| "sem db (Flp a) = converse (sem db a)"
| "sem db (Cpl A B a) = cpl A B (sem db a)"

subsection \<open>K1..K6: one completeness lemma per constructor\<close>

text \<open>
  Pure set-level statements, one per candidate rule (D-rules of
  delta-calculus.md section 7). Reading of the variables: \<open>o1, o2\<close> the old
  denotations of the operands, \<open>n1, n2\<close> the new ones, \<open>d1, d2\<close> any supersets
  of the operands' symmetric differences (the operands' candidate sets),
  \<open>w1, w2\<close> any upper envelopes of old and new, \<open>nn2\<close> any lower envelope.
  The whole-term theorem @{text K_complete} below instantiates them with the
  candidate sets and the W/N envelopes.
\<close>

lemma K1_uni:
  assumes "symdiff o1 n1 \<subseteq> d1" and "symdiff o2 n2 \<subseteq> d2"
  shows "symdiff (o1 \<union> o2) (n1 \<union> n2) \<subseteq> d1 \<union> d2"
  using assms by blast

lemma K2_isc:
  assumes "symdiff o1 n1 \<subseteq> d1" and "symdiff o2 n2 \<subseteq> d2"
    and "o1 \<union> n1 \<subseteq> w1" and "o2 \<union> n2 \<subseteq> w2"
  shows "symdiff (o1 \<inter> o2) (n1 \<inter> n2) \<subseteq> (d1 \<inter> w2) \<union> (w1 \<inter> d2)"
  using assms by blast

lemma K3_dif:
  assumes "symdiff o1 n1 \<subseteq> d1" and "symdiff o2 n2 \<subseteq> d2"
    and "o1 \<union> n1 \<subseteq> w1" and "nn2 \<subseteq> o2 \<inter> n2"
  shows "symdiff (o1 - o2) (n1 - n2) \<subseteq> (d1 - nn2) \<union> (w1 \<inter> d2)"
  using assms by blast

lemma K4_cps:
  assumes d1: "symdiff o1 n1 \<subseteq> d1" and d2: "symdiff o2 n2 \<subseteq> d2"
    and w1: "o1 \<union> n1 \<subseteq> w1" and w2: "o2 \<union> n2 \<subseteq> w2"
  shows "symdiff (o1 O o2) (n1 O n2) \<subseteq> (d1 O w2) \<union> (w1 O d2)"
proof
  fix p assume p_in: "p \<in> symdiff (o1 O o2) (n1 O n2)"
  show "p \<in> (d1 O w2) \<union> (w1 O d2)"
  proof (cases p)
    case (Pair x z)
    consider (del) "(x, z) \<in> o1 O o2" "(x, z) \<notin> n1 O n2"
      | (ins) "(x, z) \<in> n1 O n2" "(x, z) \<notin> o1 O o2"
      using p_in Pair by blast
    then show ?thesis
    proof cases
      case del
      from del(1) obtain y where xy: "(x, y) \<in> o1" and yz: "(y, z) \<in> o2"
        by auto
      show ?thesis
      proof (cases "(x, y) \<in> n1")
        case True
        with del(2) have "(y, z) \<notin> n2" by auto
        with yz d2 have "(y, z) \<in> d2" by auto
        moreover from xy w1 have "(x, y) \<in> w1" by auto
        ultimately show ?thesis using Pair by auto
      next
        case False
        with xy d1 have "(x, y) \<in> d1" by auto
        moreover from yz w2 have "(y, z) \<in> w2" by auto
        ultimately show ?thesis using Pair by auto
      qed
    next
      case ins
      from ins(1) obtain y where xy: "(x, y) \<in> n1" and yz: "(y, z) \<in> n2"
        by auto
      show ?thesis
      proof (cases "(x, y) \<in> o1")
        case True
        with ins(2) have "(y, z) \<notin> o2" by auto
        with yz d2 have "(y, z) \<in> d2" by auto
        moreover from True w1 have "(x, y) \<in> w1" by auto
        ultimately show ?thesis using Pair by auto
      next
        case False
        with xy d1 have "(x, y) \<in> d1" by auto
        moreover from yz w2 have "(y, z) \<in> w2" by auto
        ultimately show ?thesis using Pair by auto
      qed
    qed
  qed
qed

lemma K5_flp:
  assumes "symdiff o1 n1 \<subseteq> d1"
  shows "symdiff (converse o1) (converse n1) \<subseteq> converse d1"
  using assms by auto

lemma K6_cpl:
  assumes "symdiff o1 n1 \<subseteq> d1"
  shows "symdiff (cpl A B o1) (cpl A B n1) \<subseteq> d1"
  using assms unfolding cpl_def by blast

subsection \<open>The transaction locale: envelopes and whole-term completeness\<close>

text \<open>
  A transaction is two database states \<open>dbo\<close> (old) and \<open>dbn\<close> (new) and per
  relation a delta set \<open>\<Delta> r\<close> — the contents of the delta table. The single
  assumption is the delta-table contract: every pair whose membership in a
  base relation changed is recorded in that relation's delta table. The
  envelopes and the candidate set are functions of the NEW state and the
  delta tables only, because that is all the SQL can see at commit time.
\<close>

locale delta_transaction =
  fixes dbo :: "'r \<Rightarrow> ('a \<times> 'a) set"
    and dbn :: "'r \<Rightarrow> ('a \<times> 'a) set"
    and \<Delta> :: "'r \<Rightarrow> ('a \<times> 'a) set"
  assumes delta_covers: "symdiff (dbo r) (dbn r) \<subseteq> \<Delta> r"
begin

text \<open>
  The widened (\<open>pol = True\<close>) and narrowed (\<open>pol = False\<close>) envelopes, one
  function with a polarity flag: the polarity flips under the right operand
  of a difference and under a complement, exactly as \<open>widen\<close>/\<open>narrow\<close> in
  DeltaTerms.hs.
\<close>

fun env :: "bool \<Rightarrow> ('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where
  "env pol (Rel r) = (if pol then dbn r \<union> \<Delta> r else dbn r - \<Delta> r)"
| "env pol (Cst s) = s"
| "env pol (Uni a b) = env pol a \<union> env pol b"
| "env pol (Isc a b) = env pol a \<inter> env pol b"
| "env pol (Dif a b) = env pol a - env (\<not> pol) b"
| "env pol (Cps a b) = env pol a O env pol b"
| "env pol (Flp a) = converse (env pol a)"
| "env pol (Cpl A B a) = cpl A B (env (\<not> pol) a)"

abbreviation W :: "('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where "W \<equiv> env True"
abbreviation N :: "('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where "N \<equiv> env False"

text \<open>
  The envelope invariant: W bounds the term's old and new denotation from
  above, N from below. This is the sense in which W/N reconstruct enough of
  the OLD state from the new state plus the delta tables.
\<close>

lemma WN_envelope:
  "sem dbo t \<union> sem dbn t \<subseteq> W t \<and> N t \<subseteq> sem dbo t \<inter> sem dbn t"
proof (induction t)
  case (Rel r)
  show ?case using delta_covers[of r] by auto
next
  case (Cst s)
  show ?case by simp
next
  case (Uni a b)
  then show ?case by auto
next
  case (Isc a b)
  then show ?case by auto
next
  case (Dif a b)
  then show ?case by auto
next
  case (Cps a b)
  from Cps.IH have wa: "sem dbo a \<subseteq> W a" "sem dbn a \<subseteq> W a"
    and na: "N a \<subseteq> sem dbo a" "N a \<subseteq> sem dbn a" by auto
  from Cps.IH have wb: "sem dbo b \<subseteq> W b" "sem dbn b \<subseteq> W b"
    and nb: "N b \<subseteq> sem dbo b" "N b \<subseteq> sem dbn b" by auto
  have 1: "sem dbo a O sem dbo b \<subseteq> W a O W b"
    using wa(1) wb(1) by (intro relcomp_mono)
  have 2: "sem dbn a O sem dbn b \<subseteq> W a O W b"
    using wa(2) wb(2) by (intro relcomp_mono)
  have 3: "N a O N b \<subseteq> sem dbo a O sem dbo b"
    using na(1) nb(1) by (intro relcomp_mono)
  have 4: "N a O N b \<subseteq> sem dbn a O sem dbn b"
    using na(2) nb(2) by (intro relcomp_mono)
  show ?case using 1 2 3 4 by simp
next
  case (Flp a)
  then show ?case by auto
next
  case (Cpl A B a)
  then show ?case by (auto simp add: cpl_def)
qed

lemma W_upper: "sem dbo t \<union> sem dbn t \<subseteq> W t"
  using WN_envelope by blast

lemma N_lower: "N t \<subseteq> sem dbo t \<inter> sem dbn t"
  using WN_envelope by blast

text \<open>
  The candidate set, parameterised by the leaf assignment \<open>lc\<close> so that the
  per-relation decomposition below is a theorem rather than a remark:
  \<open>candg \<Delta>\<close> is the global candidate set, \<open>candg\<close> of a single-relation
  restriction is what one generated candidate query (per touched relation)
  computes. The equations mirror \<open>candidateTerms\<close> in DeltaTerms.hs and the
  D-rules of delta-calculus.md section 7 one for one.
\<close>

fun candg :: "('r \<Rightarrow> ('a \<times> 'a) set) \<Rightarrow> ('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where
  "candg lc (Rel r) = lc r"
| "candg lc (Cst s) = {}"
| "candg lc (Uni a b) = candg lc a \<union> candg lc b"
| "candg lc (Isc a b) = (candg lc a \<inter> W b) \<union> (W a \<inter> candg lc b)"
| "candg lc (Dif a b) = (candg lc a - N b) \<union> (W a \<inter> candg lc b)"
| "candg lc (Cps a b) = (candg lc a O W b) \<union> (W a O candg lc b)"
| "candg lc (Flp a) = converse (candg lc a)"
| "candg lc (Cpl A B a) = candg lc a"

definition cand :: "('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where
  "cand \<equiv> candg \<Delta>"

definition candR :: "'r \<Rightarrow> ('a, 'r) trm \<Rightarrow> ('a \<times> 'a) set" where
  "candR r \<equiv> candg (\<lambda>s. if s = r then \<Delta> r else {})"

text \<open>
  The whole-term theorem: every pair whose membership in the term differs
  between the old and the new state lies in the candidate set. This is the
  headline of the K-series; its cases are K1..K6.
\<close>

theorem K_complete: "symdiff (sem dbo t) (sem dbn t) \<subseteq> cand t"
  unfolding cand_def
proof (induction t)
  case (Rel r)
  show ?case using delta_covers[of r] by simp
next
  case (Cst s)
  show ?case by simp
next
  case (Uni a b)
  show ?case using K1_uni[OF Uni.IH(1) Uni.IH(2)] by simp
next
  case (Isc a b)
  show ?case
    using K2_isc[OF Isc.IH(1) Isc.IH(2) W_upper W_upper] by simp
next
  case (Dif a b)
  show ?case
    using K3_dif[OF Dif.IH(1) Dif.IH(2) W_upper N_lower] by simp
next
  case (Cps a b)
  show ?case
    using K4_cps[OF Cps.IH(1) Cps.IH(2) W_upper W_upper] by simp
next
  case (Flp a)
  show ?case using K5_flp[OF Flp.IH] by simp
next
  case (Cpl A B a)
  show ?case using K6_cpl[OF Cpl.IH] by simp
qed

subsection \<open>The per-relation decomposition\<close>

text \<open>
  The implementation does not evaluate one global candidate set: it generates
  one candidate query per (conjunct, touched relation) and the runtime takes
  their union. The decomposition below states that this union equals the
  global candidate set, PROVIDED untouched relations have empty delta tables
  — which is the delta-table protocol (filled during the transaction, empty
  outside). The proof rests on the candidate set being linear in the leaf
  assignment; the envelopes W and N do not depend on it.
\<close>

lemma candg_UN:
  "candg (\<lambda>s. \<Union>i\<in>I. lc i s) t = (\<Union>i\<in>I. candg (lc i) t)"
proof (induction t)
  case (Rel r) show ?case by simp
next
  case (Cst s) show ?case by simp
next
  case (Uni a b) then show ?case by auto
next
  case (Isc a b) then show ?case by auto
next
  case (Dif a b) then show ?case by auto
next
  case (Cps a b) then show ?case by (simp, blast)
next
  case (Flp a) then show ?case by auto
next
  case (Cpl A B a) then show ?case by simp
qed

theorem K_per_relation:
  assumes untouched: "\<And>s. s \<notin> R \<Longrightarrow> \<Delta> s = {}"
  shows "cand t = (\<Union>r\<in>R. candR r t)"
proof -
  have leaf: "\<Delta> = (\<lambda>s. \<Union>r\<in>R. (if s = r then \<Delta> r else {}))"
  proof
    fix s show "\<Delta> s = (\<Union>r\<in>R. (if s = r then \<Delta> r else {}))"
      using untouched[of s] by (cases "s \<in> R") auto
  qed
  show ?thesis
    unfolding cand_def candR_def
    by (subst leaf) (rule candg_UN)
qed

corollary K_touched_cover:
  assumes "\<And>s. s \<notin> R \<Longrightarrow> \<Delta> s = {}"
  shows "symdiff (sem dbo t) (sem dbn t) \<subseteq> (\<Union>r\<in>R. candR r t)"
  using K_complete K_per_relation[OF assms] by simp

end

end
