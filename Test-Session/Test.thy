theory Test 
  imports Main

begin

datatype 'a seq = Empty | Seq 'a "'a seq"

fun conc :: "'a seq \<Rightarrow> 'a seq \<Rightarrow> 'a seq"
where
  "conc Empty ys = ys"
| "conc (Seq x xs) ys = Seq x (conc xs ys)"


lemma [simp]: "conc xs Empty = xs"
  apply (induction xs)
   apply auto
  done




lemma [simp]: "conc xs Empty = xs" (* global_attribute_on_unnamed_lemma *)
  apply (induction xs)
proof auto (* apply_isar_switch *)
qed


lemma conc_empty:"conc xs Empty = xs"
  apply (induction xs)
  apply auto
  done (* use_by *)

lemma "conc xs Empty = xs"
  apply (induction xs)
  by auto (* use_by *)



lemma "conc xs Empty = xs"
  apply auto
  done (* use_by *)

lemma "conc xs Empty = xs"
  apply (induction xs)
  apply (auto;simp) (* auto_structural_composition *)
  by auto
  

lemma "A \<longrightarrow> A"
  apply rule (* implicit_rule *)
  try (* proof_finder *)
  nitpick (* counter_example_finder *)
  apply simp (* force_failure *)
  oops (* unfinished_proof *)



declare conjE[simp add] and conc_empty[cong, simp]

lemma "\<forall>(x:: nat). x \<ge> 0 "
  apply rule
  apply simp
  done

declare conc_empty[simp del] (* global_attribute_changes *)

value "(1::nat) +1" (* diagnostic_command *)

definition S :: "nat \<Rightarrow> nat" where (* short_name *)
  "S x = Suc x"

lemma ex1_equalsE: \<open>\<lbrakk>\<exists>!x. P(x); P(a); P(b)\<rbrakk> \<Longrightarrow> a = b\<close>
  apply (erule ex1E)
  apply (rule trans)
  apply (rule trans)
  apply (rule trans)
  apply (rule trans)
      apply (rule trans)
  apply(rule_tac [2] trans)
  apply (rule trans) (* low_level_apply_chain *)
  sorry

lemma "A \<longrightarrow> A"
proof auto (* complex_isar_initial_method *)
qed


axiomatization
  P :: "'a \<Rightarrow> bool"
  where
  false: "False" (* axiomatization_with_where *)



end