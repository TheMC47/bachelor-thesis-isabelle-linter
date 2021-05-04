theory Test 
  imports Main

begin

lemma "False"
  sorry


datatype 'a seq = Empty | Seq 'a "'a seq"

fun conc :: "'a seq \<Rightarrow> 'a seq \<Rightarrow> 'a seq"
where
  "conc Empty ys = ys"
| "conc (Seq x xs) ys = Seq x (conc xs ys)"

fun reverse :: "'a seq \<Rightarrow> 'a seq"
where
  "reverse Empty = Empty"
| "reverse (Seq x xs) = conc (reverse xs) (Seq x Empty)"

lemma conc_empty: "conc xs Empty = xs"
  apply (induct)[1]
  apply (induct; ((auto | blast+)[1]))
  done

lemma "\<forall>(x:: nat). x \<ge> 0 "
  apply rule
  apply simp
  done

lemma conjE[simp]:
  assumes major: \<open>P \<and> Q\<close>
    and r: \<open>\<lbrakk>P; Q\<rbrakk> \<Longrightarrow> R\<close>
  shows \<open>R\<close>
proof (rule r)
  show "P"
    by (rule major [THEN conjunct1])
  show "Q"
    by (rule major [THEN conjunct2]) 
qed

lemma ex1_equalsE: \<open>\<lbrakk>\<exists>!x. P(x); P(a); P(b)\<rbrakk> \<Longrightarrow> a = b\<close>  
  apply (erule ex1E)
  apply (rule trans)
   apply (rule_tac [2] sym)
  sorry

axiomatization (* lint:disable *)
  P :: "'a \<Rightarrow> bool"
  where
  false: "False" 

end