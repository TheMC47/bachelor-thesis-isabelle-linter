theory Test 
  imports IFOL

begin

lemma "False"
  sorry

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
   apply (assumption | erule spec [THEN mp])+
done

axiomatization (* lint:disable *)
  P :: "'a \<Rightarrow> o"
  where
  false: "False" 

end