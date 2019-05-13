load("data/3yr_data_04to13.rdata")

str(T_456_ini)
head(T_456,2)
# ======= Combine 3 years data ========

# 2004 to 2008 : T_4to8
names(T_456_ini)[1:10]
table(names(T_456_ini)==names(T_567_ini))
table(names(T_456_ini)==names(T_678_ini))

# Data for randomForest(abbreviated below)
names(T_456_rF_ini)[1:10]
table(names(T_456_rF_ini)==names(T_567_rF_ini))
table(names(T_456_rF_ini)==names(T_678_rF_ini))

# 3-year accumulated delisted
ind_456<-which(T_456_ini$v04_D_YN=="1")
ind_567<-which(T_567_ini$v04_D_YN=="1")
T_4to8_ini<-rbind(T_456_ini[ind_456,],T_567_ini[ind_567,],T_678_ini)
names(T_4to8_ini)[1:10]
T_4to8<-T_4to8_ini[,-c(1:3)]
names(T_4to8)[1:10]

# calculate rate of delisted
table(T_4to8$v04_D_YN)
115/(1737+115)

# 2009 to 2013 : T_9to3
ind_901<-which(T_901_ini$v04_D_YN=="1")
ind_012<-which(T_012_ini$v04_D_YN=="1")
T_9to13_ini<-rbind(T_901_ini[ind_901,],T_012_ini[ind_012,],T_123_ini)
T_9to13<-T_9to13_ini[,-c(1:3)]

table(T_9to13$v04_D_YN)
100/(1492+100)

names(T_4to8)
ind_q<-grep("_v22_q_asst",names(T_4to8))
names(T_4to8[,ind_q])
ind_cpl<-grep("_v26_cur_p_l_tm_dbt",names(T_4to8))
names(T_4to8[,ind_cpl])
ind_cap<-grep("_v30_shr_cap",names(T_4to8))
names(T_4to8[,ind_cap])
ind_cfo<-grep("_v36_cf_op",names(T_4to8))
names(T_4to8[,ind_cfo])
ind_cfi<-grep("_v37_cf_inv",names(T_4to8))
names(T_4to8[,ind_cfi])
ind_cff<-grep("_v38_cf_fin",names(T_4to8))
names(T_4to8[,ind_cff])
ind_si<-grep("_v51_R_sal_inc",names(T_4to8))
names(T_4to8[,ind_si])
ind_opi<-grep("_v52_R_op_prf_inc",names(T_4to8))
names(T_4to8[,ind_opi])
ind_nii<-grep("_v53_R_net_inc_inc",names(T_4to8))
names(T_4to8[,ind_nii])
ind_optc<-grep("_v54_R_op_prf_tot_cap",names(T_4to8))
names(T_4to8[,ind_optc])
ind_exp1<-grep("_v56_R_fin_exp_tot_exp",names(T_4to8))
names(T_4to8[,ind_exp1])
ind_exp2<-grep("_v57_R_cf_op_int_exp",names(T_4to8))
names(T_4to8[,ind_exp2])
ind_qr<-grep("_v60_R_q_rat",names(T_4to8))
names(T_4to8[,ind_qr])
ind_fafc<-grep("_v61_R_fix_ass_fix_cap",names(T_4to8))
names(T_4to8[,ind_fafc])
ind_it<-grep("_v64_R_inv_turn",names(T_4to8))
names(T_4to8[,ind_it])
ind_rt<-grep("_v65_R_recv_tur",names(T_4to8))
names(T_4to8[,ind_rt])
ind_se<-grep("_v66_R_sal_per_emp",names(T_4to8))
names(T_4to8[,ind_se])
ind_or<-grep("_v67_R_net_op_cap_tot_ass",names(T_4to8))
names(T_4to8[,ind_or])

ind<-c(ind_q,ind_cpl,ind_cap,ind_cfo,ind_cfi,ind_cff,ind_si,ind_opi,ind_nii,ind_optc,ind_exp1,ind_exp2,ind_qr,ind_fafc,ind_it,ind_rt,ind_se,ind_or)

T_4to8<-T_4to8[,-ind]
T_9to13<-T_9to13[,-ind]

names(T_4to8)[1:22]

# ======= Formula ========
myFormula<-v04_D_YN~.

#==================================================
# 1.party 2 sec
#==================================================
# install.packages("party")
library(party)
#================= T_4to8_p =================
# ctree modeling using party library
(st_p_4to8<-system.time(T_4to8_ctree<-ctree(myFormula,data=T_4to8)))
(T_4to8_ct_m_tr<-table(predict(T_4to8_ctree),T_4to8$v04_D_YN))
T_4to8_ct_m_tr[2,2]/sum(T_4to8_ct_m_tr[2,]);T_4to8_ct_m_tr[2,2]/sum(T_4to8_ct_m_tr[,2]);sum(diag(T_4to8_ct_m_tr))/sum(T_4to8_ct_m_tr)
# precision , detect rate , Accuracy 

plot(T_4to8_ctree,type="simple")

tPred_p_9to13<-predict(T_4to8_ctree,newdata=T_9to13)
(T_4to8_ct_m_tt<-table(tPred_p_9to13,newdata=T_9to13$v04_D_YN))
T_4to8_ct_m_tt[2,2]/sum(T_4to8_ct_m_tt[2,]);T_4to8_ct_m_tt[2,2]/sum(T_4to8_ct_m_tt[,2]);sum(diag(T_4to8_ct_m_tt))/sum(T_4to8_ct_m_tt)
# precision , detect rate , Accuracy 

#==================================================
# 2.rpart 1 sec
#==================================================
# install.packages("rpart")
library(rpart)
library(partykit) # install.packages("partykit")
#================= T_4to8_rp =================
(st_rp_4to8<-system.time(T_4to8_rp<-rpart(myFormula,data=T_4to8,control=rpart.control(minsplit=10))))
(T_4to8_rp_m_tr<-table(predict(T_4to8_rp, newdata=T_4to8,type="class"),T_4to8$v04_D_YN))
T_4to8_rp_m_tr[2,2]/sum(T_4to8_rp_m_tr[2,]);T_4to8_rp_m_tr[2,2]/sum(T_4to8_rp_m_tr[,2]);sum(diag(T_4to8_rp_m_tr))/sum(T_4to8_rp_m_tr)
# precision , detect rate , Accuracy 

attributes(T_4to8_rp)
print(T_4to8_rp)
plot(as.party(T_4to8_rp))

tPred_rp_9to13<-predict(T_4to8_rp, newdata=T_9to13,type="class")
(T_4to8_rp_m_tt<-table(tPred_rp_9to13,T_9to13$v04_D_YN))
T_4to8_rp_m_tt[2,2]/sum(T_4to8_rp_m_tt[2,]);T_4to8_rp_m_tt[2,2]/sum(T_4to8_rp_m_tt[,2]);sum(diag(T_4to8_rp_m_tt))/sum(T_4to8_rp_m_tt)
# precision , detect rate , Accuracy 

#==================================================
# 3.rpart prior 1 sec
#==================================================
# install.packages("rpart")
library(rpart)
library(partykit)
#================= T_4to8_rp =================
(st_rp_4to8<-system.time(T_4to8_rpp<-rpart(myFormula,data=T_4to8,parms=list(prior=c(0.4,0.6)))))
(T_4to8_rpp_m_tr<-table(predict(T_4to8_rpp, newdata=T_4to8,type="class"),T_4to8$v04_D_YN))
T_4to8_rpp_m_tr[2,2]/sum(T_4to8_rpp_m_tr[2,]);T_4to8_rpp_m_tr[2,2]/sum(T_4to8_rpp_m_tr[,2]);sum(diag(T_4to8_rpp_m_tr))/sum(T_4to8_rpp_m_tr)
# precision , detect rate , Accuracy 

attributes(T_4to8_rpp)
print(T_4to8_rpp)
plot(as.party(T_4to8_rpp))

tPred_rpp_9to13<-predict(T_4to8_rpp, newdata=T_9to13,type="class")
(T_4to8_rpp_m_tt<-table(tPred_rpp_9to13,T_9to13$v04_D_YN))
T_4to8_rpp_m_tt[2,2]/sum(T_4to8_rpp_m_tt[2,]);T_4to8_rpp_m_tt[2,2]/sum(T_4to8_rpp_m_tt[,2]);sum(diag(T_4to8_rpp_m_tt))/sum(T_4to8_rpp_m_tt)
# precision , detect rate , Accuracy 
