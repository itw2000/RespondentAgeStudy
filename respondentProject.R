library("foreign")
library("memisc")


par(mfrow=c(3,3))

anne_online <- read.spss("respondent_data/naes08onlinew4.sav")      
table(anne_online$WA02_a)
hist(anne_online$WA02_a, freq=TRUE, xlim=c(15,110), main="Annenberg Online", ylab=" ", xlab="Age", breaks=50)
quants_anne_online <- quantile(anne_online$WA02_a, probs=c(0.025,0.5,0.975))

anne_phone <- read.spss("respondent_data/naes08phone.sav")
#Note 64 people don't their own age
table(anne_phone$WA02_c)
hist(anne_phone$WA02_c[anne_phone$WA02_c<98], freq=TRUE, xlim=c(10,110), main="Annenberg Phone", ylab=" ", xlab="Age",breaks=50)
quants_anne_phone <- quantile(anne_phone$WA02_c[anne_phone$WA02_c<98], probs=c(0.025,0.5,0.975))

gss_2008 <- read.dta("RespondentAgeStudy/GSS2008.dta")
table(gss_2008$age)
hist(na.omit(gss_2008$age), xlim=c(15,110), freq=TRUE, main="GSS", ylab=" ", xlab="Age", breaks=50)
quants_gss <- quantile(na.omit(gss_2008$age), probs=c(0.025,0.5,0.975))

#2008 Post-Election N=2254
pew_2008_post <- read.spss("RespondentAgeStudy/Nov08PostElect.sav")
table(pew_2008_post$age)
hist(pew_2008_post$age, xlim=c(15,110), freq=TRUE, main="Pew Post-Election", ylab=" ", xlab="Age",breaks=50)
quants_pew <- quantile(pew_2008_post$age[pew_2008_post$age<99], probs=c(0.025,0.5,0.975))

anes_2008 <-read.dta("RespondentAgeStudy/anes_timeseries_2008_stata12.dta")
table(anes_2008$V081104[anes_2008$V081104>0])
hist(anes_2008$V081104[anes_2008$V081104>0], freq=TRUE, xlim=c(15,110),main="ANES",ylab=" ", xlab="Age", breaks=50)
quants_anes <- quantile(anes_2008$V081104[anes_2008$V081104>0], probs=c(0.025,0.5,0.975))

#Sep. post. Repub Convection USA Today/Gallup Pres. Elec Poll N=1022
gallup_sep_2008 <- as.data.set(spss.portable.file("RespondentAgeStudy/usa_gallup2008_sep.por"))
table(gallup_sep_2008$d2)
hist(gallup_sep_2008$d2, freq=TRUE, xlim=c(15,110),main="Gallup/USA Today September",ylab=" ", xlab="Age", breaks=50)
quants_gallup_sep <- quantile(na.omit(as.matrix(gallup_sep_2008$d2)), probs=c(0.025,0.5,0.975))

#CBS News Poll June 2008 N=1038
cbs_jun_2008 <- as.data.set(spss.portable.file("RespondentAgeStudy/cbs_june_08.por"))
table(cbs_jun_2008$age)
hist(cbs_jun_2008$age, freq=TRUE, xlim=c(15,110),main="CBS News June",ylab=" ", xlab="Age", breaks=50)
quants_cbs_jun <- quantile(na.omit(as.matrix(cbs_jun_2008$age)), probs=c(0.025,0.5,0.975))

#LA Times/Bloomberg Poll Oct N=1543
latimes_oct <- as.data.set(spss.portable.file("RespondentAgeStudy/lat_oct.por"))
table(latimes_oct$exactage)
hist(na.omit(as.numeric(as.matrix(latimes_oct$exactage))), freq=TRUE, xlim=c(15,110),main="LA Times/Bloomberg Oct",ylab=" ", xlab="Age", breaks=50)
quants_lat_oct <- quantile(na.omit(as.numeric(as.matrix(latimes_oct$exactage))), probs=c(0.025,0.5,0.975))




medians <- c(quants_anne_online[2], quants_anne_phone[2], quants_gss[2], quants_pew[2],quants_anes[2], quants_gallup_sep[2], quants_cbs_jun[2],
             quants_lat_oct[2])
names(medians) <- c("Annenberg Online", "Annenberg Phone", "GSS", "Pew Post-Election", "American National Election Studies", "Gallup/USA Today September"
                    ,"CBS News June", "LA Times/Bloomberg October")
dotchart(sort(medians), main="Median Ages of Respondents to Surveys in 2008")

