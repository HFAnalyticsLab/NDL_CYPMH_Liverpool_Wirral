#code for analysis of Children and Young People's Mental Health (Networked Data Lab Phase 2)
#Public facing code makes use of a dummy data set, as well open-data sources

#Running this code generates DUMMY figures and tables


rm (list = ls ())

library(data.table)
library(tidyverse)
library(broom)
library(tsibble)#yearmonth
library(rgdal)
library(GISTools)
library(pals)
library(dplyr)
require(janitor)
library(ggspatial)
library(sf)
library(broom)
library(ggplot2)
library(tmap)    # for static and interactive maps
library(biscale)   #for bivariate choropleth maps
library(gridExtra)
library(cowplot)


#load raw dummy data
#NOTE dummy data created from LSOAs used in the analysis, with randomly assigned IDs, data, and variables
#dummy events variables are skewed, so higher dummy rates occur in higher IMD deciles

d <- fread("./dummy_CYPMH_data_rand.csv")

#get lsoa data and break by national quantiles
lsoa_data<-fread("./ccg_dummy_data.csv")
lsoa_data[,natn_dec:= dplyr::ntile(-imd, 10)]
lsoa_data[,natn_qunt:= dplyr::ntile(-imd, 5)]
lsoa_data[,natn_terc:= dplyr::ntile(-imd, 3)]

# load lsoa boundaries
sp_lsoa <- st_read(normalizePath("./Wirral_Liverpool_LSOA_bnds.shp"))
sp_lsoa <- sp_lsoa[sp_lsoa$LSOA11CD %in% lsoa_age2_sex$lsoa11, ]


#Liverpool & Wirral single year population (2019)
lsoa_sy_pop<-fread("./liv_wir_sy_pop.csv")

#tidy 
d[New_age<0, New_age:=NA]
#recode sex as NA if > 2
d[New_sex>2,New_sex:=NA ]


#derive year and month
d$YearMonth <- format(as.Date(d$YearMonth, format="%d/%m/%Y"))
d[, year:=year(YearMonth)]
d[, month:=month(YearMonth)]

d[, year_month:=format(YearMonth,'%Y-%m')]


#add columns with totals for outpatients, A&E, and Admissions
names(d[, 10:20])
d[, tot_out:=rowSums(.SD, na.rm=T), .SDcols=10:20]
names(d[, 10:13])
d[, tot_ae:=rowSums(.SD, na.rm=T), .SDcols=10:13]
sum(d$tot_ae)
names(d[, 14:18])
d[, tot_ad:=rowSums(.SD, na.rm=T), .SDcols=14:18]
sum(d$tot_ad)

#derive variable that indicates the first contact in data set for each individual
#earliest date with an outcome
d[tot_out>0, first_contact_date:=min(YearMonth), by=.(rand_id)]
d[, fc_flag:=as.numeric(YearMonth==first_contact_date), by=.(rand_id)]


#collapse to counts per age for single year
sy_outcomes<-d[is.na(New_sex)==F, list(ecds_eating_disorder=sum(ecds_eating_disorder, na.rm=T), 
                                               ecds_self_harm=sum(ecds_self_harm, na.rm=T),
                      ecds_substance_misuse=sum(ecds_substance_misuse,na.rm = T), 
                      ecds_alcohol=sum(ecds_alcohol,na.rm = T), 
                      tot_ae=sum(tot_ae,na.rm = T), 
                      tot_ad=sum(tot_ad,na.rm = T), 
                      tot_out=sum(tot_out,na.rm = T), 
                      ip_eating_disorder=sum(ip_eating_disorder, na.rm=T),
                      ip_alcohol=sum(ip_alcohol,na.rm=T), 
                      ip_self_harm=sum(ip_self_harm, na.rm=T),
                      ip_substance_misuse=sum(ip_substance_misuse_not_alcohol,na.rm = T),
                      ip_other_MH_Diagnosis=sum(ip_other_MH_Diagnosis,na.rm = T),
                      referrals=sum(referrals, na.rm=T),
                      mh_contacts=sum(mh_contacts, na.rm = T),
                      fc_flag=sum(fc_flag, na.rm = T)
                      ), by=.(New_age, New_sex)
                      ]



#single-year population table for 0-25YOs
sy_pop<-lsoa_sy_pop[, list(an_pop=sum(value)), by=.(age,sex)]
sy_outcomes<-merge(sy_pop[age<26],sy_outcomes, by.x=c("age","sex"), by.y = c("New_age","New_sex"), all.x=T)
#missing age years should be zero
sy_outcomes[is.na(sy_outcomes)]<-0



#calculate rates
sy_outcomes[, rate_ae:=tot_ae*100/an_pop]
sy_outcomes[, rate_ed_ae:=ecds_eating_disorder*100/an_pop]
sy_outcomes[, rate_sh_ae:=ecds_self_harm*100/an_pop]
sy_outcomes[, rate_alc_ae:=ecds_alcohol*100/an_pop]
sy_outcomes[, rate_sm_ae:=ecds_substance_misuse*100/an_pop]
sy_outcomes[, rate_ad:=tot_ad*100/an_pop]
sy_outcomes[, rate_ed_ip:=ip_eating_disorder*100/an_pop]
sy_outcomes[, rate_sh_ip:=ip_self_harm*100/an_pop]
sy_outcomes[, rate_alc_ip:=ip_alcohol*100/an_pop]
sy_outcomes[, rate_sm_ip:=ip_substance_misuse*100/an_pop]
sy_outcomes[, rate_otherMH_ip:=ip_other_MH_Diagnosis*100/an_pop]
sy_outcomes[, rate_referrals:=referrals*100/an_pop]
sy_outcomes[, rate_contacts:=mh_contacts*100/an_pop]
sy_outcomes[, rate_first:=fc_flag*100/an_pop]
sy_outcomes[, rate_all:=tot_out*100/an_pop]


sy_outcomes[, sex:=factor(sex, labels = c("Male", "Female"))]

#sy_outcomes now includes all counts and rates for 0-25YOs

sum(sy_outcomes$fc_flag) 
  sum(sy_outcomes$an_pop)
  sum(sy_outcomes$fc_flag)*100/ sum(sy_outcomes$an_pop)
  sum(sy_outcomes[age>=15 & age<26 & sex=="Female"]$fc_flag)
  sum(sy_outcomes[age>=15 & age<26 & sex=="Female"]$fc_flag)*100/ sum(sy_outcomes[age>=15 & age<26 &sex=="Female"]$an_pop)
  sum(sy_outcomes[age>=15 & age<26 & sex=="Female"]$fc_flag)
  
  #Output tables for HF
  names(sy_outcomes[,1:18])
  out_sy_outcomes<-sy_outcomes
  
  fwrite(out_sy_outcomes, "./Figures_1_7_8_Results_all_Counts_Rates_0-25_single_age_year_DUMMY.csv")


  
#People counts  
  
#Figure 1 (report)
ggplot(sy_outcomes, aes(x = age, y = rate_first, fill=sex)) +
  scale_fill_manual("", values=c("olivedrab3", "orangered2")) +
  labs(x="Age", y="Per 100 population") + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("People with any mental health contact (DUMMY)")

ggsave("./Figure1_People_any_contact_DUMMY.png", width = 8, height = 6, units = 'in')

dev.off()


#Admissions

ggplot(sy_outcomes, aes(x = age, y = rate_ed_ip, fill=sex)) +
  scale_fill_manual("", values=c("olivedrab3", "orangered2")) +
  labs(x="Age", y="Incidents per 100") + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Admissions: Eating disorders (DUMMY)")

ggsave("./figure1a_ed_ip_DUMMY.png", width = 8, height = 6, units = 'in')

dev.off()


#age specific trends for two age groups
lsoa_pop <- fread("./pop2017_2020__mf.csv")

#collapse to same age categories as data
lsoa_pop[,  age_group2:=cut(age_cat, breaks=c(0,15,25), include.lowest=T, right=F)]
pop2<-lsoa_pop[age_cat<25, list(an_pop=sum(an_pop)), by=.(age_group2,lsoa11,sex,year)]


d[, age_group2:=cut(New_age, breaks=c(0,15,25), include.lowest=T, right=F)]
age_sex_month<-d[year>2018 & is.na(New_sex)==F, list(ecds_eating_disorder=sum(ecds_eating_disorder, na.rm=T), 
                                                     ecds_self_harm=sum(ecds_self_harm, na.rm=T),
                                                     ecds_substance_misuse=sum(ecds_substance_misuse,na.rm = T), 
                                                     ecds_alcohol=sum(ecds_alcohol,na.rm = T), 
                                                     ip_eating_disorder=sum(ip_eating_disorder, na.rm=T),
                                                     ip_alcohol=sum(ip_alcohol,na.rm=T), 
                                                     ip_self_harm=sum(ip_self_harm, na.rm=T),
                                                     ip_substance_misuse=sum(ip_substance_misuse_not_alcohol,na.rm = T),
                                                     ip_other_MH_Diagnosis=sum(ip_other_MH_Diagnosis,na.rm = T),
                                                     referrals=sum(referrals, na.rm=T),
                                                     mh_contacts=sum(mh_contacts, na.rm=T),
                                                     ad_em=sum(tot_ad,na.rm = T), 
                                                     ae=sum(tot_ae,na.rm = T),
                                                     fc_flag=sum(fc_flag,na.rm = T)
                                                     
), by=.(age_group2, New_sex, year_month, year)]


age_sex_month_pop<-pop2[lsoa11 %in% liv_wirral$lsoa11, list(an_pop=sum(an_pop)), by=.(age_group2,sex,year)]

age_sex_month<-merge(age_sex_month,age_sex_month_pop, by.y=c("age_group2","sex", "year"), by.x = c("age_group2","New_sex", "year"), all.x=T)


#calculate rates

age_sex_month[, rate_ed_ae:=ecds_eating_disorder*100/an_pop]
age_sex_month[, rate_sh_ae:=ecds_self_harm*100/an_pop]
age_sex_month[, rate_alc_ae:=ecds_alcohol*100/an_pop]
age_sex_month[, rate_sm_ae:=ecds_substance_misuse*100/an_pop]

age_sex_month[, rate_ed_ip:=ip_eating_disorder*100/an_pop]
age_sex_month[, rate_sh_ip:=ip_self_harm*100/an_pop]
age_sex_month[, rate_alc_ip:=ip_alcohol*100/an_pop]
age_sex_month[, rate_sm_ip:=ip_substance_misuse*100/an_pop]
age_sex_month[, rate_otherMH_ip:=ip_other_MH_Diagnosis*100/an_pop]

age_sex_month[, rate_referrals:=referrals*100/an_pop]
age_sex_month[, rate_contacts:=mh_contacts*100/an_pop]
age_sex_month[, rate_ae:=ae*100/an_pop]
age_sex_month[, rate_adem:=ad_em*100/an_pop]
age_sex_month[, rate_first:=fc_flag*100/an_pop]


age_sex_month[, sex:=factor(New_sex, labels = c("Male", "Female"))]

age_sex_month[, yrmnth:=yearmonth(year_month)]

#descriptive analysis for two age groups by LSOA across 2019
#will be used in regression models
lsoa_age2_sex<-d[,list(ecds_eating_disorder=sum(ecds_eating_disorder, na.rm=T), 
                       ecds_self_harm=sum(ecds_self_harm, na.rm=T),
                       ecds_substance_misuse=sum(ecds_substance_misuse,na.rm = T), 
                       ecds_alcohol=sum(ecds_alcohol,na.rm = T), 
                       ip_eating_disorder=sum(ip_eating_disorder, na.rm=T),
                       ip_alcohol=sum(ip_alcohol,na.rm=T), 
                       ip_self_harm=sum(ip_self_harm, na.rm=T),
                       ip_substance_misuse=sum(ip_substance_misuse_not_alcohol,na.rm = T),
                       ip_other_MH_Diagnosis=sum(ip_other_MH_Diagnosis,na.rm = T),
                       referrals=sum(referrals, na.rm=T),
                       mh_contacts=sum(mh_contacts, na.rm=T),
                       ad_em=sum(tot_ad,na.rm = T), 
                       ae=sum(tot_ae,na.rm = T),
                       fc_flag=sum(fc_flag,na.rm = T)
), by=.(lsoa11=New_LSOA, age_group2, sex=New_sex)]


#create two age groups
lsoa_pop[,  age_group2:=cut(age_cat, breaks=c(0,15,25), include.lowest=T, right=F)]
#to account for person year - and half year in 2021 - only if needed
lsoa_pop[, an_pop2:=an_pop]
lsoa_pop[year==2021, an_pop2:=an_pop2/2]
#removed '&year == 2019' as 2020 needed for covid analysus
pop_lsoa_sex<-lsoa_pop[age_cat<25 & (lsoa11 %in% liv_wirral$lsoa11), list(an_pop=sum(an_pop2)), by=.(lsoa11, age_group2, sex)]

lsoa_age2_sex<-merge(pop_lsoa_sex, lsoa_age2_sex, by=c("lsoa11","age_group2", "sex"),all.x=T)
names(lsoa_age2_sex[, 4:18])
# where there is no activity data in a cell - this should be zero not NA 
lsoa_age2_sex[, 4:18][is.na(lsoa_age2_sex[, 4:18])] <- 0
sum(lsoa_age2_sex$fc_flag)

#calculate rates

lsoa_age2_sex[, rate_ed_ae:=ecds_eating_disorder*100/an_pop]
lsoa_age2_sex[, rate_sh_ae:=ecds_self_harm*100/an_pop]
lsoa_age2_sex[, rate_alc_ae:=ecds_alcohol*100/an_pop]
lsoa_age2_sex[, rate_sm_ae:=ecds_substance_misuse*100/an_pop]

lsoa_age2_sex[, rate_ed_ip:=ip_eating_disorder*100/an_pop]
lsoa_age2_sex[, rate_sh_ip:=ip_self_harm*100/an_pop]
lsoa_age2_sex[, rate_alc_ip:=ip_alcohol*100/an_pop]
lsoa_age2_sex[, rate_sm_ip:=ip_substance_misuse*100/an_pop]
lsoa_age2_sex[, rate_otherMH_ip:=ip_other_MH_Diagnosis*100/an_pop]

lsoa_age2_sex[, rate_referrals:=referrals*100/an_pop]
lsoa_age2_sex[, rate_contacts:=mh_contacts*100/an_pop]
lsoa_age2_sex[, rate_ae:=ae*100/an_pop]
lsoa_age2_sex[, rate_adem:=ad_em*100/an_pop]
lsoa_age2_sex[, rate_first:=fc_flag*100/an_pop]

lsoa_age2_sex[, sex:=factor(sex, labels = c("Male", "Female"))]
lsoa_age2_sex<-merge(lsoa_age2_sex, lsoa_data, by="lsoa11", all.x=T)


fwrite(lsoa_age2_sex, "./national_IMD_lsoa_age2_sex_DUMMY.csv")

#output data for HF
#names(lsoa_age2_sex[,1:18])
#out_lsoa_age2_sex<-sy_outcomes[,1:18]
#out_lsoa_age2_sex[, (4:18):= replace(.SD, .SD <5, NA), .SDcols = 4:18]


#Figure table: Figures 2, 3, 4
fwrite(out_lsoa_age2_sex, "./national_imd_lsoa_19_20_age_sex_DUMMY.csv")


#charts by decile
imd_data<-lsoa_age2_sex[, list(ecds_eating_disorder=sum(ecds_eating_disorder, na.rm=T), 
                               ecds_self_harm=sum(ecds_self_harm, na.rm=T),
                               ecds_substance_misuse=sum(ecds_substance_misuse,na.rm = T), 
                               ecds_alcohol=sum(ecds_alcohol,na.rm = T), 
                               ip_eating_disorder=sum(ip_eating_disorder, na.rm=T),
                               ip_alcohol=sum(ip_alcohol,na.rm=T), 
                               ip_self_harm=sum(ip_self_harm, na.rm=T),
                               ip_substance_misuse=sum(ip_substance_misuse,na.rm = T),
                               ip_other_MH_Diagnosis=sum(ip_other_MH_Diagnosis,na.rm = T),
                               referrals=sum(referrals, na.rm=T),
                               mh_contacts=sum(mh_contacts, na.rm=T),
                               ad_em=sum(ad_em,na.rm = T), 
                               ae=sum(ae,na.rm = T),
                               fc_flag=sum(fc_flag,na.rm = T),
                               an_pop=sum(an_pop,na.rm = T)
                               
), by=.(natn_dec, natn_qunt, age_group2, sex)]



imd_data[, rate_ed_ae:=ecds_eating_disorder*100/an_pop]
imd_data[, rate_sh_ae:=ecds_self_harm*100/an_pop]
imd_data[, rate_alc_ae:=ecds_alcohol*100/an_pop]
imd_data[, rate_sm_ae:=ecds_substance_misuse*100/an_pop]

imd_data[, rate_ed_ip:=ip_eating_disorder*100/an_pop]
imd_data[, rate_sh_ip:=ip_self_harm*100/an_pop]
imd_data[, rate_alc_ip:=ip_alcohol*100/an_pop]
imd_data[, rate_sm_ip:=ip_substance_misuse*100/an_pop]
imd_data[, rate_otherMH_ip:=ip_other_MH_Diagnosis*100/an_pop]

imd_data[, rate_referrals:=referrals*100/an_pop]
imd_data[, rate_contacts:=mh_contacts*100/an_pop]
imd_data[, rate_ae:=ae*100/an_pop]
imd_data[, rate_adem:=ad_em*100/an_pop]
imd_data[, rate_first:=fc_flag*100/an_pop]

fwrite(imd_data, "./national_imd_Figures_2_3_4_imd_data_DUMMY.csv")
#HF
#imd_data[, (5:18):= replace(.SD, .SD <5, NA), .SDcols = 5:18]
fwrite(imd_data, "./national_imd_Figures_2_3_4_imd_data_DUMMY.csv")


#Figure 2 -- IMD decile, with data points -- A&E+Admissions
ggplot(imd_data, aes(x=natn_dec, y=rate_ae+rate_adem, color=sex)) +
  geom_point(size=3) +
  facet_grid(age_group2~sex, scales = "free_y") +
  scale_color_manual(values = c("Male" = "olivedrab3", "Female"="orangered2")) +
  theme_minimal(base_size = 15)+
  scale_x_continuous(breaks = 1:10)+
  ylab("Incidences per 100 person years") +
  xlab("Deprivation decile (1=least deprived, 10=Most deprived)") +
  theme(axis.text.x=element_text(size=10)) +
  theme(axis.title=element_text(size=8))+
  ggtitle("A&E and Admissions rate vs IMD (DUMMY)")

ggsave("./Figure2_Ae_Adm_imd_age_sex_DUMMY.png", width = 9, height = 6, units = 'in')


#Figure 3 -- IMD decile, with data points -- Referrals
ggplot(imd_data, aes(x=natn_dec, y=rate_referrals, color=sex)) +
  geom_point(size=3) +
  scale_x_continuous(breaks = 1:10)+
  facet_grid(age_group2~sex, scales = "free_y") +
  scale_color_manual(values = c("Male" = "olivedrab3", "Female"="orangered2")) +
  theme_minimal(base_size = 15)+
  ylab("Incidences per 100 person years") +
  xlab("Deprivation decile (1=least deprived, 10=Most deprived)") +
  theme(axis.text.x=element_text(size=10)) +
  theme(axis.title=element_text(size=10))+
  ggtitle("Referrals rate vs IMD (DUMMY)")

ggsave("./Figure3_Referrals_imd_age_sex_DUMMY.png", width = 9, height = 6, units = 'in')


#Figure 4 -- IMD decile, with data points -- Contacts
ggplot(imd_data, aes(x=natn_dec, y=rate_contacts, color=sex)) +
  geom_point(size=3) +
  facet_grid(age_group2~sex, scales = "free_y") +
  scale_color_manual(values = c("Male" = "olivedrab3", "Female"="orangered2")) +
  theme_minimal(base_size = 15)+
  scale_x_continuous(breaks = 1:10)+
  ylab("Incidences per 100 person years") +
  xlab("Deprivation decile (1=least deprived, 10=Most deprived)") +
  theme(axis.text.x=element_text(size=10)) +
  theme(axis.title=element_text(size=8))+
  ggtitle("Contacts rate vs IMD (DUMMY)")

ggsave("./Figure4_Contacts_imd_age_sex_DUMMY.png", width = 9, height = 6, units = 'in')

# chart of share of causes of A&E and admissions 
imd_data<-lsoa_age2_sex[, list(ecds_eating_disorder=sum(ecds_eating_disorder, na.rm=T), 
                               ecds_self_harm=sum(ecds_self_harm, na.rm=T),
                               ecds_substance_misuse=sum(ecds_substance_misuse,na.rm = T), 
                               ecds_alcohol=sum(ecds_alcohol,na.rm = T), 
                               ip_eating_disorder=sum(ip_eating_disorder, na.rm=T),
                               ip_alcohol=sum(ip_alcohol,na.rm=T), 
                               ip_self_harm=sum(ip_self_harm, na.rm=T),
                               ip_substance_misuse=sum(ip_substance_misuse,na.rm = T),
                               ip_other_MH_Diagnosis=sum(ip_other_MH_Diagnosis,na.rm = T),
                               referrals=sum(referrals, na.rm=T),
                               mh_contacts=sum(mh_contacts, na.rm=T),
                               ad_em=sum(ad_em,na.rm = T), 
                               ae=sum(ae,na.rm = T),
                               fc_flag=sum(fc_flag,na.rm = T),
                               an_pop=sum(an_pop,na.rm = T)
                               
), by=.(natn_qunt, age_group2, sex)]

imd_ae<-melt(imd_data, id.vars = c("natn_qunt","age_group2", "an_pop", "sex"), 
             measure.vars =c("ecds_eating_disorder","ecds_self_harm", 
                             "ecds_alcohol","ecds_substance_misuse") )

imd_ae[, rate:=value*100/an_pop]
imd_ae[, variable:=factor(variable, labels = c("Eating Disorders", "Self Harm", 
                                               "Alcohol", "Substance Misuse"))]

#Figure 5 - causes for A&E attendances by age, sex, imd
ggplot(imd_ae, aes(x=natn_qunt, y=rate, fill=variable)) +
  scale_fill_viridis_d("Reason for attendance")+
  geom_area() +facet_grid(age_group2~sex)+theme_minimal(base_size = 15)+
  ylab("Incidences per 100 person years")+xlab("Deprivation quintile (1=most deprived")+
  ggtitle("A&E attendences (DUMMY)")

ggsave("./Figure5_ae_imd_cause_age_sex_DUMMY.png", width = 9, height = 6, units = 'in')

#Figure table: Figure 5 
fwrite(imd_ae, "./national_imd_Figure_5_ae_by_reason_and_imd_DUMMY.csv")



#graph admissions by type imd and sex
imd_adm<-melt(imd_data, id.vars = c("natn_qunt","age_group2", "an_pop", "sex"), 
             measure.vars =c("ip_eating_disorder","ip_self_harm", 
                             "ip_alcohol","ip_substance_misuse", "ip_other_MH_Diagnosis") )

imd_adm[, rate:=value*100/an_pop]

imd_adm[, variable:=factor(variable, labels = c("Eating Disorders", "Self Harm", 
                                               "Alcohol", "Substance Misuse", "Other"))]

#Figure 6 - causes for hospital admissions by age, sex, imd
ggplot(imd_adm, aes(x=natn_qunt, y=rate, fill=variable)) +
  scale_fill_viridis_d("Reason for admission")+
  geom_area() +facet_grid(age_group2~sex)+theme_minimal(base_size = 15)+
  ylab("Incidences per 100 person years")+xlab("Deprivation quintile (1=most deprived)")+ggtitle("Admissions (DUMMY)")

ggsave("./Figure6_Ad_imd_cause_age_sex_DUMMY.png", width = 9, height = 6, units = 'in')


#Figure table: Figure 6 
fwrite(imd_adm, "./national_imd_Figure_6_adm_by_reason_and_imd_DUMMY.csv")


#maps

rate_lsoa <- merge(sp_lsoa,lsoa_age2_sex[age_group2=="[15,25]" & sex=="Female",
                                         .(lsoa11,time,rate_first, rate_contacts, rate_ae, rate_adem,rate_referrals, imd)],
                   by.x="LSOA11CD", by.y="lsoa11", all.x=T)



#Figure 7 - LSOA map of admission rates for 15-25 age group
ggplot() +
  geom_sf(data = rate_lsoa,aes(fill = rate_adem), lwd=0) +
  scale_fill_viridis_c(name="%", alpha = 1) +
  guides(fill = guide_legend(title = "")) +
  theme(legend.text=element_text(size=12))+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+ggtitle("Admissions of 15-25 year old young women (DUMMY)")

ggsave("./Figure7_map_admissions_15_25_DUMMY.png", width = 9, height = 6, units = 'in')


#Figure 8 - LSOA map of contacts rates for 15-25 age group
ggplot() +
  geom_sf(data = rate_lsoa[rate_lsoa$rate_contacts<500,],aes(fill = rate_contacts), lwd=0) +
  scale_fill_viridis_c(name="%", alpha = 1) +
  guides(fill = guide_legend(title = "")) +
  theme(legend.text=element_text(size=12))+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  ggtitle("Contacts with childrens / community mental health services - 15-25 year old young women (DUMMY)")

ggsave("./Figure_8_map_contacts_15_25_DUMMY.png", width = 9, height = 6, units = 'in')



# bivariate map - admissions and contacts

bivar2 <- bi_class(rate_lsoa, x = rate_adem, 
                   y = rate_contacts, style = "fisher", dim = 2)
table(bivar2$bi_class)
class(bivar2$bi_class)

#Figure table: Fig 9
fwrite(bivar2, "./national_imd_Figure_9_bivariate_classification_DUMMY.csv")


#Figure 9 - bivariate map showing areas with higher or lower rates
map2 <- ggplot() +
  geom_sf(data = bivar2,aes(fill = bi_class),color=NA,size = 0.1,show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 2) +
  labs(
    title = "Admission rate and MH service contact rate (DUMMY)", size=10
  ) +
  bi_theme(legend.position="none",legend.key.size = unit(0.2, "cm"))+theme(title = element_text(size=10))

legend2 <- bi_legend(pal = "DkCyan",
                     dim = 2,
                     xlab = "Higher admissions",
                     ylab = "Higher contacts",
                     size = 10)


finalPlot2 <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend2, 0.7, 0.7, 0.3, 0.3)

finalPlot2

ggsave("./Figure9_Bivariate_adem_contacts__15_25_f_DUMMY.png", width = 9, height = 6, units = 'in')


#regression models - cross sectional - what rates are associated with health indicators?

centre<-function(x) {(x-mean(x,na.rm=T))/sd(x, na.rm=T)}

#indicators: Community Needs INdex (CNI), Index of Multiple Deprivations (IMD), proportion White British,
#GP per 1000 population, GP distribution, 'passive' access to green spaces, lone parent

cols<-c("CNI_Score", "imd","prop_white","gpperpop", "green_pas", "lone_parent")
#find the mean for each index
lsoa_age2_sex[, (cols):=lapply(.SD, centre), .SDcols=cols]

lsoa_age2_sex[, rate:=fc_flag/an_pop]


#regression area based association with hospital attendances
#generalised linear model
m1<-glm(ad_em+ae~sex+CNI_Score+
          imd+prop_white+gpperpop+
          green_pas+lone_parent+offset(log(an_pop)),data=lsoa_age2_sex[age_group2=="[0,15)"], family = poisson(link=log))


summary(m1)
1-logLik(m1)/logLik(m0)

m2<-glm(ad_em+ae~sex+CNI_Score+
          imd+prop_white+gpperpop+
          green_pas+lone_parent+offset(log(an_pop)),data=lsoa_age2_sex[age_group2=="[15,25]"], family = poisson(link=log))
summary(m2)
1-logLik(m2)/logLik(m0)

results1<-as.data.table(tidy(
  m1,conf.int=T,
  conf.level=0.95,
  effects=c("fixed"),
  ran_prefix=NULL))

results1<-results1[term!="(Intercept)"]
results1[, result:=factor(c(1:7), labels = c("Male", "CNI","IMD", "% White ethnicity", "GP per 1000 population", "Access to green space", "% Lone parents"))]

results2<-as.data.table(tidy(
  m2,conf.int=T,
  conf.level=0.95,
  effects=c("fixed"),
  ran_prefix=NULL))


results2<-results2[term!="(Intercept)"]
results2[, result:=factor(c(1:7), labels = c("Male", "CNI","IMD", "% White ethnicity", "GP per 1000 population", "Access to green space", "% Lone parents"))]

results1[, out:="Age 0-14"]
results2[, out:="Age 15-25"]

ls<-list(results1,results2)
results<-rbindlist(ls)
results[, estimate:=exp(estimate)]
results[, conf.low:=exp(conf.low)]
results[, conf.high:=exp(conf.high)]

#Figure 10 - regressions for A&E and Emergency admissions
ggplot(results, aes(y=result, x=estimate, group=1)) +
  geom_point(shape=21, size=3, fill="white") +
  geom_vline(xintercept = 1, color="red", linetype="dashed")+
  geom_errorbarh(height=.1, aes(xmin=conf.low, xmax=conf.high)) +facet_wrap(out~.)+theme_minimal(base_size = 15)+
  ylab("")+xlab("relative effect for each sd")+
  ggtitle("A&E and Emergency admissions (DUMMY)")

ggsave("./Figure10_Reg_res_ae_ad_DUMMY.png", width = 9, height = 6, units = 'in')

results<-dcast.data.table(results,term~out, value.var = c("estimate", "p.value", "conf.low", "conf.high"))

#Figure table
fwrite(results[, .(term,`estimate_Age 0-14`,`conf.low_Age 0-14`,`conf.high_Age 0-14`,
                   `estimate_Age 15-25`,`conf.low_Age 15-25`,`conf.high_Age 15-25`)], "./national_imd_Figure_10_reg_ae_admissions_DUMMY.csv")



#regression area based association with contacts

m0<-glm(mh_contacts~1+offset(log(an_pop)),data=lsoa_age2_sex[age_group2=="[0,15)"], family = poisson(link=log))
m1<-glm(mh_contacts~sex+CNI_Score+
          imd+prop_white+gpperpop+
          green_pas+lone_parent+offset(log(an_pop)),data=lsoa_age2_sex[age_group2=="[0,15)"], family = poisson(link=log))


summary(m1)
1-logLik(m1)/logLik(m0)

m0<-glm(mh_contacts~1+offset(log(an_pop)),data=lsoa_age2_sex[age_group2=="[15,25]"], family = poisson(link=log))

m2<-glm(mh_contacts~sex+CNI_Score+
          imd+prop_white+gpperpop+
          green_pas+lone_parent+offset(log(an_pop)),data=lsoa_age2_sex[age_group2=="[15,25]"], family = poisson(link=log))
summary(m2)
1-logLik(m2)/logLik(m0)

results1<-as.data.table(tidy(
  m1,conf.int=T,
  conf.level=0.95,
  effects=c("fixed"),
  ran_prefix=NULL))

results1<-results1[term!="(Intercept)"]
results1[, result:=factor(c(1:7), labels = c("Male", "CNI","IMD", "% White ethnicity", "GP per 1000 population", "Access to green space", "% Lone parents"))]

results2<-as.data.table(tidy(
  m2,conf.int=T,
  conf.level=0.95,
  effects=c("fixed"),
  ran_prefix=NULL))


results2<-results2[term!="(Intercept)"]
results2[, result:=factor(c(1:7), labels = c("Male", "CNI","IMD", "% White ethnicity", "GP per 1000 population", "Access to green space", "% Lone parents"))]

results1[, out:="Age 0-14"]
results2[, out:="Age 15-25"]

ls<-list(results1,results2)
results<-rbindlist(ls)
results[, estimate:=exp(estimate)]
results[, conf.low:=exp(conf.low)]
results[, conf.high:=exp(conf.high)]

#Figure 11 - regressions for MH contacts
ggplot(results, aes(y=result, x=estimate, group=1)) +
  geom_point(shape=21, size=3, fill="white") +
  geom_vline(xintercept = 1, color="red", linetype="dashed")+
  geom_errorbarh(height=.1, aes(xmin=conf.low, xmax=conf.high)) +facet_wrap(out~.)+theme_minimal(base_size = 15)+
  ylab("")+xlab("relative effect for each sd")+
  ggtitle("Contacts with mental health services (DUMMY)")

ggsave("./reg_res_contacts_Figure11_DUMMY.png", width = 9, height = 6, units = 'in')

results<-dcast.data.table(results,term~out, value.var = c("estimate", "p.value", "conf.low", "conf.high"))

#Figure table
fwrite(results[, .(term,`estimate_Age 0-14`,`conf.low_Age 0-14`,`conf.high_Age 0-14`,
                   `estimate_Age 15-25`,`conf.low_Age 15-25`,`conf.high_Age 15-25`)], "./national_imd_Figure_11_reg_contacts_DUMMY.csv")



#lsoa by month and age dataset

#get population frame - by month lsoa and year
lsoa_pop[, age_group2:=cut(age_cat, breaks=c(0,15,25), include.lowest=T, right=F)]
lsoa_pop[, age_group:=cut(age_cat, breaks=c(0,10,15,20,25), include.lowest=T, right=F)]
#subset for <25YO
pop_lsoa_age_month<-lsoa_pop[age_cat<25, list(an_pop=sum(an_pop)), by=.(lsoa11,year, age_group, age_group2,sex)]
pop_lsoa_age_month<-pop_lsoa_age_month[lsoa11 %in% liv_wirral$lsoa11]

#expand to include 12 months in each year
pop_lsoa_age_month <- as.data.table(pop_lsoa_age_month[rep(1:.N, each=12),])
pop_lsoa_age_month[, month:=1:12, by=.(lsoa11,year, age_group, sex)]
pop_lsoa_age_month[, yearmon:=as.IDate(paste(year,month,1,sep="-"),"%Y-%m-%d")]


lsoa_age_month<-d[is.na(New_sex)==F, list(ecds_eating_disorder=sum(ecds_eating_disorder, na.rm=T), 
                                                  ecds_self_harm=sum(ecds_self_harm, na.rm=T),
                                                  ecds_substance_misuse=sum(ecds_substance_misuse,na.rm = T), 
                                                  ecds_alcohol=sum(ecds_alcohol,na.rm = T), 
                                                  ip_eating_disorder=sum(ip_eating_disorder, na.rm=T),
                                                  ip_alcohol=sum(ip_alcohol,na.rm=T), 
                                                  ip_self_harm=sum(ip_self_harm, na.rm=T),
                                                  ip_substance_misuse=sum(ip_substance_misuse_not_alcohol,na.rm = T),
                                                  ip_other_MH_Diagnosis=sum(ip_other_MH_Diagnosis,na.rm = T),
                                                  referrals=sum(referrals, na.rm=T),
                                                  mh_contacts=sum(mh_contacts, na.rm=T),
                                                  ad_em=sum(tot_ad,na.rm = T), 
                                                  ae=sum(tot_ae,na.rm = T),
                                                  fc_flag=sum(fc_flag,na.rm = T)
), by=.(New_LSOA,year, age_group, New_sex, month, year_month)]

lsoa_age_month[, yearmon:=as.IDate(paste(year,month,1,sep="-"),"%Y-%m-%d")]
#cut-off date
lsoa_age_month<-merge(pop_lsoa_age_month[yearmon<as.IDate("2021-07-01")], lsoa_age_month, 
by.x=c("lsoa11","year", "age_group", "sex", "yearmon"),
by.y=c("New_LSOA","year", "age_group", "New_sex", "yearmon"),
all.x=T)


cols<-names(d[10:25])
names(lsoa_age_month[, 10:23])
# where there is no activity data in a cell - this should be zero not NA 
lsoa_age_month[, 10:23][is.na(lsoa_age_month[, 10:23])] <- 0
#add LSOA data
lsoa_age_month<-merge(lsoa_age_month, lsoa_data, by="lsoa11", all.x=T)
#add crime data
lsoa_age_month[, crime_rate:=(criminal_damage_and_arson+anti_social_behaviour+burglary+robbery)/total]

#set up regression
centre<-function(x) {(x-mean(x,na.rm=T))/sd(x, na.rm=T)}

cols<-c("CNI_Score","crime_rate","prop_ibesa",
           "imd","samhi_index","prop_white","prop_u16","prop_o75","gpperpop",
           "ed_dist","gpp_dist.x","green_pas",
           "pm10_mean","prop_1less", "lone_parent", "cohab_child")

lsoa_age_month[, (cols):=lapply(.SD, centre), .SDcols=cols]
#date/time format
lsoa_age_month[, year_month:=format(yearmon,'%Y-%m')]
lsoa_age_month[, covid:=as.numeric(yearmon>as.IDate("2020-03-01"))] 
lsoa_age_month[, covid:=covid+as.numeric(yearmon>as.IDate("2020-05-01"))]
lsoa_age_month[, covid:=covid+as.numeric(yearmon>as.IDate("2021-02-01"))]

lsoa_age_month[, time:=as.numeric(as.factor(year_month))]

fwrite(lsoa_age_month, "./lsoa_age_month_DUMMY.csv")


out<-lsoa_age_month[,c(1,2,3,4,5,7,11:24)]
#out[, (7:20):= replace(.SD, .SD <5, NA), .SDcols = 7:20] # if using raw data

#Figure table
fwrite(out, "./lsoa_by_agegroup_sex_month_DUMMY.csv")


#did the pandemic unequally reduce access to services?
#regressions data
trend_d <- lsoa_age_month[an_pop>0]
trend_d <- trend_d[!year <=  2017]

lsoa_age_month[, sex:=factor(sex, labels = c("Male", "Female"))]


#IMD national tercile
model1<-glm( ad_em~age_group2*sex*as.factor(covid)*(as.factor(natn_terc))*time, data=trend_d,  offset=log(an_pop),
             family = poisson(link=log))

summary(model1)

trend_d[,  exp_ad:=predict(model1, type="response")]

trend_ad<-trend_d[, list(ad_em=sum(ad_em), an_pop=sum(an_pop)), by=.(covid,time,yearmon, natn_terc, age_group2,sex)]

trend_ad[, exp_ad:=predict(model1, type="response", newdata = trend_ad, se.fit =T)[1]]
trend_ad[, se:=predict(model1, type="response", newdata = trend_ad, se.fit =T)[2]]
trend_ad[, lcl:=exp(log(exp_ad)-2*(log(se)))]
trend_ad[, ucl:=exp(log(exp_ad)+2*(log(se)))]

#Figure table: Figs 12 & 13
fwrite(trend_ad, "./Figures_12_13_trend_ad_age_imd_DUMMY.csv")


text_plot <- data.frame(text = c("First Lockdown", "End of first lock down", "End of third lockdown"), 
                        dates = as.Date(c("2020-03-01", "2020-06-01", "2021-02-01")), 
                        stringsAsFactors = FALSE)



#Figure 12
ggplot(data=trend_ad, aes(y=exp_ad, x=yearmon, color=as.factor(natn_terc), group=natn_terc))+
  geom_line(aes(lty=as.factor(natn_terc)), size=2)+
  scale_colour_viridis_d(name="IMD tercile", 
                         labels=c( "Least Deprived", "Q2", "Most Deprived"))+
  scale_fill_viridis_d(guide="none")+
  scale_x_date(date_breaks ="months" )+
  scale_linetype(guide="none")+geom_vline(mapping = aes(xintercept = dates), data = text_plot, 
                                          show.legend = F, linetype = "dotted")+
  xlab("Month") + 
  ylab("Admissions")+
  theme_classic()+
  theme(text = element_text(size=14),legend.position="bottom", 
        legend.box = "horizontal", legend.text = element_text(size=12),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1, size=6))+ 
  theme(strip.background = element_blank())+facet_grid(age_group2~sex, scales = "free_y")+ggtitle("Emergency Admissions (DUMMY)")


ggsave("./Figure12_Trend_admissions_DUMMY.png", width = 9, height = 6, units = 'in')


# trend in contacts

model1<-glm( mh_contacts~age_group2*sex*as.factor(covid)*(as.factor(natn_terc))*time,data=data,  offset=log(an_pop),
             family = poisson(link=log))


trend_ct<-trend_d[, list(contacts=sum(mh_contacts), an_pop=sum(an_pop)), by=.(covid,time,yearmon, natn_terc, age_group2,sex)]

trend_ct[, exp_ad:=predict(model1, type="response", newdata = trend_ct, se.fit =T)[1]]
trend_ct[, se:=predict(model1, type="response", newdata = trend_ct, se.fit =T)[2]]
trend_ct[, lcl:=exp(log(exp_ad)-2*(log(se)))]
trend_ct[, ucl:=exp(log(exp_ad)+2*(log(se)))]



text_plot <- data.frame(text = c("First Lockdown", "End of first lock down", "End of third lockdown"), 
                        dates = as.Date(c("2020-03-01", "2020-06-01", "2021-02-01")), 
                        stringsAsFactors = FALSE)

#Figure x (used elsewhere, but not in report)
#ggplot(data=trend_ct, aes(y=exp_ad, x=yearmon, color=as.factor(natn_qunt), group=natn_qunt)) +geom_line()

#geom_ribbon(aes(fill=as.factor(natn_qunt), ymin=lcl, ymax=ucl),
 #           size=0, alpha=0.3)

ggplot(data=trend_ct, aes(y=exp_ad, x=yearmon, color=as.factor(natn_terc), group=natn_terc))+
  geom_line(aes(lty=as.factor(natn_terc)), size=2)+
  scale_colour_viridis_d(name="IMD tercile", 
                         labels=c( "Least Deprived", "Q2", "Most Deprived"))+
  scale_fill_viridis_d(guide="none")+
  scale_x_date(date_breaks ="months" )+
  scale_linetype(guide="none")+geom_vline(mapping = aes(xintercept = dates), data = text_plot, 
                                          show.legend = F, linetype = "dotted")+
  xlab("Month") + 
  ylab("Contacts with MH services")+
  theme_classic()+
  theme(text = element_text(size=14),legend.position="bottom", 
        legend.box = "horizontal", legend.text = element_text(size=12),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1, size=6))+ 
  theme(strip.background = element_blank())+facet_grid(age_group2~sex, scales = "free_y")+ggtitle("Contacts with MH services (DUMMY)")


ggsave("./Figure13_Trend_contacts_DUMMY.png", width = 9, height = 6, units = 'in')

