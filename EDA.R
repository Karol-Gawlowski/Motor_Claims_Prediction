library(tidyverse)
library(tidyquant)
library(fitdistrplus)


# https://stats.idre.ucla.edu/stata/seminars/regression-models-with-count-data/
# https://rpubs.com/Victorp/france_ggiraph
# https://www.kaggle.com/floser/glm-neural-nets-and-xgboost-for-insurance-pricing
# https://openacttexts.github.io/Loss-Data-Analytics/C-PortMgt.html

freq=read.csv("data/freMTPLfreq.csv")
sev=read.csv("data/freMTPLsev.csv")

data=left_join(freq, sev) %>% relocate(ClaimAmount, .before="ClaimNb") %>% as_tibble()
                              
rm(freq,sev)

data$ClaimAmount=replace_na(data$ClaimAmount,0)

data=data %>% mutate(ClaimAmount=replace_na(data$ClaimAmount,0),
                     Power=factor(Power),
                     Region=factor(Region),
                     Gas=factor(Gas),
                     Brand=factor(Brand),
                     Brand=case_when(Brand=="Fiat"~"Fiat",
                                     Brand=="Japanese (except Nissan) or Korean"~"Jap_Kor",
                                     Brand=="Mercedes, Chrysler or BMW"~"Merc_Chrys_BMW",
                                     Brand=="Opel, General Motors or Ford"~"Op_GM_F",
                                     Brand=="other"~"other",
                                     Brand=="Renault, Nissan or Citroen"~"Ren_Nis_Citr",
                                     Brand=="Volkswagen, Audi, Skoda or Seat"~"VW_Au_Sk_Se",
                                     ),
                     Region=str_replace_all(Region,"-","_"),
                     Brand=factor(Brand)) #done the 2nd time, otherwise err bc of case_when) 
                     


str_replace(data$Region,"_","-")

data %>% select(Region,DriverAge,ClaimNb) %>% 
  # filter(ClaimNb>0) %>% 
  mutate(DriverAge=case_when(DriverAge<25 ~ "Under 25",
                             (DriverAge>=25 & DriverAge<30) ~ "25 to 30",
                             (DriverAge>=30 & DriverAge<35) ~ "30 to 35",
                             (DriverAge>=35 & DriverAge<40) ~ "35 to 40",
                             (DriverAge>=40 & DriverAge<45) ~ "40 to 45",
                             (DriverAge>=45 & DriverAge<50) ~ "45 to 50",
                             (DriverAge>=50 & DriverAge<55) ~ "50 to 55",
                             (DriverAge>=55 & DriverAge<60) ~ "55 to 60",
                             DriverAge>=60 ~ "Over 60 "))

# Basic statistics  -------------------------------------------------------------------------------------
# marginal distributions of factors
data %>% group_by(Power) %>% 
         summarise(n=n()/nrow(data),
                   Avg_Severity=mean(ClaimAmount)) %>% 
         ggplot(aes(x=reorder(Power,-n),y=n,fill=Avg_Severity)) + 
         geom_bar(stat="identity") + 
         geom_text(aes(label=round(n,3)), vjust=-0.3, color="black", size=3.5)+
         ggtitle("Distribution of Power",
                 subtitle = "By claims severity")+ 
         xlab("Power")+
         ylab("")+
         scale_fill_gradient2(low = "white", high = "red")+
         theme_minimal()+
         theme(text=element_text(family="serif"),
               legend.justification = c("right", "top"))+
         labs(fill="Average Severity")
         
         
data %>% group_by(Region) %>% 
         summarise(n=n()/nrow(data),
                   Avg_Severity=mean(ClaimAmount)) %>% 
         ggplot(aes(x=reorder(Region,-n),y=n,fill=Avg_Severity)) +
         geom_bar(stat="identity") + 
         geom_text(aes(label=round(n,3)), vjust=-0.3, color="black", size=3.5)+
         ggtitle("Distribution of Regions",
                 subtitle = "By claims severity")+ 
         xlab("Region")+
         ylab("")+
         scale_x_discrete(guide = guide_axis(n.dodge = 2))+
         scale_fill_gradient2(low = "white", high = "red")+
         theme_minimal()+
         theme(text=element_text(family="serif"),
               legend.justification = c("right", "top"))+
         labs(fill="Average Severity")

data %>% group_by(Brand) %>% 
         summarise(n=n()/nrow(data),
                   Avg_Severity=mean(ClaimAmount)) %>% 
         ggplot(aes(x=reorder(Brand,-n),y=n,fill=Avg_Severity)) + 
         geom_bar(stat="identity") + 
         geom_text(aes(label=round(n,3)), vjust=-0.3, color="black", size=3.5)+
         ggtitle("Distribution of Brands",
                 subtitle = "By claims severity")+ 
         xlab("Brand type")+
         ylab("")+
         scale_x_discrete(guide = guide_axis(n.dodge = 2))+
         scale_fill_gradient2(low = "white", high = "red")+
         theme_minimal()+
         theme(text=element_text(family="serif"),
               legend.justification = c("right", "top"))+
         labs(fill="Average Severity")

data %>% group_by(Gas) %>% 
         summarise(n=n()/nrow(data)) 

# some portfolio summary statistics by region
data %>% select(Region,ClaimNb,ClaimAmount) %>% 
  group_by(Region) %>% 
  summarise(P_Count=n(),
            P_Prop=round(n()/nrow(data),2), 
            Incurred=sum(ClaimAmount),
            Frequency=mean(ClaimNb),
            Freq_stdev=sqrt(var(ClaimNb)),
            Freq_max=max(ClaimNb),
            Severity_avg=mean(ClaimAmount),
            Severity_stdev=sqrt(var(ClaimAmount)),
            Severity_over_2k=sum((ClaimAmount>2000)*1),
            Severity_top_0_5p=quantile(ClaimAmount,probs=0.995), 
            Severity_Max=max(ClaimAmount)) %>% 
  arrange(-P_Count)

# basic summary statistics by car brand
data %>% group_by(Brand) %>% 
         summarise(perc=round(n()/nrow(data),2),
                   Sev=mean(ClaimAmount),
                   Sev_upper=quantile(ClaimAmount,probs=0.99),
                   Freq=mean(ClaimNb),
                   CarAge=mean(CarAge),
                   DriverAge_Q1=quantile(DriverAge,0.25),
                   DriverAge_Q3=quantile(DriverAge,0.75),
                   DriverAge_mean=mean(DriverAge)) %>% 
        arrange(-perc)

# Claim Frequency -------------------------------------------------------------------------------------
# Excel style pivot - Acc Frequency by region and age in bins 
# the plot is not really readable
data %>% select(Region,DriverAge,ClaimNb) %>% 
         # filter(ClaimNb>0) %>% 
         mutate(DriverAge=case_when(DriverAge<25 ~ "Under 25",
                                   (DriverAge>=25 & DriverAge<30) ~ "25 to 30",
                                   (DriverAge>=30 & DriverAge<35) ~ "30 to 35",
                                   (DriverAge>=35 & DriverAge<40) ~ "35 to 40",
                                   (DriverAge>=40 & DriverAge<45) ~ "40 to 45",
                                   (DriverAge>=45 & DriverAge<50) ~ "45 to 50",
                                   (DriverAge>=50 & DriverAge<55) ~ "50 to 55",
                                   (DriverAge>=55 & DriverAge<60) ~ "55 to 60",
                                    DriverAge>=60 ~ "Over 60 ")) %>% 
        # ggplot(aes(x=ClaimNb,fill=DriverAge))+
        # geom_bar(aes(y = stat(count) / sum(count)),position="dodge")+
        # facet_wrap(~Region,scales = "free") 
        pivot_table(.rows=Region,
                     .columns=DriverAge,
                     .values=~AVERAGE(ClaimNb)) %>%
        relocate("Under 25",.before="25 to 30")

# summary of claim frequency by region split by nr of claims
data %>% pivot_table(.rows = Region,
                     .columns = ClaimNb,
                     .values = ~COUNT(ClaimNb)) %>% 
         rowwise() %>% 
         mutate(total = sum(c_across('0':'4'),na.rm = TRUE)) %>%
         mutate(across('0':'4', ~ . / total)) %>% 
         ungroup() %>% 
         mutate(Freq=1*.[[3]]+2*.[[4]]+replace_na(3*.[[5]],0)+replace_na(4*.[[6]],0)) %>% 
         arrange(-total) %>% 
         relocate(Freq, .before='0') %>% 
         relocate(total, .before=Freq)


# Claim Severity -------------------------------------------------------------------------------------
# Density of Claim Amounts by region
quantile(data$ClaimAmount,probs = seq(0.99,1,0.0005))

# claim severity density estimates - excl. attritional claims 
data %>% ggplot(aes(x=ClaimAmount, fill=Gas))+
         geom_density(alpha=0.3)+
         xlim(1,2798)+
         facet_wrap(~Region)+
         theme(legend.position="bottom")+
         ggtitle("Claim Severity by Region",subtitle = "Excluded 0.35% attritional claims")
  
# attritional claims (excl)
data %>% filter(ClaimAmount>13140)

data %>% ggplot(aes(x=ClaimAmount, fill=Gas))+
         geom_density(alpha=0.3)+
         xlim(2798,13140)+
         facet_wrap(~Region)+
         theme(legend.position="bottom")+
         ggtitle("Claim Severity by Region",subtitle = "Excluded 0.35% attritional claims")



# Check the distribution of CLaims severity
# we can see that well known prob distr won't fit well 
ggplot(data,aes(x=ClaimAmount))+
       geom_density()+
       xlim(1,13140)+
       ggtitle("Distribution of Power")+ 
       theme_minimal()+
       theme(text=element_text(family="serif"))+
       geom_vline(xintercept = 900, 
                  linetype="dashed", 
                  color = "red", 
                  size=0.5)+
       geom_vline(xintercept = 1500, 
                  linetype="dashed", 
                  color = "red", 
                  size=0.5)

# descriptive stats of policyholders for the three given intervals of ClaimAmount
# no obvious observations. 

# below 900
data %>% filter(ClaimAmount<900) %>% 
         select(CarAge,DriverAge,Density) %>% 
         apply(2,mean) %>% 
         round(2)
 
data %>% filter(ClaimAmount<900) %>% 
         select(Power,Brand,Region) %>% 
         group_by(Power) %>% 
         summarise(n=n(),
                   freq=n()/nrow(data)) %>% 
         arrange(-n)

# claims [900,1500]
data %>% filter(ClaimAmount>900 & ClaimAmount<1500) %>% 
         select(CarAge,DriverAge,Density) %>% 
         apply(2,mean) %>% 
         round(2)

data %>% filter(ClaimAmount>900 & ClaimAmount<1500) %>% 
  select(Power,Brand,Region) %>% 
  group_by(Power) %>% 
  summarise(n=n(),
            freq=n()/nrow(data)) %>% 
  arrange(-n)

# attritional claims >1500
data %>% filter(ClaimAmount>1500) %>% 
         select(CarAge,DriverAge,Density) %>% 
         apply(2,mean) %>% 
         round(2)

data %>% filter(ClaimAmount>1500) %>% 
         select(Power,Brand,Region) %>% 
         group_by(Power) %>% 
         summarise(n=n(),
                   freq=n()/nrow(data)) %>% 
         arrange(-n)


