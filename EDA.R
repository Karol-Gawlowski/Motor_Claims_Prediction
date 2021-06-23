library(tidyverse)
library(tidyquant)

freq=read.csv("data/freMTPLfreq.csv")
sev=read.csv("data/freMTPLsev.csv")

data=left_join(freq, sev) %>% relocate(ClaimAmount, .before="ClaimNb") %>% as_tibble()
                              
rm(freq,sev)

data$ClaimAmount=replace_na(data$ClaimAmount,0)

# basic summary statistics
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

# Excel style pivot - Acc Frequency by Brand and Car Age
data %>% pivot_table(.rows=Brand,
                     .columns=CarAge,
                     .values=~AVERAGE(ClaimNb))

# Excel style pivot - Acc Frequency by region and age in bins 
data %>% select(Region,DriverAge,ClaimNb) %>% 
         mutate(DriverAge=case_when(DriverAge<25 ~ "Under 25",
                                   (DriverAge>=25 & DriverAge<30) ~ "25 to 30",
                                   (DriverAge>=30 & DriverAge<35) ~ "30 to 35",
                                   (DriverAge>=35 & DriverAge<40) ~ "35 to 40",
                                   (DriverAge>=40 & DriverAge<45) ~ "40 to 45",
                                   (DriverAge>=45 & DriverAge<50) ~ "45 to 50",
                                   (DriverAge>=50 & DriverAge<55) ~ "50 to 55",
                                   (DriverAge>=55 & DriverAge<60) ~ "55 to 60",
                                    DriverAge>=60 ~ "Over 60 ")) %>% 
         pivot_table(.rows=Region,
                     .columns=DriverAge,
                     .values=~AVERAGE(ClaimNb)) %>% 
         relocate("Under 25",.before="25 to 30") 

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
                   Severity_Max=max(ClaimAmount)) %>% 
         arrange(-P_Count)
    

# Density of Claims by region
quantile(data$ClaimAmount,probs = seq(0.99,1,0.0005))

data %>% ggplot(aes(x=ClaimAmount, fill=Gas))+
         geom_density(alpha=0.3)+
         xlim(1,2798)+
         facet_wrap(~Region)+
         theme(legend.position="bottom")+
         ggtitle("Claim Severity by Region",subtitle = "Excluded 0.35% attritional claims")
  

# Histogram of claims frequency by car- not meaningful
data %>% ggplot(aes(x=ClaimNb))+
         geom_histogram()+
         facet_wrap(~Region)+
         theme(legend.position="bottom")+
         ggtitle("Claim Severity by Region",subtitle = "Excluded 0.35% attritional claims")

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
