rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)


#________________________________________________
#### SINGLE DATASET TRAJECTORIES (2014-2020) ####

#NB: 
# for privacy reason all the trajectories should be translated by a random value 
# in FinnGen thwy are translated by 15 days .. going to use the same

#......................
# PURCHASES
rm(list = ls())
purch <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
purch <- purch %>% arrange(HETU, OSTOPV)
#View(purch)

to_plot <- purch[1:208,c('HETU','OSTOPV')]
pat_list <- to_plot$HETU %>% unique()
for(pat in pat_list){
  #select translation days 
  n <- floor(runif(1,min=-15,max=15))
  #extract new translated trajectory
  new_value <- sapply(to_plot[to_plot$HETU==pat,'OSTOPV'],as.integer ) + n 
  #substitute the original trajectory
  to_plot[to_plot$HETU==pat,'OSTOPV'] <- sapply(new_value,as.IDate )
}

ggplot(to_plot, aes()) +
  geom_point(aes(x=OSTOPV, y=HETU),col='red') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  labs(title="STATIN PURCHASE TRAJECTORY",x ="Time", y = "patient")


#.......................
# PRESCRIPTION
rm(list = ls())
presc <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')
presc <- presc %>% arrange(PATIENT_ID, CREATION_DATE)
#View(presc)

to_plot <- presc[1:52, c('PATIENT_ID','CREATION_DATE')]
pat_list <- to_plot$PATIENT_ID %>% unique()
for(pat in pat_list){
  #select translation days 
  n <- as.integer(floor(runif(1,min=-15,max=15)))
  #extract new translated trajectory
  new_value <- sapply(to_plot[to_plot$PATIENT_ID==pat,'CREATION_DATE'],as.integer ) + n 
  #substitute the original trajectory
  to_plot[to_plot$PATIENT_ID==pat,'CREATION_DATE'] <- sapply(new_value,as.IDate)
}

ggplot(to_plot, aes()) +
  geom_point(aes(x=CREATION_DATE, y=PATIENT_ID),col='black') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  labs(title="STATIN PRESCRIPTION TRAJECTORY",x ="Time", y = "patient")


#......................
# DELIVERIES
rm(list = ls())
deliv <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_deliveries.rds')
deliv <- deliv %>% arrange(PATIENT_ID, CREATION_DATE)
#View(deliv)

to_plot <- deliv[1:180, c('PATIENT_ID','CREATION_DATE')]
#transalte single trajectories
pat_list <- to_plot$PATIENT_ID %>% unique()
for(pat in pat_list){
  #select translation days 
  n <- as.integer(floor(runif(1,min=-15,max=15)))
  #extract new translated trajectory
  new_value <- sapply(to_plot[to_plot$PATIENT_ID==pat,'CREATION_DATE'],as.integer ) + n 
  #substitute the original trajectory
  to_plot[to_plot$PATIENT_ID==pat,'CREATION_DATE'] <- sapply(new_value,as.IDate)
}

ggplot(to_plot, aes()) +
  geom_point(aes(x=CREATION_DATE, y=PATIENT_ID),col='blue') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  labs(title="STATIN DELIVERY TRAJECTORY",x ="Time", y = "patient")


#_______________________________________________
#### JOINT DATASET TRAJECTORIES (2014-2020)####
#NB: the following graph is not translated because is not going to be used 

rm(list = ls()); gc()
final_df = readRDS("/data/projects/project_mferro/kela_kanta_reliability/data/final_inner_df.rds")
#NB: code take long time to execute

final_df <- final_df %>% group_by(HETU) %>% arrange(HETU)
#View(final_df)
to_plot <- final_df[1:275, c('HETU','OSTOPV','RKPV','CREATION_DATE')]

ggplot(to_plot, aes()) +
  #create graph
  geom_point(aes(x=OSTOPV, y=HETU, colour = "purchase_in_kela")) +
  geom_point(aes(x=RKPV, y=HETU, colour="prescription_in_kanta"),size=5)+
  geom_point(aes(x=CREATION_DATE, y=HETU, colour="delivery_in_kanta"),size = 3,shape='cross') + 
  #set legend and titles
  scale_color_manual(values = c("purchase_in_kela" = "red", 
                                "prescription_in_kanta" = "black",
                                "delivery_in_kanta" = "dark green")) +
  theme_minimal() +
  theme(legend.position="bottom", axis.text.y = element_blank()) +
  labs(title="",x ="Time", y = "patient", colour="Date of ")



rm(list = ls())
gc()

