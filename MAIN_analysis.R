
rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(lubridate)

library(ggplot2)
library(gridExtra)

#_________________________________________
#### NON CLEAN DATA ####

#...........................
##### analysis 1 ####
# see how many buy different drug from the one prescribed (by year)

purch = readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
temp <- purch %>% mutate(YEAR = format(OSTOPV, format="%Y"))

n <- temp %>% group_by(YEAR) %>% summarise( TOT = n()) 
diff <- temp %>% group_by(YEAR) %>% summarise( DIFF = sum(VNRO != RGTNO) )
result <- 100*(diff$DIFF)/(n$TOT)

#PLOT:
barplot(result,
        names.arg = n$YEAR,
        main="purchase of a different drug than the one prescribed",
        xlab="year", ylab = 'percent (%)')



#_________________________________________
####   CLEAN DATA    ####

#use only purchase and prescription 
kela_purchase <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_purchases.rds')
kanta_prescription <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/joined_statins_prescriptions.rds')
df <- kela_purchase %>% inner_join(kanta_prescription, by = c('HETU'='PATIENT_ID','RKPV'='CREATION_DATE')) 

rm(kela_purchase,kanta_prescription)
n_start <- nrow(df)
pat_start <- length(unique(df$HETU))

#................................
##### 1. remove ANJA patients ####

df <- df %>% filter(!ANJA %in% c('U','B','K'))
#nrow(df); length(unique(df$HETU))

#............................................
##### 2. remove 90 and 250 package users ####

to_remove <- df[(df$PKOKO==90 | df$PKOKO==250),]$HETU %>% unique()
df <- df[!df$HETU %in% to_remove,]
#nrow(df); length(unique(df$HETU))

#..........................................
##### 3. remove duplicates and arrange ####
#NB: code runs in approx 20 mins

# mantain only the first purchase if duplicate
df <- df %>%
  group_by(HETU,RKPV,OSTOPV) %>%
  filter(row_number()==1) %>%
  arrange(HETU,RKPV,OSTOPV) %>%
  ungroup()
#nrow(df); length(unique(df$HETU))


#.................................
#####  4. remove over-buyers  ####
#NB: remove patient that have more than 12 purchases given the same prescription

df <- df %>% mutate(purch_count = ifelse(RKPV!=lag(RKPV), 1, NA) ) 
df[1:3,'purch_count'] <- c(1,2,3)

i=12
while(i>1){
  print(paste0('cycle number: ',12-i))
  df <- df %>% mutate(purch_count = if_else(is.na(purch_count), lag(purch_count)+1, purch_count) )
  i=i-1
}

to_remove <- df[is.na(df$purch_count),]$HETU %>% unique()
df <- df[!df$HETU %in% to_remove,]

#CHECKPOINT: save the list of patient to then use it in other project
clean_patients <- df$HETU %>% unique
saveRDS(clean_patients,'/data/projects/project_mferro/adherence_and_disease_onset/data/clean_patients.rds')


#....................................
#check removal percentage 
n_post <- nrow(df)
pat_post <- length(unique(df$HETU))
100*n_post/n_start
100*pat_post/pat_start

#CHECKPOINT: save data
saveRDS(df,'/data/projects/project_mferro/kela_kanta_reliability/data/clean_data.rds')


#_________________________
#####    ANALYSIS     ####
df <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/clean_data.rds')
df <- df %>% mutate(lag = time_length(difftime(OSTOPV,RKPV), unit="days"))

#......................................................
#extract max number of purchase given one prescription
max_purch <- df %>% 
  group_by(HETU,RKPV) %>% 
  filter(row_number()==n())
#CHECKPOINT: save data
saveRDS(max_purch,'/data/projects/project_mferro/kela_kanta_reliability/data/max_purch.rds')

#...............................
#pack size comparison
max_purch <- readRDS('/data/projects/project_mferro/kela_kanta_reliability/data/max_purch.rds')

max_purch %>% 
  ungroup() %>% 
  group_by(PKOKO) %>% 
  summarise(avg = mean(purch_count),
            std_dev = sd(purch_count),
            minimum = min(purch_count),
            maximum = max(purch_count))

#...............................
#plot number of purchases

ggplot(data=max_purch,aes(purch_count)) +
  geom_bar() +
  xlab('purchases') + ylab('total') +
  labs(title = 'number of purchases given the same prescription') +
  scale_x_continuous(breaks = seq(1,12))

#..............................
#define mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#extract summary statistics
for(i in 1:4){
  temp <- df %>% filter(purch_count==i) 
  print(
    temp %>% summarise(avg = mean(lag),std_dev = sd(lag), m = median(lag), most_freq = mode(lag) )
  )
}

#extract summary statistics by pack size  
for(i in 1:4){
  temp <- df %>% filter(purch_count==i) %>% group_by(PKOKO)
  print(
    temp %>% summarise(avg = mean(lag),std_dev = sd(lag), m = median(lag), most_freq = mode(lag) )
  )
}

#...........................
# plot the value distributions
temp <- df %>% filter(purch_count==1)
p1 <- ggplot(temp,aes(lag)) + geom_histogram(binwidth = 5) +
  labs(title = 'days between prescription and 1st purchase') + xlab('days') + ylab('total') +
  geom_vline(xintercept = c(0,38.1), colour=c('blue','red'),size=0.2) +
  scale_x_continuous(breaks = seq(0,800,by=100),labels = c('0','100','200','300','400','500','600','700','800'),limits = c(-5,800))

temp <- df %>% filter(purch_count==2)
p2 <- ggplot(temp,aes(lag)) + geom_histogram(binwidth = 5) + 
  labs(title = 'days between prescription and 2nd purchase') + xlab('days') + ylab('total') +
  geom_vline(xintercept = c(98,145), colour=c('blue','red'),size=0.2) +
  scale_x_continuous(breaks = seq(0,800,by=100),labels = c('0','100','200','300','400','500','600','700','800'),limits = c(-5,800))

temp <- df %>% filter(purch_count==3)
p3 <- ggplot(temp,aes(lag)) + geom_histogram(binwidth = 5) + 
  labs(title = 'days between prescription and 3rd purchase') + xlab('days') + ylab('total') +
  geom_vline(xintercept = c(203,244), colour=c('blue','red'),size=0.2) +
  scale_x_continuous(breaks = seq(0,800,by=100),labels = c('0','100','200','300','400','500','600','700','800'),limits = c(-5,800))

temp <- df %>% filter(purch_count==4)
p4 <- ggplot(temp,aes(lag)) + geom_histogram(binwidth = 5) + 
  labs(title = 'days between prescription and 4th purchase') + xlab('days') + ylab('total') +
  geom_vline(xintercept = c(301,333), colour=c('blue','red'),size=0.2) +
  scale_x_continuous(breaks = seq(0,800,by=100),labels = c('0','100','200','300','400','500','600','700','800'),limits = c(-5,800))

grid.arrange(p1,p2,p3,p4,ncol=2)

#can always see a second peak after 100 days from thefirst peak, why? 
#second peak is due to people asking for new prescription when buying the last package 

#can see smaller peaks before main peak, why?
#generally due to pack size 
#NB: 28/30 packs days variability is increasing with purchase number


#...........................
# plot the value distributions by pack size
df$PKOKO <- as.factor(df$PKOKO)

temp <- df %>% filter(purch_count==1)
p1 <- ggplot(temp,aes(lag,color=PKOKO)) + geom_freqpoly(binwidth = 5) + labs(title = 'days between prescription and 1st purchase') 
temp <- df %>% filter(purch_count==2)
p2 <- ggplot(temp,aes(lag,color=PKOKO)) + geom_freqpoly(binwidth = 5) + labs(title = 'days between prescription and 2nd purchase')
temp <- df %>% filter(purch_count==3)
p3 <- ggplot(temp,aes(lag,color=PKOKO)) + geom_freqpoly(binwidth = 5) + labs(title = 'days between prescription and 3rd purchase')
temp <- df %>% filter(purch_count==4)
p4 <- ggplot(temp,aes(lag,color=PKOKO)) + geom_freqpoly(binwidth = 5) + labs(title = 'days between prescription and 4th purchase')
grid.arrange(p1,p2,p3,p4,ncol=2)


#....................................
##### analysis 1   ####
temp <- df %>% mutate(YEAR = format(OSTOPV, format="%Y"))
n <- temp %>% group_by(YEAR) %>% summarise( TOT = n()) 
diff <- temp %>% group_by(YEAR) %>% summarise( DIFF = sum(VNRO != RGTNO) )
result <- 100*(diff$DIFF)/(n$TOT)

#plot result by year
barplot(result,
        names.arg = n$YEAR,
        main="purchase of a different drug than the one prescribed",
        xlab="year", ylab = 'percent (%)',ylim = c(0,40))
