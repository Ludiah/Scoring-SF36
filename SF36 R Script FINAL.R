### Rscript for Scoring the SF36 ###################################################################
## by Ludiah Bagakas

#libraries 
library(dplyr)
library(missMethods)

#setworking directory
setwd("C:/Users/lmnba/OneDrive/Documents/STA 635/Scoring SF36")


#read data
SF36=read.csv("SF36 FINAL.csv", sep="," )
attach(SF36)
head(SF36)

#Physical Functioning(1:3)
SF36['M3a']=SF36['Q3a']
SF36['M3b']=SF36['Q3b']
SF36['M3c']=SF36['Q3c']
SF36['M3d']=SF36['Q3d']
SF36['M3e']=SF36['Q3e']
SF36['M3f']=SF36['Q3f']
SF36['M3g']=SF36['Q3g']
SF36['M3h']=SF36['Q3h']
SF36['M3i']=SF36['Q3i']
SF36['M3j']=SF36['Q3j']

#Role Physical(1:5)
SF36['M4a']=SF36['Q4a']
SF36['M4b']=SF36['Q4b']
SF36['M4c']=SF36['Q4c']
SF36['M4d']=SF36['Q4d']

#Bodily Pain Re-coding (1:6)
Q7=SF36['Q7'] 
SF36['M7']=case_when(
  Q7==1~ 6.0, 
  Q7==2~ 5.4,
  Q7==3~ 4.2,
  Q7==4~ 3.1,
  Q7==5~ 2.2,
  Q7==6~ 1.0)

SF36['M8'] <- case_when(
  (Q7 == 1 & Q8 == 1) ~ 6.0,
  ((Q7 == 2 | Q7 == 3 | Q7 == 4 | Q7 == 5 | Q7 == 6) & Q8 == 1) ~ 5.0,
  ((Q7 == 1 | Q7 == 2 | Q7 == 3 | Q7 == 4 | Q7 == 5 | Q7 == 6) & Q8 == 2) ~ 4.0,
  ((Q7 == 1 | Q7 == 2 | Q7 == 3 | Q7 == 4 | Q7 == 5 | Q7 == 6) & Q8 == 3) ~ 3.0,
  ((Q7 == 1 | Q7 == 2 | Q7 == 3 | Q7 == 4 | Q7 == 5 | Q7 == 6) & Q8 == 4) ~ 2.0,
  ((Q7 == 1 | Q7 == 2 | Q7 == 3 | Q7 == 4 | Q7 == 5 | Q7 == 6) & Q8 == 5) ~ 1.0,
  (is.na(Q7) & Q8 == 1) ~ 6.0,
  (is.na(Q7) & Q8 == 2) ~ 4.75,
  (is.na(Q7) & Q8 == 3) ~ 3.5,
  (is.na(Q7) & Q8 == 4) ~ 2.25,
  (is.na(Q7) & Q8 == 5) ~ 1.0,
  TRUE ~ NA)

#General Health re-code (1:5)
Q1=SF36['Q1'] 
SF36['M1']=case_when(
  Q1==1~ 5.0, 
  Q1==2~ 4.4,
  Q1==3~ 3.4,
  Q1==4~ 2.0,
  Q1==5~ 1.0)

SF36['M11a']=SF36['Q11a']

Q11b=SF36['Q11b'] 
SF36['M11b']=case_when(
  Q11b==1~ 5.0, 
  Q11b==2~ 4.0,
  Q11b==3~ 3.0,
  Q11b==4~ 2.0,
  Q11b==5~ 1.0)

SF36['M11c']=SF36['Q11c']

Q11d=SF36['Q11d'] 
SF36['M11d']=case_when(
  Q11d==1~ 5.0, 
  Q11d==2~ 4.0,
  Q11d==3~ 3.0,
  Q11d==4~ 2.0,
  Q11d==5~ 1.0)
head(SF36)

#Vitality re-code (1:5)
Q9a=SF36['Q9a'] 
SF36['M9a']=case_when(
  Q9a==1~ 5.0, 
  Q9a==2~ 4.0,
  Q9a==3~ 3.0,
  Q9a==4~ 2.0,
  Q9a==5~ 1.0)

Q9e=SF36['Q9e'] 
SF36['M9e']=case_when(
  Q9e==1~ 5.0, 
  Q9e==2~ 4.0,
  Q9e==3~ 3.0,
  Q9e==4~ 2.0,
  Q9e==5~ 1.0)

SF36['M9g']=SF36['Q9g']
SF36['M9i']=SF36['Q9i']

#Social Functioning re-code (1:5)
SF36['M10']=SF36['Q10']

Q6=SF36['Q6'] 
SF36['M6']=case_when(
  Q6==1~ 5.0, 
  Q6==2~ 4.0,
  Q6==3~ 3.0,
  Q6==4~ 2.0,
  Q6==5~ 1.0)

#Role Emotional(1:5) 
SF36['M5a']=SF36['Q5a']
SF36['M5b']=SF36['Q5b']
SF36['M5c']=SF36['Q5c']

#Mental Health re-code(1:5) 
SF36['M9b']=SF36['Q9b']
SF36['M9c']=SF36['Q9c']

Q9d=SF36['Q9d'] 
SF36['M9d']=case_when(
  Q9d==1~ 5.0, 
  Q9d==2~ 4.0,
  Q9d==3~ 3.0,
  Q9d==4~ 2.0,
  Q9d==5~ 1.0)

SF36['M9f']=SF36['Q9f']

Q9h=SF36['Q9h'] 
SF36['M9h']=case_when(
  Q9h==1 ~ 5.0, 
  Q9h==2~ 4.0,
  Q9h==3~ 3.0,
  Q9h==4~ 2.0,
  Q9h==5~ 1.0)
head(SF36)

#out of range values 
    #Physical Functioning
SF36$M3a[SF36$Q3a > 3] <- NA
SF36$M3b[SF36$Q3b > 3] <- NA
SF36$M3c[SF36$Q3c > 3] <- NA
SF36$M3d[SF36$Q3d > 3] <- NA
SF36$M3i[SF36$Q3i > 3] <- NA
    #Bodily Pain
SF36$M7[SF36$Q7 > 6] <- NA
    #Role Physical
SF36$M4b[SF36$Q4b > 5] <- NA
SF36$M4d[SF36$Q4d > 5] <- NA
    #General Health
SF36$M1[SF36$Q1 > 5] <- NA
    #Vitality
SF36$M9a[SF36$Q9a > 5] <- NA
    #Role Emotional
SF36$M5b[SF36$Q5b > 5] <- NA
SF36$M5c[SF36$Q5c > 5] <- NA
    #Mental Health
SF36$M9c[SF36$Q9c > 5] <- NA
SF36$M9d[SF36$Q9d > 5] <- NA
head(SF36)

#Mean imputation
    #Physical Functioning
freq.na3=count_NA(SF36[38:47], type="rows")
Q3.mis<- data.frame(SF36[38:47])
Q3.imp<-case_when(
  freq.na3<=5~impute_mean(Q3.mis, type = "rowwise", convert_tibble = TRUE),
  TRUE~Q3.mis
)
    #Bodily Pain
freq.na4=count_NA(SF36[48:51], type="rows")
Q4.mis<- data.frame(SF36[48:51])
Q4.imp<-case_when(
  freq.na4<=2~impute_mean(Q4.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q4.mis
)
    
#Role Physical
freq.na78=count_NA(SF36[52:53], type="rows")
Q7.8.mis<- data.frame(SF36[52:53])
Q7.8.imp<-case_when(
  freq.na78<=1~impute_mean(Q7.8.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q7.8.mis
)
    #General Health
freq.na111=count_NA(SF36[54:58], type="rows")
Q1.11.mis<- data.frame(SF36[54:58])
Q1.11.imp<-case_when(
  freq.na111<=2~impute_mean(Q1.11.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q1.11.mis
)
    #Vitality
freq.na9a=count_NA(SF36[59:62], type="rows")
Q9a.mis<- data.frame(SF36[c(59:62)])
Q9a.imp<-case_when(
  freq.na9a<=2~impute_mean(Q9a.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q9a.mis
)
    #Social Functioning
freq.na610=count_NA(SF36[63:64], type="rows")
Q6.10.mis<- data.frame(SF36[c(63,64)])
Q6.10.imp<-case_when(
  freq.na610<=1~impute_mean(Q6.10.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q6.10.mis
)
    #Role Emotional
freq.na5=count_NA(SF36[65:67], type="rows")
Q5.mis<- data.frame(SF36[65:67])
Q5.imp<-case_when(
  freq.na5<=1~impute_mean(Q5.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q5.mis
)
    #Mental Health
freq.na9b=count_NA(SF36[68:72], type="rows")
Q9b.mis<- data.frame(SF36[68:72])
Q9b.imp<-case_when(
  freq.na9b<=2~impute_mean(Q9b.mis, type = "rowwise", convert_tibble = TRUE), 
  TRUE~Q9b.mis
)

#Raw Scoring
SF36['RAW_PF']=(Q3.imp[1]+Q3.imp[2]+Q3.imp[3]+Q3.imp[4]+Q3.imp[5]+Q3.imp[6]+Q3.imp[7]+Q3.imp[8]+Q3.imp[9]+Q3.imp[10])
SF36['RAW_RP']=Q4.imp[1]+Q4.imp[2]+Q4.imp[3]+Q4.imp[4]
SF36['RAW_BP']=Q7.8.imp[1]+Q7.8.imp[2]
SF36['RAW_GH']=Q1.11.imp[1]+Q1.11.imp[2]+Q1.11.imp[3]+Q1.11.imp[4]+Q1.11.imp[5]
SF36['RAW_VT']=Q9a.imp[1]+Q9a.imp[2]+Q9a.imp[3]+Q9a.imp[4]
SF36['RAW_SF']=Q6.10.imp[1]+Q6.10.imp[2]
SF36['RAW_RE']=Q5.imp[1]+Q5.imp[2]+Q5.imp[3]
SF36['RAW_MH']=Q9b.imp[1]+Q9b.imp[2]+Q9b.imp[3]+Q9b.imp[4]+Q9b.imp[5]
head(SF36)

#Transformed Scale
SF36['Transformed_PF']=((SF36['RAW_PF']-10)/20)*100
SF36['Transformed_RP']=((SF36['RAW_RP']-4)/16)*100
SF36['Transformed_BP']=((SF36['RAW_BP']-2)/10)*100
SF36['Transformed_GH']=((SF36['RAW_GH']-5)/20)*100
SF36['Transformed_VT']=((SF36['RAW_VT']-4)/16)*100
SF36['Transformed_SF']=((SF36['RAW_SF']-2)/8)*100
SF36['Transformed_RE']=((SF36['RAW_RE']-3)/12)*100
SF36['Transformed_MH']=((SF36['RAW_MH']-5)/20)*100
head(SF36)

#z-score standardization
SF36['Standardized_PF']=(SF36['Transformed_PF']-83.29094)/23.75883
SF36['Standardized_RP']=(SF36['Transformed_RP']-82.50964)/25.52028
SF36['Standardized_BP']=(SF36['Transformed_BP']-71.32527)/23.66224
SF36['Standardized_GH']=(SF36['Transformed_GH']-70.84570)/20.97821
SF36['Standardized_VT']=(SF36['Transformed_VT']-58.31411)/20.01923
SF36['Standardized_SF']=(SF36['Transformed_SF']-84.30250)/22.91921
SF36['Standardized_RE']=(SF36['Transformed_RE']-87.39733)/21.43778
SF36['Standardized_MH']=(SF36['Transformed_MH']-74.98685)/17.75604
head(SF36)

#Norm-based transformation 
SF36['NormBase_PF']=50+(SF36['Standardized_PF']*10)
SF36['NormBase_RP']=50+(SF36['Standardized_RP']*10)
SF36['NormBase_BP']=50+(SF36['Standardized_BP']*10)
SF36['NormBase_GH']=50+(SF36['Standardized_GH']*10)
SF36['NormBase_VT']=50+(SF36['Standardized_VT']*10)
SF36['NormBase_SF']=50+(SF36['Standardized_SF']*10)
SF36['NormBase_RE']=50+(SF36['Standardized_RE']*10)
SF36['NormBase_MH']=50+(SF36['Standardized_MH']*10)
head(SF36)

#Aggregation of scores
# Aggregation of scores
SF36['AGG_PHYS'] = (SF36['Standardized_PF'] *  .42402 +
                      SF36['Standardized_RP'] *  .35119 +
                      SF36['Standardized_BP'] *  .31754 +
                      SF36['Standardized_GH'] *  .24954 +
                      SF36['Standardized_VT'] *  .02877 +
                      SF36['Standardized_SF'] * -.00753 +
                      SF36['Standardized_RE'] * -.19206 +
                      SF36['Standardized_MH'] * -.22069
)
SF36['AGG_MENT'] = (SF36['Standardized_PF'] * -.22999 +
                      SF36['Standardized_RP'] * -.12329 +
                      SF36['Standardized_BP'] * -.09731 +
                      SF36['Standardized_GH'] * -.01571 +
                      SF36['Standardized_VT'] *  .23534 +
                      SF36['Standardized_SF'] *  .26876 +
                      SF36['Standardized_RE'] *  .43407 +
                      SF36['Standardized_MH'] *  .48581
)
head(SF36)

#T-score transformation
SF36['PCS']=50+(SF36['AGG_PHYS']*10)
SF36['MCS']=50+(SF36['AGG_MENT']*10)
head(SF36)

#remove columns 38-47
SF36=subset(SF36, select = -c(38,39,40,41,42,43,44,45,
                              46,47,48,49,50,51,52,53,
                              54,55,56,57,58,59,60,61,
                              62,63,64,65,66,67,68,69,
                              70,71,72))
head(SF36)

#rounding
cols.num <- c(2:73)
SF36[cols.num] <- sapply(SF36[cols.num],as.numeric)
sapply(SF36, class)
SF36<-SF36%>%mutate_if(is.numeric, round, digits=2)
head(SF36)

#replace NA with empty
SF36[is.na(SF36)] <- ""
head(SF36)

#save data as .csv file
write.csv(SF36, "C:/Users/lmnba/OneDrive/Documents/STA 635/Scoring SF36/SF36_A_Trans16.csv")
