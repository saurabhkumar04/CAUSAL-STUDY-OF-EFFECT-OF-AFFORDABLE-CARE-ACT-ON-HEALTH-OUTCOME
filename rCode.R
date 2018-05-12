library(survey)			# load survey package (analyzes complex design surveys)
library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)
library(stringr)

setwd("/N/dc2/projects/SocialDataScience/brfss")
medicaid_states <- read.delim("/N/dc2/projects/SocialDataScience/brfss/medicaid_states.csv")

res <- list()
for (i in 2008){
  year <- toString(i)
  load( paste0('b',year, ' design.rda' ))
  brfss.d <- open( brfss.design , driver = MonetDB.R() )	# single-year design
  brfss.d <-
    update(
      brfss.d ,
      hlthpln1 = factor( hlthpln1 ) ,
      sex = factor( sex ) ,
      medcost = factor( medcost )
    )
  #dbfolder <- paste0( getwd() , "/MonetDB" )
  dbfolder <- "."
  db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
  query <- paste0("SELECT xstate,(case when diabete2=1 then 1 else 0 end) as diabetec FROM b",year)
  #query <-paste0("select * FROM b",year," where 1=2")
  data <- dbGetQuery( db , query)
  data$year <- year
  data <- merge(data,medicaid_states, by.x="xstate", by.y="xstate", all.x=TRUE)
  res[[i-2007]]<-data
}

for (i in 2011:2015){
  year <- toString(i)
  load( paste0('b',year, ' design.rda' ))
  brfss.d <- open( brfss.design , driver = MonetDB.R() )	# single-year design
  brfss.d <-
    update(
      brfss.d ,
      hlthpln1 = factor( hlthpln1 ) ,
      sex = factor( sex ) ,
      medcost = factor( medcost )
    )
  #dbfolder <- paste0( getwd() , "/MonetDB" )
  dbfolder <- "/gpfs/home/s/h/shmitra/brfss/MonetDB"
  db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
  query <- paste0("SELECT xstate,(case when diabete3=1 then 1 else 0 end) as diabetec FROM b",year)
  #query <-paste0("select * FROM b",year," where 1=2")
  data <- dbGetQuery( db , query)
  data$year <- year
  data <- merge(data,medicaid_states, by.x="xstate", by.y="xstate", all.x=TRUE)
  res[[i-2007]]<-data
}


fin<-res[[1]]
for(i in 2:length(res)){
  fin<-rbind(fin,res[[i]])
}

names(fin)<-c("xstate",
                            "diabetic",
                            "year",
                            "Location",
                            "Marketplace_Type",
                            "medicaid_status")
levels(fin$medicaid_status)=c("Medicaid","No_Medicaid")
							
fin$PreOrPost = "temp"

fin$PreOrPost[fin$year<=2013] = "Pre"
fin$PreOrPost[fin$year>2013] = "Post"


model <- glm(diabetic ~medicaid_status+PreOrPost+medicaid_status*PreOrPost+factor(Location),family=binomial(link='logit'),data=fin)
summary(model)
							
res <- list()
for (i in 2011:2015){
  year <- toString(i)
  load( paste0('b',year, ' design.rda' ))
  brfss.d <- open( brfss.design , driver = MonetDB.R() )	# single-year design
  brfss.d <-
    update(
      brfss.d ,
      hlthpln1 = factor( hlthpln1 ) ,
      sex = factor( sex ) ,
      medcost = factor( medcost )
    )
  #dbfolder <- paste0( getwd() , "/MonetDB" )
  dbfolder <- "/gpfs/home/s/h/shmitra/brfss/MonetDB"
  db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
  #query <- paste0("SELECT xstate,COUNT(*) AS num_records,sum(case when diabete3=1 then 1 else 0 end) as diabetec FROM b",year, " group by xstate")
  query <- paste0("SELECT xstate,(case when CHCKIDNY=1 then 1 else 0 end) as kidney FROM b",year)
  #, " where HLTHPLN1=1")
	#query <-paste0("select * FROM b",year," where 1=2")
  data <- dbGetQuery( db , query)
  data$year <- year
  data <- merge(data,medicaid_states, by.x="xstate", by.y="xstate", all.x=TRUE)
  res[[i-2010]]<-data
}

fin<-res[[1]]
for(i in 2:length(res)){
  fin<-rbind(fin,res[[i]])
}
head(fin)
library(sqldf)
names(fin)<-c("xstate",
	"kidney_disease",
	"year",
	"Location",
	"Marketplace_Type",
	"medicaid_status")
	levels(fin$medicaid_status)=c("Medicaid","No_Medicaid")

fin$PreOrPost = "temp"

fin$PreOrPost[fin$year<=2013] = "Pre"
fin$PreOrPost[fin$year>2013] = "Post"


model <- glm(kidney_disease ~medicaid_status+PreOrPost+medicaid_status*PreOrPost,family=binomial(link='logit'),data=fin)
summary(model)

fin2<-sqldf("select year,medicaid_status,xstate,sum(diabetec)/sum(num_records)
      from fin group by year,medicaid_status,xstate")

fin3<-sqldf("lect case when year<2013 then 0 else 1 end as post
,case when medicaid_status='Adopted the Medicaid Expansion' then 1
            else 0 end as treatment,xstate,sum(diabetec) as diabetec_cnt
            ,sum(num_records) as rec_cnt
            from fin group by post,treatment,xstate")

write.csv(fin2)

2012-2015 chccopd1
2011-2015 addepev2
2011-2015 chckidny
2008-2015 cvdcrhd4


model <- glm(emphysema ~medicaid_status+PreOrPost+medicaid_status*PreOrPost,family=binomial(link='logit'),data=fin)
summary(model)

Call:
glm(formula = diabetec ~ medicaid_status + year + medicaid_status *
    year, family = binomial(link = "logit"), data = fin)

Deviance Residuals:
    Min       1Q   Median       3Q      Max
-0.5419  -0.5379  -0.5235  -0.5021   2.0656

Coefficients:
                                     Estimate Std. Error  z value Pr(>|z|)
(Intercept)                         -2.007344   0.005842 -343.583   <2e-16 ***
medicaid_statusNo Medicaid           0.163035   0.008710   18.719   <2e-16 ***
year2014                             0.089164   0.008156   10.933   <2e-16 ***
medicaid_statusNo Medicaid:year2014 -0.105087   0.012491   -8.413   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 720275  on 940022  degrees of freedom
Residual deviance: 719840  on 940019  degrees of freedom
  (16414 observations deleted due to missingness)
AIC: 719848

Number of Fisher Scoring iterations: 4

View(as.list(aggregate(emphysema ~ year+medicaid_status, data = fin, FUN = function(x) c(disease = sum(x), total = length(x) ) )))


res <- list()
for (i in 2011:2015){
  year <- toString(i)
  load( paste0('b',year, ' design.rda' ))
  brfss.d <- open( brfss.design , driver = MonetDB.R() )	# single-year design
  brfss.d <-
    update(
      brfss.d ,
      hlthpln1 = factor( hlthpln1 ) ,
      sex = factor( sex ) ,
      medcost = factor( medcost )
    )
  #dbfolder <- paste0( getwd() , "/MonetDB" )
  dbfolder <- "/gpfs/home/s/h/shmitra/brfss/MonetDB"
  db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
  #query <- paste0("SELECT xstate,COUNT(*) AS num_records,sum(case when diabete3=1 then 1 else 0 end) as diabetic FROM b",year, " group by xstate")
  query <- paste0("SELECT xstate,(case when diabete3=1 then 1 else 0 end) as diabetes,(case when SMOKE100=1 then 1 else 0 end) as tobacco
  ,(case when HPVADSHT=1 then 1 else 0 end) as hpv
  ,(case when (SCNTWRK1>=1) AND (SCNTWRK1<=96) then SCNTWRK1 else 0 end) as soc_context
  FROM b",year)
  #, " where HLTHPLN1=1")
	#query <-paste0("select * FROM b",year," where 1=2")
  data <- dbGetQuery( db , query)
  data$year <- year
  data <- merge(data,medicaid_states, by.x="xstate", by.y="xstate", all.x=TRUE)
  res[[i-2010]]<-data
}

fin<-res[[1]]
for(i in 2:length(res)){
  fin<-rbind(fin,res[[i]])
}

 names(fin)<-c("xstate",
                            "diabetes",
							"tobacco",
							"HPV",
							"soc_context",
                            "year",
                            "Location",
                            "Marketplace_Type",
                            "medicaid_status")
                            levels(fin$medicaid_status)=c("Medicaid","No Medicaid")

fin$PreOrPost = "temp"

fin$PreOrPost[fin$year<=2013] = "Pre"
fin$PreOrPost[fin$year>2013] = "Post"

model <- glm(diabetes ~medicaid_status+PreOrPost+medicaid_status*PreOrPost+tobacco+HPV+soc_context,family=binomial(link='logit'),data=fin)
summary(model)

View(as.list(aggregate(kidney ~ year+medicaid_status, data = fin, FUN = function(x) c(disease = sum(x), total = length(x) ) )))


y2006 <- read.delim('b2006 design.rda')
head(y2006$X)

ym <- load('b2007 design.rda')
ls()

local({
  load('b2006 design.rda')
  ls()
}) 
ym

     
     
     
     
     
     