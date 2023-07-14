########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(ggplot2)
require(rprojroot)

homedir<-find_root(
  criterion=has_file('reparations.Rproj')
)
datadir<-file.path(
  homedir,"data"
)
# filesdir<-file.path(
#   homedir,"files"
# )
# outputdir<-file.path(
#   homedir,"output"
# )
# codedir<-file.path(
#   homedir,"code"
# )

########################################################
########################################################

#LOAD DATA

#wages
#https://www.measuringworth.com/datasets/uswage/
setwd(datadir); dir()
wagesdf<-fread('USWAGE_1774-2022.csv')
#ipolate pwages back, as Craemer does
#based on movement in unskilled costs
names(wagesdf)<-c('year','unskilled_costs','pworker_wage')
wagesdf<-data.frame(lapply(wagesdf,as.numeric))
wagesdf$index<-wagesdf$unskilled_costs/wagesdf$unskilled_costs[wagesdf$year==1790]
wagesdf$pworker_wage[wagesdf$year<1790] <- wagesdf$pworker_wage[wagesdf$year==1790] *
  wagesdf$index[wagesdf$year<1790]

#before 1776, assume nominal wages had grown at rate of economy from 1650-1775
#this was an era of deflation (https://www-jstor-org.ezp-prod1.hul.harvard.edu/stable/1171338?seq=2)
#so this is in fact probably conservative
setwd(datadir); dir()
tmpdf<-fread('gdp-per-capita-maddison.csv')
names(tmpdf)<-c('countryname','code','year','realgdp','notes')
tmpdf<-tmpdf[countryname=='United States',]
growthrate<-(tmpdf$realgdp[tmpdf$year==1775]/tmpdf$realgdp[tmpdf$year==1650])^(1/(1775-1650)) - 1 #0.8%
extradf<-data.frame(
  year=1619:min(wagesdf$year)
)
extradf$pworker_wage <- sapply(extradf$year,function(t0) {
  (wagesdf$pworker_wage[wagesdf$year==1774])/((1 + growthrate)^(1774-t0))
})
wagesdf<-rbind.fill(
  extradf,
  wagesdf
)
ggplot(wagesdf[wagesdf$year<1776,],aes(x=year,y=pworker_wage)) + geom_line()

#slavepop
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7716878/
setwd(datadir); dir()
require(rvest)
tmp<-read_html('slavepop_2020.html') %>%
  html_table()
slavedf<-tmp[[1]]
slavedf<-slavedf[-1,c(1,2)]
names(slavedf)<-c('year','slavepop')
slavedf$year <- str_extract(slavedf$year,'[0-9]{4}') %>% as.numeric
slavedf$slavepop <- str_replace_all(slavedf$slavepop,"\\,","") %>% as.numeric
newrow<-data.frame(
  year=1865,
  slavepop=3630336
)
slavedf<-rbind.fill(newrow,slavedf)
slavedf<-slavedf[!is.na(slavedf$year),]

tmpyears<-min(slavedf$year):max(slavedf$year)
slavedf<-rbind.fill(
  slavedf,
  data.frame(
    year=tmpyears[!tmpyears%in%slavedf$year],
    slavepop=NA
  )
)
slavedf<-slavedf[order(slavedf$year),]
slavedf$slavepop<-na.approx(slavedf$slavepop,na.rm=F)
slavedf$slavepop_wage <- slavedf$slavepop *
  (1 - 0.1669) #craemer (2015) says 16.69% are below 5 years of age

# #https://www-statista-com.ezp-prod1.hul.harvard.edu/statistics/1010169/black-and-slave-population-us-1790-1880/
# setwd(datadir); dir()
# slavedf<-fread('slavepop.csv',skip=3)
# names(slavedf)<-c('year','totalpop','slavepop','freepop')
# newrow<-data.frame(
#   year=1776,
#   slavepop=423394
# )
# slavedf<-rbind.fill(newrow,slavedf)
# slavedf<-slavedf[,c('year','slavepop')]
# slavedf$slavepop <- str_replace_all(slavedf$slavepop,"\\,","") %>%
#   as.numeric
# #fill in the intervening years
# tmpyears<-c(1776:1860)
# newrows<-data.frame(
#   year=tmpyears[!tmpyears%in%slavedf$year],
#   slavepop=NA
# )
# slavedf <- rbind.fill(
#   slavedf,
#   newrows
# )
# slavedf <- slavedf[order(slavedf$year),]
# slavedf$slavepop <- na.approx(slavedf$slavepop,na.rm=F)
# slavedf$slavepop_wage <- slavedf$slavepop *
#   (1 - 0.1669) #craemer (2015) says 16.69% are below 5 years of age

#interest rates, as earning asssets for banks
#https://www.nber.org/system/files/chapters/c6961/c6961.pdf
#http://www.econ.yale.edu/~shiller/data.htm
#first sheet
setwd(datadir); dir()
irdf<-fread('interestrates_1.csv')
irdf<-lapply(irdf,function(x) {
  if(sum(!is.na(x))>0) {
    x
  } else {
    NULL
  }
})
irdf<-irdf[!sapply(irdf,is.null)] %>% as.data.frame
names(irdf)<-c('year','boston','ma','ri','nyc','philly','pa')
irdf<-irdf[!is.na(irdf$year),]
#second sheet
setwd(datadir); dir()
irdf2<-fread('interestrates_2.csv')
irdf2<-irdf2[,sapply(irdf2[1,],function(x) !is.na(x)),with=F]
names(irdf2)<-unlist(irdf2[1,]) %>% unname %>% tolower
irdf2<-irdf2[-1,]
irdf2$year<-as.numeric(irdf2$year)
irdf2<-irdf2[!is.na(irdf2$year),]
#put together
irdf<-merge(irdf2,irdf,by='year')
irdf<-pivot_longer(
  irdf,
  cols=names(irdf)[names(irdf)!="year"],
  values_to = 'val',
  names_to = 'var'
)
irdf$val<-str_replace(irdf$val,"\\%","") %>%
  str_replace('\\.\\s','\\.')
tmp<-as.numeric(irdf$val)
irdf$val[is.na(tmp)] #all clear
irdf$val<-as.numeric(irdf$val)
irdf<-pivot_wider(
  irdf,
  names_from='var',
  values_from='val'
)
#add shiller interest rate
setwd(datadir); dir()
irdf3<-fread('shiller_irate.csv')
irdf3<-irdf3[9:nrow(irdf3),c(1,5)]
names(irdf3)<-c('year','us')
irdf3<-irdf3[irdf3$year!="" & irdf3$us!="",]
irdf3$us<-as.numeric(irdf3$us)
irdf<-merge(irdf,irdf3,all=T)
#add missing years
tmpyears<-1776:2023
tmpdf<-data.frame(
  year=tmpyears[!tmpyears%in%irdf$year]
)
irdf<-rbind.fill(
  irdf,
  tmpdf
)  
irdf<-irdf[order(irdf$year),]
#extend historic series back
irdf$carolina<-na.locf(irdf$carolina,fromLast = T,na.rm=F)
irdf$philly<-na.locf(irdf$philly,fromLast = T,na.rm=F)
irdf$rate<-apply(irdf[,-1],1,mean,na.rm=T)
irdf$rate<-na.approx(irdf$rate,na.rm=F) 
irdf$rate<-na.locf(irdf$rate,na.rm=F) #this can be improved w/ new data
irdf$year<-as.numeric(irdf$year)
mean(irdf$rate[irdf$year%in%1776:1860])

#as an alternative, consider the coupon rate of all treasury bonds
#https://github.com/jepayne/US-Federal-Debt-Public/blob/master/Industrial-Suite/DataFrames/BondList.csv
setwd(datadir); dir()
couponsdf<-fread('BondList.csv')
names(couponsdf)<-tolower(names(couponsdf)) %>% str_replace_all("\\s","")
couponsdf$firstyear<-str_extract(couponsdf$authorizingact,"[0-9]{4}")
tmp<-is.na(couponsdf$firstyear)
couponsdf$firstyear[tmp]<-str_extract(couponsdf$`treasury'snameofissue`[tmp],"[0-9]{4}")
tmp<-is.na(couponsdf$firstyear)
couponsdf$firstyear[tmp]<-str_extract(couponsdf$payabledate[tmp],"[0-9]{4}")
tmp<-is.na(couponsdf$firstyear)
sum(tmp); sum(!tmp) #basically got all of them
couponsdf$endyear<-str_extract(couponsdf$finalredemptiondate,"[0-9]{4}")
tmp<-is.na(couponsdf$couponrate)
couponsdf$couponrate[tmp]<-
  apply(couponsdf[tmp,c('couponrate:min','couponrate:max')],1,mean,na.rm=T)
couponsdf<-couponsdf[
  !is.na(couponsdf$firstyear) & !is.na(couponsdf$couponrate),
  c('l1id','firstyear','endyear','couponrate')
]
couponsdf$firstyear<-as.numeric(couponsdf$firstyear)
couponsdf$endyear<-as.numeric(couponsdf$endyear)
tmp<-!is.na(couponsdf$endyear)
#median legnth is about 15
median(couponsdf$endyear[tmp] - couponsdf$firstyear[tmp])
couponsdf$endyear[!tmp]<-couponsdf$firstyear[!tmp] + 15
#now, get average rate in each year
ratesdf <- lapply(1776:1982,function(y) {
  tmp<-apply(couponsdf[,c('firstyear','endyear')],1,function(x) y%in%(x[1]:x[2]))
  tmpdf<-couponsdf[tmp,]
  data.frame(
    year=y,
    rate=mean(tmpdf$couponrate)
  )
}) %>% rbind.fill %>% data.table
ratesdf<-ratesdf[!is.na(ratesdf$rate),]
tmpyears<-1776:1982
tmpdf<-data.frame(
  year=tmpyears[!tmpyears%in%ratesdf$year]
)
ratesdf<-rbind.fill(
  ratesdf,
  tmpdf
)  
#from 1871-2023, we can fill this in with annual yield on 10-year treasury bills
# from https://www.multpl.com/10-year-treasury-rate/table/by-year
setwd(datadir); dir()
tmpdf<-fread('tbills.csv')
names(tmpdf)<-tolower(names(tmpdf))
tmpdf$year<-str_extract(tmpdf$date,"[0-9]{4}") %>% as.numeric
tmpdf$year[is.na(tmpdf$year)]<-as.numeric(str_extract(tmpdf$date[is.na(tmpdf$year)],"[0-9]{2}")) + 1900
tmpdf$year[1:25]<-tmpdf$year[1:25] + 100 #fix 2000s
tmpdf$rate2 <- str_replace(tmpdf$value,"\\%","") %>% as.numeric
tmpdf<-tmpdf[,c('year','rate2')]
ratesdf<-merge(
  ratesdf,
  tmpdf,
  all=T
)
#we take the average of the coupons from 1776-1871
#and then we take the 10-year tbill annual yield afterwards
ratesdf$rate[!is.na(ratesdf$rate2)]<-ratesdf$rate2[!is.na(ratesdf$rate2)]
ratesdf$rate2<-NULL

#for both irdf and ratesdf, we take historic data from the BoE paper
#https://www.bankofengland.co.uk/working-paper/2020/eight-centuries-of-global-real-interest-rates-r-g-and-the-suprasecular-decline-1311-2018
setwd(datadir); dir()
hratesdf<-fread('bankofengland.csv',skip=1)
hratesdf<-hratesdf[6:nrow(hratesdf),c(1,11)]#,14,22)]
names(hratesdf)<-c('year','rate')#,'rate_safe','rate_private')
hratesdf<-lapply(hratesdf,as.numeric) %>% as.data.frame
hratesdf<-hratesdf[!is.na(hratesdf$year) & !is.na(hratesdf$rate),]
ratesdf<-rbind.fill(
  hratesdf[hratesdf$year%in%(min(hratesdf$year):min(ratesdf$year)),],
  ratesdf
)
irdf<-rbind.fill(
  hratesdf[hratesdf$year%in%(min(hratesdf$year):min(irdf$year)),],
  irdf
)

mean(hratesdf$rate)
mean(hratesdf$rate[hratesdf$year%in%1619:1776]) #6%
mean(hratesdf$rate[hratesdf$year%in%1777:1865]) #6%

mean(ratesdf$rate[ratesdf$year%in%1619:1865]) #5.7%

########################################################
########################################################

#REDO CRAEMER

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

loopdf<-expand.grid(
  slavepop=c('over5','everyone'),
  workingday=c(12,24),
  startyear=c(1619,1776),
  presentyear=c(1865,2009,2023),
  rateofinterest=c('craemer','shiller','treasury')
)

# tmp<-loopdf$slavepop=='everyone' &
#   loopdf$workingday==24 & 
#   loopdf$startyear==1619 & 
#   loopdf$presentyear==2023 & 
#   loopdf$rateofinterest=='treasury'
# prefrow<-loopdf[tmp,]
# extrarow<-lapply(names(prefrow),function(var) {
#   #var<-'workingday'
#   print(var)
#   extradf<-data.frame(
#     loopdf[[var]][!loopdf[[var]]%in%prefrow[[var]]] %>% unique
#   ) 
#   names(extradf)<-var
#   extrarow<-expand.grid.df(
#     prefrow[,names(prefrow)!=var],
#     extradf
#   ) 
# }) %>% rbind.fill 
# prefrow$prefmod<-T
# extrarow$prefmod<-F

#trim this to just have preferred

# #trim
# tmp<-loopdf$slavepop=='over5' & 
#   loopdf$workingday==12 & 
#   loopdf$presentyear==2009
# loopdf<-loopdf[tmp,]

loopdf$i<-1:nrow(loopdf)
loopdf$reparations_bill <- sapply(loopdf$i,function(i) {
  
  #i<-1
  print(i)
  thisrow<-loopdf[i,]
  
  #this gives us the wage bill for each year of slavery
  tmpdf <- lapply(thisrow$startyear:1865,function(y) {
    #y<-1619
    ## (1) total working hours = number of slaves * 365 * workinghours
    #number of slaves
    slavepop_y <- ifelse(
      thisrow$slavepop=='over5',
      slavedf$slavepop_wage[slavedf$year==y],
      slavedf$slavepop[slavedf$year==y]
    )
    #working hours
    whours <- slavepop_y * 365 * thisrow$workingday
    ## (2) total wage bill = total working hours * production wage in the given year
    wagebill <- whours * wagesdf$pworker_wage[wagesdf$year==y]
    data.frame(
      year=y,
      wagebill=wagebill
    )
  }) %>% rbind.fill %>% data.table
  
  #each unpaid wage bill is invested at a rate of interest
  #which compounds until the present
  tmpdf2 <- lapply(1:nrow(tmpdf),function(j) {
    #j<-1
    #thisrow$rateofinterest<-'creamer'
    #print(j)
    
    #either we choose craemer's method,
    #which is just to assume 3% interest
    if(thisrow$rateofinterest=='craemer') {
      
      years_to_prez <- thisrow$presentyear - tmpdf$year[j]
      mytotal <- tmpdf$wagebill[j] * ((1 + 0.03) ^ years_to_prez)
      mytotal
      
      #or we use best guess, which is a variable rate of interest
      #based on the average rate of interest in each year, carried forward
      #until the year before the present year
    } else if (thisrow$rateofinterest=='shiller') {
      
      thesers<-irdf$rate[irdf$year%in%tmpdf$year[j]:(thisrow$presentyear-1)]
      mytotal <- tmpdf$wagebill[j]
      for(k in 1:length(thesers)) {
        mytotal <- mytotal * (1 + thesers[k]/100)
      }
    } else if (thisrow$rateofinterest=='treasury') {
      
      thesers<-ratesdf$rate[ratesdf$year%in%tmpdf$year[j]:(thisrow$presentyear-1)]
      mytotal <- tmpdf$wagebill[j]
      for(k in 1:length(thesers)) {
        mytotal <- mytotal * (1 + thesers[k]/100)
      }
    }
    mytotal
    
    #return
    data.frame(
      year=tmpdf$year[j],
      wagebill_interest=mytotal
    )
  }) %>% rbind.fill %>% data.table
  
  sum(tmpdf2$wagebill_interest)
  
})

#put this in billions
loopdf$reparations_bill <- loopdf$reparations_bill/10^9
loopdf

########################################################
########################################################

#p. 9, https://www2.census.gov/library/publications/1949/compendia/hist_stats_1789-1945/hist_stats_1789-1945-chA.pdf
#in 1860, estimated national wealth was  
natwealth_1860 <- 17.013 * 10^9 #lower than PZ estimate below.. 
natwealth_2023 <- 100 * 10^12
(natwealth_2023/natwealth_1860)^(1/163) - 1 #5.4%

#https://www.measuringworth.com/slavery.php
#in 1860, estimated non-slave wealth in the South was
nonslavewealth_1860 <- 1559 * 10^6

#thus, the reparations bill owed in 1865 was X times the national wealth stock
thisrow<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 &
    loopdf$presentyear==1865
  ,
]
thisrow$reparations_bill
(thisrow$reparations_bill)*10^9/natwealth_1860
(thisrow$reparations_bill)*10^9/nonslavewealth_1860

#at shiier's rates
thisrow<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='shiller' &
    loopdf$startyear==1619 &
    loopdf$presentyear==1865
  ,
]
thisrow
(thisrow$reparations_bill)*10^9/natwealth_1860
(thisrow$reparations_bill)*10^9/nonslavewealth_1860

#if we don't count everyone and 12 hrs
thisrow<-loopdf[
  loopdf$slavepop=='over5' & 
    loopdf$workingday=='12' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 &
    loopdf$presentyear==1865
  ,
]
thisrow$reparations_bill
(thisrow$reparations_bill)*10^9/natwealth_1860
(thisrow$reparations_bill)*10^9/nonslavewealth_1860

thisrow<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='craemer' &
    loopdf$startyear==1619 &
    loopdf$presentyear==1865
  ,
]
thisrow$reparations_bill
(thisrow$reparations_bill)*10^9/natwealth_1860
(thisrow$reparations_bill)*10^9/nonslavewealth_1860

########################################################
########################################################

#how did this compare to the size of the global economy?

#https://ourworldindata.org/economic-growth
#size of world economy in 1860 
setwd(datadir); dir()
tmpdf<-fread('world-gdp-over-the-last-two-millennia.csv')
names(tmpdf)<-c('countryname','code','year','gdp')
tmpyears <- min(tmpdf$year):max(tmpdf$year)
tmpdf<-rbind.fill(
  data.frame(
    year=tmpyears[!tmpyears%in%tmpdf$year],
    gdp=NA
  ),
  tmpdf
)
tmpdf<-tmpdf[order(tmpdf$year),]
tmpdf$gdp<-na.approx(tmpdf$gdp,na.rm=F)

worldgdp_1865 <- tmpdf$gdp[tmpdf$year==1865] #1.8 trillion in 2011 dollars
#using wealth-income ratios from around the world today, we estimate world wealth
worldwealth_1865 <- worldgdp_1865 * 4.5 #https://www-degruyter-com.ezp-prod1.hul.harvard.edu/document/doi/10.4159/9780674984769-017/html

#put both in 1860 dollars using shiller's cpi
setwd(datadir); dir()
tmpdf<-fread('shiller_irate.csv')
tmpdf<-tmpdf[9:nrow(tmpdf),c(1,7)]
names(tmpdf)<-c('year','cpi')
tmpdf<-tmpdf[tmpdf$year!="" & tmpdf$cpi!="",]
tmpdf$cpi<-as.numeric(tmpdf$cpi)
#to put 2011 dollars in 187X terms, divide by deflator
deflator_1871<-tmpdf$cpi[tmpdf$year==2011]/
  tmpdf$cpi[tmpdf$year==1871] #closest we get to 1860
worldgdp_1865 <- worldgdp_1865/deflator_1871
worldwealth_1865 <- worldwealth_1865/deflator_1871

worldgdp_1865/10^9
worldwealth_1865/10^9

thisrow<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 & 
    loopdf$presentyear==1865
  ,
]
(thisrow$reparations_bill)*10^9/worldwealth_1865
(thisrow$reparations_bill)*10^9/worldgdp_1865

#and today?
thisrow<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 & 
    loopdf$presentyear==2023
  ,
]
thisrow$reparations_bill


########################################################
########################################################

#taint grows at the rate of interest
#this is around 5% (in nominal terms)

#what does income/wealth grow at? 
#from piketty and zucman 2014 QJE
setwd(datadir); dir()
pzdf<-fread('usa_wealth_pz.csv',skip=8)
pzdf<-pzdf[,c(1,2,3,4,5)]
names(pzdf)<-c('year','income','wealth','income_real','wealth_real')
pzdf<-pzdf[!is.na(pzdf$year) & pzdf$year!="",]
pzdf$year<-str_extract(pzdf$year,"[0-9]{4}") %>% 
  as.numeric %>% na.approx(na.rm=F) 
pzdf <- lapply(pzdf,function(x) {
  str_replace_all(x,"\\,","") %>% as.numeric
}) %>% as.data.frame

#calculate long-run (nominal) growth rate of wealth stock
#r = (yf/y0)^(1/t) - 1
yf<-pzdf$wealth[pzdf$year==2010]
y0<-pzdf$wealth[pzdf$year==1770]
t<-2010-1770
100 * ((yf/y0)^(1/t) - 1) #grows at about 5.1%

yf<-pzdf$wealth[pzdf$year==1860]
y0<-pzdf$wealth[pzdf$year==1770]
t<-1860-1770
100 * ((yf/y0)^(1/t) - 1) #grows at about 4.4%

#real grwoth rate of wealth stock
yf<-pzdf$wealth_real[pzdf$year==2010]
y0<-pzdf$wealth_real[pzdf$year==1770]
t<-2010-1770
100 * ((yf/y0)^(1/t) - 1) #grows at 3.85%

#nominal growth rate of income
yf<-pzdf$income[pzdf$year==2010]
y0<-pzdf$income[pzdf$year==1770]
t<-2010-1770
100 * ((yf/y0)^(1/t) - 1) #grows at about 5.02%

##so, 
#taint grows roughly at the rate of the national economy
#a little larger, over the 1800s.. 
#it will not outgrow the economy, 
#unless we choose a higher interest rate

# #what does world gdp grow at?
# setwd(datadir); dir()
# tmpdf<-fread('world-gdp-over-the-last-two-millennia.csv')
# names(tmpdf)<-c('countryname','code','year','gdp')
# tmpyears <- min(tmpdf$year):max(tmpdf$year)

########################################################
########################################################









