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

#unskilled wages
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

#slave population, 1619-1865
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

#as our 'prudent man' alternative, consider the coupon rate of all treasury bonds
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

#for both irdf and ratesdf, we take historic rates (pre 1776) from the BoE paper
#https://www.bankofengland.co.uk/working-paper/2020/eight-centuries-of-global-real-interest-rates-r-g-and-the-suprasecular-decline-1311-2018
setwd(datadir); dir()
hratesdf<-fread('bankofengland.csv',skip=1)
hratesdf<-hratesdf[6:nrow(hratesdf),c(1,11)]#,14,22)]
names(hratesdf)<-c('year','rate')#,'rate_safe','rate_private')
hratesdf<-lapply(hratesdf,as.numeric) %>% as.data.frame
hratesdf<-hratesdf[!is.na(hratesdf$year) & !is.na(hratesdf$rate),]
#some summary stats
mean(hratesdf$rate)
mean(hratesdf$rate[hratesdf$year%in%1619:1776]) #6%
mean(hratesdf$rate[hratesdf$year%in%1777:1865]) #6%
mean(ratesdf$rate[ratesdf$year%in%1619:1865]) #5.7%
mean(ratesdf$rate[ratesdf$year%in%1777:1865])

#put everything togetehr
ratesdf<-rbind.fill(
  hratesdf[hratesdf$year%in%(min(hratesdf$year):min(ratesdf$year)),],
  ratesdf
)
irdf<-rbind.fill(
  hratesdf[hratesdf$year%in%(min(hratesdf$year):min(irdf$year)),],
  irdf
)

########################################################
########################################################

#CALCULATE THE REPARTAIONS BILL

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

#this loops through the various permutations
loopdf<-expand.grid(
  slavepop=c('over5','everyone'), #count over5 or everyone?
  workingday=c(12,24), #how long is the working day?
  startyear=c(1619,1776), #to when do we date the bill?
  presentyear=c(1865,2009,2023), #for how long does the bill compound?
  rateofinterest=c('craemer','shiller','treasury') #which interest rate?
)

#if we want to trim it
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

#loops through the permutations and performs calculations
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
  #which compounds until the relevant date
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
      
      #or we use a variable rate of interest
      #based on the average rate of interest in each year, carried forward
      #until the year before the present year
    } else if (thisrow$rateofinterest=='shiller') {
      
      thesers<-irdf$rate[irdf$year%in%tmpdf$year[j]:(thisrow$presentyear-1)]
      mytotal <- tmpdf$wagebill[j]
      for(k in 1:length(thesers)) {
        mytotal <- mytotal * (1 + thesers[k]/100)
      }
      
      #or we use the 'prudent man' rule, who invests
      #assets in government/sovereign bonds
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

#put the total bill in billions, so it is easier to read
loopdf$reparations_bill <- loopdf$reparations_bill/10^9

########################################################
########################################################

#what does this bill come to as a measure of total wealth?

#p. 9, https://www2.census.gov/library/publications/1949/compendia/hist_stats_1789-1945/hist_stats_1789-1945-chA.pdf
#in 1860, estimated national wealth was  
natwealth_1860 <- 17.013 * 10^9 #note that this is lower than PZ estimate below.. 
#https://www.cnn.com/2023/06/08/economy/household-wealth-net-worth/index.html#:~:text=Americans'%20wealth%20rises%20by%20%243%20trillion%20but%20remains%20below%20early%202022%20peak&text=Elevated%20by%20a%20strong%20stock,shy%20of%20its%20recent%20peak.
natwealth_2023 <- 149 * 10^12
(natwealth_2023/natwealth_1860)^(1/163) - 1 #5.7%

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

#at shiller's rates
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

#at craemer's rates
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

#and how big is the bill today?
thisrow<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 & 
    loopdf$presentyear==2023
  ,
]
thisrow$reparations_bill*10^9/natwealth_2023

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

#put both in 1865 dollars using shiller's cpi
setwd(datadir); dir()
tmpdf<-fread('shiller_irate.csv')
tmpdf<-tmpdf[9:nrow(tmpdf),c(1,7)]
names(tmpdf)<-c('year','cpi')
tmpdf<-tmpdf[tmpdf$year!="" & tmpdf$cpi!="",]
tmpdf$cpi<-as.numeric(tmpdf$cpi)
#to put 2011 dollars in 187X terms, divide by deflator
deflator_1871<-tmpdf$cpi[tmpdf$year==2011]/
  tmpdf$cpi[tmpdf$year==1871] #closest we get to 1865
worldgdp_1865 <- worldgdp_1865/deflator_1871
worldwealth_1865 <- worldwealth_1865/deflator_1871

#in billions
worldgdp_1865/10^9
worldwealth_1865/10^9

#make the calculations
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

#note that since average of rates on 'prudent man' rule are a little lower than this
mean(ratesdf$rate[ratesdf$year%in%1770:2010]) #about 4.8%
#the taint will eventually tend to zero, as a proportion of wealth stock
#how long would this take? 
taint<-10
wealth<-taint/(loopdf[
    loopdf$slavepop=='everyone' & 
      loopdf$workingday=='24' & 
      loopdf$rateofinterest=='treasury' &
      loopdf$startyear==1619 & 
      loopdf$presentyear==2023
    ,
    'reparations_bill'
  ] * 10^9 / natwealth_2023
)
taint_gr <- mean(ratesdf$rate[ratesdf$year%in%1770:2010])
wealth_gr <- 100 * ((yf/y0)^(1/t) - 1) #grows at about 5.1%
tmpdf<-lapply(1:5000,function(x) {
  data.frame(
    year=x,
    taint=taint * (1 + taint_gr/100)^x,
    wealth=wealth * (1 + wealth_gr/100)^x
  )
}) %>% rbind.fill
tmpdf$wealthbigger <- as.numeric(tmpdf$wealth>tmpdf$taint)
min(tmpdf$year[tmpdf$wealthbigger==1]) #it takes almost 1,000 years

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

########################################################
########################################################

#how well would Social Democracy vindicate the property rights of ex-slaves?

#us economy
usgdp_2022  <- 25462.70 * 10^9 #https://tradingeconomics.com/united-states/gdp
kworthy <- 0.10 #kenworthy 2014, p. 72

#https://www.usgovernmentspending.com/ 
welfare<- 1743 * 10^9
socialsecurity <- 1219 * 10^9
medicare <- 755 * 10^9
medicaid <- 835 * 10^9
socialspending <- 
  welfare + 
  socialsecurity + 
  medicare + 
  medicaid
socialspending/usgdp_2022 #aobut 18% of GDP

#https://usafacts.org/data/topics/people-society/population-and-demographics/population-data/households/
ushh_2022 <- 131.202 * 10^6
ushh_black_2022 <- 17.698 * 10^6

#reparations bills
reparations_radical<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 &
    loopdf$presentyear==2023
  ,
  'reparations_bill'
] * 10^9
#https://www.rand.org/blog/rand-review/2023/05/what-would-it-take-to-close-americas-black-white-wealth-gap.html#:~:text=The%20median%20wealth%20gap%20in,%2415%20trillion%20to%20eliminate%20it.
reparations_moderate<-15000 * 10^9

#calculations
#how much more needed to bring social democracy?
kworthy * usgdp_2022 #2.5 trilliion
#current redistribution
socialspending/ushh_2022 #35,000
#with the increase
(socialspending + kworthy*usgdp_2022)/ushh_2022 #55,000
#how much is each black household owed?
reparations_radical/ushh_black_2022 #78 million
(reparations_radical/ushh_black_2022) * .05 #earns about this in interest

#if we count social democracy as partial reparations, 
#how much of the debt of slavery is unpaid?

#social spending over time
setwd(datadir); dir()
tmpdf<-fread('social-spending-oecd-longrun.csv')
names(tmpdf)<-c('countryname','code','year','socialspending')
tmpdf<-tmpdf[countryname=='United States',c('year','socialspending')]
tmpdf<-rbind.fill(
  tmpdf,
  data.frame(
    year=2022,
    #not likely to be exactly equivalent, but this is hardly going to be important
    socialspending=c(100 * socialspending/usgdp_2022
    )
  )
)
tmpyears<-1865:2022
tmpdf2<-data.frame(
  year=tmpyears[!tmpyears%in%tmpdf$year],
  socialspending=NA
)
tmpdf<-rbind.fill(
  tmpdf,
  tmpdf2
)
tmpdf<-tmpdf[order(tmpdf$year),]
tmpdf$socialspending <- na.approx(tmpdf$socialspending,na.rm=F)/100
tmpdf$socialspending <- na.locf(tmpdf$socialspending,fromLast = T)
ssdf<-tmpdf

#us nominal gdp over time
setwd(datadir); dir()
tmpdf<-fread('nominalgdp.csv')
tmpdf<-tmpdf[countryname=='United States of America',c('year','value')]
names(tmpdf)<-c('year','nominalgdp')
gdpdf<-tmpdf

#us population over time
setwd(datadir); dir()
tmpdf<-fread('maddison.csv')
tmpdf<-tmpdf[countryname=='United States' & statistic=='population',c('year','value')]
names(tmpdf)<-c('year','population')
popdf<-tmpdf

#this means, per capita social spending in USA was roughly:
tmpdf<-merge(
  ssdf,
  gdpdf,
  by='year'
)
tmpdf<-merge(
  tmpdf,
  popdf,
  by='year'
)
tmpdf$nomgdp_percapita <- tmpdf$nominalgdp/tmpdf$population
tmpdf$socialspending_percapita <- tmpdf$socialspending * tmpdf$nomgdp_percapita

#as a percentage of the reparations bill, this is minisicule
#in 1865:
reparations_radical<-loopdf[
  loopdf$slavepop=='everyone' & 
    loopdf$workingday=='24' & 
    loopdf$rateofinterest=='treasury' &
    loopdf$startyear==1619 &
    loopdf$presentyear==1865
  ,
  'reparations_bill'
] * 10^9
repbill<-reparations_radical/(4.4 * 10^6) #black people were owed about 300,000 per person,
ssgiven<-tmpdf$socialspending_percapita[tmpdf$year==1865] #was about 50 cents per person!
100 * ssgiven/(repbill * .05) 

########################################################
########################################################

#germany reparations to israel
#between 1952 and 1966
reparations_sum <- 3 * 10^9
#w german gdp at this time
setwd(datadir); dir()
tmpdf<-fread('nominalgdp.csv')
tmpdf<-tmpdf[countryname=='West Germany' & year%in%1952:1965,c('year','value')]
1/(((reparations_sum))/sum(tmpdf$value)) #1/1248th of national product

########################################################
########################################################

#how large if we look at ancient injustice?
income<-100 #~https://delong.typepad.com/print/20061012_LRWGDP.pdf
discountrate <- 0.05 #using very high discount rate
assetvalue<-100/discountrate
bill <- assetvalue * ((1+discountrate)^10500)

#global economy today
globalwealth <- 454.4 * 10^12
bill/globalwealth


