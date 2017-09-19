##----LibCall----
library(Quandl)
Quandl.api_key('t1Hc9QxXD75WZYvR3cga')
library(knitr)
library(reshape2)
library(ggplot2)
Company = "BAJFINANCE"
freq = "A"
from="31-03-2013"
till = "31-03-2017"

##----Income_Statement---- 
Revenue_Annual = Quandl(paste("DEB/",Company,"_",freq,"_","SR",sep=""),type="xts",start_date=from,end_date = till)[,1]
Operating_Expense = Quandl(paste("DEB/",Company,"_",freq,"_","OEXPNS",sep=""),type="xts",start_date=from,end_date = till)[,1]
Operating_Profit = Quandl(paste("DEB/",Company,"_",freq,"_","OP",sep=""),type="xts",start_date=from,end_date = till)[,1]
Operating_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","OPMPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]
Other_Income = Quandl(paste("DEB/",Company,"_",freq,"_","OI",sep=""),type="xts",start_date=from,end_date = till)[,1]

EBITDA = Quandl(paste("DEB/",Company,"_",freq,"_","EBIDT",sep=""),type="xts",start_date=from,end_date = till)[,1]
EBITDA_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","EBIDTPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]

Interest = Quandl(paste("DEB/",Company,"_",freq,"_","INT",sep=""),type="xts",start_date=from,end_date = till)[,1]
Depreciation = Quandl(paste("DEB/",Company,"_",freq,"_","DEP",sep=""),type="xts",start_date=from,end_date = till)[,1]

PBT = Quandl(paste("DEB/",Company,"_",freq,"_","PBT",sep=""),type="xts",start_date=from,end_date = till)[,1]
PBT_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","IBTPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]

Tax = Quandl(paste("DEB/",Company,"_",freq,"_","TAX",sep=""),type="xts",start_date=from,end_date = till)[,1]

Net_Income = Quandl(paste("DEB/",Company,"_",freq,"_","NP",sep=""),type="xts",start_date=from,end_date = till)[,1]
Net_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","NETPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]

Income_Statement = cbind(Revenue_Annual,Operating_Expense,Operating_Profit,Operating_Margin,Other_Income,EBITDA,EBITDA_Margin,Interest,Depreciation,PBT,PBT_Margin,Tax,Net_Income,Net_Margin)
colnames(Income_Statement) = c("Revenue","Expense","Operating Profit","Operating_Margin","Other Income","EBITDA","EBITDA_Margin","Interest","Depreciation","PBT","PBT Margin","Tax","Net Income","Net Income Margin")
Income_Statement = t(Income_Statement)
#digits = c(0,0,0,2,0,0,2,0,0,0,2,0,0,2)
print(kable(Income_Statement,format="pandoc"))

##----Growth_Rates----

# Income Statement Rations
Rev_growth = Quandl(paste("DEB/",Company,"_",freq,"_","REV1",sep=""),type="xts",start_date=from,end_date = till)[,1]
OPCF_growth = Quandl(paste("DEB/",Company,"_",freq,"_","OPCF1",sep=""),type="xts",start_date=from,end_date = till)[,1]
DIV_growth = Quandl(paste("DEB/",Company,"_",freq,"_","DIV1",sep=""),type="xts",start_date=from,end_date = till)[,1]
NI_growth = Quandl(paste("DEB/",Company,"_",freq,"_","NI1",sep=""),type="xts",start_date=from,end_date = till)[,1]
OPM1_growth = Quandl(paste("DEB/",Company,"_",freq,"_","OPM1",sep=""),type="xts",start_date=from,end_date = till)[,1]

#Per Share Growth Rates
EPS_growth = Quandl(paste("DEB/",Company,"_",freq,"_","EPS1",sep=""),type="xts",start_date=from,end_date = till)[,1]
REVSH1_growth = Quandl(paste("DEB/",Company,"_",freq,"_","NI1",sep=""),type="xts",start_date=from,end_date = till)[,1]
OPMSH1_growth = Quandl(paste("DEB/",Company,"_",freq,"_","OPMSH1",sep=""),type="xts",start_date=from,end_date = till)[,1]
EBIDTSH1_growth = Quandl(paste("DEB/",Company,"_",freq,"_","OPMSH1",sep=""),type="xts",start_date=from,end_date = till)[,1]


Growth_Rates_IS = cbind(Rev_growth,OPCF_growth,DIV_growth,NI_growth,OPM1_growth)
colnames(Growth_Rates_IS) = c("Rev Growth","Operating Cash Flow","Dividend","net Income","operating Profit")
Growth_Rates_melt_IS = melt(t(Growth_Rates_IS))

Growth_Rates_PS = cbind(EPS_growth,REVSH1_growth,OPMSH1_growth,EBIDTSH1_growth)
colnames(Growth_Rates_PS) = c("EPS","Rev per Share","Operating Profit per share","EBITDA per Share")
Growth_Rates_melt_PS = melt(t(Growth_Rates_PS))
colnames(Growth_Rates_melt_IS) =c("Variable","date","value")

ggplot(Growth_Rates_melt_IS, aes(date,value, col=Variable,group=Variable)) +   geom_point() +  geom_line() 

colnames(Growth_Rates_melt_PS) =c("Variable","date","value")

ggplot(Growth_Rates_melt_PS, aes(date,value, col=Variable,group=Variable))  +   geom_point() +  geom_line() 


##----DCF----

DIVSH = Quandl(paste("DEB/",Company,"_",freq,"_","DIVSH",sep=""),type="xts",start_date=from,end_date = till)[,1]

#Assumptions
Rf
beta
Rp

coe = Rf + beta*Rp

#Growth Phase
DPR = c(0.2,0.2,0.2,0.2)
ROE = c(0.4,0.4,0.4,0.4)
growth_rate = (1-DPR)*ROE
Dividend = (EPS*growth_rate)*DPR
discount_rate = (1+coe)^(1/(1:4))
Dividend_PV = Dividend*discount_rate


#Transition Phase
DPR = c(0.2,0.2,0.2,0.2)
ROE = c(0.4,0.4,0.4,0.4)
growth_rate = (1-DPR)*ROE
Dividend = (EPS*growth_rate)*DPR
discount_rate = (1+coe)^(1/(1:4))
Dividend_PV = Dividend*discount_rate

#Final Phase



