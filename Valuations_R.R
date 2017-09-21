
##----LibCall----
library(Quandl)
Quandl.api_key('----')
library(knitr)
library(reshape2)
library(ggplot2)
library(scales)
Company = "BAJFINANCE"
freq = "A"
from="31-03-2013"
till = "31-03-2017"
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


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

Income_Statement = cbind(Revenue_Annual,Operating_Expense,Operating_Profit)
Income_Statement_1 = specify_decimal(t(Income_Statement),0)
OMAR = percent(as.vector(Operating_Margin[,1]))
Income_Statement_2 = rbind(Income_Statement_1,OMAR)
Other_Income_d = specify_decimal(Other_Income,0)
EBITDA_d = specify_decimal(EBITDA,0)
Income_Statement_3 = rbind(Income_Statement_2,t(Other_Income_d),t(EBITDA_d))
EBITDAMAR = percent(as.vector(EBITDA_Margin[,1]))
Income_Statement_4 = rbind(Income_Statement_3,EBITDAMAR)
Interest_d = specify_decimal(Interest,0)
Depreciation_d = specify_decimal(Depreciation,0)
PBT_d = specify_decimal(PBT,0)
Income_Statement_5 = rbind(Income_Statement_4,t(Interest_d),t(Depreciation_d),t(PBT_d))
PBTMAR = percent(as.vector(PBT_Margin[,1]))
Tax_d = specify_decimal(Tax,0)
Net_Income_d = specify_decimal(Net_Income,0)
Income_Statement_6 = rbind(Income_Statement_5,PBTMAR,t(Tax_d),t(Net_Income_d))
NETMAR = percent(as.vector(Net_Margin[,1]))
Income_Statement_7 = rbind(Income_Statement_6,NETMAR)

rownames(Income_Statement_7) = c("Revenue","Expense","Operating Profit","Operating Margin","Other Income","EBITDA","EBITDA Margin","Interest","Depreciation","PBT","PBT Margin","Tax","Net Income","Net Income Margin")
#digits = c(0,0,0,2,0,0,2,0,0,0,2,0,0,2)
print(kable(Income_Statement_7,format="pandoc"))

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

EPS = last(Quandl(paste("DEB/",Company,"_",freq,"_","EPS",sep=""),type="xts",start_date=from,end_date = till)[,1])

#Assumptions
Rf= 0.05
beta = 0.88
Rp = 0.0739
Country_premium = 0.0170



coe = Rf + beta*(Rp+Country_premium)
cat("Growth Phase")
#Growth Phase
DPR = c(0.1,0.1,0.15,0.15)
ROE = c(0.2,0.2,0.2,0.2)
growth_rate = (1-DPR)*ROE
EPS_growth_phase = as.vector(EPS)*((1+growth_rate)^(1:4))
Dividend = EPS_growth_phase*DPR
discount_rate = (1+coe)^(1/(1:4))
Dividend_PV = Dividend*discount_rate

PV_sum = sum(Dividend_PV)


Dividend_Growth_1 = cbind(EPS_growth_phase)
Dividend_Growth_2 = specify_decimal(Dividend_Growth_1,1)
growth_rate_d = percent(as.vector(growth_rate))
DPR_d = percent(as.vector(DPR))
Dividend_Growth_3 = cbind(Dividend_Growth_2,growth_rate_d,DPR_d)
Dividend_d = specify_decimal(Dividend,1)
Dividend_Growth_4 = cbind(Dividend_Growth_3,Dividend_d)

rownames(Dividend_Growth_4) = c("2018-03-31","2019-03-31","2020-03-31","2021-03-31")
colnames(Dividend_Growth_4) =c("EPS","growth rate","Div. Payout Ratio","Dividend")

print(kable(t(Dividend_Growth_4),format="pandoc"))

cat("Transition Phase")
#Transition Phase
DPR = c(0.2,0.3,0.35,0.40)
ROE = c(0.2,0.18,0.15,0.10)
growth_rate = (1-DPR)*ROE
EPS_Transition = last(EPS_growth_phase)*((1+growth_rate)^(1:4))
Dividend = EPS_Transition*DPR
discount_rate = (1+coe)^(1/(5:8))
Dividend_PV = Dividend*discount_rate

Dividend_Growth_1 = cbind(EPS_Transition)
Dividend_Growth_2 = specify_decimal(Dividend_Growth_1,1)
growth_rate_d = percent(as.vector(growth_rate))
DPR_d = percent(as.vector(DPR))
Dividend_Growth_3 = cbind(Dividend_Growth_2,growth_rate_d,DPR_d)
Dividend_d = specify_decimal(Dividend,1)
Dividend_Growth_4 = cbind(Dividend_Growth_3,Dividend_d)

rownames(Dividend_Growth_4) = c("2022-03-31","2023-03-31","2024-03-31","2025-03-31")
colnames(Dividend_Growth_4) =c("EPS","growth rate","Div. Payout Ratio","Dividend")

print(kable(t(Dividend_Growth_4),format="pandoc"))


PV_sum = PV_sum + sum(Dividend_PV)


#Final Phase
coe_last = Rf+Rp
last_payout = 0.4
g = 0.10
PV_last = last(EPS_Transition)*last_payout/(coe_last - g)

PV_sum = PV_last + PV_sum

Growth_Rates_PS = cbind(EPS_growth,REVSH1_growth,OPMSH1_growth,EBIDTSH1_growth)
Payout_ratio = Quandl(paste("DEB/",Company,"_",freq,"_","DIVPAY",sep=""),type="xts",start_date=from,end_date = till)[,1]
ROE = Quandl(paste("DEB/",Company,"_",freq,"_","ROE",sep=""),type="xts",start_date=from,end_date = till)[,1]

cat("Cost of Equity Calculation")
Rf_d = percent(Rf)
Rp_d = percent(Rp)
Country_premium_d = percent(Country_premium)
beta_d = specify_decimal(beta,2)
assumptions = rbind(Rf_d,beta,Rp_d,Country_premium_d)
rownames(assumptions) = c("Risk Free Rate","Beta","Risk Premium","Country Risk Premium")
Total = rbind(PV_last,PV_sum)
rownames(Total) = c("Last Value","Value per Share")
print(kable(assumptions,format = "pandoc"))


cat("Final Values")
Total_d = specify_decimal(Total,0)
print(kable(Total_d,format = "pandoc"))

