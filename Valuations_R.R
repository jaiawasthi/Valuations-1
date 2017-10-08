

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


##----Per_Share_Ratios----
Diluted_EPS = Quandl(paste("DEB/",Company,"_",freq,"_","EPS",sep=""),type="xts",start_date=from,end_date = till)[,1]
Cash_EPS = Quandl(paste("DEB/",Company,"_",freq,"_","CEPS",sep=""),type="xts",start_date=from,end_date = till)[,1]
BookValue_PS = Quandl(paste("DEB/",Company,"_",freq,"_","BVSH",sep=""),type="xts",start_date=from,end_date = till)[,1]
Dividend_PS = Quandl(paste("DEB/",Company,"_",freq,"_","DIVSH",sep=""),type="xts",start_date=from,end_date = till)[,1]
Revenue_PS = Quandl(paste("DEB/",Company,"_",freq,"_","REVSH",sep=""),type="xts",start_date=from,end_date = till)[,1]
Operating_Profit_PS = Quandl(paste("DEB/",Company,"_",freq,"_","OPMSH",sep=""),type="xts",start_date=from,end_date = till)[,1]

PerShare_Ratios = cbind(Diluted_EPS,Cash_EPS,BookValue_PS,Dividend_PS,Revenue_PS,Operating_Profit_PS)
PerShare_Ratios_1 = specify_decimal(t(PerShare_Ratios),2)

rownames(PerShare_Ratios_1) = c("EPS","Cash EPS","Book Value EPS","Dividend EPS","Revenue EPS","Operating Profit EPS")
#digits = c(0,0,0,2,0,0,2,0,0,0,2,0,0,2)
print(kable(PerShare_Ratios_1,format="pandoc"))

##----Profitability_Ratios----
LTDE = Quandl(paste("DEB/",Company,"_",freq,"_","LTDE",sep=""),type="xts",start_date=from,end_date = till)[,1]
EBIDT_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","EBIDTPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]
IBT_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","IBTPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]
NETProfit_Margin = Quandl(paste("DEB/",Company,"_",freq,"_","NETPCT",sep=""),type="xts",start_date=from,end_date = till)[,1]
ROCE = Quandl(paste("DEB/",Company,"_",freq,"_","ROCE",sep=""),type="xts",start_date=from,end_date = till)[,1]
ROA = Quandl(paste("DEB/",Company,"_",freq,"_","ROA",sep=""),type="xts",start_date=from,end_date = till)[,1]
Asset_Turnover = Quandl(paste("DEB/",Company,"_",freq,"_","ASETTO",sep=""),type="xts",start_date=from,end_date = till)[,1]


Profitability_Ratios = t(LTDE)
EBIDTPCT = percent(as.vector(EBIDT_Margin[,1]))
Profitability_Ratios_1 = rbind(Profitability_Ratios,EBIDTPCT)
IBTPCT = percent(as.vector(IBT_Margin[,1]))
Profitability_Ratios_2 = rbind(Profitability_Ratios_1,IBTPCT)
NET_Profit_PCT = percent(as.vector(NETProfit_Margin[,1]))
Profitability_Ratios_3 = rbind(Profitability_Ratios_2,NET_Profit_PCT)
ROCE = percent(as.vector(ROCE[,1]))
Profitability_Ratios_4 = rbind(Profitability_Ratios_3,ROCE)
ROA = percent(as.vector(ROA[,1]))
Profitability_Ratios_5 = rbind(Profitability_Ratios_4,ROA)
Asset_Turnover = percent(as.vector(Asset_Turnover[,1]))
Profitability_Ratios_6 = rbind(Profitability_Ratios_5,Asset_Turnover)

rownames(Profitability_Ratios_6) = c("Long Term Debt to Equity","EBITD Margin","IBT Margin","Net Profit Margin","ROCE","ROA","Asset Turnover")
#digits = c(0,0,0,2,0,0,2,0,0,0,2,0,0,2)
print(kable(Profitability_Ratios_6,format="pandoc"))


##----Liquidity_Ratios----
Current_Ratios = Quandl(paste("DEB/",Company,"_",freq,"_","CRATIO",sep=""),type="xts",start_date=from,end_date = till)[,1]
Dividend_Payout = Quandl(paste("DEB/",Company,"_",freq,"_","DIVPAY",sep=""),type="xts",start_date=from,end_date = till)[,1]
Retention_Ratio = Quandl(paste("DEB/",Company,"_",freq,"_","RETRATIO",sep=""),type="xts",start_date=from,end_date = till)[,1]
CFO_to_Sales = Quandl(paste("DEB/",Company,"_",freq,"_","CFO_SALES",sep=""),type="xts",start_date=from,end_date = till)[,1]
Fixed_Asset_Turnover = Quandl(paste("DEB/",Company,"_",freq,"_","NBTO",sep=""),type="xts",start_date=from,end_date = till)[,1]
Working_capital_turnover = Quandl(paste("DEB/",Company,"_",freq,"_","WCTO",sep=""),type="xts",start_date=from,end_date = till)[,1]


Liquidity_Ratios = cbind(Current_Ratios,CFO_to_Sales,Fixed_Asset_Turnover,Working_capital_turnover)
Liquidity_Ratios = specify_decimal(t(Liquidity_Ratios),1)
Dividend_Payout = percent(as.vector(Dividend_Payout[,1]))
Liquidity_Ratios = rbind(Liquidity_Ratios,Dividend_Payout)
Retention_Ratio = percent(as.vector(Retention_Ratio[,1]))
Liquidity_Ratios = rbind(Liquidity_Ratios,Retention_Ratio)
rownames(Liquidity_Ratios) = c("Current Ratios","CFO to Sales","Fixed Asset Turnover","Working Capital Turnover","Dividend Payout Ratio","Retention Ratio")
#digits = c(0,0,0,2,0,0,2,0,0,0,2,0,0,2)
print(kable(Liquidity_Ratios,format="pandoc"))

##----Valuation_Ratios----
Enterprise_Value = Quandl(paste("DEB/",Company,"_",freq,"_","EV",sep=""),type="xts",start_date=from,end_date = till)[,1]
EV_to_EBIDTA = Quandl(paste("DEB/",Company,"_",freq,"_","EVEBIDTA",sep=""),type="xts",start_date=from,end_date = till)[,1]
EV_to_Sales = Quandl(paste("DEB/",Company,"_",freq,"_","EVREV",sep=""),type="xts",start_date=from,end_date = till)[,1]
EV_to_Earnings = Quandl(paste("DEB/",Company,"_",freq,"_","EV_NP",sep=""),type="xts",start_date=from,end_date = till)[,1]
Price_to_Sales = Quandl(paste("DEB/",Company,"_",freq,"_","PS",sep=""),type="xts",start_date=from,end_date = till)[,1]
Price_to_Earnings = Quandl(paste("DEB/",Company,"_",freq,"_","PE",sep=""),type="xts",start_date=from,end_date = till)[,1]
Price_to_BookValue = Quandl(paste("DEB/",Company,"_",freq,"_","PBV",sep=""),type="xts",start_date=from,end_date = till)[,1]
Earnings_Yield = Quandl(paste("DEB/",Company,"_",freq,"_","EYIELD",sep=""),type="xts",start_date=from,end_date = till)[,1]


Valuation_Ratios = cbind(Enterprise_Value,EV_to_EBIDTA,EV_to_Sales,EV_to_Earnings,Price_to_Sales,Price_to_Earnings,Price_to_BookValue)
Valuation_Ratios = specify_decimal(t(Valuation_Ratios),2)
Earnings_Yield = percent(as.vector(Earnings_Yield[,1]))
Valuation_Ratios = rbind(Valuation_Ratios,Earnings_Yield)
rownames(Valuation_Ratios) = c("Enterprise Value","EV to EBITDA","EV to Sales","EV to Earnings","Price to Sales","Price to Earnings","Price to Book Value","Earnings Yield")
#digits = c(0,0,0,2,0,0,2,0,0,0,2,0,0,2)
print(kable(Valuation_Ratios,format="pandoc"))





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
DPR = c(0.2,0.2,0.25,0.25)
ROE = c(0.4,0.4,0.3,0.35)
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
DPR = c(0.25,0.3,0.35,0.40)
ROE = c(0.30,0.25,0.15,0.10)
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


##----FCFE----
EPS = last(Quandl(paste("DEB/",Company,"_",freq,"_","EPS",sep=""),type="xts",start_date=from,end_date = till)[,1])


cat("Growth Phase")
#Growth Phase
DPR = c(0.1,0.1,0.15,0.15)
ROE = c(0.2,0.2,0.2,0.2)
growth_rate = (1-DPR)*ROE
EPS_growth_phase = as.vector(EPS)*((1+growth_rate)^(1:4))
Capital_Adequacy = 0.05
FCFE = EPS_growth_phase*0.95
discount_rate = (1+coe)^(1/(1:4))
FCFE_PV = FCFE*discount_rate

PV_sum = sum(FCFE_PV)


FCFE_Growth_1 = cbind(EPS_growth_phase)
FCFE_Growth_2 = specify_decimal(FCFE_Growth_1,1)
growth_rate_d = percent(as.vector(growth_rate))
Capital_Adequacy_d = percent(as.vector(Capital_Adequacy))
FCFE_Growth_3 = cbind(FCFE_Growth_2,growth_rate_d,Capital_Adequacy_d)
FCFE_d = specify_decimal(FCFE,1)
FCFE_Growth_4 = cbind(FCFE_Growth_3,FCFE_d)

rownames(FCFE_Growth_4) = c("2018-03-31","2019-03-31","2020-03-31","2021-03-31")
colnames(FCFE_Growth_4) =c("EPS","growth rate","Capital Adequacy","FCFE")

print(kable(t(FCFE_Growth_4),format="pandoc"))

cat("Transition Phase")
#Transition Phase
DPR = c(0.2,0.3,0.35,0.40)
ROE = c(0.2,0.18,0.15,0.10)
growth_rate = (1-DPR)*ROE
EPS_Transition = last(EPS_growth_phase)*((1+growth_rate)^(1:4))
FCFE = EPS_Transition*0.95
Capital_Adequacy = 0.05
discount_rate = (1+coe)^(1/(5:8))
FCFE_PV = FCFE*discount_rate

FCFE_Growth_1 = cbind(EPS_Transition)
FCFE_Growth_2 = specify_decimal(FCFE_Growth_1,1)
growth_rate_d = percent(as.vector(growth_rate))
Capital_Adequacy_d = percent(as.vector(Capital_Adequacy))
FCFE_Growth_3 = cbind(FCFE_Growth_2,growth_rate_d,Capital_Adequacy_d)
FCFE_d = specify_decimal(FCFE,1)
FCFE_Growth_4 = cbind(FCFE_Growth_3,FCFE_d)

rownames(FCFE_Growth_4) = c("2022-03-31","2023-03-31","2024-03-31","2025-03-31")
colnames(FCFE_Growth_4) =c("EPS","growth rate","Capital Adequacy","FCFE")

print(kable(t(FCFE_Growth_4),format="pandoc"))


PV_sum = PV_sum + sum(FCFE_PV)


#Final Phase
coe_last = Rf+Rp
last_payout = 0.4
g = 0.10
PV_last = last(EPS_Transition)*0.80/(coe_last - g)

PV_sum = PV_last + PV_sum


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

##----AUM_consumer----
Baj_consumer_aum = read.csv("Bajaj Finance Consumer Lending.csv",stringsAsFactors = F)

rownames(Baj_consumer_aum) = c("2W & 3W finance","Consumer durable finance","Digital product finance","Lifestyle Product finance","Retail EMI and ecommerce","Personal loans cross sell","Salaried Personal loans","Salaried Home loans","BFS Direct")
colnames(Baj_consumer_aum) = c("Variable","Q1 2016","Q2 2016","Q3 2016","Q4 2016","Q1 2017","Q2 2017","Q3 2017","Q4 2017","Q1 2018")
Baj_finance_melt = melt(t(Baj_consumer_aum[,-1]))
colnames(Baj_finance_melt) = c("date","Variable","value")
Baj_finance_melt[,3] = as.integer(as.character(Baj_finance_melt[,3]))
ggplot(Baj_finance_melt, aes(date,value, col=Variable,group=Variable)) +   geom_point() +  geom_line() + ggtitle("Consumer Segment")

##----AUM_sme----


Baj_sme_aum = read.csv("Bajaj Finance SME business.csv",stringsAsFactors = F)

rownames(Baj_sme_aum) = c("Business Loans(BL)","Professional Loans","Loans against property","Self imployed home loans","RM Business")
colnames(Baj_sme_aum) = c("Variable","Q1 2016","Q2 2016","Q3 2016","Q4 2016","Q1 2017","Q2 2017","Q3 2017","Q4 2017","Q1 2018")
Baj_finance_melt = melt(t(Baj_sme_aum[,-1]))
colnames(Baj_finance_melt) = c("date","Variable","value")
Baj_finance_melt[,3] = as.integer(as.character(Baj_finance_melt[,3]))
ggplot(Baj_finance_melt, aes(date,value, col=Variable,group=Variable)) +   geom_point() +  geom_line() + ggtitle("SME segment")

##----AUM_commercial----


Baj_commercial_aum = read.csv("Bajaj Finance Commercial Lending.csv",stringsAsFactors = F)

rownames(Baj_commercial_aum) = c("Securities Lending","Vendor Financing","Financial institutions group","Corporate Finance","Infrastrucutre Lending","Rural Lending")
colnames(Baj_commercial_aum) = c("Variable","Q1 2016","Q2 2016","Q3 2016","Q4 2016","Q1 2017","Q2 2017","Q3 2017","Q4 2017","Q1 2018")
Baj_finance_melt = melt(t(Baj_commercial_aum[,-1]))
colnames(Baj_finance_melt) = c("date","Variable","value")
Baj_finance_melt[,3] = as.integer(as.character(Baj_finance_melt[,3]))
ggplot(Baj_finance_melt, aes(date,value, col=Variable,group=Variable)) +   geom_point() +  geom_line() + ggtitle("Commercial + Rural segment")


