from math import  sqrt, exp, log, erf
from decimal import *

# These are the values we are using

S = float(input("current stock price:"))
X = float (input("strike price:"))
T = float(input("Days to expiration:"))
R = float(input("risk free rate:"))
sigma = float(input("Standard deviation of stocks return:"))
divrate = float(input("dividend return:"))

sigTsquared = sqrt(Decimal(T)/365)*sigma
edivT = exp((-divrate*T)/365)
ert = exp((-R*T)/365)
d1 = (log(S*edivT/X)+(R+.5*(sigma**2))*T/365)/sigTsquared
d2 = d1-sigTsquared
Nd1 = (1+erf(d1/sqrt(2)))/2
Nd2 = (1+erf(d2/sqrt(2)))/2
iNd1 = (1+erf(-d1/sqrt(2)))/2
iNd2 = (1+erf(-d2/sqrt(2)))/2

callprice = round(S*edivT*Nd1-X*ert*Nd2, 2)
putprice = round(X*ert*iNd2-S*edivT*iNd1, 2)

print("")
print("Call Price = "+str(callprice))
print("Put Prce = "+str(putprice))
