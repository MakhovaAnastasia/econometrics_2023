library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(quantmod)

library(reshape2)
library(CausalImpact)
library(zoo)
library (corrplot)
library(stargazer)


##### Выгрузим цены акций из yahoo

setSymbolLookup(HYBE=list(src="yahoo", name="352820.KS", from ='2020-10-21'))
getSymbols("HYBE")

setSymbolLookup(SM=list(src="yahoo", name="041510.KQ", from ='2020-10-21'))
getSymbols("SM")

setSymbolLookup(JYP=list(src="yahoo", name="035900.KQ", from ='2020-10-21'))
getSymbols("JYP")

setSymbolLookup(YG=list(src="yahoo", name="122870.KQ", from ='2020-10-21'))
getSymbols("YG")

setSymbolLookup(CUBE=list(src="yahoo", name="182360.KQ", from ='2020-10-21'))
getSymbols("CUBE")

setSymbolLookup(KAKAO=list(src="yahoo", name="035720.KS", from ='2020-10-21'))
getSymbols("KAKAO")

setSymbolLookup(NAVER=list(src="yahoo", name="035420.KS", from ='2020-10-21'))
getSymbols("NAVER")

setSymbolLookup(SAMSUNG=list(src="yahoo", name="005930.KS", from ='2020-10-21'))
getSymbols("SAMSUNG")

setSymbolLookup(HYUNDAI =list(src="yahoo", name="005380.KS", from ='2020-10-21'))
getSymbols("HYUNDAI")

##### Датафрейм цен закрытия компаний

df<- data.frame(HYBE[,4], SM[,4], JYP[,4], YG[,4], CUBE[,4], KAKAO[,4],NAVER[,4], SAMSUNG[,4], HYUNDAI[,4])
colnames(df) <- c("HYBE","SM", "JYP", "YG","CUBE","KAKAO","NAVER", "SAMSUNG", "HYUNDAI")
df <- na.omit(df)

cor(df)
corrplot(cor(df), tl.col = "black")

##### Приведем к стационарному виду

dUse <- data.frame(HYBE = exp(diff(log(HYBE[,4]))) - 1, 
                   SM = exp(diff(log(SM[,4]))) - 1, 
                   JYP = exp(diff(log(JYP[,4]))) - 1,
                   YG = exp(diff(log(YG[,4]))) - 1,
                   CUBE = exp(diff(log(CUBE[,4]))) - 1,
                   KAKAO = exp(diff(log(KAKAO[,4]))) - 1,
                   NAVER = exp(diff(log(NAVER[,4]))) - 1,
                   SAMSUNG = exp(diff(log(SAMSUNG[,4]))) - 1,
                   HYUNDAI = exp(diff(log(HYUNDAI[,4]))) - 1)
colnames(dUse) <- c("HYBE","SM", "JYP", "YG","CUBE","KAKAO","NAVER", "SAMSUNG", "HYUNDAI")
dUse <- na.omit(dUse)

###### То же самое, только без лишних компаний
dUse <- data.frame(HYBE = exp(diff(log(HYBE[,4]))) - 1, 
                   SM = exp(diff(log(SM[,4]))) - 1, 
                   JYP = exp(diff(log(JYP[,4]))) - 1,
                   YG = exp(diff(log(YG[,4]))) - 1,
                   CUBE = exp(diff(log(CUBE[,4]))) - 1)
colnames(dUse) <- c("HYBE","SM", "JYP", "YG","CUBE")
dUse <- na.omit(dUse)

##### Тест на стационарность
adf.test(dUse$HYBE)

##### VAR(1)
m1 <- VAR(dUse,lag.max = 10, ic = "AIC")
summary(m1)
stargazer(m1$varresult$HYBE)

##### VAR(3)
m3<- VAR(dUse, p = 3)
summary(m3)
stargazer(m3$varresult$HYBE)


##### impulse response function (IRF)

plot(irf(m3, ortho = TRUE, runs = 1000))
plot(irf(m3, impulse = "HYBE", cumulative = TRUE, runs = 1000)) 

plot(irf(m1, ortho = TRUE, runs = 1000))
plot(irf(m1, impulse = "HYBE", cumulative = TRUE, runs = 1000)) 

##### FEVD
f<-fevd(m1)
win.graph(width=25,height=25)
layout(matrix(1:5,ncol=1))
plot.varfevd(f,plot.type = "single", col=1:5)


##### График цен всех компаний

dUse <- cbind("time" = as.Date(rownames(dUse),format =" %Y-%m-%d " ), dUse)
dUse2 <- melt(dUse ,  id.vars = 'time', variable.name = 'Company')
ggplot(dUse2, aes(x = time,y = value)) + geom_line(aes(colour = Company))+ scale_x_date(date_breaks = "3 month", date_labels = "%B\n%Y")

##### CausalImpact

dUse <- cbind("time" = as.Date(rownames(dUse),format =" %Y-%m-%d " ), dUse)
# установим пре- и пост- периоды отностилельно события (ГГГГ-ММ-ДД)

event = as.Date("2021-11-27") #событие
start = max(event -360, as.Date("2020-10-21")) 
pre_event = event -1 #за день до
end =  event +30 #посмотрим на 1 мес. вперед

pre = as.Date(c(start,pre_event),format =" %Y-%m-%d " )
post = as.Date(c(event, end),format =" %Y-%m-%d " )

sub <- dUse[dUse$time > start & dUse$time < end, ]
sub <-subset(sub, select = -HYBE)
z<-read.zoo(sub,index = 1, format =" %Y-%m-%d ")
impact <- CausalImpact(z, pre.period = pre, post.period = post)
plot(impact)

summary(impact)
summary(impact,"report")


##### чтобы FEVD нормально отображался
plot.varfevd  <-function (x, plot.type = c("multiple", "single"), names = NULL,
                          main = NULL, col = NULL, ylim = NULL, ylab = NULL, xlab = NULL,
                          legend = NULL, names.arg = NULL, nc, mar = par("mar"), oma = par("oma"),
                          addbars = 1, ...)
{
  K <- length(x)
  ynames <- names(x)
  plot.type <- match.arg(plot.type)
  if (is.null(names)) {
    names <- ynames
  }
  else {
    names <- as.character(names)
    if (!(all(names %in% ynames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      names <- ynames[1]
    }
  }
  nv <- length(names)
  #    op <- par(no.readonly = TRUE)
  ifelse(is.null(main), main <- paste("FEVD for", names), main <- rep(main,
                                                                      nv)[1:nv])
  ifelse(is.null(col), col <- gray.colors(K), col <- rep(col,
                                                         K)[1:K])
  ifelse(is.null(ylab), ylab <- rep("Percentage", nv), ylab <- rep(ylab,
                                                                   nv)[1:nv])
  ifelse(is.null(xlab), xlab <- rep("Horizon", nv), xlab <- rep(xlab,
                                                                nv)[1:nv])
  ifelse(is.null(ylim), ylim <- c(0, 1), ylim <- ylim)
  ifelse(is.null(legend), legend <- ynames, legend <- legend)
  if (is.null(names.arg))
    names.arg <- c(paste(1:nrow(x[[1]])), rep(NA, addbars))
  plotfevd <- function(x, main, col, ylab, xlab, names.arg,
                       ylim, ...) {
    addbars <- as.integer(addbars)
    if (addbars > 0) {
      hmat <- matrix(0, nrow = K, ncol = addbars)
      xvalue <- cbind(t(x), hmat)
      barplot(xvalue, main = main, col = col, ylab = ylab,
              xlab = xlab, names.arg = names.arg, ylim = ylim,
              legend.text = legend, ...)
      abline(h = 0)
    }
    else {
      xvalue <- t(x)
      barplot(xvalue, main = main, col = col, ylab = ylab,
              xlab = xlab, names.arg = names.arg, ylim = ylim,
              ...)
      abline(h = 0)
    }
  }
  if (plot.type == "single") {
    #        par(mar = mar, oma = oma)
    #        if (nv > 1)
    #            par(ask = TRUE)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col,
               ylab = ylab[i], xlab = xlab[i], names.arg = names.arg,
               ylim = ylim, ...)
    }
  }
  else if (plot.type == "multiple") {
    if (missing(nc)) {
      nc <- ifelse(nv > 4, 2, 1)
    }
    nr <- ceiling(nv/nc)
    par(mfcol = c(nr, nc), mar = mar, oma = oma)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col,
               ylab = ylab[i], xlab = xlab[i], names.arg = names.arg,
               ylim = ylim, ...)
    }
  }
  #    on.exit(par(op))
}