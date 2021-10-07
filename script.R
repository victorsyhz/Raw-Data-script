
#The script:

 require(foreign)

 require(ggplot2)

 require(MASS)

 require(Hmisc)

 require(reshape2)


##import the file, and named as "dat"

dat=read.csv("ni_490_zhidao.csv")


##build the model, "Pragmatics" as a factor.
m1 <- polr(as.factor(Pragmatics)~Duration+Tempo+Pre.pause+Post.pause+F0+Intensity, data=dat)



summary(m1)


#Re-fitting to get Hessian


#Call:
#polr(formula = as.factor(Pragmatics) ~ Duration + Tempo + Pre.pause + 
#    Post.pause + F0 + Intensity, data = dat)

#Coefficients:
#               Value Std. Error  t value
#Duration   -7.804265    0.66955 -11.6560
#Tempo      26.934629    0.15885 169.5626
#Pre.pause  -1.591390    0.26372  -6.0343
#Post.pause -5.133923    0.77538  -6.6212
#F0          0.005188    0.00150   3.4588
#Intensity  -0.002840    0.01185  -0.2397

#Intercepts:
#    Value    Std. Error t value 
#C|E  -1.1279   0.8637    -1.3060
#E|F   0.7229   0.8625     0.8381
#F|T   2.4687   0.8817     2.7999

#Residual Deviance: 1079.165 
#AIC: 1097.165

###look at the p-value
ctable <- coef(summary(m1))
 p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
 ctable <- cbind(ctable, "p value" = p)
 ctable

#                  Value  Std. Error     t value      p value
#Duration   -7.804265325 0.669547834 -11.6560236 2.137958e-31
#Tempo      26.934628585 0.158847711 169.5625859 0.000000e+00
#Pre.pause  -1.591389607 0.263722936  -6.0343239 1.596296e-09
#Post.pause -5.133922659 0.775378456  -6.6211830 3.563355e-11
#F0          0.005188320 0.001500051   3.4587626 5.426629e-04
#Intensity  -0.002840317 0.011849088  -0.2397077 8.105569e-01
#C|E        -1.127937567 0.863652616  -1.3060084 1.915497e-01
#E|F         0.722867676 0.862498212   0.8381092 4.019694e-01
#F|T         2.468715835 0.881722787   2.7998775 5.112200e-03



##further check with Chisq
library(car)

library(splines)

#library(



Anova(m1)

#Analysis of Deviance Table (Type II tests)



#Response: Pragmatics

 #          LR Chisq Df Pr(Chisq)    

#Duration     30.205  1  3.888e-08 ***

#Tempo        19.693  1  9.094e-06 ***

#Pre.pause    12.730  1  0.0003599 ***

#Post.pause   25.970  1  3.468e-07 ***

#F0           15.762  1  7.183e-05 ***

#Intensity    18.970  1  1.328e-05 ***

#---

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



####all variables are significant



###plot the effects



library(effects)



effects  <- allEffects(m1, partial.residuals=T)

pdf("effects_all1.pdf", wi=14, he=8.5)
#plot(effects)

plot(effects, multiline=TRUE, ci.style="bands", colors = c("red",
"blue", "green", "yellow"), lwd=2, cex=0.1, smooth.residuals=F, ask=FALSE, lines=list(multiline=TRUE), grid=TRUE)

dev.off()       


 


