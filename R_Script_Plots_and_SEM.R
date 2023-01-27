# Set working directory and load data 
# ---------------------------------------------------------------------------- #

setwd("~/Documents/Uni/Semester 3/Data Literacy/Project")
library(readxl)
library(tidyverse)
library(dplyr)
library(lavaan)

# ---------------------------------------------------------------------------- #
# Data prep
# ---------------------------------------------------------------------------- #
{
  # Read dataset
  df <- read_excel("dataset.xls", skip=1)
  # Assign column names
  colnames(df) <- c('Year','Condition','Line','HEA', 'MAT', 'RIP', 'HEI', 'TGW', 'EAR', 'GPE', 'DRY', 'YLD', 'HI')
  head(df) # Check data

  # Assign factors (R provides extra functionality / requires factors to be factors)
  df$Year <- as.factor(df$Year)
  df$Condition <- as.factor(df$Condition)
  df$Line <- as.factor(df$Line)

  # Split into Contorl and treatment groups
  dt_c <- df[df$Condition=="Control",]
  dt_t <- df[df$Condition=="Saline",]

  # Calculate SWP for 2013 and 2014
  ST1_2014 <- dt_t[dt_t$Year==2014,12]/sqrt(dt_c[dt_c$Year==2014,12])
  ST1_2013 <- dt_t[dt_t$Year==2013,12]/sqrt(dt_c[dt_c$Year==2013,12])

  # Calculate converted Dataframes for 2013 and 2014 (Test and train)
  dt_2014 <- dt_t[dt_t$Year==2014,4:11]/sqrt(dt_c[dt_c$Year==2014,4:11])
  dt_2013 <- dt_t[dt_t$Year==2013,4:11]/sqrt(dt_c[dt_c$Year==2013,4:11])

}


# ---------------------------------------------------------------------------- #
# OLS
# ---------------------------------------------------------------------------- #

{
  # Define mode
  mod <- lm("YLD ~ HEA + MAT + RIP + HEI + EAR + GPE", dt_t[dt_t$Year==2014,4:12]/sqrt(dt_c[dt_c$Year==2014,4:12]))
  summary(mod)
  
  # Plot outputs
  par(mfrow=c(1,2))
  par(mar = c(4, 2, 2, 0)) 
  plot(mod, which=c(1,2), col=alpha(color,alph), pch=20)
  
  
}


# ---------------------------------------------------------------------------- #
# PCA
# ---------------------------------------------------------------------------- #

{

  par(mar = c(4, 0, 4, 0), mfrow = c(1,1)) 
  # All in one plot. Just interested in how the dimensions look. 
  # PCA analysis initially done in Python Notebook
  compositions::coloredBiplot(x=prcomp(dt_2014, center=T, scale.=T),pc.biplot=FALSE, scale=1,
              xlabs.pc=20, xlabs.col=alpha(color,alph), col="black",
            main="Data Projected onto scaled first 2 principal components")
}


# ---------------------------------------------------------------------------- #
# SEM Testing - Not all tested models shown. Only major structural changes shown
# Smaller tweaks part of the process were not explicitly saved.
# ---------------------------------------------------------------------------- #


# Model 1 -  Unidimensional
# ---------------------------------------------------------------------------- #
{
  # Define model
  # All vars load on 1 factor  
  mod <- 'ST =~ HEI + TGW + EAR + GPE + DRY + HEA + MAT + RIP' 

  # Fit/estimate the model
  fit <- sem(mod, dt_2013, estimator="ULS")
  summary(fit)
  fitMeasures(fit, fitindices)       # Check fit measures

  # Predict factor scores on non transformed data to see what happens.
  # Not relevant.
  factors_c <- lavPredict(fit,dt_c)
  factors_t <- lavPredict(fit,dt_t)

  # Predict factor scores on test data
  st_score <- lavPredict(fit,dt_2014)
  
  # Calculate histograms
  h1 <- hist(factors_c)
  h2 <- hist(factors_t)

  # Plot factor against SWP
  par(mfrow = c(1,2)) 
  plot(x=t(ST1_2014), y = st_score) # High correlation
  
  # Plot histograms
  plot(h1,col=rgb(0,0,1,1/10))
  plot(h2,col=rgb(1,0,0,1/10), add=T)
  par(mfrow = c(1,1)) 
}

# Model 2 (1.5) -  Bidimensional with general salt tolerance factor
# ---------------------------------------------------------------------------- #
# Note negative variance
{
  mod1.5 <- 'Salt.Tol =~ ST_Dev + ST_Prod
    ST_Prod =~ EAR + GPE + DRY + TGW 
    ST_Dev =~ HEA + MAT + RIP + HEI'

  fit1.5 <- sem(mod1.5, dt_2013, estimator="ULS")
  summary(fit1.5)
  fitMeasures(fit1.5, fitindices)

  # Get factor scores for test and train data
  f1_2014 <-lavPredict(fit1.5,dt_2014)
  f1_2013 <- lavPredict(fit1.5,dt_2013)
  
  # Plot factors (test) against SWP
  {
    color <- "#5f0f40"
    alph <- 0.5
    par(mfrow=c(1,3))
    par(mar = c(4, 4, 4, 0), xpd=FALSE) 
    plot(x=t(ST1_2014), y=f1_2014[,1], col = alpha(color,alph),cex.main=1,
       ylab = "Reproduction", xlab="SWP", main="Relationship between SWP and ST latent trait",
       pch=20)
    plot(x=t(ST1_2014), y=f1_2014[,2], col = alpha(color,alph), cex.main=1,
       ylab = "Vegetative", xlab="SWP", main="Relationship between SWP and Reprod latent trait",
       pch=20
    )
    plot(x=t(ST1_2014), y=f1_2014[,3], col = alpha(color,alph), cex.main=1,
       ylab = "Vegetative", xlab="SWP", main="Relationship between SWP and Vegetative latent trait",
       pch=20
    )

  }
}

# Save best model specification for testing
# ---------------------------------------------------------------------------- #
mod2_safe_original <- ' 
    # Measurement
    ST_Yield =~ EAR + GPE + DRY + TGW + HEI
    ST_Growth =~ HEA + MAT + RIP + HEI + TGW
    
    # Regressions
    ST_Yield ~ ST_Growth

    # Covariances - Something else correlating these, not explained by the model...
    HEA ~~ 0 * HEA
    TGW ~~ HEI'

# Model 2 - Bidimensional (Best model)
# ---------------------------------------------------------------------------- #
{
  mod2 <- ' 
    # Measurement
    ST_Yield =~ EAR + GPE + DRY + TGW + HEI
    ST_Growth =~ HEA + MAT + RIP + HEI + TGW
    

    # Covariances - Something else correlating these, not explained by the model...
    #ST_Yield ~~ ST_Growth # Insignificant
    HEA ~~ 0 * HEA
    TGW ~~ HEI'

  fit2 <- sem(mod2, dt_2014, estimator='ULS')
  summary(fit2) # Check the fit
  semPlot::semPaths(fit2) # Plot the fit
  fitindices <- c("chisq","df","cfi","tli","rmsea","rmr","aic","bic")
  fitMeasures(fit2, fitindices)       # Check fit measures
  modindices(fit2, sort=TRUE) # Check modification Indices
  fitMeasures(fit2)
  # Get LV scores / Factor Scores
  f_2014 <-lavPredict(fit2,dt_2014)
  f_2013 <- lavPredict(fit2,dt_2013)

  # Plot latent factors against SWP
  {
    color <- "#5f0f40"
    alph <- 0.5
    par(mfrow=c(1,2))
    par(mar = c(4, 4, 4, 0), xpd=FALSE) 
    plot(x=t(ST1_2014), y=f_2014[,1], col = alpha(color,alph),cex.main=1,
       ylab = "Reproduction", xlab="SWP", main="Relationship between SWP and Reproduction latent trait",
       pch=20)
    points(x=t(ST1_2013), y=f_2013[,1], pch=20, col = alpha("#d4aa00",alph))
    legend(x=1, y=0.55,c("2014 - Train data", "2013 - Test Data"), pch=20, 
       col=c(alpha(color,alph),alpha("#d4aa00",alph)),y.intersp=2, bty="n")
    plot(x=t(ST1_2014), y=f_2014[,2], col = alpha(color,alph), cex.main=1,
     ylab = "Vegetative", xlab="SWP", main="Relationship between SWP and Vegetative latent trait",
     pch=20
     )
    points(x=t(ST1_2013), y=f_2013[,2], pch= 20,col = alpha("#d4aa00",alph) )
  }
}

# Check fit against fit 2
lavTestLRT(fit,fit2) # use fit2


# Model 4. Simple model. Not relevant
# ---------------------------------------------------------------------------- #
{
  mod4 <- 'ST_Prod ~~ ST_Dev
    ST_Prod =~ EAR + GPE + DRY + TGW 
    ST_Dev =~ HEA + MAT + RIP + HEI'

  fit4 <- sem(mod4, dt_2013, estimator="ULS")
  summary(fit4)
}

# Compare fits for table in PDF
# ---------------------------------------------------------------------------- #
{
  fitMeasures(fit, fitindices)
  fitMeasures(fit1.5, fitindices)
  fitMeasures(fit2, fitindices)
  
  # LRT for nested models
  lavTestLRT(fit,fit1.5,fit2)
}


