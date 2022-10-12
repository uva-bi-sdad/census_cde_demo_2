edashape<-function(var, name=NA, units=NA, scatter=TRUE){
  # Creates a graphic with three plots:  histogram, box, and Q-Q along
  # with descriptive statistics and an inferential test for normality:
  # D'Agostino-Pearson K^2 Test for Normality
  # American Statistician 1990, vol. 44, No. 4, 316-321
  # This function requires the function K2 which is included.
  # Written by Vicki Lancaster lasted edited January 6, 2019
  # var = the variable
  # names = graph title
  # units = variable unit of measurement to be written below the graph title
  # scatter = if scatter is TRUE jittered points are placed over the boxplot 
  # All NAs are removed prior to analysis
  # Requires the packages ggpubr and grid
  
  #Remove missing values  
  var<-var[!is.na(var)]
  df<-data.frame(var) 
  
  #make sure packages are loaded
  if (!"ggpubr" %in% installed.packages()) install.packages("ggpubr")
  library(grid)
  library(ggpubr)

##################################  
#function to calculate D'Agostino-Pearson K^2 Test for Normality  
  K2 <- function(x)
  {
    # D'Agostino-Pearson K^2 Test for Normality
    # American Statistician 1990, vol. 44, No. 4, 316-321
    # Check to see if x is numeric and treat it as a vector 
    if(mode(x) != "numeric") stop("need numeric data")
    x <- as.vector(x)	#Remove any NA's
    x <- x[!is.na(x)]
    
    n <- length(x)	#Object for calculating the central moments
    centralmoment <- function(x, k)
    {
      sum((x - mean(x))^k)/length(x)
    }
    #Compute coefficient of skewness
    sqrt.b.1 <- centralmoment(x, 3)/centralmoment(x, 2)^(3/2)	
    #Compute coefficient of kurtosis
    b.2 <- centralmoment(x, 4)/centralmoment(x, 2)^2	
    #Test of Skewness
    y <- sqrt.b.1 * sqrt(((n + 1) * (n + 3))/(6 * (n - 2)))
    B.2.sqrt.b.1 <- (3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3))/((n - 2) * (n + 5) * (n + 7) * (n + 9))
    W2 <- -1 + sqrt(2 * (B.2.sqrt.b.1 - 1))
    W <- sqrt(W2)
    delta <- 1/sqrt(log(W))
    alpha <- sqrt(2/(W2 - 1))
    Z.sqrt.b.1 <- delta * log(y/alpha + sqrt((y/alpha)^2 + 1))	
    #Normal approximation
    prob.skewness <- (1 - pnorm(abs(Z.sqrt.b.1), 0, 1)) 	
    #Test of Kurtosis
    #Compute the mean of b_2
    exp.b.2 <- (3 * (n - 1))/(n + 1)	#Compute the variance of b.2
    var.b.2 <- (24 * n * (n - 2) * (n - 3))/((n + 1)^2 * (n + 3) * (n + 5))	
    #Compute the standardized version of b.2
    std.b.2 <- (b.2 - exp.b.2)/sqrt(var.b.2)	
    #Compute the third standardized mpment of b.2
    sqrt.B.1.b.2 <- ((6 * (n^2 - 5 * n + 2))/((n + 7) * (n + 9))) * sqrt((6 * (n + 3) * (n + 5))/(n * (n - 2) * (n - 3)))
    A <- 6 + (8/sqrt.B.1.b.2) * (2/sqrt.B.1.b.2 + sqrt((1 + 4/(sqrt.B.1.b.2^2))))
    Z.b.2 <- ((1 - 2/(9 * A)) - ((1 - 2/A)/(1 + std.b.2 * sqrt(2/(A - 4))))^(1/3))/(sqrt(2/(9 * A)))	#Normal approximation
    prob.kurtosis <- (1 - pnorm(abs(Z.b.2), 0, 1))	
    #Omnibus K2 Test of Normality (Skewness/Kurtosis) 
    K2 <- Z.sqrt.b.1^2 + Z.b.2^2
    prob.K2 <- 1 - pchisq(K2, 2)
    ret.val <- list("Coefficient of Skewness" = sqrt.b.1, 
                    "Normal Approx. for Skewness" = Z.sqrt.b.1, 
                    "Prob(Normal Approx. for Skewness)" = prob.skewness, 
                    "Coefficient of Kurtosis" = b.2, "Normal Approx. for Kurtosis" = 
                      Z.b.2, "Prob(Normal Approx. for Kurtosis)" = prob.kurtosis, K2 = K2, 
                    "Prob(K2)" = prob.K2)
    return(ret.val)
  }
##################################  
  
  
    
  sturges<-diff(range(var))/(1+log2(length(var)))
  hp<-ggplot(df, aes(var))+geom_histogram(aes(y=..density..), binwidth=sturges, colour="peru", fill="peru")+
    geom_density()+geom_rug()+labs(x="Observed Values", y="Probability", title="Histogram") 
  hp<-hp+theme(axis.title.y=element_text(size=8, angle=90),
               axis.text.y=element_text(size=7),
               axis.title.x=element_text(size=8),
               axis.text.x=element_text(size=7),
               plot.title=element_text(size=12))
  
  Y<-quantile(var, c(0.25, 0.75))
  X<-qnorm(c(0.25, 0.75))
  slope<-unname(diff(Y))/unname(diff(X))
  int<-unname(Y[1L])-slope*unname(X[1L])
  qp<-ggplot(df, aes(sample=var))+stat_qq(size=0.6)+
    geom_abline(slope=slope, intercept=int, lwd=1, linetype=1, colour="peru")+
    labs(x="Theoretical Quantile", y="Observed Values", title="Q-Q Plot") 
  qp<-qp+theme(axis.title.y=element_text(size=8, angle=90), 
               axis.text.y=element_text(size=7),
               axis.title.x=element_text(size=8),
               axis.text.x=element_text(size=7),
               plot.title=element_text(size=12)) 
  
  bp<-ggplot(df, aes(var))+geom_boxplot(aes(x="", y=var), outlier.colour="white", fill="peru")+
    labs(x="", y="Observed Values", title="Box Plot")
  bp<-bp+theme(axis.title.y=element_text(size=8, angle=90), 
               axis.text.y=element_text(size=7),
               plot.title=element_text(size=12)) 
  if(scatter==TRUE){bp<-bp+geom_jitter(aes(x="", y=var), size=0.6)}
   
  n<-length(var)	
  parm<-round(summary(var), 6)
  parm1<-round(sqrt(var(var)), 6)
  parm2<-sd(var, na.rm=TRUE)/mean(var, na.rm=TRUE)
  test<-K2(var)
  
  ########	
  x<-1:30
  y<-1:30
  DF<-data.frame(x,y)
  blank<-ggplot(DF, aes(x,y)) + geom_blank() 
  blank<-blank+ 
    annotate("text", x=6, y=30, label=paste(name), size=4.5, adj=0)+
    annotate("text", x=6, y=30, label=paste(name), size=4.5, adj=0)+
    annotate("text", x=6, y=29.25, label=paste(units), size=3, adj=0)+
    annotate("text", x=6, y=27.5, label="Descriptive Statistics", size=4.5, adj=0)+
    annotate("text", x=6, y=27.5, label="Descriptive Statistics", size=4.5, adj=0)+
    annotate("text", x=6, y=26, label=paste("N =", n), size=4, adj=0)+
    annotate("text", x=6, y=25, label=paste("Min. =", parm[[1]]), size=4, adj=0)+
    annotate("text", x=6, y=24, label=paste("Q(.25) =", parm[[2]]), size=4, adj=0)+
    annotate("text", x=6, y=23, label=paste("Median =", parm[[3]]), size=4, adj=0)+
    annotate("text", x=6, y=22, label=paste("Q(.75) =", parm[[5]]), size=4, adj=0)+
    annotate("text", x=6, y=21, label=paste("Max. =", parm[[6]]), size=4, adj=0)+
    annotate("text", x=6, y=20, label=paste("Mean =", round(parm[[4]],4)), size=4, adj=0)+
    annotate("text", x=6, y=19, label=paste("SD =", round(parm1, 4)), size=4, adj=0)+
    annotate("text", x=6, y=18, label=paste("CS =", round(test[[1]],4)), size=4, adj=0)+
    annotate("text", x=6, y=17, label=paste("CK =", round(test[[4]],4)), size=4, adj=0)+
    annotate("text", x=6, y=16, label=paste("CV =", round(parm2,4)), size=4, adj=0)+
    annotate("text", x=6, y=13.5, label="Inferential Statistics", size=4.5, adj=0)+
    annotate("text", x=6, y=13.5, label="Inferential Statistics", size=4.5, adj=0)+
    annotate("text", x=6, y=12, label="D'Agostino & Pearson", size=4, adj=0)+
    annotate("text", x=6, y=11, label="Test for Normality", size=4, adj=0)+
    annotate("text", x=6, y=10, label=paste("K2 =", round(test[[7]],4)), size=4, adj=0)+	
    annotate("text", x=6, y=9, label=paste("p(K2) =", round(test[[8]],4)), size=4, adj=0)+	
    annotate("text", x=6, y=7, label="Test for Skewness", size=4, adj=0)+
    annotate("text", x=6, y=6, label=paste("Z(CS) =", round(test[[2]],4)), size=4, adj=0)+	
    annotate("text", x=6, y=5, label=paste("p(Z(CS)) =", round(test[[3]],4)), adj=0)+
    annotate("text", x=6, y=3, label="Test for Kurtosis", size=4, adj=0)+
    annotate("text", x=6, y=2, label=paste("Z(CK) =", round(test[[5]],4)), size=4, adj=0)+	
    annotate("text", x=6, y=1, label=paste("p(Z(CK)) =", round(test[[6]],4)), size=4, adj=0)+
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.title.x=element_blank(), axis.text.x=element_blank(),
          panel.background=element_blank(), axis.ticks=element_blank(),
          panel.grid.minor=element_blank())
  
  #Create the legend  
  Acronym<-c("Min.", "Q(.25)", "Median", "Q(.57)", "Max.", "Mean", "SD", "CS",
             "CK", "CV", "K2", "p(K2)", "Z(CS)", "p(Z(CS))", "Z(CK)", "p(Z(CK))")  
  Definition<-c("Minimum Value", "Lower Quartile", "        Median of the Distribution        ",
                "Upper Quartile", "Maximum Value", "Mean of the Distribution",
                "Standard Deviation", "Coefficient of Skewness", "Coefficient of Kurtosis",
                "Coefficient of Variation", 
                "Test Statistic for Ho: Data=Normal", "p-value for K2", 
                "Test Statistic for Ho: CS=0 (Normal)", "p-value for Z(CS)", 
                "Test Statistic for Ho: CK=3 (Normal)", "p-value for Z(CK)")
  
  #Construct the legend
  Legend1<-data.frame(cbind(Acronym[1:8], Definition[1:8])) 
  Legend2<-data.frame(cbind(Acronym[9:16], Definition[9:16])) 
  tbody.style<-tbody_style(color="black", face="plain", size=10,
                           fill=c("grey92", "grey92"), linewidth=2, 
                           linecolor="white")
  LEGEND1<-ggtexttable(Legend1, cols=NULL, rows=NULL,
                       theme=ttheme(tbody.style=tbody.style)) 
  LEGEND2<-ggtexttable(Legend2, cols=NULL, rows=NULL,
                       theme=ttheme(tbody.style=tbody.style)) 
  
  #Move to a new page
  grid.newpage()
  #Create layout: nrow=13, ncol=11
  pushViewport(viewport(layout=grid.layout(nrow=13, ncol=11)))
  # A helper function to define a region on the layout
  define_region<-function(row, col){
    viewport(layout.pos.row=row, layout.pos.col=col, just="center")
  } 
  #Arrange the plots
  print(blank, vp=define_region(row=2:10, col=2:5))   #Span over 9 rows
  print(LEGEND1, vp=define_region(row=12, col=2:5))   
  print(hp, vp=define_region(row=2:4, col=7:10))
  print(qp, vp=define_region(row=5:7, col=7:10))
  print(bp, vp=define_region(row=8:10, col=7:10))
  print(LEGEND2, vp=define_region(row=12, col=7:10))  
  
}   