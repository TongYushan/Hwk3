library(poliscidata)
xtp(nes, envjob_3,pid_3,wt)
xtp(nes, envjob_3,pid_3,wt,
    ylab = "Environ vs. Jobs",
    xlab = "Party Identification",
    main = "Environmental Opinions, by Party Identification")
nes$envir = as.numeric(nes$envjob_3=="Envir")
# Every respondent in "Middle" or "Jobs will be coded 0 on nes$envir
freqC(nes$envir, nes$wt)
freq(nes$envjob_3, nes$wt)
nes$envir = 100*nes$envir
# The mean of an indicator is eqaul to the proportion of cases coded 1 on the indicator.
plotmeansC(nes,
           ~envir,
           ~pid_3,
           envir~pid_3,
           w=~wt,
           xlab = "Party Identification",
           ylab = "Percent Pro-Environment",
           main = "Percentage Favoring Environment over Jobs, \n by Party Identification")
# Note for main title: Use "\n" to break long titles into two lines
compmeans(nes$ft_hclinton, 
          nes$pid_x,
          nes$wt,
          plot = F)
# Compmeans does not have a "data" argument, so we must use "$".
# If the data is already weighted, the plot must be suppressed. (plot= F)
printC(compmeans(nes$ft_hclinton, 
                 nes$pid_x,
                 nes$wt,
                 plot = F))
# Cross tabulation table
# in the Table.Output. html
plotmeansC(nes,
          ~ft_hclinton,
          ~pid_x,
          ft_hclinton~pid_x,
          w=~wt,
          xlab = "Party Identification",
          ylab = "Ratings of Hillary Clinton",
          main = "Ratings of Hillary Clinton, \n by Party Identification")
svyboxplot(ft_hclinton~pid_x,nesD, all.outliers=T)
# To graph outliers, add the argument, "all.outliers = T"
svyboxplot(ft_hclinton~pid_x,
           nesD,
           all.outliers =T,
           xlab = "Party Identification",
           ylab = "Clinton Rating",
           main = "Clinton Rating, by Party ID",
           col = "lightblue",
           varwidth = T)
# "varwidth = T" makes box widths proportional
compmeans(states$union10,
         states$region,
         plot = F)
stripchart(union10~region,
           data = states,
           xlab = "Region",
           ylab = "Percent Unionized",
           main = "Unionization by Region",
           vertical = TRUE,
           method = "jitter",
           font.main = 1)
# "vertical = TRUE": displays strips vertically
# "jitter" adds a small amount of random noise to data points
compmeans(nes$fedspend_scale,
          nes$pid_x,
          nes$wt)
svyboxplot(fedspend_scale~pid_x,
           nesD,
           all.outliers = T,
           xlab = "Party ID",
           ylab = "Pro-spending Scale Score",
           main = "Pro-government Spending Scale, by Party ID",
           col = "lightblue",
           varwidth = T)
print(svyboxplot(fedspend_scale~pid_x,
                  nesD,
                  all.outliers = T,
                  xlab = "Party ID",
                  ylab = "Pro-spending Scale Score",
                  main = "Pro-government Spending Scale, by Party ID",
                  col = "lightblue",
                  varwidth = T))
# Open a pdf file
pdf("Pro-government Spending Scale by Party ID.pdf") 
# 2. Create a plot
svyboxplot(fedspend_scale~pid_x,
           nesD,
           all.outliers = T,
           xlab = "Party ID",
           ylab = "Pro-spending Scale Score",
           main = "Pro-government Spending Scale, by Party ID",
           col = "lightblue",
           varwidth = T)
# Close the pdf file
dev.off() 
plotmeansC(nes, 
          ~fedspend_scale,
          ~pid_x,
          fedspend_scale ~ pid_x,
          w = ~wt,
          xlab = "Party ID",
          ylab = "Pro-spending Scale Score",
          main = "Pro-government Spending Scale, by Party ID")
pdf("Line chart Pro-government Spending Scale.pdf")
plotmeansC(nes, 
           ~fedspend_scale,
           ~pid_x,
           fedspend_scale ~ pid_x,
           w = ~wt,
           xlab = "Party ID",
           ylab = "Pro-spending Scale Score",
           main = "Pro-government Spending Scale, by Party ID")
dev.off()
xtp(nes, 
    gay_marry, 
    libcon3,
    wt,
    ylab = "Opinion on Gay Marriage",
    xlab = "Ideology",
    main = "Gay Marriage Opinions, by Ideology")
pdf("Hwk3-Mosaic plot Gay Marraige Opinions by Ideology.pdf")
xtp(nes, 
    gay_marry, 
    libcon3,
    wt,
    ylab = "Opinion on Gay Marriage",
    xlab = "Ideology",
    main = "Gay Marriage Opinions, by Ideology")
dev.off()
class(world)
compmeans(world$durable, 
          world$regime_type3,
          xlab = "Regime Type",
          ylab = "Durability by years",
          main = "Durability by Regime type")
printC(compmeans(world$durable, 
                 world$regime_type3,
                 xlab = "Regime Type",
                 ylab = "Durability in years",
                 main = "Durability by Regime type"))
stripchart(durable~regime_type3,
           data = world,
           xlab = "Regime Type",
           ylab = "Durability in years",
           main ="Durability by Regime types",
           vertical = TRUE,
           method = "jitter",
           font.main = 1)
jpeg("Durability by Regime types.jpg")
stripchart(durable~regime_type3,
           data = world,
           xlab = "Regime Type",
           ylab = "Durability in years",
           main ="Durability by Regime types",
           vertical = TRUE,
           method = "jitter",
           font.main = 1)
dev.off()
jpeg("Svyboxplot Durability by Regime types.jpg")
svyboxplot(durable~regime_type3,
           worldD,
           all.outliers = T,
           xlab = "Regime Type",
           ylab = "Durability in years",
           main = "Durability by Regime types",
           col = "lightblue",
           varwidth = T)
dev.off()