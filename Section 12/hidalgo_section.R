
##Load data
load("~/Dropbox/Teaching/231B_Spring2016/231b_github/section_10/brazil_evm.RData")

## @knitr setup
#library(plm)
library(ggplot2)
#library(lmtest)
library(plyr)
library(reshape2)
library(scales)
#library(Hmisc)
library(car)
library(mgcv)
library(grid)
library(sandwich)

##Rounding Options
options(digits = 1)

##Function for Arranging Plots
#vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
#arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
 #   dots <- list(...)
 #   n <- length(dots)
 #   if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
 #   if(is.null(nrow)) { nrow = ceiling(n/ncol)}
 #   if(is.null(ncol)) { ncol = ceiling(n/nrow)}
    ## NOTE see n2mfrow in grDevices for possible alternative
 #   grid.newpage()
 #   pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
 #   ii.p <- 1
 #   for(ii.row in seq(1, nrow)){
 #       ii.table.row <- ii.row 
 #       if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
 #       for(ii.col in seq(1, ncol)){
 #           ii.table <- ii.p
 #           if(ii.p > n) break
 #           print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
 #           ii.p <- ii.p + 1
 #       }
 #   }
#}

#nboots <- 200


## @knitr invalid_latam
#latam.invalid <- data.frame(Country=c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay","Peru", "Uruguay", "Venezuela", "India"), invalid = c(4.94, 10.17, 33.29, 3.29, 10.52, 5.61, 1.7, 22.41, 8.5, 11.72, 4.66, 2.94, 6.81, 5.61, 3.1, 24.3, 3.32, 14.58, 2.4))
#latam.invalid <- latam.invalid[order(latam.invalid$invalid, decreasing=TRUE),]
#latam.invalid$Country <- reorder(latam.invalid$Country, latam.invalid$invalid)
#latam.invalid$Brazil <- ifelse(latam.invalid$Country=="Brazil" | latam.invalid$Country=="India", 1,0)

#latam_gg <- ggplot(latam.invalid, aes(y= Country, x=invalid, color = Brazil))
#latam_gg + geom_point(size=3) + theme_bw() + 
#    scale_x_continuous("Average \\% Blank and Invalid Ballots (1980-2000)")

## @knitr prepare_brazil_data
brazil.data <- brazil.data[brazil.data$remove != 1, ]
brazil.data$comp.voting.98 <- as.factor(brazil.data$comp.voting.98)

brazil.data$depestbranconulo98.pct <- with(brazil.data, (depest_blank_votes + depest_null_votes) / totalvote) * 100
brazil.data$depfedbranconulo98.pct <- with(brazil.data, (depfed_blank_votes + depfed_null_votes) / totalvote) * 100

brazil.data$depfedbranco98.pct <-  with(brazil.data, (depfed_blank_votes) / totalvote) * 100
brazil.data$depfednulo98.pct <-  with(brazil.data, (depfed_null_votes) / totalvote) * 100
brazil.data$govbranconulo98.pct <- with(brazil.data, (gov_blank_votes + gov_null_votes) / totalvote) * 100
#brazil.data$presbranconulo98.pct <- with(brazil.data, (presbranco9 + presnulo98) / totalvotes98) * 100
brazil.data$senbranconulo98.pct <- with(brazil.data, (sen_blank_votes + sen_null_votes) / totalvote) * 100

brazil.data$depfed.validvotes <- brazil.data$totalvote - (brazil.data$depfed_blank_votes + brazil.data$depfed_null_votes)
brazil.data$depest.validvotes <- brazil.data$totalvote - (brazil.data$depest_blank_votes + brazil.data$depest_null_votes)
brazil.data$depfed.list.pct <- 100 * brazil.data$depfed_legenda_votes / brazil.data$depfed.validvotes
brazil.data$depest.list.pct <- 100 * brazil.data$depest_legenda_votes / brazil.data$depest.validvotes

brazil.data$depfed.PTlist.pct <- 100 * brazil.data$depfed_list_PT_votes / brazil.data$depfed.validvotes
brazil.data$depfed.PSDBlist.pct <- 100 * brazil.data$depfed_list_PSDB_votes / brazil.data$depfed.validvotes
brazil.data$depfed.PMDBlist.pct <- 100 * brazil.data$depfed_list_PMDB_votes / brazil.data$depfed.validvotes
brazil.data$depfed.PPBlist.pct<- 100 * brazil.data$depfed_list_PPB_votes / brazil.data$depfed.validvotes
brazil.data$depfed.PFLlist.pct<- 100 * brazil.data$depfed_list_PFL_votes / brazil.data$depfed.validvotes

brazil.data$depest.PTlist.pct <- 100 * brazil.data$depest_list_PT_votes / brazil.data$depest.validvotes
brazil.data$depest.PSDBlist.pct <- 100 * brazil.data$depest_list_PSDB_votes / brazil.data$depest.validvotes
brazil.data$depest.PMDBlist.pct <- 100 * brazil.data$depest_list_PMDB_votes / brazil.data$depest.validvotes
brazil.data$depest.PPBlist.pct<- 100 * brazil.data$depest_list_PPB_votes / brazil.data$depest.validvotes
brazil.data$depest.PFLlist.pct<- 100 * brazil.data$depest_list_PFL_votes / brazil.data$depest.validvotes

brazil.data$depfed.PT.pct <- 100 * brazil.data$depfed_PT_votes / brazil.data$depfed_nominal_votes
brazil.data$depfed.PSDB.pct <- 100 * brazil.data$depfed_PSDB_votes / brazil.data$depfed_nominal_votes
brazil.data$depfed.PMDB.pct <- 100 * brazil.data$depfed_PMDB_votes / brazil.data$depfed_nominal_votes
brazil.data$depfed.PPB.pct <- 100 * brazil.data$depfed_PPB_votes / brazil.data$depfed_nominal_votes
brazil.data$depfed.PFL.pct <- 100 * brazil.data$depfed_PFL_votes / brazil.data$depfed_nominal_votes

brazil.data$depest.PT.pct <- 100 * brazil.data$depest_PT_votes / brazil.data$depest_nominal_votes
brazil.data$depest.PSDB.pct <- 100 * brazil.data$depest_PSDB_votes / brazil.data$depest_nominal_votes
brazil.data$depest.PMDB.pct <- 100 * brazil.data$depest_PMDB_votes / brazil.data$depest_nominal_votes
brazil.data$depest.PPB.pct <- 100 * brazil.data$depest_PPB_votes / brazil.data$depest_nominal_votes
brazil.data$depest.PFL.pct <- 100 * brazil.data$depest_PFL_votes / brazil.data$depest_nominal_votes

br.pollingstation.data$depestbranconulo98.pct <- with(br.pollingstation.data, (depest_blank_votes + depest_null_votes) / totalvote) * 100
br.pollingstation.data$depfedbranconulo98.pct <- with(br.pollingstation.data, (depfed_blank_votes + depfed_null_votes) / totalvote) * 100
br.pollingstation.data$depfed.validvotes <- br.pollingstation.data$totalvote - (br.pollingstation.data$depfed_blank_votes + br.pollingstation.data$depfed_null_votes)
br.pollingstation.data$depest.validvotes <- br.pollingstation.data$totalvote - (br.pollingstation.data$depest_blank_votes + br.pollingstation.data$depest_null_votes)

br.pollingstation.data$depfed.incumbcoal.pct <- 100 * br.pollingstation.data$depfed_incumbcoal_vote / br.pollingstation.data$depfed_nominal_votes
br.pollingstation.data$depest.incumbcoal.pct <- 100 * br.pollingstation.data$depest_incumbcoal_vote / br.pollingstation.data$depest_nominal_votes

br.pollingstation.data <- merge(brazil.data[, c('tse.municode', 'electorate.96', 'remove', 'comp.voting.98', 'region')], br.pollingstation.data, all.x = TRUE, all.y = FALSE )

br.incumbcoal.data <- melt(br.pollingstation.data[, c('tse.municode', 'depfed_incumbcoal_vote', 'depest_incumbcoal_vote')], id.vars = 'tse.municode')
br.incumbcoal.data <- dcast(br.incumbcoal.data, tse.municode ~ variable, sum, na.rm = TRUE)
brazil.data <- merge(brazil.data, br.incumbcoal.data, all.x = TRUE)
brazil.data$depfed.incumbcoal.pct <- 100 *  brazil.data$depfed_incumbcoal_vote / brazil.data$depfed.validvotes
brazil.data$depest.incumbcoal.pct <- 100 * brazil.data$depest_incumbcoal_vote / brazil.data$depest.validvotes


## @knitr brazil_results
rd.estim <- function(outcome.var, data){
    library(car)
    data$outcomeVar <- data[, outcome.var]
    data$fv <- data$electorate.96 - 40500
    data$wts <- ifelse(abs(data$fv / 15000) <= 1, 
                       1 - abs(data$fv / 15000), 0)
    ll.results <- lm(outcomeVar ~ comp.voting.98 * fv , data = data, weights = wts)
    dm.results <- lm(outcomeVar ~ comp.voting.98, data = data, subset = electorate.96 > 35500 & electorate.96 < 45500)
    fullsample.results <- lm(outcomeVar ~ comp.voting.98, data = data)
    # #   ll.out <- c(bandwidth = 15000, coef(ll.results)[[2]], sqrt(vcovHC(ll.results, type = "HC1")[2,2]), ll.results$df.residual + 2, coef(ll.results)[[1]])
    ll.out <- c(bandwidth = 15000, coef(ll.results)[[2]], sqrt(vcov(ll.results)[2,2]), ll.results$df.residual + 2, coef(ll.results)[[1]])
    names(ll.out) <- c("Bandwidth", "RD Estimate", "Standard Error", "N", "Baseline") 
    dm.out <- c(coef(dm.results)[[2]], sqrt(vcovHC(dm.results, type = "HC1")[2,2]), dm.results$df.residual + 2, coef(dm.results)[[1]])
    names(dm.out) <- c("RD Estimate", "Standard Error", "N", "Baseline") 
    fullsample.out <- c(coef(fullsample.results)[[2]], sqrt(vcovHC(fullsample.results, type = "HC1")[2,2]), fullsample.results$df.residual + 2, coef(fullsample.results)[[1]])
    names(fullsample.out) <- c("RD Estimate", "Standard Error", "N", "Baseline") 
    
    list(ll.results = ll.out, dm.results = dm.out, fullsample.results = fullsample.out, sd = sd(data$outcomeVar, na.rm = TRUE))
}



##Balance
#brazil.baldata <- brazil.baldata[brazil.baldata$remove != 1, ]
#br.bal.covar <- c("gini_1991", "pt_pres_1994", "income_1991", "psdb_1996", "pfl_1996",  "poverty_high_1991", "frac_25_illit_1991", "depfednulo.94", "depestbranco.94", "depestnulo.94", "depfedPT.94", "depfedPMDB.94", "depfedPFL.94", "depfedPSDB.94", "ver.blank.pct.96", "ver.null.pct.96", "SP", "BA", "MG")
#br.balance <- lapply(br.bal.covar, function(x) rd.estim(x, brazil.baldata))
#names(br.balance) <- br.bal.covar

branconulo.results <- list()
branconulo.results$depfedbranco <-  rd.estim("depfedbranco98.pct", brazil.data)
branconulo.results$depfednulo <-  rd.estim("depfednulo98.pct", brazil.data)
branconulo.results$depfedbranconulo <-  rd.estim("depfedbranconulo98.pct", brazil.data)
branconulo.results$depestbranconulo <-  rd.estim("depestbranconulo98.pct", brazil.data)
branconulo.results$senbranconulo <-  rd.estim("senbranconulo98.pct", brazil.data)
branconulo.results$govbranconulo <-  rd.estim("govbranconulo98.pct", brazil.data)
#branconulo.results$presbranconulo <-  rd.estim("presbranconulo98.pct", brazil.data)

##Heterogenous Treatment Effects
#brazilhetero.data <- brazil.data[brazil.data$electorate.96 > (40500 - 5000) & brazil.data$electorate.96 < (40500 + 5000), c("comp.voting.98", "electorate.96", "depfedbranconulo98.pct", "frac_25_illit_1991", "income_1991")]
#brazilhetero.data$evm1 <- 1
#brazilhetero.data$evm0 <- 0
#brazilhetero.data$logincome91 <- log(brazilhetero.data$income_1991)
#brazilhetero.data <- brazilhetero.data[brazilhetero.data$logincome91 > 4, ]

#gam.hetero <- function(covariate, data){
#    data$covariate <- data[, covariate]
#    gammodel <- gam(depfedbranconulo98.pct ~ comp.voting.98 + s(covariate) + s( covariate, by = comp.voting.98), data = data)
#    cate <- predict(gammodel, newdata = list(comp.voting.98 = data$evm1, covariate = data$covariate)) - predict(gammodel, newdata = list(comp.voting.98 = data$evm0, covariate = data$covariate))
#}

#brazilhetero.data$cate.logincome <- gam.hetero("logincome91", brazilhetero.data)
#brazilhetero.data$cate.logincome.se <- apply(replicate(nboots, gam.hetero("logincome91", brazilhetero.data[sample(1:nrow(brazilhetero.data), size = nrow(brazilhetero.data), replace = TRUE) ,])), 1, sd)
#brazilhetero.data$cate.illiterate <- gam.hetero("frac_25_illit_1991", brazilhetero.data)
#brazilhetero.data$cate.illiterate.se <- apply(replicate(nboots, gam.hetero("frac_25_illit_1991", brazilhetero.data[sample(1:nrow(brazilhetero.data), size = nrow(brazilhetero.data), replace = TRUE) ,])), 1, sd)

##
#Party List
##
#brazil.party.results <- list()
#brazil.party.results$depfedlist <- rd.estim("depfed.list.pct", brazil.data)
#brazil.party.results$depestlist <- rd.estim("depest.list.pct", brazil.data)
#brazil.party.results$depfedPTlist <- rd.estim("depfed.PTlist.pct", brazil.data)
#brazil.party.results$depfedPSDBlist <- rd.estim("depfed.PSDBlist.pct", brazil.data)
#brazil.party.results$depfedPMDBlist <- rd.estim("depfed.PMDBlist.pct", brazil.data)
#brazil.party.results$depfedPPBlist <- rd.estim("depfed.PPBlist.pct", brazil.data)
#brazil.party.results$depfedPFLlist <- rd.estim("depfed.PFLlist.pct", brazil.data)

#brazil.party.results$depestlist <- rd.estim("depest.list.pct", brazil.data)
#brazil.party.results$depestlist <- rd.estim("depest.list.pct", brazil.data)
#brazil.party.results$depestPTlist <- rd.estim("depest.PTlist.pct", brazil.data)
#brazil.party.results$depestPSDBlist <- rd.estim("depest.PSDBlist.pct", brazil.data)
#brazil.party.results$depestPMDBlist <- rd.estim("depest.PMDBlist.pct", brazil.data)
#brazil.party.results$depestPPBlist <- rd.estim("depest.PPBlist.pct", brazil.data)
#brazil.party.results$depestPFLlist <- rd.estim("depest.PFLlist.pct", brazil.data)

#brazil.party.results$depfedPT <- rd.estim("depfed.PT.pct", brazil.data)
#brazil.party.results$depfedPSDB <- rd.estim("depfed.PSDB.pct", brazil.data)
#brazil.party.results$depfedPMDB <- rd.estim("depfed.PMDB.pct", brazil.data)
#brazil.party.results$depfedPPB <- rd.estim("depfed.PPB.pct", brazil.data)
#brazil.party.results$depfedPFL <- rd.estim("depfed.PFL.pct", brazil.data)

#brazil.party.results$depestPT <- rd.estim("depest.PT.pct", brazil.data)
#brazil.party.results$depestPSDB <- rd.estim("depest.PSDB.pct", brazil.data)
#brazil.party.results$depestPMDB <- rd.estim("depest.PMDB.pct", brazil.data)
#brazil.party.results$depestPPB <- rd.estim("depest.PPB.pct", brazil.data)
#brazil.party.results$depestPFL <- rd.estim("depest.PFL.pct", brazil.data)

##Machine States
#machine.results <- list()
#machine.results$machine.depfed.incumbcoal <- rd.estim("depfed.incumbcoal.pct", brazil.data[brazil.data$region == "Nordeste" & (brazil.data$uf == "BA" | brazil.data$uf == "PB" | brazil.data$uf == "MA"), ])
#machine.results$nonmachine.depfed.incumbcoal <- rd.estim("depfed.incumbcoal.pct", brazil.data[brazil.data$region == "Nordeste" & (brazil.data$uf != "BA" & brazil.data$uf != "PB" & brazil.data$uf != "MA" & brazil.data$uf != "CE"), ])
#machine.results$machine.depest.incumbcoal <- rd.estim("depest.incumbcoal.pct", brazil.data[brazil.data$region == "Nordeste" & (brazil.data$uf == "BA" | brazil.data$uf == "PB" | brazil.data$uf == "MA"), ])
#machine.results$nonmachine.depest.incumbcoal <- rd.estim("depest.incumbcoal.pct", brazil.data[brazil.data$region == "Nordeste" & (brazil.data$uf != "BA" & brazil.data$uf != "PB" & brazil.data$uf != "MA"  & brazil.data$uf != "CE"), ])


# @knitr brazil_balance_plot

#ll.pval <- pt(abs(sapply(br.balance, function(x) x$ll.results[[2]] / x$ll.results[[3]])), lower.tail = FALSE, df = 1000)
#fullsample.pval <- pt(abs(sapply(br.balance, function(x) x$fullsample.results[[1]] / x$fullsample.results[[2]])), lower.tail = FALSE, df = 1000)
#dm.pval <- pt(abs(sapply(br.balance, function(x) x$dm.results[[1]] / x$dm.results[[2]])), lower.tail = FALSE, df = 151)

#ll.stddiff <-sapply(br.balance, function(x) x$ll.results[[2]] / x$sd)
#fullsample.stddiff <-sapply(br.balance, function(x) x$fullsample.results[[1]] / x$sd)
#dm.stddiff <-sapply(br.balance, function(x) x$dm.results[[1]] / x$sd)

#br.balvar.names <- c("Gini (1991)", "PT Pres. Vote Share (1994)", "GDP per Cap (1991)", "PSDB Mayoral Vote Share (1996)", "PFL Mayoral Vote Share (1996)",  "Poverty Share (1991)", "Illiteracy Share (1991)", "Fed. Deputy Null Vote \\% (1994)" , "State Deputy Blank Vote \\% (1994)", "State Deputy Null Vote \\% (1994)", "Fed. Deputy PT Vote Share (1994)", "Fed. Deputy PMDB Vote Share (1994)", "Fed. Deputy PFL Vote Share (1994)", "Fed. Deputy PSDB Vote Share (1994)", "City Council Blank Vote \\% (1996)", "City Council Null Vote \\% (1996)", "State: Sao Paulo", "State: Bahia", "State: Minas Gerais")

#br.baltable <- data.frame(covar = rep(br.balvar.names, 3), pvalue = c(ll.pval, fullsample.pval, dm.pval), std.diff = c(ll.stddiff, fullsample.stddiff, dm.stddiff), Specification = c(rep("Local\nLinear", length(br.bal.covar)), rep("Full Sample", length(br.bal.covar)), rep("Difference\nin-Means", length(br.bal.covar))))
#br.baltable$covar <- reorder(br.baltable$covar, abs(br.baltable$std.diff * 100) , mean)

#br.baltable <- melt(br.baltable, id.vars = c("covar", "Specification"))
#br.baltable$variable <- ifelse(br.baltable$variable == "pvalue", "t-test p-value", "Standardized Difference")
#br.baltable$vline <- ifelse(br.baltable$variable == "t-test p-value", .05, 0)


#ggplot(br.baltable, aes(x = value, y = covar, color = Specification, shape = Specification)) +  
#    geom_point(alpha = .7, size = 3) + scale_colour_brewer(palette = "Set1", 
#                                                           guide = guide_legend(nrow = 1)) + theme_bw() + scale_x_continuous("Statistic") + 
#    scale_y_discrete("") + facet_grid(. ~ variable, scales = "free_x")  + 
#    geom_vline(aes(xintercept = vline), linetype = 2, size = .2)


## @knitr brazil_nulobranco_rdplot
ggplot(brazil.data[brazil.data$electorate.96 > 0 & brazil.data$electorate.96 < 80000, ], 
       aes(x = electorate.96, y = depfedbranconulo98.pct)) + 
    geom_point(aes(color = comp.voting.98), size = 2, alpha = .5) + 
    stat_smooth(aes(group = comp.voting.98), color = "black", size = 1.8, method = "loess") + 
    theme_bw()  + scale_colour_brewer(palette = "Set1") + geom_vline(xintercept = 40500, lty = 2) + 
    scale_x_continuous("Electorate (1996)", labels = comma_format(digits = 5)) + 
    scale_y_continuous("Invalid Votes (\\%)")  + guides(colour=FALSE)


#############################################################
#library(grDevices)
transp_red <- rgb(red=1, green=0, blue=0, alpha=.3)
transp_blue <- rgb(red=0, green=0, blue=1, alpha=.3)

treated <- brazil.data[brazil.data$electorate.96 > 0 & brazil.data$electorate.96 < 80000 &
                           brazil.data$electorate.96 >= 40500,]
control <- brazil.data[brazil.data$electorate.96 > 0 & brazil.data$electorate.96 < 80000 &
                           brazil.data$electorate.96 < 40500,]

with(control, plot(electorate.96, depfedbranconulo98.pct, 
                   pch=16, col=transp_red, xlab="Electorate (1996)", ylab="Invalid Votes (\\%)",
                   xlim=c(0,80000), ylim=c(0,65)))
abline(v=40000, lty=4, lwd=3)
with(treated, points(electorate.96, depfedbranconulo98.pct, 
                     pch=16, col=transp_blue))

loess_control <- with(control, loess(depfedbranconulo98.pct ~ electorate.96))
lines(seq(0, 40500, 1), predict(loess_control, seq(0, 40500, 1)), lwd=4)

loess_treat <- with(treated, loess(depfedbranconulo98.pct ~ electorate.96))
lines(seq(40500, 80000, 1), predict(loess_treat, seq(40500, 80000, 1)), lwd=4)
#############################################################



## @knitr brazil_nulobranco_resultsplot
branconulo.results.table <- data.frame('Variable' = c(rep(names(branconulo.results), 2)),
                                       'Est' = c(sapply(branconulo.results, function(x) x$ll.results['RD Estimate']), sapply(branconulo.results, function(x) x$dm.results['RD Estimate'])),
                                       'SE' = c(sapply(branconulo.results, function(x) x$ll.results['Standard Error']), sapply(branconulo.results, function(x) x$dm.results['Standard Error'])),
                                       'Baseline' = c(sapply(branconulo.results, function(x) x$ll.results['Baseline']), sapply(branconulo.results, function(x) x$dm.results['Baseline'])),
                                       'N' = c(sapply(branconulo.results, function(x) x$ll.results['N']), sapply(branconulo.results, function(x) x$dm.results['N'])),
                                       'Bandwidth' = c(sapply(branconulo.results, function(x) round(x$ll.results['Optimal Bandwidth'])), rep(5000, length(branconulo.results))),
                                       'Specification' = c(rep("Local Linear", length(branconulo.results)), rep("Difference\nin-Means", length(branconulo.results)))
)
branconulo.results.table 
#branconulo.results.table$Variable <- c("Federal Deputy\nBlank ", "Federal Deputy\nNull ", "Federal Deputy \nInvalid ", "State Deputy \nInvalid ", "Senator \nInvalid ", "Governor \nInvalid ", "President \nInvalid ", "Federal Deputy\nBlank ", "Federal Deputy\nNull ", "Federal Deputy \nInvalid ", "State Deputy \nInvalid ", "Senator \nInvalid ", "Governor \nInvalid ", "President \nInvalid ")

branconulo.results.table$Variable <- c("Federal Deputy\nBlank", "Federal Deputy\nNull", "Federal Deputy \nInvalid", "State Deputy \nInvalid", "Senator \nInvalid", "Governor \nInvalid", "Federal Deputy\nBlank", "Federal Deputy\nNull", "Federal Deputy \nInvalid", "State Deputy \nInvalid", "Senator \nInvalid", "Governor \nInvalid")
branconulo.results.table$Variable <- factor(branconulo.results.table$Variable, levels = c("Federal Deputy\nBlank", "Federal Deputy\nNull", "Federal Deputy \nInvalid", "State Deputy \nInvalid", "Senator \nInvalid", "Governor \nInvalid"))
branconulo.results.table$Office <- "Other Offices"
branconulo.results.table$Office[grep("Federal", branconulo.results.table$Variable)] <- "Federal Deputy"
branconulo.results.table 

ggplot(branconulo.results.table, aes(x = Variable, y = Est, color = Specification, shape = Specification)) + 
    geom_pointrange(aes(ymin = Est - 1.96 * SE, ymax = Est + 1.96 * SE),  position = position_dodge(width = 0.4)) + 
    theme_bw() + scale_colour_brewer(palette = "Set1") + #geom_hline(yintercep = 0, lty = 2) + 
    facet_wrap( ~ Office, nrow = 1, scales = "free_x") + scale_x_discrete("") + 
    scale_y_continuous("Estimate (\\%)")



## @knitr brazil_hetero_resultsplot
#brazil.hetero.logincome.plot <- ggplot(brazilhetero.data, aes(x = logincome91, y = cate.logincome)) + 
#    geom_line(lwd = 1.3) + geom_ribbon(aes(ymin = cate.logincome - 1.96 * cate.logincome.se, ymax = cate.logincome + 1.96 * cate.logincome.se), alpha = .2) + 
#    geom_rug() + scale_y_continuous("Treatment Effect (Federal Deputy Invalid Vote \\%)") + 
#    scale_x_continuous("Log GPD per Capita (1991)") + theme_bw()

#brazil.hetero.illiterate.plot <- ggplot(brazilhetero.data, aes(x = frac_25_illit_1991, y = cate.illiterate)) + geom_line(lwd = 1.3) + geom_ribbon(aes(ymin = cate.illiterate - 1.96 * cate.illiterate.se, ymax = cate.illiterate + 1.96 * cate.illiterate.se), alpha = .2) + geom_rug()  + scale_y_continuous("Treatment Effect (Federal Deputy Invalid Vote \\%)") + scale_x_continuous("Percent Illiterate (1991)") +theme_bw()

#arrange(brazil.hetero.logincome.plot, brazil.hetero.illiterate.plot)


## @knitr brazil_ideal
#median.voter <- median(brazil.ideal.pt$ideal)

#brazil.ideal.pt <- brazil.ideal.pt[brazil.ideal.pt$party=="PFL"|brazil.ideal.pt$party=="PPB"|brazil.ideal.pt$party=="PSDB"|brazil.ideal.pt$party=="PT"|brazil.ideal.pt$party=="PMDB",]
#brazil.ideal.pt <- brazil.ideal.pt[!is.na(brazil.ideal.pt$party),]
#brazil.ideal.pt$Party <- brazil.ideal.pt$party[,drop=TRUE]
#cols <- c("PFL" = "blue", "PPB" = "orange", "PSDB" = "green", "PT" = "red", "PMDB" = "purple")

#ggplot(brazil.ideal.pt, aes(ideal, color = Party, fill= Party)) + geom_density(alpha=.1, size=1.2) + geom_vline(xintercept=median.voter, size=1.2) + geom_rug()  +  scale_x_continuous("Ideal Point") + theme_bw() + annotate("text", label = "Chamber Median", x = -.3, y = 2, size = 4) + scale_colour_manual(values=cols)


## @knitr brazil_partylist_rdplot
#ggplot(brazil.data[brazil.data$electorate.96 > 0 & brazil.data$electorate.96 < 80000, ], 
#       aes(x = electorate.96, y = depfed.list.pct)) + 
#    geom_point(aes(color = comp.voting.98), size = 2, alpha = .5) + 
 #   stat_smooth(aes(group = comp.voting.98), color = "black", size = 1.8, method = "loess") + 
 #   theme_bw()  + scale_colour_brewer(palette = "Set1") + geom_vline(xintercept = 40500, lty = 2) + 
 #   scale_x_continuous("Electorate (1996)", labels = comma_format(digits = 5)) + 
 #   scale_y_continuous("Federal Deputy Party List Votes (\\%)")


## @knitr brazil_partylist_resultsplot

#partylist.results.table <- data.frame('Variable' = c(rep(names(brazil.party.results[1:12]), 2)),
 #                                     'Est' = c(sapply(brazil.party.results[1:12], function(x) x$ll.results['RD Estimate']), sapply(brazil.party.results[1:12], function(x) x$dm.results['RD Estimate'])),
  #                                    'SE' = c(sapply(brazil.party.results[1:12], function(x) x$ll.results['Standard Error']), sapply(brazil.party.results[1:12], function(x) x$dm.results['Standard Error'])),
   #                                   'Baseline' = c(sapply(brazil.party.results[1:12], function(x) x$ll.results['Baseline']), sapply(brazil.party.results[1:12], function(x) x$dm.results['Baseline'])),
    #                                  'N' = c(sapply(brazil.party.results[1:12], function(x) x$ll.results['N']), sapply(brazil.party.results[1:12], function(x) x$dm.results['N'])),
     #                                 'Bandwidth' = c(sapply(brazil.party.results[1:12], function(x) round(x$ll.results['Optimal Bandwidth'])), rep(5000, length(brazil.party.results[1:12]))),
      #                                'Specification' = c(rep("Local Linear", length(brazil.party.results[1:12])), rep("Difference\nin-Means", length(brazil.party.results[1:12])))
#)
#partylist.results.table$Office <- NA
#partylist.results.table$Office[grep("depfed", partylist.results.table$Variable)] <- "Party List Votes: Federal Deputy"
#partylist.results.table$Office[grep("depest", partylist.results.table$Variable)] <- "Party List Votes: State Deputy"
#partylist.results.table$Variable <- c("All\nParties", "All\nParties", "PT", "PSDB", "PMDB", "PPB", "PFL", "PT", "PSDB", "PMDB", "PPB", "PFL", "All\nParties", "All\nParties", "PT", "PSDB", "PMDB", "PPB", "PFL", "PT", "PSDB", "PMDB", "PPB", "PFL")
#partylist.results.table$Variable <- factor(partylist.results.table$Variable, levels = c("All\nParties", "PT", "PMDB", "PPB", "PSDB", "PFL"))

#ggplot(partylist.results.table[partylist.results.table$Variable == 'All\nParties', ], aes(x = Office, y = Est, color = Specification, shape = Specification)) + geom_pointrange(aes(ymin = Est - 1.96 * SE, ymax = Est + 1.96 * SE),  position = position_dodge(width = 0.4), size = .8) + theme_bw() + scale_colour_brewer(palette = "Set1") + geom_hline(yintercep = 0, lty = 2) + scale_x_discrete("") + scale_y_continuous("Estimate (\\%)") 

#ggplot(partylist.results.table[partylist.results.table$Variable != 'All\nParties', ], aes(x = Variable, y = Est, color = Specification, shape = Specification)) + geom_pointrange(aes(ymin = Est - 1.96 * SE, ymax = Est + 1.96 * SE),  position = position_dodge(width = 0.4), size = .8) + theme_bw() + scale_colour_brewer(palette = "Set1") + geom_hline(yintercep = 0, lty = 2) + scale_x_discrete("") + scale_y_continuous("Estimate (\\%)") + facet_wrap( ~ Office, ncol = 1, scales = 'free_y')



## @knitr brazil_partycand_results
#partycand.results.table <- data.frame('Variable' = c(rep(names(brazil.party.results[13:22]), 2)),
 #                                     'Est' = c(sapply(brazil.party.results[13:22], function(x) x$ll.results['RD Estimate']), sapply(brazil.party.results[13:22], function(x) x$dm.results['RD Estimate'])),
  #                                    'SE' = c(sapply(brazil.party.results[13:22], function(x) x$ll.results['Standard Error']), sapply(brazil.party.results[13:22], function(x) x$dm.results['Standard Error'])),
  #                                    'Baseline' = c(sapply(brazil.party.results[13:22], function(x) x$ll.results['Baseline']), sapply(brazil.party.results[13:22], function(x) x$dm.results['Baseline'])),
  #                                    'N' = c(sapply(brazil.party.results[13:22], function(x) x$ll.results['N']), sapply(brazil.party.results[13:22], function(x) x$dm.results['N'])),
   #                                   'Bandwidth' = c(sapply(brazil.party.results[13:22], function(x) round(x$ll.results['Optimal Bandwidth'])), rep(5000, length(brazil.party.results[13:22]))),
    #                                  'Specification' = c(rep("Local Linear", length(brazil.party.results[13:22])), rep("Difference\nin-Means", length(brazil.party.results[13:22])))
#)
#partycand.results.table$Office <- NA
#partycand.results.table$Office[grep("depfed", partycand.results.table$Variable)] <- "Candidate Votes: Federal Deputy"
#partycand.results.table$Office[grep("depest", partycand.results.table$Variable)] <- "Candidate Votes: State Deputy"
#partycand.results.table$Variable <- c("PT", "PSDB", "PMDB", "PPB", "PFL", "PT", "PSDB", "PMDB", "PPB", "PFL", "PT", "PSDB", "PMDB", "PPB", "PFL", "PT", "PSDB", "PMDB", "PPB", "PFL")
#partycand.results.table$Variable <- factor(partycand.results.table$Variable, levels = c("PT", "PMDB", "PPB", "PSDB", "PFL"))

#ggplot(partycand.results.table, aes(x = Variable, y = Est, color = Specification, shape = Specification)) + geom_pointrange(aes(ymin = Est - 1.96 * SE, ymax = Est + 1.96 * SE),  position = position_dodge(width = 0.4), size = .8) + theme_bw() + scale_colour_brewer(palette = "Set1") + geom_hline(yintercep = 0, lty = 2) + facet_wrap( ~ Office, nrow = 1, scales = "free_x") + scale_x_discrete("") + scale_y_continuous("Estimate (\\%)")


## @knitr brazil_NEinvalid

#NEinvalid.data <- br.pollingstation.data[br.pollingstation.data$region == 'Nordeste', c('electorate.96',  'comp.voting.98', 'uf', 'depfedbranconulo98.pct', 'depfed.incumbcoal.pct', 'depestbranconulo98.pct', 'depest.incumbcoal.pct')]
#NEinvalid.data$Type <- ifelse(NEinvalid.data$uf == 'BA' | NEinvalid.data$uf == 'MA' | NEinvalid.data$uf == 'PB', 'Northeastern Machine State', 'Northeastern Non-Machine State' )
#NEinvalid.data <- NEinvalid.data[NEinvalid.data$uf != "CE", ]
#NEinvalid.data$Ballot <- ifelse(NEinvalid.data$comp.voting.98 == 1, "Electronic", "Paper")

#ggplot(NEinvalid.data[NEinvalid.data$electorate.96 < 45500 & NEinvalid.data$electorate.96 > 35500 & NEinvalid.data$depfedbranconulo98.pct < 60, ], aes(depfedbranconulo98.pct, depfed.incumbcoal.pct, color = Ballot, group = Ballot)) + geom_point(size = 1, alpha = .5) + geom_smooth(se = FALSE, method = "loess", size = 2, aes(lty = Ballot)) + scale_y_continuous('Incumbent Coalition Votes (\\%)', limits = c(0,100)) + scale_x_continuous('Invalid Votes (\\%)') + theme_bw() + facet_grid(~Type) + scale_colour_brewer(palette = "Set1")



## @knitr NE_incumbcoal
#ggplot(brazil.data[brazil.data$electorate.96 > 0 & brazil.data$electorate.96 < 90000 & brazil.data$region == "Nordeste" & (brazil.data$uf != "BA"  | brazil.data$uf != "MA" | brazil.data$uf != "PB" ), ], aes(electorate.96, depfed.incumbcoal.pct, color = comp.voting.98)) + geom_point() + geom_smooth(se = TRUE, method = "loess", span = 1, degree = 1) + theme_bw()  + scale_colour_brewer(palette = "Set1") + geom_vline(xintercept = 40500, lty = 2) + scale_x_continuous("Electorate (1996)", labels = comma_format(digits = 5)) + scale_y_continuous(limits = c(0,100), "Incumbent Coalition Votes (\\%)") + labs(title = "Competitive Northeastern State")

#ggplot(brazil.data[brazil.data$electorate.96 > 0 & brazil.data$electorate.96 < 90000 & (brazil.data$uf == "BA"  | brazil.data$uf == "MA" | brazil.data$uf == "PB"), ], aes(electorate.96, depfed.incumbcoal.pct, color = comp.voting.98)) + geom_point() + geom_smooth(se = TRUE, method = "loess", degree = 1, span = 1) + theme_bw()  + scale_colour_brewer(palette = "Set1") + geom_vline(xintercept = 40500, lty = 2) + scale_x_continuous("Electorate (1996)", labels = comma_format(digits = 5)) + scale_y_continuous(limits = c(0,100), "Incumbent Coalition Votes (\\%)") + labs(title = "Political Machine State")



## @knitr NE_incumbcoal_results

#machine.results.table <- data.frame('Variable' = c(rep(names(machine.results), 2)),
 #                                   'Est' = c(sapply(machine.results, function(x) x$ll.results['RD Estimate']), sapply(machine.results, function(x) x$dm.results['RD Estimate'])),
  #                                  'SE' = c(sapply(machine.results, function(x) x$ll.results['Standard Error']), sapply(machine.results, function(x) x$dm.results['Standard Error'])),
 #                                   'Baseline' = c(sapply(machine.results, function(x) x$ll.results['Baseline']), sapply(machine.results, function(x) x$dm.results['Baseline'])),
  #                                  'N' = c(sapply(machine.results, function(x) x$ll.results['N']), sapply(machine.results, function(x) x$dm.results['N'])),
   #                                 'Bandwidth' = c(sapply(machine.results, function(x) round(x$ll.results['Optimal Bandwidth'])), rep(5000, length(machine.results))),
    #                                'Specification' = c(rep("Local Linear", length(machine.results)), rep("Difference\nin-Means", length(machine.results)))
#)
#machine.results.table$Office <- NA
#machine.results.table$Office[grep("depfed", machine.results.table$Variable)] <- "Federal Dep. Incumbent\nCoalition Vote Share"
#machine.results.table$Office[grep("depest", machine.results.table$Variable)] <- "State Dep. Incumbent\nCoalition Vote Share"
#machine.results.table$Variable <- rep(c("Northeastern Machine State", "Northeastern Non-Machine State"),4)
#machine.results.table$Variable <- factor(machine.results.table$Variable, levels = c("Northeastern Non-Machine State", "Northeastern Machine State"))

#ggplot(machine.results.table, aes(x = Office, y = Est, color = Specification, shape = Specification)) + geom_pointrange(aes(ymin = Est - 1.96 * SE, ymax = Est + 1.96 * SE),  position = position_dodge(width = 0.4), size = .8) + theme_bw() + scale_colour_brewer(palette = "Set1") + geom_hline(yintercep = 0, lty = 2) + facet_wrap( ~ Variable, nrow = 1, scales = "free_x") + scale_x_discrete("") + scale_y_continuous("Estimate (\\%)")

