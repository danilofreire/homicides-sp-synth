########################
### Robustness Tests ###
########################

# Please set your working directory to the data/ folder

# Clear the workspace
rm(list = ls())

# Load data
df <- read.csv("df.csv", header = TRUE)

# Prepare dataset
df$state <- as.character(df$state) # required by dataprep()

## Placebo Test -- Control ends in 1994
dataprep.out1 <-
        dataprep(df,
                 predictors = c("state.gdp.capita",
                                "state.gdp.growth.percent",
                                "population.projection.ln",
                                "years.schooling.imp"
                 ),
                 special.predictors = list(
                         list("homicide.rates", 1990:1994, "mean"),
                         list("proportion.extreme.poverty", 1990:1994, "mean"),
                         list("gini.imp", 1990:1994, "mean")
                 ),
                 predictors.op = "mean",
                 dependent     = "homicide.rates",
                 unit.variable = "code",
                 time.variable = "year",
                 unit.names.variable   = "state",
                 treatment.identifier  = 35,
                 controls.identifier   = c(11:17, 21:27, 31:33, 41:43, 50:53),
                 time.predictors.prior = c(1990:1994),
                 time.optimize.ssr     = c(1990:1994),
                 time.plot             = c(1990:1998)
                 )

# Run synth
synth.out1 <- synth(dataprep.out1)

# Get result tables
print(synth.tables   <- synth.tab(
        dataprep.res = dataprep.out1,
        synth.res    = synth.out1)
      )

# Placebo test: graph
setEPS()
postscript(file    = "placebo.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,
           height  = 5.25)

path.plot(synth.res       = synth.out1,
          dataprep.res    = dataprep.out1,
          Ylab            = c("Homicide Rates"),
          Xlab            = c("Year"),
          Legend          = c("São Paulo","Synthetic São Paulo"),
          Legend.position = c("bottomleft"),
          Ylim            = c(0, 50)
)

abline(v   = 1995,
       lty = 2)

arrows(1994, 40, 1995, 40,
       col    = "black",
       length = .1)

text(1993, 40,
     "Placebo \nPolicy Change",
     cex = .8)

invisible(dev.off())

## Leave-one-out

# Loop over leave one outs
storegaps <- matrix(NA, length(1990:2009), 4)

colnames(storegaps) <- c(14, 33, 42, 53) # RR, RJ, SC, DF
co <- unique(df$code)
co <- co[-25]

for(k in 1:4){

        # Data prep for training model
        omit <- c(14, 33, 42, 53)[k]

        # Prepare data for synth
        dataprep.out2 <-
                dataprep(df,
                         predictors = c("state.gdp.capita",
                                        "state.gdp.growth.percent",
                                        "population.projection.ln",
                                        "years.schooling.imp"
                         ),
                         special.predictors = list(
                                 list("homicide.rates", 1990:1998, "mean"),
                                 list("proportion.extreme.poverty", 1990:1998, "mean"),
                                 list("gini.imp", 1990:1998, "mean")
                         ),
                         predictors.op = "mean",
                         dependent     = "homicide.rates",
                         unit.variable = "code",
                         time.variable = "year",
                         unit.names.variable   = "state",
                         treatment.identifier  = 35,
                         controls.identifier   = co[-which(co==omit)],
                         time.predictors.prior = c(1990:1998),
                         time.optimize.ssr     = c(1990:1998),
                         time.plot             = c(1990:2009)
                )

        # Run synth
        synth.out2 <- synth(dataprep.out2)

        storegaps[,k] <- (dataprep.out2$Y0%*%synth.out2$solution.w)
} # Close loop over leave one outs

# Leave-one-out: graph
setEPS()
postscript(file    = "leave-one-out.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,
           height  = 5.25)

path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)

for(i in 1:4){
        lines(1990:2009,
              storegaps[,i],
              col = "darkgrey",
              lty = "solid")
}

lines(1990:2009,
      dataprep.out$Y0plot %*% synth.out$solution.w,
      col = "black",
      lty = "dashed",
      lwd = 2)

legend(x = "bottomleft",
       legend = c("São Paulo",
                  "Synthetic São Paulo",
                  "Synthetic São Paulo (leave-one-out)"
       ),
       lty    = c("solid", "dashed", "solid"),
       col    = c("black", "black", "darkgrey"),
       cex    = .8,
       bg     = "white",
       lwdc(2, 2, 1)
)

invisible(dev.off())

## Permutation test
states <- c(11:17, 21:27, 31:33, 35, 41:43, 50:53)

# Prepare data for synth
results <- list()
results_synth <- list()
gaps <- list()

for (i in states) {
    dataprep.out <-
            dataprep(df,
                     predictors = c("state.gdp.capita",
                                    "state.gdp.growth.percent",
                                    "population.projection.ln",
                                    "years.schooling.imp"
                                    ),
                     special.predictors = list(
                             list("homicide.rates", 1990:1998, "mean"),
                             list("proportion.extreme.poverty", 1990:1998, "mean"),
                             list("gini.imp", 1990:1998, "mean")
                             ),
                     predictors.op = "mean",
                     dependent     = "homicide.rates",
                     unit.variable = "code",
                     time.variable = "year",
                     unit.names.variable   = "state",
                     treatment.identifier  = i,
                     controls.identifier   = states[which(states!=i)],
                     time.predictors.prior = c(1990:1998),
                     time.optimize.ssr     = c(1990:1998),
                     time.plot             = c(1990:2009)
                     )
    results[[as.character(i)]] <- dataprep.out
    results_synth[[as.character(i)]] <- synth(results[[as.character(i)]])
    gaps[[as.character(i)]] <- results[[as.character(i)]]$Y1plot - (results[[as.character(i)]]$Y0plot %*% results_synth[[as.character(i)]]$solution.w)

}

# Permutation test: graph
## Permutation test
setEPS()
postscript(file    = "permutation-gaps2.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,
           height  = 5.25)

plot(1990:2009,
     ylim = c(-30, 30),
     xlim = c(1990,2009),
     ylab = "Gap in Homicide Rates",
     xlab = "Year"
)

for (i in states) {
        lines(1990:2009,
              gaps[[as.character(i)]],
              col = "lightgrey",
              lty = "solid",
              lwd = 2
        )
}

lines(1990:2009,
      gaps[["35"]], # São Paulo
      col = "black",
      lty = "solid",
      lwd = 2
)

abline(v   = 1999,
       lty = 2)

abline(h   = 0,
       lty = 1,
       lwd = 1)

arrows(1997, 25, 1999, 25,
       col    = "black",
       length = .1)

text(1995, 25,
     "Policy Change",
     cex = .8)

legend(x = "bottomleft",
       legend = c("São Paulo",
                  "Control States"),
       lty  = c("solid", "solid"),
       col  = c("black", "darkgrey"),
       cex  = .8,
       bg  = "white",
       lwdc(2, 2, 1)
)

invisible(dev.off())

# Permutation graph: states with MSPE no higher than 2x São Paulo's
low.mspe <- c(13, 15, 17, 21, 23, 24, 25, 31, 41:43, 53)

setEPS()
postscript(file    = "low-mspe.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,
           height  = 5.25)

plot(1990:2009,
     ylim = c(-30, 30),
     xlim = c(1990,2009),
     ylab = "Gap in Homicide Rates",
     xlab = "Year"
)

for (i in low.mspe) {
lines(1990:2009,
      gaps[[as.character(i)]],
      col = "lightgrey",
      lty = "solid",
      lwd = 2
      )
}

lines(1990:2009,
      gaps[["35"]], # São Paulo
      col = "black",
      lty = "solid",
      lwd = 2
      )

abline(v   = 1999,
       lty = 2)

abline(h   = 0,
       lty = 1,
       lwd = 1)

arrows(1997, 25, 1999, 25,
       col    = "black",
       length = .1)

text(1995, 25,
     "Policy Change",
     cex = .8)

legend(x = "bottomleft",
       legend = c("São Paulo",
                  "Control States (MSPE Less Than Two Times That of São Paulo)"),
       lty    = c("solid", "solid"),
       col    = c("black", "darkgrey"),
       cex    = .8,
       bg     = "white",
       lwdc(2, 2, 1)
)

invisible(dev.off())

## CausalImpact
# Uncomment the lines below to install necessary packages
# install.packages(c("devtools", "dtw"))
# library(devtools)
# install_github("google/CausalImpact")
# install_github("klarsen1/MarketMatching", build_vignettes=TRUE)

# Load packages
library(CausalImpact)
library(MarketMatching)

# Prepare data
df$year2 <- as.Date(paste(df$year, sep = "", "-01-01"))

# Estimate model
mm <- best_matches(data=df,
                   id_variable="code",
                   date_variable="year2",
                   matching_variable="homicide.rates",
                   parallel=TRUE,
                   warping_limit=1, # warping limit=1
                   dtw_emphasis=1, # rely only on dtw for pre-screening
                   matches=5, # request 5 matches
                   start_match_period="1990-01-01",
                   end_match_period="1998-01-01")

# View best matches
subset(mm$BestMatches, code == 35) # SP

# Results
results <- MarketMatching::inference(matched_markets = mm,
                                     test_market = "35",
                                     end_post_period = "2009-01-01")

# Predictions
results$Predictions

# Plot results
results$PlotActualVersusExpected +
        ggtitle("São Paulo versus Synthetic São Paulo") + theme_bw() +
        geom_line(aes(results$PlotActualVersusExpected$data$test_market),colour="#000099")
results$PlotCumulativeEffect

# Graph
setEPS()
postscript(file    = "causal-impact.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,     # 17.8 cm
           height  = 5.25)  # 13.3 cm

plot(x = (1990:2009),
     y = as.numeric(results$Predictions$Response),
     type = "l",
     ylim = c(0, 60),
     xlim = c(1990, 2009),
     xlab = "Year",
     ylab = "Homicide Rates",
     cex = 3,
     lwd = 2)

lines(x = (1990:2009),
      y = as.numeric(results$Predictions$Predicted),
      type = "l",
      lty = 2,
      cex = 3,
      lwd = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)

abline(v   = 1999,
       lty = 2)

legend(x = "bottomleft",
       legend = c("São Paulo",
                  "Brazil (average)"),
       lty    = c("solid", "dashed"),
       cex    = .8,
       bg     = "white",
       lwdc(2, 2)
)

invisible(dev.off())
