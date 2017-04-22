#####################
### Data Analysis ###
#####################

## Please set your working directory to the data/ folder

# Clear the workspace
rm(list = ls())

# Load necessary packages
library(dplyr) # data manipulation
library(Synth) # models

# Load data
df <- read.csv("df.csv", header = TRUE)

# Prepare dataset
df$state <- as.character(df$state) # required by dataprep()

# Plot: Homicide rates for Sao Paulo and Brazil (average)
df1 <- df %>%
        mutate(homicide.sp = ifelse(homicide.rates & state == "São Paulo", homicide.rates, NA)) %>%
        select(year, homicide.sp)

df2 <- df %>%
        mutate(homicide.rates1 = ifelse(homicide.rates & state != "São Paulo", homicide.rates, NA)) %>%
        group_by(year) %>%
        summarise(homicide.br = mean(homicide.rates1, na.rm = TRUE))

setEPS()
postscript(file    = "br.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,     # 17.8 cm
           height  = 5.25)  # 13.3 cm

plot(x = df1$year,
     y = df1$homicide.sp,
     type = "l",
     ylim = c(0, 60),
     xlim = c(1990, 2009),
     xlab = "Year",
     ylab = "Homicide Rates",
     cex = 3,
     lwd = 2,
     xaxs = "i",
     yaxs = "i"
)

lines(df2$year,
      df2$homicide.br,
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

# Prepare data for synth
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
                 treatment.identifier  = 35,
                 controls.identifier   = c(11:17, 21:27, 31:33, 41:43, 50:53),
                 time.predictors.prior = c(1990:1998),
                 time.optimize.ssr     = c(1990:1998),
                 time.plot             = c(1990:2009)
                 )

# Run synth
synth.out <- synth(dataprep.out)

# Get result tables
print(synth.tables   <- synth.tab(
        dataprep.res = dataprep.out,
        synth.res    = synth.out)
      )

# Plot: Main model
setEPS()
postscript(file    = "trends.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,     # 17.8 cm
           height  = 5.25)  # 13.3 cm

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

invisible(dev.off())

# Main model: gaps plot
setEPS()
postscript(file    = "gaps.eps",
           horiz   = FALSE,
           onefile = FALSE,
           width   = 7,
           height  = 5.25)

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)

invisible(dev.off())

## Calculating how many lives were saved during the treatment period

# Weights below retrieved form dataprep.out
# State Code  State Weight  State Name        State Abbreviation
# 42          0.274         Santa Catarina    SC
# 53          0.210         Distrito Federal  DF
# 32          0.209         Espirito Santo    ES
# 33          0.169         Rio de Janeiro    RJ
# 14          0.137         Roraima           RR
# 14          0.001         Pernambuco        PB
# 35          treat         Sao Paulo         SP

# Get years after policy change
df.2 <- df[which(df$year >= 1999),]

# Calculate total number of deaths in SP
num.deaths.sp <- sum( (df.2$homicide.rates[which(df.2$abbreviation == "SP")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))

#Calculate estimated number of deaths in Synthetic São Paulo
num.deaths.synthetic.sp <- sum( (0.274 * (df.2$homicide.rates[which(df.2$abbreviation == "SC")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))
                                + (0.210 * (df.2$homicide.rates[which(df.2$abbreviation == "DF")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))
                                + (0.209 * (df.2$homicide.rates[which(df.2$abbreviation == "ES")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))
                                + (0.169 * (df.2$homicide.rates[which(df.2$abbreviation == "RJ")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))
                                + (0.137 * (df.2$homicide.rates[which(df.2$abbreviation == "RR")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))
                                + (0.001 * (df.2$homicide.rates[which(df.2$abbreviation == "PB")])/100000 * (df.2$population.projection[which(df.2$abbreviation == "SP")]))
                                )

lives.saved <- num.deaths.synthetic.sp - num.deaths.sp
lives.saved # Between 1999 and 2009
