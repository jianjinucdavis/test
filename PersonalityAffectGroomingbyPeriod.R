grmrec.df.scaled <- read.csv("./CleanData//grmrec.df.scaled.csv")
hud.df.scaled <- read.csv("./CleanData//hud.df.scaled.csv")
pcps <- read.csv("./data//pcps.data.csv")

summary(grmrec.df.scaled)
library(dplyr)
grmrec.df.scaled.female <- filter(grmrec.df.scaled, Sex == "F")
grmrec.personality.scaled.lm <- lm(grmrec.post ~ grmrec.pre.scaled + 
                                     ExcitabilityPre.scaled  + SociabilityPre.scaled
                                   + ConfidencePre.scaled * grmrec.pre.scaled ,
                                   data = grmrec.df.scaled.female)

summary(grmrec.personality.scaled.lm)
grmrecNonkin.personality.scaled.lm <- 
  lm(grmrecNonkin.post ~ grmrecNonkin.pre.scaled + 
       ExcitabilityPre.scaled * grmrecNonkin.pre.scaled +
       ConfidencePre.scaled  + SociabilityPre.scaled, 
     data = grmrec.df.scaled.female)
summary(grmrecNonkin.personality.scaled.lm)

names(pcps)
library(lmerTest)


grmini.lmer <- lmer(grmini ~ 
                     bold.ctr * Period + warm.ctr * Period + excitable1.ctr * Period
                          + (1|Matriline) +(1|ID),
                          REML = FALSE,
                          data = pcps)
summary(grmini.lmer)


