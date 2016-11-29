library(tidyverse)
install.packages("car", dep=T)

# there are 2 variations of the t-test... one assumes homogeneity of variance, the other doesn't!

mdata <- read_csv("drugData.csv")
mdata$Group <- as.factor(mdata$Group)

## these get descriptives using Tidyverse...
# mdata_grouped <- group_by(mdata, Group)
# mdata_grouped %>% summarise(M=mean(Arousal, na.rm=TRUE), SD=sd(Arousal)) # remember: na.rm makes sure to get rid of any missing values

# conduct Levene's before t-test to check homo. of variance... 
car::leveneTest(mdata$Arousal, group=mdata$Group, center="median") # non-signif means that the variance are the same

# take the original data and break those groups out...
ex.group.rows <- mdata %>% filter(Group==0)
control.group.rows <- mdata %>% filter(Group==1)
# note: filter gives you rows; select gives you columns

t.test(x=ex.group.rows$Arousal, y=control.group.rows$Arousal, var.equal = TRUE)
# t.test(x=ex.group.rows$Arousal, y=control.group.rows$Arousal, var.equal = TRUE) ... if no homo of var. 

# install.packages("MBESS", dep=T)
library(MBESS)

?smd
smd(Group.1=ex.group.rows$Arousal, Group.2=control.group.rows$Arousal)
smd(Mean.1=3.2, s.1=8, Mean.2=2.45, s.2 = .91, n.1=10, n.2=10)

?ci.smd
ci.smd(smd=0.8753837, n.1=10, n.2=10)

# one way ANOVA
mdata <- read_csv("Viagra.csv")
mdata$dose <- as.factor(mdata$dose)
levels(mdata$dose) <- list("Placebo" =1, "Low Dose" =2, "High Dose" = 3)

car::leveneTest(mdata$libido, group=mdata$dose, center="median")

options(contrasts = c("contr.sum", "contr.ploy"))

oneway.results <- lm(libido~dose, data=mdata)
# can use carr:Anova(oneway.results, type=3)... but David recommends using apaTables instead...
library(apaTables)
apa.aov.table(oneway.results)

# why do we use 90% CI???

apa.1way.table(iv=dose, dv=libido, data=mdata)

# add graphing code lines from handouts

apa.d.table(iv=dose, dv=libido, data=mdata) # don't need to use MBESS then
