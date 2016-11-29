library(tidyverse)
crf.data <- read_csv("crfData.csv")

# Using factors
crf.data$anxiety <- as.factor(crf.data$anxiety) 
crf.data$preparation <- as.factor(crf.data$preparation)

levels(crf.data$anxiety) <- list("Low Anxiety"=1, 
                                 "High Anxiety"=2) 

levels(crf.data$preparation) <- list("Low Preparation"=1,
                                     "Medium Preparation"=2,
                                     "High Preparation"=3)


# Setting contrasts
options(contrasts = c("contr.sum", "contr.poly"))


#Run the analysis
crf.lm <- lm(mark ~ anxiety * preparation, data=crf.data)
library(apaTables)
apa.aov.table(crf.lm, filename = "Table1.doc")

apa.2way.table(iv1=preparation, iv2=anxiety, dv=mark, data=crf.data,
               show.marginal.means = TRUE, filename="Table2.doc")

# note: publish bar graph and the table!

# install.packages("phia", dep=T) # always use dep T
library(phia)

# for simple main effects...
testInteractions(crf.lm, fixed="anxiety", across="preparation",adjustment="none")

# note: this does not give you effect sizes... expect this to be on the final exam!!! --> see hand out for how to get this!! and script here...

get.ci.partial.eta.squared(F.value=7.1411, df1=2, df2=24, conf.level = .90)

get.ci.partial.eta.squared(F.value=0.9733, df1=2, df2=24, conf.level = .90)

# now you have CIs and effect sizes... now write up according to example in handout

# bonferroni added...for paired comparisons
testInteractions(crf.lm, fixed="anxiety", 
                 pairwise="preparation", 
                 adjustment="bonferroni") 
