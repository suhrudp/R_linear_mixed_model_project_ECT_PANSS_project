# set working directory
setwd("C:/Users/SURHUD/Desktop/Desktop/Stats/ReQuir Stats/Sagar Bhalke Analysis + Results")

# set a seed for the random number generator
set.seed(13)

# load libraries
library(readxl)
library(tidyverse)
library(gtsummary)
library(flextable)
library(reshape2)
library(ggsci)
library(lmerTest)
library(emmeans)
library(report)

# import data
df <- read_excel("C:/Users/SURHUD/Desktop/Desktop/Stats/ReQuir Stats/Sagar Bhalke Analysis + Results/Data.xlsx")

df %>% colnames

df$Group <- as.factor(df$Group)

# get the summary statistics table
table1 <- df[,c(2:34)] %>% 
            tbl_summary(by = "Group",
                        type = list(where(is.numeric) ~ "continuous"),
                        missing_text = "Missing data") %>%
            add_p() %>%
            add_overall()

# save the table as a .docx file
table1 %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 1.docx")

# convert wide data into long data for repeated measures
df1 <- df %>% melt(id.vars = c("ID", "Group"),
                   measure.vars = c("PANSS pre-ECT Total", 
                                    "PANSS mid-ECT Total", 
                                    "PANSS post-ECT Total"))

df2 <- df %>% melt(id.vars = c("ID", "Group"),
                   measure.vars = c("B4ECT ReCoDe pre-ECT Total objective score", 
                                    "B4ECT ReCoDe post-ECT Total objective score"))

# create a linear mixed effects model with an interaction term and each participant treated as a random intercept
mod1 <- lmer(data = df1,
             value ~ variable*Group + variable + Group + (1|ID))

# get automated statistical reports
report(mod1)

# get estimated marginal means for the interaction term and save it as a dataframe
emm1 <- as.data.frame(emmeans(mod1, ~variable*Group))

# get pairwise contrasts for the estimated marginal means and save it
emmdat1 <- emmeans(mod1, pairwise ~ variable*Group)

con1 <- as.data.frame(emmdat1$contrasts)

write.csv(con1, file = "mod1.csv")

# from the saved dataframe, plot the estimated marginal means (AKA least squares means AKA "adjusted means")
plot1 <- ggplot(emm1, aes(x = variable, y = emmean, color = Group)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), size = 0.75, lwd = 0.75, position = position_dodge(width = 0.25)) +
  geom_line(aes(group = Group), lwd = 0.75, position = position_dodge(width = 0.25)) +
  scale_color_jco() + 
  labs(x = "Time",
       y = "PANSS Total")

# save the plot in high quality
ggsave(plot1,
       filename = "Plot 1.png",
       width = 10,
       height = 5,
       dpi = 600)

# repeat for the second model
mod2 <- lmer(data = df2,
             value ~ variable*Group + variable + Group + (1|ID))

report(mod2)

emm2 <- as.data.frame(emmeans(mod2, ~variable*Group))

emmdat2 <- emmeans(mod2, pairwise ~ variable*Group)

con2 <- as.data.frame(emmdat2$contrasts)

write.csv(con2, file = "mod2.csv")

plot2 <- ggplot(emm2, aes(x = variable, y = emmean, color = Group)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), size = 0.75, lwd = 0.75, position = position_dodge(width = 0.25)) +
  geom_line(aes(group = Group), lwd = 0.75, position = position_dodge(width = 0.25)) +
  scale_color_jco() + 
  labs(x = "Time",
       y = "B4ECT ReCoDe Total Objective score")

ggsave(plot2,
       filename = "Plot 2.png",
       width = 10,
       height = 5,
       dpi = 600)
