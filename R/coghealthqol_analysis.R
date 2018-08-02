load(file="./inst/data/derived_data/qol_1_5.RData")
library(plm)
library(ggplot2)
library(forcats)
library(sjPlot)
source("./R/coghealthqol_functions.R")

qol_subset <- qol_1_5 %>%
  filter(wave < 5) %>%
  select(id, wave, qolsum, memory_self, heart_issue, other_issue,
         recall_immediate, recall_delayed, badfall, joint,
         pain, smokes, vig_act, mod_act, mild_act)

pgmm_data <- pdata.frame(qol_subset, index=c("id", "wave"))

pgmm_formula <- qolsum ~ lag(qolsum, 1) + memory_self + heart_issue +
  other_issue + recall_immediate + recall_delayed + badfall + joint + pain +
  smokes + vig_act + mod_act + mild_act | lag(qolsum, 2:99)

pgmm_m1 <- pgmm(pgmm_formula,
                   data = pgmm_data,
                   effect = "twoways",   # should consider two-way for small T
                   transformation = "ld")

plm_formula <- qolsum ~ lag(qolsum, 1) + memory_self + heart_issue +
  other_issue + recall_immediate + recall_delayed + badfall + joint + pain +
  smokes + vig_act + mod_act + mild_act

plm_m1 <- plm(plm_formula, data=pgmm_data, model="within", effects="twoway", index=c("id","wave"))

summary(plm_m1)
pgmm_summary(pgmm_m1)
###

sim_n <- 10000
pgmm_simparam <- MASS::mvrnorm(sim_n, coefficients(pgmm_m1), vcovHC(pgmm_m1))
plm_simparam <- MASS::mvrnorm(sim_n, coefficients(plm_m1), vcovHC(plm_m1))

pgmm_simbetas <- pgmm_simparam[,2:ncol(pgmm_simparam)]
pgmm_simbetas <- cbind(pgmm_simbetas[,13], pgmm_simbetas[,-13])			# Move the constant to the front of the matrix!

colnames(pgmm_simbetas)[1] <- "intercept"
colnames(pgmm_simbetas)[14] <- "wave_3"
colnames(pgmm_simbetas)[15] <- "wave_4"

plm_simbetas <- plm_simparam[,2:ncol(plm_simparam)]

pgmm_cf_data <- qol_subset %>%
  select(-id, -wave, -qolsum) %>%
  mutate(intercept=1, heart_issue=0, other_issue=0, badfall=0, joint=0, smokes=0, wave_3=0, wave_4=0) %>%
  select(intercept, everything()) %>%
  summarize_all(mean, na.rm=T) %>%
  slice(rep(1L, each=24))

pgmm_cf_sds <- qol_subset %>%
  select(-id, -wave, -qolsum) %>%
  mutate(intercept=1, wave_3=0, wave_4=0) %>%
  select(intercept, everything()) %>%
  summarize_all(sd, na.rm=T) %>%
  mutate(heart_issue=1, other_issue=1, badfall=1, joint=1, smokes=1)

for (i in 1:12){
  pgmm_cf_data[i,i+1] <- pgmm_cf_data[i,i+1] + pgmm_cf_sds[1,i+1]
}

##
pgmm_sim_pe <- as.matrix(pgmm_cf_data) %*% t(pgmm_simbetas)
plm_sim_pe <- as.matrix(pgmm_cf_data[-c(1,14,15)]) %*% t(plm_simbetas)

pgmm_sim_diff <- pgmm_sim_pe[1:12,] - pgmm_sim_pe[13:24,]
plm_sim_diff <- plm_sim_pe[1:12,] - plm_sim_pe[13:24,]

ordered_vars <- c("S.R. Memory", "Heart Issue*", "Other Issue*",
                  "Imm. Recall", "Del. Recall", "Fall*", "Joints*", "Pain*", "Smokes*", "Vig. Activity",
                  "Mod. Activity", "Mild Activity")
pgmm_estimated_diff <- setNames(as.data.frame( t(apply(pgmm_sim_diff, 1, extract_pe_ci))),c("PE", "LB", "UB"))
pgmm_estimated_diff$scenario <- names(pgmm_cf_sds)[-c(1,14,15)]
pgmm_estimated_diff$Model <- "PGMM"
pgmm_estimated_diff$varname <- ordered_vars

plm_estimated_diff <- setNames(as.data.frame( t(apply(plm_sim_diff, 1, extract_pe_ci))),c("PE", "LB", "UB"))
plm_estimated_diff$scenario <- names(pgmm_cf_sds)[-c(1,14,15)]
plm_estimated_diff$Model <- "PLM"
plm_estimated_diff$varname <- ordered_vars

combined_estimated_diff <- rbind(pgmm_estimated_diff, plm_estimated_diff)
combined_estimated_diff <- combined_estimated_diff %>%
  mutate(varname=fct_rev(fct_relevel(factor(varname), ordered_vars)))

# Ignore error, works anyway.
estimates_plot <- combined_estimated_diff %>%
  ggplot(aes(x=varname, y=PE, group=Model, color=Model)) +
  scale_color_manual(values=c("black", "grey60")) +
  geom_errorbar(aes(ymin=LB, ymax=UB), position=position_dodge(width=-0.4), width=0.3) +
  geom_point(aes(shape=Model), position=position_dodge(width=-0.4)) +
  coord_flip() + theme_minimal() +
  xlab("Variable") + ylab("Difference in Expected Quality of Life") +
  ggtitle("Estimated Variable Effects", subtitle="Effect of 1 SD or unit (*) increase on Quality of Life")

ggsave(filename="./inst/figures/estimates_plot.png", plot=estimates_plot, height=6, width=5)
