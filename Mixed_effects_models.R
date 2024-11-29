library("lme4")
library("sjmisc")
library("sjstats")
library("sjPlot")
library("lmerTest")

mixed.lmer <- lmer(RNA_NADPH ~ respiration * MAOM_POM + (1|both), data = neof_paper_figures)
summary(mixed.lmer)

tab_model(mixed.lmer)
plot_model(mixed.lmer, vline.color = "red", show.values =TRUE)

mixed.lmer2 <- lmer(RNA_NADPH ~ MAOM_POM + (1|both), data = neof_paper_figures)
summary(mixed.lmer2)

performance::r2(mixed.lmer)


attributes(mixed.lmer(gm)$stddev)


my_model <- lmer(DNA_NOS ~ respiration + C_N + MAOM_POM + (1|both), data = neof_paper_figures)
summary(my_model)

anova(my_model)

set_theme(
  base = theme_blank(), axis.linecolor = "black"
)
p = plot_model(my_model, colors = c("#00aedb", "#d11141"))
#p + ggplot2::ylim(-1000, 600)
p





