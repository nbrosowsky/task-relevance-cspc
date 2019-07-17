load(file="data-analysis/sim_effectSizes.Rda")


sim_effectSizes <- sim_effectSizes %>% filter(effect.size < 41)

plot.1 <- ggplot(sim_effectSizes[sim_effectSizes$effect != "CSPC x Condition",], aes(effect.size, prop.sig, color = effect)) + 
  geom_smooth( aes(color = effect), se = FALSE, method = "gam", formula = y ~ s(log(x)))+
  geom_point(aes(x = effect.size, y = prop.sig, color=effect, shape = effect), size = 2) +
  geom_vline(xintercept = 20, linetype = "dashed") +
  scale_color_manual(values = c("#006d2c", "#41ab5d", "#a1d99b")) +
  labs(y = "Proportion p < .05", x = "Effect Size (ms)", color = "Effect", shape = "Effect") +
  scale_x_continuous(limits = c(0, 40), breaks=c(seq(from=0, to=40, by=5)), expand = expand_scale(mult = c(0,0.05)))+
  scale_y_continuous(limits = c(0, 1.01), breaks=c(seq(from=0, to=1, by=.1)), expand = expand_scale(mult =c(0,0.01)) )+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),       
        legend.position = c(.6, .3),
        #legend.direction = "horizontal",
        #legend.background = element_rect(colour = "white", fill = "white", size=1),
        #legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  ) 

sim_effectSizes <- sim_effectSizes[sim_effectSizes$effect == "CSPC x Condition",]
levels(sim_effectSizes$effect) <- c("CSPC x\nCondition", "CSPC (N = 50)","CSPC (N = 150)")

plot.2 <- ggplot(sim_effectSizes, aes(effect.size, prop.sig, color = effect)) + 
  geom_smooth( aes(color = effect), se = FALSE, method = "gam", formula = y ~ s(log(x)))+
  geom_point(aes(x = effect.size, y = prop.sig, color=effect, shape = effect), size = 2) +
  geom_vline(xintercept = 20, linetype = "dashed") +
  scale_color_manual(values = c("#006d2c", "#41ab5d", "#a1d99b")) +
  labs(y = "Proportion p < .05", x = "Effect Size (ms)", color = "Effect", shape = "Effect") +
  scale_x_continuous(limits = c(0, 40), breaks=c(seq(from=0, to=40, by=5)), expand = expand_scale(mult = c(0,0.05)))+
  scale_y_continuous(limits = c(0, 1.01), breaks=c(seq(from=0, to=1, by=.1)), expand = expand_scale(mult =c(0,0.01)) )+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),       
        legend.position = c(.6, .3),
        #legend.direction = "horizontal",
        #legend.background = element_rect(colour = "white", fill = "white", size=1),
        #legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  ) 


load(file="data-analysis/sim_sampleSizes.Rda")


sim_sampleSizes <- sim_sampleSizes %>% 
  mutate(sample.size = as.numeric(as.character(sample.size)),
         effect.size = factor(effect.size)) %>%
  filter(sample.size < 141)

levels(sim_sampleSizes$effect.size) <- c("15 ms", "20 ms", "25 ms")

plot.3 <- ggplot(sim_sampleSizes[sim_sampleSizes$effect == "PC",], aes(sample.size, prop.sig)) +
  #geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_smooth(aes(color = effect.size), se = FALSE, method = "gam", formula = y ~ s(log(x)), size = .9) +
  geom_point(aes(color = effect.size, shape = effect.size), size = 2) +
  geom_hline(yintercept = .8, linetype ="dashed") +
  scale_color_manual(values = c("#253494", "#2c7fb8", "#41b6c4"))+
  labs(y = "Proportion p < .05", x = "Sample Size", color = "CSPC Effect", shape = "CSPC Effect") +
  scale_x_continuous(limits = c(0, 140), breaks=c(seq(from=0, to=140, by=20)), expand = expand_scale(mult = c(0,0.05)))+
  scale_y_continuous(limits = c(0, 1.01), breaks=c(seq(from=0, to=1, by=.1)), expand = expand_scale(mult =c(0,0.01)) )+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),       
        legend.position = c(.6, .3),
        #legend.direction = "horizontal",
        #legend.background = element_rect(colour = "white", fill = "white", size=1),
        #legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  ) 


plot.4 <- ggplot(sim_sampleSizes[sim_sampleSizes$effect != "PC",], aes(sample.size, prop.sig)) +
  #geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  scale_color_manual(values = c("#253494", "#2c7fb8", "#41b6c4"))+
  geom_smooth(aes(color = effect.size), se = FALSE, method = "gam", formula = y ~ s(log(x)), size = .9) +
  geom_point(aes(color = effect.size, shape = effect.size), size = 2) +
  geom_hline(yintercept = .8, linetype ="dashed") +
  labs(y = "Proportion p < .05", x = "Sample Size", color = "CSPC x\nCondition Effect", shape =  "CSPC x\nCondition Effect") +
  scale_x_continuous(limits = c(0, 140), breaks=c(seq(from=0, to=140, by=20)), expand = expand_scale(mult = c(0,0.05)))+
  scale_y_continuous(limits = c(0, 1.01), breaks=c(seq(from=0, to=1, by=.1)), expand = expand_scale(mult =c(0,0.01)) )+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),       
        legend.position = c(.6, .3),
       #legend.direction = "horizontal",
       #legend.background = element_rect(colour = "white", fill = "white", size=1),
       #legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  ) 



########## ARRANGE #########
figure5 <- plot_grid(plot.1,NULL,plot.2, plot.3, NULL, plot.4,
                   nrow = 2, ncol = 3,
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B", "C","","D"))

#title <- ggdraw() + draw_label("Experiment 1", fontface='bold')
#figure2<-plot_grid(title, figure2, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

#figure2

ggsave("figure5.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 5, units = "in") 