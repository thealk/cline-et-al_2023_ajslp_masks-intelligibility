# R script to acccompany Cline, Knowles, & Badh (submitted): Effect of face masks and speech style on speech intelligibility and listener effort in Parkinson’s disease
# This R script will be sourced in the .Rmd manuscript file.

library(tidyverse)
library(readr)
library(tidylog)
library(lme4)
library(stringr)
library(car) # logit is from car
library(cowplot)
library(flextable)
library(psych) # for ICC
library(irr) # for kappa

# Load data
# Note: df corresponds to "sim_perception_data_cleaned.csv"
load("sim_perception_workspace.RData")


# Reliability ----

# ..Interpretations: ----
# Interpretation for ICC from koo & li 2016:
# "Values less than 0.5 are indicative of poor reliability, values between 0.5 and 0.75 indicate moderate reliability, values between 0.75 and 0.9 indicate good reliability, and values greater than 0.90 indicate excellent reliability."

# ..Intra-rater ----
# 2-way mixed model (icc3k)

df_icc_names <- c("type","ICC","F","df1","df2","p","lower bound","upper bound","participant")
df_icc_i <- data.frame(matrix(ncol=9, nrow=0)) # accuracy
df_icc_e <- data.frame(matrix(ncol=9, nrow=0)) # effort
names(df_icc_i) <- df_icc_names
names(df_icc_e) <- df_icc_names


# Calculate for each listener participant and report average
for(pid in levels(df_reliability$participant)){
  
  current_pid <- df_reliability %>%
    filter(participant == pid)
  
  # ICC
  ## Accuracy
  current_icc_i <- current_pid %>%
    select(prop_correct.x, prop_correct.y) %>%
    psych::ICC(lmer=FALSE) # lmer=FALSE to facilitate convergence
  
  current_icc_i <- current_icc_i$results %>%
    mutate(participant = pid) %>%
    filter(type=="ICC3k")
  
  df_icc_i <- rbind(df_icc_i, current_icc_i)
  
  ## Effort
  current_icc_e <- current_pid %>%
    select(effort.x, effort.y) %>%
    psych::ICC(lmer=FALSE) # lmer=FALSE to facilitate convergence
  
  current_icc_e <- current_icc_e$results %>%
    mutate(participant = pid) %>%
    filter(type=="ICC3k")
  
  df_icc_e <- rbind(df_icc_e, current_icc_e)
  
}

df_icc_intra_acc <- df_icc_i
df_icc_intra_eff <- df_icc_e

mean(df_icc_intra_acc$ICC) # .76
mean(df_icc_intra_eff$ICC) # .73



# ..Inter-rater ----
# 2-way random model (icc2k)

# As with intra-rater reliability, we are using ICC for both effort and prop_correct (accuracy)
# Note that generic vars are getting overwritten here from above. The relevant reported vars are saved as, e.g. df_icc_inter_eff

df_icc_names <- c("type","ICC","F","df1","df2","p","lower bound","upper bound","speaker")
df_icc_i <- data.frame(matrix(ncol=9, nrow=0))
df_icc_e <- data.frame(matrix(ncol=9, nrow=0))
names(df_icc_i) <- df_icc_names
names(df_icc_e) <- df_icc_names


# Calculate for each speaker playlist
for(spid in unique(df$speaker)){
  
  # each speaker listened to by 8 - 11 unique listeners
  # listeners only heard 1 speaker, so calculate on a speaker-by-speaker basis
  current_speaker <- df %>%
    filter(speaker == spid) %>%
    select(participant,speaker,file,effort, prop_correct)
  #view(current_speaker)
  
  # Speaker playlists
  # each listener is now a unique column
  ## Accuracy
  current_speaker_i <- current_speaker %>%
    select(-effort) %>%
    spread(participant,prop_correct)
  
  ## Effort
  current_speaker_e <- current_speaker %>%
    select(-prop_correct) %>%
    spread(participant,effort)
  #view(current_speaker_effort)
  
  # ICC
  #   Effort
  current_icc_e <- current_speaker_e %>%
    select(3:length(current_speaker_e)) %>%
    psych::ICC(lmer=TRUE) # convergence ok so use lmer
  
  current_icc_e <- current_icc_e$results %>%
    mutate(speaker = spid) %>%
    filter(type=="ICC3k")
  
  df_icc_e <- rbind(df_icc_e, current_icc_e)
  
  #   Intell
  current_icc_i <- current_speaker_i %>%
    select(3:length(current_speaker_i)) %>%
    psych::ICC(lmer=TRUE) # convergence ok so use lmer
  
  current_icc_i <- current_icc_i$results %>%
    mutate(speaker = spid) %>%
    filter(type=="ICC3k")
  
  df_icc_i <- rbind(df_icc_i, current_icc_i)
  
}
  
df_kappa_inter_acc <- df_kappa
df_icc_inter_eff <- df_icc_e
df_icc_inter_acc <- df_icc_i

mean(df_icc_inter_acc$ICC) # .86, good
mean(df_icc_inter_eff$ICC) # .87, good

# FOR OSF: UP TO HERE IS OK ----

# Add acoustics ----
df_ac <- read.csv("simPD_data_cleaned.csv")

df_ac <- df_ac %>%
  # Create shortened filename to merge with main df by removing channel info
  mutate(file_short = str_sub(filename, start = 1, end = -5))
df_ac %>% select(filename, file_short) %>% head()

# Fix OC16 clear speech files which were mislabelled
df_ac <- df_ac %>%
  mutate(file_short = str_replace_all(file_short, 
                                      "oc16_clear_nm_h1",
                                      "oc16_clear_sm_h1")) %>%
  mutate(file_short = str_replace_all(file_short, 
                                      "oc16_clear_kn_h17",
                                      "oc16_clear_nm_h17")) %>%
  mutate(file_short = str_replace_all(file_short, 
                                      "oc16_clear_kn_h13",
                                      "oc16_clear_nm_h17"))

df_ac %>% select(filename, file_short) %>% head()

df <- df %>%
  mutate(file_short = str_sub(file, start = 1, end = -26))# %>% 
df %>% select(file, file_short) %>% head()

nrow(df_ac)
nrow(df)

df_ac_short <- df_ac %>%
  # remove redundant/unnecessary columns from acoustics df
  select(-X, -participant, -exp, -cond_speech, -cond_mask, -list, -utterance, -gender, -group, -HvCL, -CvL, -NMvM, -SMvKN)

# Merge df with acoustic data (and drop duplicate rows)
df <- left_join(df, df_ac_short, by = "file_short") %>% distinct


# Models ----
# .. Contrasts ----
# Set up contrasts for:
#   - group (pd, oc)
#   - cond_mask (nm, kn)
#   - cond_speech (habitual, clear, loud)
# Factor (but no contrasts needed):
#   - participant (listener ids)
#   - id (list_sentence)

unique(df$group)
unique(df$cond_mask)
unique(df$cond_speech)

df <- df %>%
  mutate(participant = factor(participant),
         id = factor(id),
         group = factor(group,
                        levels = c("oc","pd")),
         cond_mask = factor(cond_mask,
                            levels = c("nm","kn")),
         cond_speech = factor(cond_speech,
                              levels = c("habitual","clear","loud"))) %>%
  mutate(prop_effort = effort/100) %>% # make effort a proportion
  mutate(logit_intell = car::logit(prop_correct),
         logit_effort = car::logit(effort))


# CONTRASTS: set so that left comparison = ++, right comparison = --
# Group
levels(df$group) # oc, pd
contrasts(df$group) <- contr.sum(2) # oc = 1, pd = -1
solve(cbind(1,contrasts(df$group)))
colnames(contrasts(df$group)) <- c("OC vs PD")

# Mask
unique(df$cond_mask)
contrasts(df$cond_mask) <- contr.sum(2) # nm = 1, kn = -1
solve(cbind(1,contrasts(df$cond_mask)))
colnames(contrasts(df$cond_mask)) <- c("NM vs KN")

# Speech
# reverse helmert contrasts
unique(df$cond_speech)
contrasts(df$cond_speech) <- matrix(c(2/3, -1/3, -1/3,
                                        0, 1/2, -1/2),
                                      ncol = 2)
solve(cbind(1,contrasts(df$cond_speech)))
colnames(contrasts(df$cond_speech)) <- c("Habit vs Clear/Loud", "Clear vs Loud")

# ..Slopes ----
contrasts(df$group)
model.matrix(~group, df) %>% head()
df$OCvPD <- model.matrix(~group, df)[,2]

contrasts(df$cond_mask)
model.matrix(~cond_mask, df) %>% head()
df$NMvKN <- model.matrix(~cond_mask, df)[,2]

contrasts(df$cond_speech)
model.matrix(~cond_speech, df) %>% head()
df$HvCL <- model.matrix(~cond_speech, df)[,2]
df$CvL <- model.matrix(~cond_speech, df)[,3]



# .. Model ----
# Fixed effects: group, mask, speech, and all possible interactions
# Random effects: mask, speech by participant and by speaker was singular, so only including slopes for speech conds (see syntax below):
#   - by-listener participant slopes for habit vs clear/loud
#   - by-speaker slopes for habit vs clear/loud AND clear vs loud


# note: doing the logit transform in the model has the advantage of being able to back-transform using emmeans, but using the logit transformed data from the df (transform first), allows us to plot CIs in sjPlot::plot_model. 
# The model outcomes are the same, though.


mod_i <- lmerTest::lmer(
  #logit_intell ~ 
  car::logit(prop_correct) ~
                    group*cond_mask*cond_speech +
                    #(1 + (HvCL + CvL) | participant)+ # singular
                    #(1 + HvCL | participant)+ # did not converge
                    #(1 + NMvKN | participant)+ # did not converge
                    (1 | participant)+ # did not converge
                    (1 + (HvCL + CvL) | speaker)+
                    (1|id),
  control = lme4::lmerControl(optimizer ="bobyqa"),
  data=df)


# Inspection: looks good
# qqnorm(resid(mod_i))
# qqline(resid(mod_i)) # normal
# plot(resid(mod_i)) # good

sjPlot::tab_model(mod_i)
car::vif(mod_i)

# PAIRWISE
# What is the speech condition difference within each group and mask condition?
# This shows us that there are larger differences between habitual and clear/loud when PD are not wearing masks
# This also shows us that clear > loud without a mask, but loud > clear with a mask
# THIS is what we want
emmeans::emmeans(mod_i, pairwise ~ cond_speech|cond_mask|group,
                 type = "response", # response doesn't work
                 adjust = "tukey") # tukey adjustments can happen here because we compare > 2 means (3 speech conditions) within each by group
# This is actually what we want anyways, I think --> what is the difference in intelligibility across each speech condition in each mask condition

# What is the group difference within each mask and speech condition?
# This is not what we want because this just confirms there is a group difference across the board, which we already know from our main effect. It could show larger differences between PD and Controls in the masks, but that's not the main finding we are trying to showcase.
emmeans::emmeans(mod_i, pairwise ~ group|cond_mask|cond_speech,
                 type = "response",
                 adjust = "tukey") # tukey adjustments won't happen with all interactions because there are just two means in each by group! see emmeans FAQ

# What is the mask condition difference within each group and speech condition?
# This is NOT of interest; just tells us what we know from the main effect: no mask is more intelligible than KN.
emmeans::emmeans(mod_i, pairwise ~ cond_mask|cond_speech|group,
                 type = "response", # response doesn't work
                 adjust = "tukey")



mod_e <- lmerTest::lmer(
  car::logit(prop_effort) ~
                    group*cond_mask*cond_speech +
                    #(1 + (HvCL + CvL)*(NMvKN) | participant)+ # singular
                    #(1 + (HvCL)*(NMvKN) | participant)+ # did not converge
                    (1 + (HvCL + CvL) | participant)+ # converged
                    (1 + (HvCL + CvL) | speaker)+
                    (1|id),
  control = lme4::lmerControl(optimizer ="bobyqa"),
  data=df)

# Inspection: looks good
# qqnorm(resid(mod_e))
# qqline(resid(mod_e)) # normal
# plot(resid(mod_e)) # good

sjPlot::tab_model(mod_e)
car::vif(mod_e)
emmeans::emmeans(mod_e, pairwise ~ group|cond_mask|cond_speech)

sjPlot::tab_model(mod_i, mod_e)

# sjPlot::plot_model(mod_i, type="pred",
#                    terms = c("group",
#                              "cond_mask",
#                              "cond_speech"))
# 
# sjPlot::plot_model(mod_e, type="pred",
#                    terms = c("group",
#                              "cond_mask",
#                              "cond_speech"))

# This is most helpful!
p_i <- emmeans::emmip(mod_i, group ~ cond_mask|cond_speech,
               CIs = TRUE)+
  theme_bw()+
  ggtitle("Intelligibility")

p_e <- emmeans::emmip(mod_e, group ~ cond_mask|cond_speech,
               CIs = TRUE)+
  theme_bw()+
  ggtitle("Listener effort")

cowplot::plot_grid(p_i,p_e)

# Interactions involving group & masks
## group x mask
emmeans::emmip(mod_i, group ~ cond_mask, CIs = TRUE)
## group x mask x speech
emmeans::emmip(mod_i, group ~ cond_mask|cond_speech, CIs = TRUE)

# ..Coefs ----
coefs_i <- summary(mod_i)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Contrast") %>%
  mutate(Contrast = janitor::make_clean_names(Contrast)) %>%
  rename("t" = "t value",
         "p" = "Pr(>|t|)")

coefs_e <- summary(mod_e)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Contrast") %>%
  mutate(Contrast = janitor::make_clean_names(Contrast)) %>%
  rename("t" = "t value",
         "p" = "Pr(>|t|)")



contrast_terms_cleaned <- c(
  "group_oc_vs_pd" = "Group",
  "cond_mask_nm_vs_kn" = "Mask",
  "cond_speech_habit_vs_clear_loud" = "Speech_HvCL",
  "cond_speech_clear_vs_loud" = "Speech_CvL"
)

coefs_i_cleaned <- coefs_i %>%
  mutate(Contrast = str_replace_all(Contrast, contrast_terms_cleaned)) %>%
  mutate_if(is.numeric, round, 3) 

coefs_e_cleaned <- coefs_e %>%
  mutate(Contrast = str_replace_all(Contrast, contrast_terms_cleaned)) %>%
  mutate_if(is.numeric, round, 3) 

# Uses formatted coefs from sim_perception.R with custom snippets for in-line reporting
intellB <- column_to_rownames(coefs_i_cleaned, "Contrast")[1]

effortB <- column_to_rownames(coefs_e_cleaned, "Contrast")[1]

intellP <- column_to_rownames(coefs_i_cleaned, "Contrast")[5]
effortP <- column_to_rownames(coefs_e_cleaned, "Contrast")[5]



# ..Model output ----
sjPlot::tab_model(mod_i, mod_e, 
                  df.method = "satterthwaite")
# pval output is the same as lmertest if satterthwaite is used


# ..emmeans ----

# .... Mask differences within speech and group ----
# Within each group and speech condition, what are the differences between masks?
# Note: 
#   Odds ratio of >1 here for nm/kn means higher value for nm than kn
#   Odds ratio of <1 would mean lower value. The closer to 1, the more similar they are. The further from 1, the more different.
# KN95 mask consistently lower intelligibility, higher effort than No Mask
# Note: no adjustments made here because technically there is only one comparison within each group

emm_mask_i <- emmeans::emmeans(mod_i, 
                                     pairwise ~ cond_mask|cond_speech|group,
                                 tran = "logit",
                                 type = "response",
                               adjust = "tukey"
                               )

emm_mask_e <- emmeans::emmeans(mod_e, pairwise  ~ cond_mask|cond_speech|group,
                                 tran = "logit",
                                 type = "response",
                               adjust = "bonferroni")

# .... Speech cond differences ----
# Within each group and mask condition, what are the differences between speech styles?
emm_speech_i <- emmeans::emmeans(mod_i, pairwise ~ cond_speech|cond_mask|group,
                 tran = "logit",
                 type = "response",
                 adjust = "bonferroni")
                 # bias.adj = TRUE,
                 # sigma = sigma_i)

emm_speech_e <- emmeans::emmeans(mod_e, pairwise ~ cond_speech|cond_mask|group,
                                 tran = "logit",
                                 type = "response",
                                 adjust = "bonferroni")
                                 # bias.adj = TRUE,
                                 # sigma = sigma_e)
# Make it pretty
emm_speech_i_ft <- emm_speech_i$contrasts %>%
  as.data.frame() %>% 
  select(group,everything(),-df) %>%
  mutate_if(is.numeric,round,3) %>%
  flextable::flextable() %>% bold(~p.value < 0.05,'p.value')

emm_speech_e_ft <- emm_speech_e$contrasts %>%
  as.data.frame() %>% 
  select(group,everything(),-df) %>%
  mutate_if(is.numeric,round,3) %>%
  flextable::flextable() %>% bold(~p.value < 0.05,'p.value')



# Empirical dataviz ----
df <- df %>%
  mutate(hsentence = paste(list,sentenceID.x,sep="_"))


# Effort x Accuracy ----
# averaged over listeners (participant) and individual sentences (by list)
df_grouped <- df %>% 
  group_by(speaker,
           group,
           cond_mask,
           cond_speech,
           list) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

# Empirical means for the 3 way group-mask-speech interaction
df_grouped_conditions <- df_grouped %>%
  group_by(group,cond_mask,cond_speech) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup() %>%
  select(group, cond_mask, cond_speech, prop_correct,effort)

# Empirical means for 2 way group-mask interaction
df_grouped_masks <- df_grouped %>%
  group_by(group,cond_mask) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup() %>%
  select(group, cond_mask, prop_correct,prop_effort)


# 1 point per speaker per utterance per condition
df_grouped_speakers_long <- df %>%
  group_by(
    speaker,
    group,
    cond_mask,
    cond_speech,
    hsentence
  ) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()


# 1 point per speaker per condition
df_grouped_speakers <- df %>%
  group_by(speaker,
           group,
           cond_mask,
           cond_speech) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

corr_ie_all <- rmcorr::rmcorr(participant=speaker,
                              measure1 = prop_correct,
                              measure2 = effort,
                              dataset=df_grouped_speakers)
thear::report_p(corr_ie_all$p)

corr_ie_nm <- rmcorr::rmcorr(participant=speaker,
               measure1 = prop_correct,
               measure2 = effort,
               dataset=subset(df_grouped_speakers, cond_mask=="nm"))
thear::report_p(corr_ie_nm$p)
corr_ie_nm$r
corr_ie_nm$CI
corr_ie_nm$model

corr_ie_kn <- rmcorr::rmcorr(participant=speaker,
               measure1 = prop_correct,
               measure2 = effort,
               dataset=subset(df_grouped_speakers, cond_mask=="kn"))
thear::report_p(corr_ie_kn$p)
corr_ie_kn$r
corr_ie_kn$CI

# Exploratory dataviz
df_grouped_speakers %>%
  ggplot(aes(x = effort,
             y = prop_correct,
             color = group))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(cond_mask~cond_speech)

df_grouped_speakers %>%
  # error in this speaker in loud_nm; remove for now
  filter(!(speaker=="pd11" & cond_speech == "loud")) %>%
  ggplot(aes(x = cond_mask,
             y = prop_correct,
             color = group,
             group = speaker))+
  #geom_boxplot()+
  #ggbeeswarm::geom_quasirandom(width=.25)+
  geom_point()+
  geom_line()+
  facet_grid(~cond_speech)

#-----------Nate's Plot Playground-----------#
# 
# goblin <- c("#FFDAC0", "#C099DD", "#66CC99") #speech condition
# wolverine <- c("#EEEE99", "#6699EE") #mask condition
# spider <- c("#EE9999", "#9999EE") #speaker group
# 
# # Plot Nomenclature: bp: boxplot, sp: scatterplot, rp: raincloud plot, x: by, g: grouped by
# #                    A: accuracy, E: effort, S: speech, M: mask, G: speaker group
# 
# bpAxSMgG <- df_grouped %>%
#   ggplot(aes(x = cond_mask,
#              y = prop_correct,
#              fill = cond_speech)) +
#   geom_boxplot() +
#   facet_wrap(~group,
#              labeller = labeller(group = c(oc = "Control", pd = "Parkinson's"))) +
#   labs(title = "Listener Accuracy by Speech and Mask Conditions Grouped by Speaker Group",
#        x = "Speaker Group",
#        y = "Listener Accuracy",
#        fill = "Speech Condition") +
#   scale_x_discrete(breaks = c("nm", "kn"),
#                    labels = c("No Mask", "KN95")) +
#   scale_fill_manual(values = goblin) +
#   theme_bw()
# 
# spEAxSM <- df_grouped_listeners %>%
#   ggplot(aes(x = effort,
#              y = prop_correct,
#              color = group))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   facet_grid(cond_mask~cond_speech,
#              labeller = labeller(cond_mask = c(nm = "No Mask", kn = "KN95"))) +
#   labs(title = "Listener Accuracy by Perceived Effort",
#        x = "Perceived Effort",
#        y = "Listener Accuracy",
#        color = "Speaker Group") +
#   scale_color_manual(values = spider,
#                      name = "Speaker Group",
#                      breaks = c("oc", "pd"),
#                      labels = c("Control", "Parkinson's")) +
#   theme_bw()
# 
# bpAxSM <- df_grouped %>%
#   ggplot(aes(x = cond_mask,
#              y = prop_correct,
#              fill = cond_mask)) +
#   geom_boxplot() +
#   facet_wrap(~cond_speech,
#              labeller = labeller(cond_speech = c(habitual = "Habitual", clear = "Clear", loud = "Loud"))) +
#   labs(title = "Listener Accuracy by Speech and Mask Conditions",
#        x = "Mask Condition",
#        y = "Listener Accuracy",
#        fill = "Speech Condition") +
#   scale_x_discrete(breaks = c("nm", "kn"),
#                    labels = c("No Mask", "KN95")) +
#   scale_fill_manual(values = wolverine) +
#   theme_bw() +
#   guides(fill = FALSE)
# 
# rpAxSMgG <- df_grouped %>% # INCOMPLETE
#   ggplot(aes(x = cond_mask,
#              y = prop_correct,
#              fill = cond_speech)) +
#   ggdist::stat_halfeye(adjust = .5,
#                        width = .6,
#                        justification = -.2,
#                        .width = 0,
#                        point_color = NA) +
#   geom_boxplot(width = .12,
#                outlier.color = NA) +
#   gghalves::geom_half_point(side = "l",
#                             range_scale = .4,
#                             alpha = .3) +
#   coord_cartesian(xlim = c(1.2, NA), clip = "off") +
#   facet_wrap(~group,
#              labeller = labeller(group = c(oc = "Control", pd = "Parkinson's"))) +
#   labs(title = "Listener Accuracy by Speech and Mask Conditions Grouped by Speaker Group",
#        x = "Speaker Group",
#        y = "Listener Accuracy",
#        fill = "Speech Condition") +
#   scale_x_discrete(breaks = c("nm", "kn"),
#                    labels = c("No Mask", "KN95")) +
#   scale_fill_manual(values = goblin) +
#   theme_bw()
# 
# rpSMgGC <- df_grouped %>% 
#   filter(group == "oc") %>%
#   #filter(cond_mask == "nm") %>%
#   ggplot(aes(x = cond_speech,
#              y = prop_correct,
#              fill = cond_speech)) +
#   ggdist::stat_halfeye(adjust = .5,
#                        width = .6,
#                        justification = -.2,
#                        .width = 0,
#                        point_color = NA) +
#   geom_boxplot(width = .12,
#                outlier.color = NA) +
#   gghalves::geom_half_point(side = "l",
#                             range_scale = .4,
#                             alpha = .3) +
#   coord_cartesian(xlim = c(1.2, NA), clip = "off") +
#   facet_wrap(~cond_mask,
#              labeller = labeller(cond_mask = c(nm = "No Mask", kn = "KN95"))) +
#   labs(title = "Listener Accuracy by Speech and Mask Conditions for the Control Group",
#        x = "Mask Condition",
#        y = "Listener Accuracy",
#        fill = "Speech Condition") +
#   scale_fill_manual(values = goblin) +
#   theme_bw()
# 
# rpSMgGP <- df_grouped %>% 
#   filter(group == "pd") %>%
#   #filter(cond_mask == "nm") %>%
#   ggplot(aes(x = cond_speech,
#              y = prop_correct,
#              fill = cond_speech)) +
#   ggdist::stat_halfeye(adjust = .5,
#                        width = .6,
#                        justification = -.2,
#                        .width = 0,
#                        point_color = NA) +
#   geom_boxplot(width = .12,
#                outlier.color = NA) +
#   gghalves::geom_half_point(side = "l",
#                             range_scale = .4,
#                             alpha = .3) +
#   coord_cartesian(xlim = c(1.2, NA), clip = "off") +
#   facet_wrap(~cond_mask,
#              labeller = labeller(cond_mask = c(nm = "No Mask", kn = "KN95"))) +
#   labs(title = "Listener Accuracy by Speech and Mask Conditions for the Parkinson's Group",
#        x = "Mask Condition",
#        y = "Listener Accuracy",
#        fill = "Speech Condition") +
#   scale_fill_manual(values = goblin) +
#   theme_bw()
# 
# plot_grid(bpAxSMgG, bpAxSM, spEAxSM)
# plot_grid(rpSMgGC, rpSMgGP) #best way to organize this info?
# 
# #save.image("analysis/sim_perception_2022-05-05.RData")



# Acoustics ----

# Exploratory correlations
# All correlations are pretty weak
rmcorr::rmcorr(participant=speaker,
               measure1 = prop_correct,
               measure2 = mid_1_3k,
               dataset=df)
rmcorr::rmcorr(participant=speaker,
               measure1 = prop_correct,
               measure2 = tilt,
               dataset=df)
rmcorr::rmcorr(participant=speaker,
               measure1 = prop_correct,
               measure2 = int_corrected,
               dataset=df)



# Model acoustics ----
# Original models, update with acoustic data
# Update 2022-11-30: models not converging, so redo with random intercepts only
# Update 2022-12-20: models now converging because used bobyqa optimizer in original models
#   - intercepts only model not required
# mod_i_intercepts <- lmerTest::lmer(
#   car::logit(prop_correct) ~
#     group*cond_mask*cond_speech +
#     (1 | participant)+
#     (1 | speaker)+
#     (1|id),
#   data=df)
# 
# mod_e_intercepts <- lmerTest::lmer(
#   car::logit(prop_effort) ~
#     group*cond_mask*cond_speech +
#     (1 | participant)+
#     (1 | speaker)+
#     (1|id),
#   data=df)
# 
# mod_i_ac_mid <- update(mod_i_intercepts, . ~ . + mid_1_3k)
# mod_i_ac_tilt <- update(mod_i_intercepts, . ~ . + tilt)
# mod_i_ac_int <- update(mod_i_intercepts, . ~ . + int_corrected)

# i = intelligibility; a = acoustics
mod_i_a_mid <- update(mod_i, . ~ . + mid_1_3k)
mod_i_a_tilt <- update(mod_i, . ~ . + tilt)
mod_i_a_int <- update(mod_i, . ~ . + int_corrected)



# Model improvement
# Tilt: improves fit at p < 0.001; AIC = 44499, BIC 44660 # tilt has lowest AIC and BIC
lrt_tilt <- anova(mod_i, mod_i_a_tilt) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  mutate_if(is.numeric, round, 3)

# Intensity: improves fit at p < 0.001; AIC = 44520, BIC 44681
lrt_int <- anova(mod_i, mod_i_a_int) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  mutate_if(is.numeric, round, 3) 

# Mid: improves fit at p=0.02 but not at our more conservative threshold of p<0.01; AIC = 44530, BIC 44691
lrt_mid <- anova(mod_i, mod_i_a_mid) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  mutate_if(is.numeric, round, 3) 

# Tilt model has lowest AIC and BIC; now build up based on that
#   Adding intensity: int improves over tilt alone; AIC = 44483, BIC = 44652 # lowest AIC/BIC
mod_i_a_tilt_int <- update(mod_i_a_tilt, . ~ . + int_corrected)
lrt_tilt_int <- anova(mod_i_a_tilt, mod_i_a_tilt_int) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  mutate_if(is.numeric, round, 3)


# Final model includes tilt and int
mod_i_a <- mod_i_a_tilt_int
sjPlot::tab_model(mod_i_a) # flatter tilt = better intell; but higher intensity = worse intell
sjPlot::plot_model(mod_i_a_tilt_int, type = "pred", terms = c("int_corrected"))
sjPlot::plot_model(mod_i_a_tilt_int, type = "pred", terms = c("tilt"))

# Assumptions & VIF
# qqnorm(resid(mod_i_a_tilt_int))
# qqline(resid(mod_i_a_tilt_int)) # normal
# plot(resid(mod_i_a_tilt_int)) # good

# Is GVIF (= VIF = squared df adjusted GVIF) < 5 for all predictors?
#   - Yes (<2 for all)
car::vif(mod_i_a) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  mutate(vif = gvif_1_2_df^2)

rmcorr::rmcorr(participant=speaker,
               measure1 = tilt,
               measure2 = int_corrected,
               dataset=df)

# ..Coefs ----
coefs_i_a <- summary(mod_i_a_tilt_int)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Contrast") %>%
  mutate(Contrast = janitor::make_clean_names(Contrast)) %>%
  rename("t" = "t value",
         "p" = "Pr(>|t|)")

contrast_terms_cleaned_i_a <- c(
  "group_oc_vs_pd" = "Group",
  "cond_mask_nm_vs_kn" = "Mask",
  "cond_speech_habit_vs_clear_loud" = "Speech_HvCL",
  "cond_speech_clear_vs_loud" = "Speech_CvL",
  "int_corrected" = "intensity",
  "tilt" = "tilt"
)

coefs_i_a <- coefs_i_a %>%
  mutate(Contrast = str_replace_all(Contrast, contrast_terms_cleaned_i_a)) %>%
  mutate_if(is.numeric, round, 3) %>%
  column_to_rownames("Contrast")
