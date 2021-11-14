library(tidyverse)
idbc <- FUDBAccess::FU_db_connect()$IPEDS

get_grad_time <- function(idbc,fname){
  tbl(idbc,fname) %>%
    filter(GRTYPE %in% c(8,13,14,15)) %>% # 2 is cohort size, 3 is grads in 150%
    select(UNITID,
           GRTYPE,
           N =     GRTOTLT) %>% 
    collect() %>%
    spread(GRTYPE, N, sep = "_") %>%
    mutate(Cohort   = GRTYPE_8,
           Grad4    = GRTYPE_13 / GRTYPE_8,
           Grad5    = GRTYPE_14 / GRTYPE_8,
           Grad6    = GRTYPE_15 / GRTYPE_8,
           GradTime = (Grad4*3.75 + Grad5*4.75 + Grad6*5.75)/(Grad4 + Grad5 + Grad6)) %>% 
    na.omit()
}

nongrad_career_lag <- 10 # years from start of college

grad_time <- rbind( get_grad_time(idbc,"gr2019"),
                    get_grad_time(idbc,"gr2018"),
                    get_grad_time(idbc,"gr2017")) %>% 
             group_by(UNITID) %>% 
             summarize(Grad4 = weighted.mean(Grad4, Cohort),
                       Grad5 = weighted.mean(Grad5, Cohort),
                       Grad6 = weighted.mean(Grad6, Cohort),
                       GradTime = weighted.mean(GradTime, Cohort)) %>% 
             mutate(CareerLag = (Grad4*3.75 + Grad5*4.75 + Grad6*5.75 + (1-Grad4-Grad5-Grad6)*nongrad_career_lag))

# can we estimate GradTime from four year rate?
m1 <- lm(GradTime ~ poly(Grad4,3), grad_time)
summary(m1) # R2 = .81

# create a nice graph with interpolant
custom_colors <- c("St. John's College" = "orange", 
                   "Youngstown State U" = "red", 
                   "Other"              = "#99999988")

custom_sizes  <- c("St. John's College" = 3, 
                   "Youngstown State U" = 3, 
                   "Other"              = 1)

grad_time %>% 
  mutate(Institution = case_when(
    UNITID == 163976 ~ "St. John's College",
    UNITID == 206695 ~ "Youngstown State U",
    TRUE ~ "Other"
  )) %>% 
  ggplot(aes(x = Grad4, y = GradTime,  size = Institution, color = Institution)) +
  geom_point() +
  scale_colour_manual(values = custom_colors) +
  scale_size_manual(values = custom_sizes) +
  geom_smooth(se = FALSE, color = "black", method = "lm", formula = y ~ poly(x,3)) +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Four Year Graduation Rate") +
  ylab("Avg. Time to Graduation") +
  geom_hline(yintercept = 3.75, color = "red", linetype = "dashed") +
  guides(color = guide_legend(order = 1), 
           size = "none")


############### Career Lag #####################


m2 <- lm(CareerLag~ poly(Grad4,3), grad_time)
summary(m2) # R2 = .91

grad_time %>% 
  mutate(Institution = case_when(
    UNITID == 163976 ~ "St. John's College",
    UNITID == 206695 ~ "Youngstown State U",
    TRUE ~ "Other"
  )) %>% 
  ggplot(aes(x = Grad4, y = CareerLag,  size = Institution, color = Institution)) +
  geom_point() +
  scale_colour_manual(values = custom_colors) +
  scale_size_manual(values = custom_sizes) +
  geom_smooth(se = FALSE, color = "black", method = "lm", formula = y ~ poly(x,3)) +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Four Year Graduation Rate") +
  ylab("Career Lag") +
  geom_hline(yintercept = 3.75, color = "red", linetype = "dashed") +
  guides(color = guide_legend(order = 1), 
         size = "none")
