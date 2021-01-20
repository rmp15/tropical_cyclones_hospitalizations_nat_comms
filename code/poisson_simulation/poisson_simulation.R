# RUN SIMULATION FOR POISSON

# This script will generate 10,000 simulations for each distribution of Poisson values and calculate the relative risk

# Hurricanes will either cause an effect or have a null effect
# Hurricanes control days will either be included or not

# set denom
denom = 100

# set number of simulations and the relative rates of harm
nsims = 10000
rate_harm = 0.2 * denom
rate_noharm = 0.1 * denom
rate_protect = 0.05 * denom

# calculate true relative risks
RR_true_harm = data.frame(effect='Harm',RR=rate_harm/rate_noharm)
RR_true_noharm = data.frame(effect='Null',RR=rate_noharm/rate_noharm)
RR_true_protect = data.frame(effect='Protect',RR=rate_protect/rate_noharm)

RR_true = rbind(RR_true_harm,RR_true_noharm,RR_true_protect)

# 1. Harmful effect of hurricanes
set.seed(1234) ; nohurr_ctrl = rpois(nsims,rate_noharm)
set.seed(5678) ; hurr = rpois(nsims,rate_harm)
set.seed(9123) ; nohurr_ctrl_2 = rpois(nsims,rate_noharm)
set.seed(4567) ; hurr_ctrl = rpois(nsims,rate_harm)

# a. include hurricane control days
hurr_cases = hurr + hurr_ctrl
no_hurr_cases = nohurr_ctrl + nohurr_ctrl_2

hurricanes_total_denom = no_hurricanes_total_denom = denom + denom

RR_harm_hurr_control = data.frame(effect='Harm',hurr_control='Y',RR=((hurr_cases/hurricanes_total_denom) / (no_hurr_cases/hurricanes_total_denom)))
RR_harm_hurr_control_mean = data.frame(effect='Harm',hurr_control='Y',RR_mean=mean(RR_harm_hurr_control$RR))
# b. do not include hurricane control days

hurr_cases = hurr
no_hurr_cases = nohurr_ctrl + nohurr_ctrl_2

hurricanes_total_denom = denom
no_hurricanes_total_denom = denom + denom

RR_harm_no_hurr_control = data.frame(effect='Harm',hurr_control='N',RR=((hurr_cases/hurricanes_total_denom) / (no_hurr_cases/hurricanes_total_denom)))
RR_harm_no_hurr_control_mean = data.frame(effect='Harm',hurr_control='N',RR_mean=mean(RR_harm_no_hurr_control$RR))


# 2. null effect of hurricanes
set.seed(1234) ; nohurr_ctrl = rpois(nsims,rate_noharm)
set.seed(5678) ; hurr = rpois(nsims,rate_noharm)
set.seed(9123) ; nohurr_ctrl_2 = rpois(nsims,rate_noharm)
set.seed(4567) ; hurr_ctrl = rpois(nsims,rate_noharm)

# a. include hurricane control days
hurr_cases = hurr + hurr_ctrl
no_hurr_cases = nohurr_ctrl + nohurr_ctrl_2

hurricanes_total_denom = no_hurricanes_total_denom = denom + denom

RR_no_harm_hurr_control = data.frame(effect='Null',hurr_control='Y',RR=(hurr_cases/hurricanes_total_denom) / (no_hurr_cases/hurricanes_total_denom))
RR_no_harm_hurr_control_mean = data.frame(effect='Null',hurr_control='Y',RR_mean=mean(RR_no_harm_hurr_control$RR))

# b. do not include hurricane control days

hurr_cases = hurr
no_hurr_cases = nohurr_ctrl + nohurr_ctrl_2

hurricanes_total_denom = denom
no_hurricanes_total_denom = denom + denom

RR_no_harm_no_hurr_control = data.frame(effect='Null',hurr_control='N',RR=(hurr_cases/hurricanes_total_denom) / (no_hurr_cases/hurricanes_total_denom))
RR_no_harm_no_hurr_control_mean = data.frame(effect='Null',hurr_control='N',RR_mean=mean(RR_no_harm_no_hurr_control$RR))

# 3. protective effect of hurricanes
set.seed(1234) ; nohurr_ctrl = rpois(nsims,rate_noharm)
set.seed(5678) ; hurr = rpois(nsims,rate_protect)
set.seed(9123) ; nohurr_ctrl_2 = rpois(nsims,rate_noharm)
set.seed(4567) ; hurr_ctrl = rpois(nsims,rate_protect)

# a. include hurricane control days
hurr_cases = hurr + hurr_ctrl
no_hurr_cases = nohurr_ctrl + nohurr_ctrl_2

hurricanes_total_denom = no_hurricanes_total_denom = denom + denom

RR_protect_hurr_control = data.frame(effect='Protect',hurr_control='Y',RR=(hurr_cases/hurricanes_total_denom) / (no_hurr_cases/hurricanes_total_denom))
RR_protect_hurr_control_mean = data.frame(effect='Protect',hurr_control='Y',RR_mean=mean(RR_protect_hurr_control$RR))

# b. do not include hurricane control days

hurr_cases = hurr
no_hurr_cases = nohurr_ctrl + nohurr_ctrl_2

hurricanes_total_denom = denom
no_hurricanes_total_denom = denom + denom

RR_protect_no_hurr_control = data.frame(effect='Protect',hurr_control='N',RR=(hurr_cases/hurricanes_total_denom) / (no_hurr_cases/hurricanes_total_denom))
RR_protect_no_hurr_control_mean = data.frame(effect='Protect',hurr_control='N',RR_mean=mean(RR_protect_no_hurr_control$RR))


RR_data_frame = rbind(RR_harm_hurr_control,RR_harm_no_hurr_control,RR_no_harm_hurr_control,RR_no_harm_no_hurr_control,
                        RR_protect_hurr_control,RR_protect_no_hurr_control)

RR_estimated = rbind(RR_harm_hurr_control_mean,RR_harm_no_hurr_control_mean,RR_no_harm_hurr_control_mean,RR_no_harm_no_hurr_control_mean,RR_protect_hurr_control_mean,RR_protect_no_hurr_control_mean)

# PLOTS
library(ggplot2)

dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/poisson_simulation/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

pdf(paste0(dir.output,'density_plots.pdf'),paper='a4r',width=0,height=0)
ggplot() +
    geom_density(data=RR_data_frame,aes(x=RR,color=hurr_control)) +
    geom_vline(data=RR_true,aes(xintercept=RR)) +
    geom_vline(data=RR_estimated,aes(xintercept=RR_mean,color=hurr_control)) +
    facet_wrap(~effect) +

    theme_bw() + theme(text = element_text(size = 10),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()







