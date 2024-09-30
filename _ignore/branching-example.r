p_stage1 = 0.05
p_stage2 = 0.03
p_stage12 = 0.10
p_stage21 = 0.10
p_death = 0.03


# Approach 1
p_healthy = 1 - p_death - p_stage1 - p_stage2
p_h_s1_ = p_stage1; p_h_s1_
p_h_s2_ = p_stage2; p_h_s2_

probs_1 <- c(p_h_s1_, p_h_s2_)

# approach 2
p_survive = 1 - p_death
p_progress = (p_stage1 + p_stage2) / (1 - p_death); p_progress
p_h_s1 = p_survive * p_progress * (p_stage1 / (p_stage1 + p_stage2)); p_h_s1
p_h_s2 = p_survive * p_progress * (p_stage2 / (p_stage1 + p_stage2)); p_h_s2

probs_2 <- c(p_h_s1, p_h_s2)
  
r_stage1 =  -log(1-p_stage1); r_stage1
r_stage2 =  -log(1-p_stage2); r_stage2
r_stage12 =  -log(1-p_stage12); r_stage12
r_stage21 =  -log(1-p_stage21); r_stage21
r_death =  -log(1-p_death); r_death

rp <- function(r) 1-exp(-r)

# Approach 1
pr_healthy = 1 - rp(r_death) - rp(r_stage1) - rp(r_stage2); pr_healthy
pr_h_s1_ = rp(r_stage1); pr_h_s1_
pr_h_s2_ = rp(r_stage2); pr_h_s2_

rates_1 <- c(pr_h_s1_, pr_h_s2_); rates_1

# approach 2
pr_survive = 1 - rp(r_death); pr_survive
pr_progress = rp(r_stage1 + r_stage2) / (1 - rp(r_death)); pr_progress
pr_h_s1 = pr_survive * pr_progress * (rp(r_stage1) / (rp(r_stage1) + rp(r_stage2))); pr_h_s1
pr_h_s2 = pr_survive * pr_progress * (rp(r_stage2) / (rp(r_stage1) + rp(r_stage2))); pr_h_s2

rates_2 = c(pr_h_s1, pr_h_s1); rates_2


rbind(probs_1,probs_2,rates_1,rates_2)


