################################################################################
#
# OPENECO-UNIFIED model — pure post-2017 simulation in R
# Simplified single-household variant of Carnevali, Deleidi, Pariboni,
# Veronese Passarella (2021).
#
# Simplifications applied vs. paper:
#   * Unified household (no worker/capitalist split)
#   * Cross-border equity dropped (HH hold only own-country equity)
#   * Option B: all firm profit not retained -> dividends (no F^m residual)
#   * Cross-border dividend flows zeroed
#   * GL closure and FIXED - GL is a deterministic closure. Need to move to something else. Gold reserves with exogenous growth rate? 
#   * Cash + deposits structure preserved as in the paper
#
# To run Scenario 5 (Greenland-only green MOIS from 2025): set run_s5 = TRUE
#
################################################################################

# 1) CLEAR ALL ------------------------------------------------------------------
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

################################################################################
# 2) TIME SPAN
################################################################################

nPeriods <- 44

run_s5    <- FALSE
s5_start  <- 9     # period 9 = 2025
s5_uplift <- 0.5

################################################################################
# 3) ALLOCATE MODEL VARIABLES
################################################################################

vnames <- c(
  "yd_brown","yd_green",
  "yd_hs_brown","yd_hs_green",
  "y_h_brown","y_h_green",
  "v_brown","v_green",
  "cg_b_brown","cg_e_brown","cg_b_green","cg_e_green",
  "t_brown","t_green",
  "cons","cons_brown","cons_green",
  "y_brown","y_green","y_w_brown","y_w_green",
  "f_brown","fu_brown","fd_brown","f_m_brown",
  "f_green","fu_green","fd_green","f_m_green",
  "f_d_brownbrown","f_d_browngreen","f_d_greenbrown","f_d_greengreen",
  "f_bank_brown","f_bank_green",
  "k_brown","k_gr_brown","k_con_brown",
  "k_green","k_gr_green","k_con_green",
  "da_brown","da_gr_brown","da_con_brown",
  "da_green","da_gr_green","da_con_green",
  "af_brown","af_green","inv_brown","inv_green",
  "inv_gr_brown","inv_gr_green","inv_gr_brown_t","inv_gr_green_t",
  "inv_con_brown","inv_con_green","l_firm_brown","l_firm_green",
  "x_brown","im_brown","x_green","im_green","tb_brown","tb_green",
  "b_brownbrown_d","b_brownbrown_s","b_browngreen_d","b_browngreen_s",
  "b_greenbrown_d","b_greenbrown_s","b_greengreen_d","b_greengreen_s",
  "b_brown_s","b_green_s","b_cb_brownbrown_s","b_cb_greengreen_s",
  "b_brown_bank","b_green_bank","b_brown_bank_not","b_green_bank_not",
  "z_brown","z_green",
  "e_brownbrown_d","e_brownbrown_s","e_browngreen_d","e_browngreen_s",
  "e_greenbrown_d","e_greenbrown_s","e_greengreen_d","e_greengreen_s",
  "e_brown_real_s","e_green_real_s","p_e_brown","p_e_green",
  "r_e_brown","r_e_brown_t","r_e_green","r_e_green_t",
  "h_brown_h","h_brown_s",
  "h_green_h","h_green_s",
  "dep_brown","dep_bank_brown",
  "dep_green","dep_bank_green",
  "l_s_brown","l_s_green","a_d_brown","a_s_brown","a_d_green","a_s_green",
  "gov_tot_brown","gov_tot_green","gov_con_brown","gov_con_green",
  "gov_gr_brown","gov_gr_green","f_cb_brown","f_cb_green",
  "psbr_brown","psbr_green","nafa_brown","nafa_green",
  "cab_brown","cab_green","kabp_brown","kabp_green","bp_brown","bp_green",
  "gnp_brown","gnp_green","xr_brown","xr_green",
  "or_brown","or_green",
  "y_mat_brown","y_mat_green","mat_brown","mat_green",
  "rec_brown","rec_green","dis_brown","dis_green",
  "dc_brown","dc_green","k_se_brown","k_se_green","wa_brown","wa_green",
  "k_m_brown","k_m_green","k_m","conv_m_brown","conv_m_green",
  "res_m_brown","res_m_green","res_m",
  "cen_brown","cen_green","o2_brown","o2_green",
  "e_brown","e_green","er_brown","er_green","en_brown","en_green",
  "ed_brown","ed_green",
  "k_e_brown","k_e_green","k_e","conv_e_brown","conv_e_green",
  "res_e_brown","res_e_green","res_e",
  "emis_brown","emis_green","emis_l","emis",
  "co2_at","co2_up","co2_lo","f","f_ex","temp_at","temp_lo",
  "mu_brown","mu_green","epsilon_brown","epsilon_green",
  "beta_brown","beta_green","eta_brown","eta_green",
  "depl_m_brown","depl_m_green","depl_e_brown","depl_e_green",
  "d_t_brown","d_t_green","delta_brown","delta_green",
  "q_brown","q_green","lev_f_brown","lev_f_green",
  "per_brown","per_green","liq_b_brown","liq_b_green",
  "y","inv","gov","yd","k","v"
)
for (vn in vnames) assign(vn, numeric(nPeriods))

################################################################################
# 4) PARAMETERS
################################################################################

omega_brown    <- 0.62      ; omega_green    <- 0.62
alpha1_brown   <- 0.676919  ; alpha1_green   <- 0.676919
alpha2_brown   <- 0.020834  ; alpha2_green   <- 0.020834

gamma0_brown <- 0.093272 - 0.05 ; gamma0_green <- 0.093272 - 0.05
gamma1_brown <- 1.008213        ; gamma1_green <- 1.008213
gamma2_brown <- 0.005           ; gamma2_green <- 0.005

chi1_brown <- 0.2  ; chi1_green <- 0.2
chi2_brown <- 0.02 ; chi2_green <- 0.02
chi3_brown <- 30   ; chi3_green <- 30

# --- Rate-channel for green-investment target (Option B: interest savings as flow) ---
# saving = (r_l_con - r_l_green) * L_green_outstanding
# When r_l_green = r_l_con (baseline), saving = 0 (no rate effect).
# When MDB lowers r_l_green, saving is positive and pushes green investment.
# chi4 has same units and default value as chi1 (T USD push per T USD of saving).
chi4_brown      <- 0.2 ; chi4_green      <- 0.2
mdb_subsidy_br  <- 0   ; mdb_subsidy_gn  <- 0   # subtracted from r_l_green
xi_sub_brown    <- 1   ; xi_sub_green    <- 1   # additional multiplier on r_l_green

ret_brown   <- 0.02   ; ret_green   <- 0.02
pi_dy_brown <- 0.00555; pi_dy_green <- 0.00555
xi_brown    <- 0.01   ; xi_green    <- 0.01

eps0 <- -2.1 ; eps1 <- 0.5 ; eps2 <- 1.228
mu0  <- -2.1 ; mu1  <- 0.5 ; mu2_par  <- 1.228

# --- Endogenous trade elasticities (Souza & Silva / structuralist channel) ---
# Trade income elasticities depend on the green capital share of total capital
# (proxy for technological sophistication / industrialization):
#   eps_x_b = zeta0 + zeta1 * share_gr_brown   (export elasticity rises with K_gr)
#   eta_m_b = phi0_m - phi1_m * share_gr_brown (import elasticity falls with K_gr)
# Set use_endog_elast = FALSE to recover the constant-elasticity baseline.
use_endog_elast <- TRUE
zeta0_par   <- 0.70   ; zeta1_par   <- 0.60    # exports: weak at low share_gr
phi0_m_par  <- 1.30   ; phi1_m_par  <- 0.50    # imports: strong at low share_gr

# --- Confidence channel: foreign demand for Br bills responds to fundamentals ---
# lambda50_effective = lambda50 * confidence_br
# confidence_br = exp( kappa_tb * tb_brown/y_brown + kappa_res * (or_brown/or_target - 1) )
# When trade deteriorates or reserves fall below target, confidence drops,
# reducing foreign demand for Br bills. Set use_confidence = FALSE to disable.
use_confidence <- TRUE
kappa_tb       <- 5      # sensitivity to trade-balance/GDP ratio
kappa_res      <- 0.5    # sensitivity to reserves vs target
or_target_br   <- 50     # reserve target (matches or_init)

lambda10 <- 0.14707 ; lambda11 <- 1 ; lambda12 <- 1 ; lambda13 <- 0    ; lambda14 <- 0
lambda20 <- 0.04902 ; lambda21 <- 1 ; lambda22 <- 1 ; lambda23 <- 0    ; lambda24 <- 0
lambda40 <- 0.14707 ; lambda41 <- 1 ; lambda42 <- 1 ; lambda43 <- 0    ; lambda44 <- 0
lambda50 <- 0.04902 ; lambda51 <- 1 ; lambda52 <- 1 ; lambda53 <- 0    ; lambda54 <- 0
lambda70 <- 0.02451 ; lambda71 <- 0 ; lambda72 <- 0 ; lambda73 <- 0.01 ; lambda74 <- 0.01
lambda80 <- 0.02451 ; lambda81 <- 0 ; lambda82 <- 0 ; lambda83 <- 0.01 ; lambda84 <- 0.01
lambda90 <- 0.02451 ; lambda91 <- 0 ; lambda92 <- 0 ; lambda93 <- 0.01 ; lambda94 <- 0.01
lambda100<- 0.02451 ; lambda101<- 0 ; lambda102<- 0 ; lambda103<- 0.01 ; lambda104<- 0.01

# Bank-deposit share of HH liquid wealth (rest is cash held by HH)
depsh_brown <- 0.7  ; depsh_green <- 0.7

r_brown     <- 0.03 ; r_green     <- 0.03
r_l_brown   <- 0.035; r_l_green   <- 0.035

theta_brown  <- 0.144193    ; theta_green  <- 0.144193
delta0_brown <- 0.100609    ; delta0_green <- 0.100609
ad_k_brown   <- 0.75        ; ad_k_green   <- 0.75

mu_gr_brown   <- 0.71  ; mu_gr_green   <- 0.51
mu_con_brown  <- 0.86  ; mu_con_green  <- 0.66
eps_gr_brown  <- 7.65  ; eps_gr_green  <- 5.65
eps_con_brown <- 9.32  ; eps_con_green <- 7.32
beta_gr_brown  <- 0.048154 - 0.01
beta_gr_green  <- 0.048154 - 0.02
beta_con_brown <- 0.048154 + 0.01
beta_con_green <- 0.048154
eta_con_brown <- 0     ; eta_con_green <- 0.05
eta_gr_brown  <- 0.075 ; eta_gr_green  <- 0.15

rho_brown     <- 0.2     ; rho_green     <- 0.28
sigma_m_brown <- 0.00034 ; sigma_m_green <- 0.00034
sigma_e_brown <- 0.00177 ; sigma_e_green <- 0.00177
zeta_brown    <- 0.015   ; zeta_green    <- 0.015
car           <- 3.67

co2_at_pre <- 2156.2
co2_up_pre <- 4950.5
co2_lo_pre <- 36670
phi11 <- 0.9817 ; phi12 <- 0.0183 ; phi21 <- 0.0080
phi22 <- 0.9915 ; phi23 <- 0.0005 ; phi32 <- 0.0001 ; phi33 <- 0.9999
f2   <- 3.8
sens <- 3
t1   <- 0.027
t2   <- 0.018
t3   <- 0.005
fex_incr <- 0.005

dam1_brown <- 0 ; dam2_brown <- 0.00284 ; dam3_brown <- 5e-6 ; dam4_brown <- 6.6754
dam1_green <- 0 ; dam2_green <- 0.00284 ; dam3_green <- 5e-6 ; dam4_green <- 6.6754
ad_exp <- 0.05 ; ad_im <- 0.05

g_land <- 0.044

g_beta_brown <- 0.02
g_beta_green <- 0.04
greening_starts <- 4
beta0_base <- 6.904820 / 2

xr_relax <- 0.15
nIter <- 150

# --- FX closure -----------------------------------------------------------
# "GL"    = Godley-Lavoie floating; xr clears Br bill market every period.
# "FIXED" = peg xr_green = 1; FX reserves (or_brown, or_green) absorb BP.
fx_closure <- "FIXED"
or_init    <- 50    # initial reserves in each area (under FIXED)

################################################################################
# 5) INITIAL CONDITIONS (period 1)
################################################################################

y[1]          <- 75.482  ; y_brown[1]    <- 37.740   ; y_green[1]    <- 37.742
cons[1]       <- 49.098  ; cons_brown[1] <- 24.549   ; cons_green[1] <- 24.549
y_w_brown[1]  <- 23.399  ; y_w_green[1]  <- 23.400
y_h_brown[1]  <- 35.159  ; y_h_green[1]  <- 35.160
yd[1]         <- 60.178
yd_brown[1]   <- 30.091  ; yd_green[1]   <- 30.092
yd_hs_brown[1]<- 30.091  ; yd_hs_green[1]<- 30.092
t_brown[1]    <- 5.067   ; t_green[1]    <- 5.068

inv[1]         <- 13.808
inv_brown[1]   <- 6.904  ; inv_green[1]   <- 6.904
k_brown[1]     <- 60.299 ; k_green[1]     <- 60.299
k_con_brown[1] <- 51.640 ; k_con_green[1] <- 51.640
k_gr_brown[1]  <- 8.660  ; k_gr_green[1]  <- 8.660
k[1]           <- 120.598
da_brown[1]    <- 5.202  ; da_green[1]    <- 5.202
da_gr_brown[1] <- 0.743  ; da_gr_green[1] <- 0.743
da_con_brown[1]<- 4.459  ; da_con_green[1]<- 4.459
af_brown[1]    <- 5.202  ; af_green[1]    <- 5.202
delta_brown[1] <- 0.0888 ; delta_green[1] <- 0.0888

f_brown[1]     <- 8.067  ; f_green[1]     <- 8.067
fu_brown[1]    <- 0.161  ; fu_green[1]    <- 0.161
fd_brown[1]    <- 0.290  ; fd_green[1]    <- 0.290
f_m_brown[1]   <- 0      ; f_m_green[1]   <- 0
f_bank_brown[1]<- 2.861  ; f_bank_green[1]<- 2.861
f_d_brownbrown[1] <- 0.290 ; f_d_browngreen[1] <- 0
f_d_greenbrown[1] <- 0     ; f_d_greengreen[1] <- 0.290

gov[1]           <- 12.574
gov_tot_brown[1] <- 6.287 ; gov_tot_green[1] <- 6.287
gov_con_brown[1] <- 5.287 ; gov_con_green[1] <- 5.287
gov_gr_brown[1]  <- 1     ; gov_gr_green[1]  <- 1

x_brown[1]  <- 10.575 ; x_green[1]  <- 10.574
im_brown[1] <- 10.576 ; im_green[1] <- 10.573
tb_brown[1] <- -0.001 ; tb_green[1] <- 0.001

v[1]          <- 351.730
v_brown[1]    <- 175.880 ; v_green[1]    <- 175.882
b_brown_s[1]  <- 135.417; b_green_s[1]  <- 135.416
b_brownbrown_d[1] <- 25.865 ; b_brownbrown_s[1] <- 25.865
b_browngreen_d[1] <- 8.622  ; b_browngreen_s[1] <- 8.620
b_greenbrown_d[1] <- 8.621  ; b_greenbrown_s[1] <- 8.623
b_greengreen_d[1] <- 25.864 ; b_greengreen_s[1] <- 25.864
b_brown_bank[1]   <- 61.098 ; b_green_bank[1]   <- 61.099
b_cb_brownbrown_s[1] <- 39.832 ; b_cb_greengreen_s[1] <- 39.833

e_brown_real_s[1] <- 5.803 ; e_green_real_s[1] <- 5.803
e_brownbrown_d[1] <- 4.311 ; e_brownbrown_s[1] <- 4.311
e_browngreen_d[1] <- 0     ; e_browngreen_s[1] <- 0
e_greenbrown_d[1] <- 0     ; e_greenbrown_s[1] <- 0
e_greengreen_d[1] <- 4.311 ; e_greengreen_s[1] <- 4.311
p_e_brown[1]      <- 1.486 ; p_e_green[1]      <- 1.486
r_e_brown[1]      <- 0.0352; r_e_green[1]      <- 0.0352

# --- Money, deposits, exchange rate (cash + deposits, paper structure) ---
h_brown_s[1]  <- 39.832 ; h_green_s[1]  <- 39.833
h_brown_h[1]  <- 39.832 ; h_green_h[1]  <- 39.833
dep_brown[1]  <- 92.940 ; dep_green[1]  <- 92.943
dep_bank_brown[1] <- 92.940 ; dep_bank_green[1] <- 92.943
l_firm_brown[1]   <- 31.843 ; l_firm_green[1]   <- 31.844
l_s_brown[1]      <- 31.843 ; l_s_green[1]      <- 31.844
xr_brown[1] <- 1 ; xr_green[1] <- 1
or_brown[1] <- or_init ; or_green[1] <- or_init

d_t_brown[1]    <- 0.00235; d_t_green[1]    <- 0.00235
dc_brown[1]     <- 483.59 ; dc_green[1]     <- 483.58

mu_brown[1]      <- 0.8385 ; mu_green[1]      <- 0.6385
epsilon_brown[1] <- 9.080  ; epsilon_green[1] <- 7.080
beta_brown[1]    <- 0.0553 ; beta_green[1]    <- 0.0453
eta_brown[1]     <- 0.0108 ; eta_green[1]     <- 0.0644
k_se_brown[1]    <- 630.37 ; k_se_green[1]    <- 476.12
k_m_brown[1]     <- 2970.1 ; k_m_green[1]     <- 3196.7
k_m[1]           <- 6166.8
k_e_brown[1]     <- 19300.0; k_e_green[1]     <- 21976.3
k_e[1]           <- 41276.4

res_m_brown[1]   <- 195464.7 ; res_m_green[1] <- 195464.7
res_m[1]         <- 390929.5
res_e_brown[1]   <- 274381.6 ; res_e_green[1] <- 274381.6
res_e[1]         <- 548763.2

emis[1]          <- 36.179
emis_brown[1]    <- 16.342 ; emis_green[1]  <- 16.342
emis_l[1]        <- 3.495
co2_at[1]        <- 3088.89
co2_up[1]        <- 5359.56
co2_lo[1]        <- 36606.18
temp_at[1]       <- 0.91
temp_lo[1]       <- 0.081
f[1]             <- 2.536
f_ex[1]          <- 0.565

################################################################################
# 6) SIMULATION LOOP
################################################################################

for (i in 2:nPeriods) {

  xr_green[i] <- xr_green[i-1]
  xr_brown[i] <- xr_brown[i-1]

  if (i >= greening_starts) {
    cint_b_mult <- (1 + g_beta_brown)^(-(i - greening_starts))
    cint_g_mult <- (1 + g_beta_green)^(-(i - greening_starts))
  } else {
    cint_b_mult <- 1
    cint_g_mult <- 1
  }
  beta0_b_now <- beta0_base * cint_b_mult
  beta0_g_now <- beta0_base * cint_g_mult

  for (iter in 1:nIter) {

    # ------------------------ I. INCOME AND WEALTH ------------------------ #

    cg_b_brown[i]    <- (xr_green[i] - xr_green[i-1]) * b_browngreen_s[i-1]
    cg_e_brown[i]    <- (xr_green[i] - xr_green[i-1]) * e_browngreen_s[i-1]
    cg_b_green[i]    <- (xr_brown[i] - xr_brown[i-1]) * b_greenbrown_s[i-1]
    cg_e_green[i]    <- (xr_brown[i] - xr_brown[i-1]) * e_greenbrown_s[i-1]

    y_h_brown[i] <- y_w_brown[i] + f_m_brown[i] + f_bank_brown[i] +
                    r_brown*b_brownbrown_s[i-1] +
                    xr_green[i-1]*r_green*b_browngreen_s[i-1] +
                    f_d_brownbrown[i-1] + f_d_browngreen[i-1]
    y_h_green[i] <- y_w_green[i] + f_m_green[i] + f_bank_green[i] +
                    r_green*b_greengreen_s[i-1] +
                    xr_brown[i-1]*r_brown*b_greenbrown_s[i-1] +
                    f_d_greengreen[i-1] + f_d_greenbrown[i-1]

    yd_brown[i]     <- y_h_brown[i] * (1 - theta_brown)
    yd_green[i]     <- y_h_green[i] * (1 - theta_green)
    yd_hs_brown[i]  <- yd_brown[i] + cg_b_brown[i] + cg_e_brown[i]
    yd_hs_green[i]  <- yd_green[i] + cg_b_green[i] + cg_e_green[i]

    v_brown[i] <- v_brown[i-1] + yd_hs_brown[i] - cons_brown[i]
    v_green[i] <- v_green[i-1] + yd_hs_green[i] - cons_green[i]

    t_brown[i] <- y_h_brown[i] * theta_brown
    t_green[i] <- y_h_green[i] * theta_green

    # ------------------- II. CONSUMPTION AND TOTAL OUTPUT ------------------ #

    cons_brown[i] <- (alpha1_brown*yd_brown[i] + alpha2_brown*v_brown[i-1]) *
                     (1 - d_t_brown[i-1])
    cons_green[i] <- (alpha1_green*yd_green[i] + alpha2_green*v_green[i-1]) *
                     (1 - d_t_green[i-1])
    cons[i]       <- cons_brown[i] + cons_green[i]

    y_brown[i] <- cons_brown[i] + gov_tot_brown[i] +
                  x_brown[i] - im_brown[i] + inv_brown[i]
    y_green[i] <- cons_green[i] + gov_tot_green[i] +
                  x_green[i] - im_green[i] + inv_green[i]
    y_w_brown[i] <- y_brown[i] * omega_brown
    y_w_green[i] <- y_green[i] * omega_green

    # Option B: no managerial salary residual. All non-retained profit
    # is distributed as dividends to domestic households.
    # Interest expense split by capital type. When rates are equal (baseline),
    # this reduces to r_l_brown * l_firm_brown[i-1] exactly.
    share_gr_b_lag <- if (k_brown[i-1] > 0) k_gr_brown[i-1] / k_brown[i-1] else 0
    r_l_green_br   <- max(r_l_brown * xi_sub_brown - mdb_subsidy_br, 0)
    r_l_con_br     <- r_l_brown
    interest_brown <- r_l_green_br * (l_firm_brown[i-1] * share_gr_b_lag) +
                      r_l_con_br   * (l_firm_brown[i-1] * (1 - share_gr_b_lag))
    f_brown[i]        <- y_brown[i] - y_w_brown[i] - da_brown[i] - interest_brown
    fu_brown[i]       <- f_brown[i] * ret_brown
    fd_brown[i]       <- f_brown[i] - fu_brown[i]
    f_m_brown[i]      <- 0
    f_d_brownbrown[i] <- fd_brown[i]
    f_d_browngreen[i] <- 0

    share_gr_g_lag <- if (k_green[i-1] > 0) k_gr_green[i-1] / k_green[i-1] else 0
    r_l_green_gn   <- max(r_l_green * xi_sub_green - mdb_subsidy_gn, 0)
    r_l_con_gn     <- r_l_green
    interest_green <- r_l_green_gn * (l_firm_green[i-1] * share_gr_g_lag) +
                      r_l_con_gn   * (l_firm_green[i-1] * (1 - share_gr_g_lag))
    f_green[i]        <- y_green[i] - y_w_green[i] - da_green[i] - interest_green
    fu_green[i]       <- f_green[i] * ret_green
    fd_green[i]       <- f_green[i] - fu_green[i]
    f_m_green[i]      <- 0
    f_d_greengreen[i] <- fd_green[i]
    f_d_greenbrown[i] <- 0

    # ------------------- III. INVESTMENT AND CAPITAL ----------------------- #

    da_gr_brown[i]  <- delta_brown[i] * k_gr_brown[i-1]
    da_con_brown[i] <- delta_brown[i] * k_con_brown[i-1]
    da_brown[i]     <- da_gr_brown[i] + da_con_brown[i]
    af_brown[i]     <- da_brown[i]
    da_gr_green[i]  <- delta_green[i] * k_gr_green[i-1]
    da_con_green[i] <- delta_green[i] * k_con_green[i-1]
    da_green[i]     <- da_gr_green[i] + da_con_green[i]
    af_green[i]     <- da_green[i]

    inv_brown[i] <- (gamma0_brown + gamma1_brown*inv_brown[i-1] +
                     gamma2_brown*gov_tot_brown[i-1]) * (1 - d_t_brown[i-1])
    inv_green[i] <- (gamma0_green + gamma1_green*inv_green[i-1] +
                     gamma2_green*gov_tot_green[i-1]) * (1 - d_t_green[i-1])

    saving_brown <- max(r_l_con_br - r_l_green_br, 0) *
                    (l_firm_brown[i-1] * share_gr_b_lag)
    inv_gr_brown_t[i] <- (chi1_brown*gov_gr_brown[i] + chi2_brown*y_brown[i] +
                          chi3_brown*d_t_brown[i-1] +
                          chi4_brown*saving_brown) * (1 - d_t_brown[i-1])
    saving_green <- max(r_l_con_gn - r_l_green_gn, 0) *
                    (l_firm_green[i-1] * share_gr_g_lag)
    inv_gr_green_t[i] <- (chi1_green*gov_gr_green[i] + chi2_green*y_green[i] +
                          chi3_green*d_t_green[i-1] +
                          chi4_green*saving_green) * (1 - d_t_green[i-1])
    inv_gr_brown[i]   <- min(inv_gr_brown_t[i], inv_brown[i])
    inv_gr_green[i]   <- min(inv_gr_green_t[i], inv_green[i])
    inv_con_brown[i]  <- inv_brown[i] - inv_gr_brown[i]
    inv_con_green[i]  <- inv_green[i] - inv_gr_green[i]

    k_gr_brown[i]  <- k_gr_brown[i-1]  + inv_gr_brown[i]  - da_gr_brown[i]
    k_con_brown[i] <- k_con_brown[i-1] + inv_con_brown[i] - da_con_brown[i]
    k_brown[i]     <- k_gr_brown[i] + k_con_brown[i]
    k_gr_green[i]  <- k_gr_green[i-1]  + inv_gr_green[i]  - da_gr_green[i]
    k_con_green[i] <- k_con_green[i-1] + inv_con_green[i] - da_con_green[i]
    k_green[i]     <- k_gr_green[i] + k_con_green[i]

    l_firm_brown[i] <- l_firm_brown[i-1] + inv_brown[i] - af_brown[i] - fu_brown[i] -
                       (e_greenbrown_s[i] - e_greenbrown_s[i-1]) -
                       (e_brownbrown_s[i] - e_brownbrown_s[i-1])
    l_firm_green[i] <- l_firm_green[i-1] + inv_green[i] - af_green[i] - fu_green[i] -
                       (e_browngreen_s[i] - e_browngreen_s[i-1]) -
                       (e_greengreen_s[i] - e_greengreen_s[i-1])

    # -------------------------- IV. TRADE ---------------------------------- #
    # When use_endog_elast = TRUE, Br's import-export elasticities respond to
    # its green-capital share (structuralist Thirlwall channel). Intercepts
    # are re-anchored at period-1 to preserve initial trade levels — only
    # the *evolution* of trade differs from the baseline.

    yb_i    <- max(y_brown[i], 1e-6)
    yg_i    <- max(y_green[i], 1e-6)
    xrb_lag <- max(xr_brown[i-1], 1e-6)

    if (use_endog_elast) {
      share_gr_b_now <- if (k_brown[i-1] > 0) k_gr_brown[i-1]/k_brown[i-1] else 0.144
      eps_x_b <- zeta0_par  + zeta1_par * share_gr_b_now
      eta_m_b <- phi0_m_par - phi1_m_par * share_gr_b_now
      # Re-anchor intercepts so period-1 trade levels match initial conditions.
      # At i=1: y_brown=y_green=37.74, xr=1, x_brown=10.575, im_brown=10.576.
      # Solve for X0_eff, M0_eff such that x_brown[1] equals initial.
      X0_eff <- log(10.575) - eps_x_b * log(37.742)
      M0_eff <- log(10.576) - eta_m_b * log(37.740)
    } else {
      eps_x_b <- eps2
      eta_m_b <- mu2_par
      X0_eff  <- eps0
      M0_eff  <- mu0
    }

    x_brown[i]  <- exp(X0_eff - eps1*log(xrb_lag) + eps_x_b*log(yg_i)) *
                   (1 - ad_exp*d_t_brown[i-1])
    im_brown[i] <- exp(M0_eff + mu1*log(xrb_lag) + eta_m_b*log(yb_i)) *
                   (1 + ad_im *d_t_green[i-1])
    x_green[i]  <- im_brown[i] * xr_brown[i]
    im_green[i] <- x_brown[i]  * xr_brown[i]
    tb_brown[i] <- x_brown[i] - im_brown[i]
    tb_green[i] <- x_green[i] - im_green[i]

    # ------------------- V. PORTFOLIO DEMANDS ------------------------------ #

    b_brownbrown_d[i] <- v_brown[i]*(lambda10 + lambda11*r_brown - lambda12*r_green -
                                     lambda13*r_e_brown[i-1] - lambda14*r_e_green[i-1])
    b_browngreen_d[i] <- v_brown[i]*(lambda20 - lambda21*r_brown + lambda22*r_green -
                                     lambda23*r_e_brown[i-1] - lambda24*r_e_green[i-1])
    e_browngreen_d[i] <- 0
    e_brownbrown_d[i] <- v_brown[i]*((lambda90 + lambda70) - lambda91*r_brown - lambda92*r_green +
                                     lambda93*r_e_brown[i-1] - lambda94*r_e_green[i-1])
    b_greengreen_d[i] <- v_green[i]*(lambda40 - lambda41*r_brown + lambda42*r_green -
                                     lambda43*r_e_brown[i-1] - lambda44*r_e_green[i-1])
    if (use_confidence) {
      tb_ratio   <- if (y_brown[i-1] > 0) tb_brown[i-1] / y_brown[i-1] else 0
      res_ratio  <- or_brown[i-1] / or_target_br - 1
      conf_br    <- exp(kappa_tb * tb_ratio + kappa_res * res_ratio)
      conf_br    <- max(min(conf_br, 2), 0.1)   # clamp 0.1–2 for stability
    } else {
      conf_br    <- 1
    }
    b_greenbrown_d[i] <- v_green[i]*(lambda50*conf_br + lambda51*r_brown - lambda52*r_green -
                                     lambda53*r_e_brown[i-1] - lambda54*r_e_green[i-1])
    e_greenbrown_d[i] <- 0
    e_greengreen_d[i] <- v_green[i]*((lambda100 + lambda80) - lambda101*r_brown - lambda102*r_green -
                                     lambda103*r_e_brown[i-1] + lambda104*r_e_green[i-1])

    # --------- VI. FINANCIAL SUPPLIES, EQUITY ISSUANCE, RETURNS ------------ #

    b_brownbrown_s[i] <- b_brownbrown_d[i]
    b_greengreen_s[i] <- b_greengreen_d[i]
    b_browngreen_s[i] <- b_browngreen_d[i] * xr_brown[i]
    e_browngreen_s[i] <- 0
    e_greengreen_s[i] <- e_greengreen_d[i]
    e_greenbrown_s[i] <- 0
    e_brownbrown_s[i] <- e_brownbrown_d[i]
    e_brown_real_s[i] <- e_brown_real_s[i-1] + xi_brown*inv_brown[i-1]/max(p_e_brown[i-1],1e-6)
    e_green_real_s[i] <- e_green_real_s[i-1] + xi_green*inv_green[i-1]/max(p_e_green[i-1],1e-6)
    p_e_brown[i]      <- e_brownbrown_d[i] / max(e_brown_real_s[i], 1e-6)
    p_e_green[i]      <- e_greengreen_d[i] / max(e_green_real_s[i], 1e-6)
    r_e_brown_t[i]    <- f_brown[i] / max(e_brown_real_s[i-1]*p_e_brown[i-1], 1e-6)
    r_e_green_t[i]    <- f_green[i] / max(e_green_real_s[i-1]*p_e_green[i-1], 1e-6)
    r_e_brown[i]      <- (1 - pi_dy_brown)*r_brown + pi_dy_brown*r_e_brown_t[i]
    r_e_green[i]      <- (1 - pi_dy_green)*r_green + pi_dy_green*r_e_green_t[i]

    # ----------- VII. BANK BALANCE SHEETS AND ADVANCES --------------------- #
    # Cash + deposits split (paper structure): HH residual wealth is split
    # between bank deposits (depsh) and cash held by HH (1 - depsh).

    residual_brown <- v_brown[i] - b_brownbrown_s[i] - e_brownbrown_s[i] -
                      (b_browngreen_s[i] + e_browngreen_s[i]) * xr_green[i]
    dep_brown[i]    <- residual_brown * depsh_brown
    h_brown_h[i]    <- residual_brown - dep_brown[i]

    residual_green <- v_green[i] - b_greengreen_s[i] - e_greengreen_s[i] -
                      (b_greenbrown_s[i] + e_greenbrown_s[i]) * xr_brown[i]
    dep_green[i]    <- residual_green * depsh_green
    h_green_h[i]    <- residual_green - dep_green[i]

    dep_bank_brown[i] <- dep_brown[i]
    dep_bank_green[i] <- dep_green[i]
    l_s_brown[i]      <- l_firm_brown[i]
    l_s_green[i]      <- l_firm_green[i]
    b_brown_bank_not[i] <- dep_bank_brown[i] - l_s_brown[i]
    b_green_bank_not[i] <- dep_bank_green[i] - l_s_green[i]
    if (is.na(b_brown_bank_not[i])) b_brown_bank_not[i] <- 0
    if (is.na(b_green_bank_not[i])) b_green_bank_not[i] <- 0
    z_brown[i] <- if (b_brown_bank_not[i] > 0) 1 else 0
    z_green[i] <- if (b_green_bank_not[i] > 0) 1 else 0
    b_brown_bank[i] <- b_brown_bank_not[i] * z_brown[i]
    b_green_bank[i] <- b_green_bank_not[i] * z_green[i]
    a_d_brown[i] <- -b_brown_bank_not[i] * (1 - z_brown[i])
    a_d_green[i] <- -b_green_bank_not[i] * (1 - z_green[i])
    a_s_brown[i] <- a_d_brown[i] ; a_s_green[i] <- a_d_green[i]
    f_bank_brown[i] <- r_brown*b_brown_bank[i-1] + r_l_brown*l_s_brown[i-1]
    f_bank_green[i] <- r_green*b_green_bank[i-1] + r_l_green*l_s_green[i-1]

    # ----------- VIII. CENTRAL BANK AND GOVERNMENT (G&L floating) ---------- #

    h_brown_s[i]         <- h_brown_h[i]
    h_green_s[i]         <- h_green_h[i]
    b_cb_brownbrown_s[i] <- h_brown_s[i] - a_s_brown[i]
    b_cb_greengreen_s[i] <- h_green_s[i] - a_s_green[i]
    f_cb_brown[i]        <- r_brown * b_cb_brownbrown_s[i-1]
    f_cb_green[i]        <- r_green * b_cb_greengreen_s[i-1]

    gov_con_brown[i] <- 0.076167 + 1.003373 * gov_con_brown[i-1]
    gov_con_green[i] <- 0.076167 + 1.003373 * gov_con_green[i-1]
    gov_gr_brown[i]  <- 1
    gov_gr_green[i]  <- if (run_s5 && i >= s5_start) 1 + s5_uplift else 1
    gov_tot_brown[i] <- gov_con_brown[i] + gov_gr_brown[i]
    gov_tot_green[i] <- gov_con_green[i] + gov_gr_green[i]

    b_brown_s[i] <- b_brown_s[i-1] + gov_tot_brown[i] + r_brown*b_brown_s[i-1] -
                    t_brown[i] - f_cb_brown[i]
    b_green_s[i] <- b_green_s[i-1] + gov_tot_green[i] + r_green*b_green_s[i-1] -
                    t_green[i] - f_cb_green[i]
    b_greenbrown_s[i] <- b_brown_s[i] - b_brownbrown_s[i] - b_cb_brownbrown_s[i] -
                         b_brown_bank[i]

    # --------------- IX. EXCHANGE RATE (GL or FIXED) ----------------------- #

    if (fx_closure == "GL") {
      xr_green_new <- if (abs(b_greenbrown_d[i]) > 1e-6)
                        b_greenbrown_s[i] / b_greenbrown_d[i] else 1
      if (is.na(xr_green_new) || !is.finite(xr_green_new) ||
          xr_green_new < 0.1 || xr_green_new > 10) xr_green_new <- 1
      xr_green[i] <- (1 - xr_relax) * xr_green[i] + xr_relax * xr_green_new
      xr_brown[i] <- 1 / xr_green[i]
    } else if (fx_closure == "FIXED") {
      # Peg at 1. With confidence channel active, foreign demand for Br bills
      # is a hard constraint. The supply-demand gap is absorbed by CB^Br as
      # a reserve outflow (CB sells reserves to buy back unwanted Br bills).
      xr_green[i] <- 1
      xr_brown[i] <- 1
      if (use_confidence) {
        excess_supply  <- b_greenbrown_s[i] - b_greenbrown_d[i]
        # CB absorbs excess into its own bill holdings, financed by reserves
        b_cb_brownbrown_s[i] <- b_cb_brownbrown_s[i] + max(excess_supply, 0)
        b_greenbrown_s[i]    <- b_greenbrown_d[i]   # foreign holdings capped at demand
        # The reserves drain is recorded via bp_brown below (kabp picks it up)
      }
    }

    # ------------------- X. ECOSYSTEM: MATERIAL FLOWS ---------------------- #

    y_mat_brown[i] <- mu_brown[i] * y_brown[i]
    y_mat_green[i] <- mu_green[i] * y_green[i]
    dis_brown[i]   <- mu_brown[i] * (da_brown[i] + zeta_brown * dc_brown[i-1])
    dis_green[i]   <- mu_green[i] * (da_green[i] + zeta_green * dc_green[i-1])
    rec_brown[i]   <- rho_brown * dis_brown[i]
    rec_green[i]   <- rho_green * dis_green[i]
    mat_brown[i]   <- y_mat_brown[i] - rec_brown[i]
    mat_green[i]   <- y_mat_green[i] - rec_green[i]
    dc_brown[i]    <- dc_brown[i-1] + cons_brown[i] - tb_brown[i] - zeta_brown*dc_brown[i-1]
    dc_green[i]    <- dc_green[i-1] + cons_green[i] - tb_green[i] - zeta_green*dc_green[i-1]
    k_se_brown[i]  <- k_se_brown[i-1] + y_mat_brown[i] - dis_brown[i]
    k_se_green[i]  <- k_se_green[i-1] + y_mat_green[i] - dis_green[i]
    wa_brown[i]    <- mat_brown[i] - (k_se_brown[i] - k_se_brown[i-1])
    wa_green[i]    <- mat_green[i] - (k_se_green[i] - k_se_green[i-1])

    conv_m_brown[i] <- sigma_m_brown * res_m_brown[i-1]
    conv_m_green[i] <- sigma_m_green * res_m_green[i-1]
    res_m_brown[i]  <- res_m_brown[i-1] - conv_m_brown[i]
    res_m_green[i]  <- res_m_green[i-1] - conv_m_green[i]
    k_m_brown[i]    <- k_m_brown[i-1] + conv_m_brown[i] - mat_brown[i]
    k_m_green[i]    <- k_m_green[i-1] + conv_m_green[i] - mat_green[i]
    k_m[i]          <- k_m_brown[i] + k_m_green[i]
    res_m[i]        <- res_m_brown[i] + res_m_green[i]
    cen_brown[i]    <- emis_brown[i] / car
    cen_green[i]    <- emis_green[i] / car
    o2_brown[i]     <- emis_brown[i] - cen_brown[i]
    o2_green[i]     <- emis_green[i] - cen_green[i]

    # ------------------- XI. ECOSYSTEM: ENERGY FLOWS ----------------------- #

    e_brown[i]    <- epsilon_brown[i] * y_brown[i]
    e_green[i]    <- epsilon_green[i] * y_green[i]
    er_brown[i]   <- eta_brown[i] * e_brown[i]
    er_green[i]   <- eta_green[i] * e_green[i]
    en_brown[i]   <- e_brown[i] - er_brown[i]
    en_green[i]   <- e_green[i] - er_green[i]
    ed_brown[i]   <- er_brown[i] + en_brown[i]
    ed_green[i]   <- er_green[i] + en_green[i]
    conv_e_brown[i] <- sigma_e_brown * res_e_brown[i-1]
    conv_e_green[i] <- sigma_e_green * res_e_green[i-1]
    res_e_brown[i]  <- res_e_brown[i-1] - conv_e_brown[i]
    res_e_green[i]  <- res_e_green[i-1] - conv_e_green[i]
    k_e_brown[i]    <- k_e_brown[i-1] + conv_e_brown[i] - en_brown[i]
    k_e_green[i]    <- k_e_green[i-1] + conv_e_green[i] - en_green[i]
    k_e[i]          <- k_e_brown[i] + k_e_green[i]
    res_e[i]        <- res_e_brown[i] + res_e_green[i]

    # ------------------- XII. EMISSIONS AND CLIMATE ------------------------ #

    emis_l[i]     <- emis_l[i-1] * (1 - g_land)
    emis_brown[i] <- max(beta0_b_now + beta_brown[i]*en_brown[i], 0)
    emis_green[i] <- max(beta0_g_now - 4 + beta_green[i]*en_green[i], 0)
    emis[i]       <- emis_brown[i] + emis_green[i] + emis_l[i]

    co2_at[i] <- emis[i] + phi11*co2_at[i-1] + phi21*co2_up[i-1]
    co2_up[i] <- phi12*co2_at[i-1] + phi22*co2_up[i-1] + phi32*co2_lo[i-1]
    co2_lo[i] <- phi23*co2_up[i-1] + phi33*co2_lo[i-1]
    f_ex[i]   <- f_ex[i-1] + fex_incr
    f[i]      <- f2 * log(co2_at[i] / co2_at_pre, base = 2) + f_ex[i]
    temp_at[i] <- temp_at[i-1] +
                  t1*(f[i] - (f2/sens)*temp_at[i-1] - t2*(temp_at[i-1] - temp_lo[i-1]))
    temp_lo[i] <- temp_lo[i-1] + t3*(temp_at[i-1] - temp_lo[i-1])

    # ------------------- XIII. ECOLOGICAL EFFICIENCY ----------------------- #

    wgr_b <- if (k_brown[i] > 0) k_gr_brown[i] / k_brown[i] else 0
    wco_b <- 1 - wgr_b
    mu_brown[i]      <- mu_gr_brown*wgr_b  + mu_con_brown*wco_b
    epsilon_brown[i] <- eps_gr_brown*wgr_b + eps_con_brown*wco_b
    beta_brown[i]    <- (beta_gr_brown*wgr_b + beta_con_brown*wco_b) * cint_b_mult
    eta_brown[i]     <- eta_gr_brown*wgr_b + eta_con_brown*wco_b

    wgr_g <- if (k_green[i] > 0) k_gr_green[i] / k_green[i] else 0
    wco_g <- 1 - wgr_g
    mu_green[i]      <- mu_gr_green*wgr_g  + mu_con_green*wco_g
    epsilon_green[i] <- eps_gr_green*wgr_g + eps_con_green*wco_g
    beta_green[i]    <- (beta_gr_green*wgr_g + beta_con_green*wco_g) * cint_g_mult
    eta_green[i]     <- eta_gr_green*wgr_g + eta_con_green*wco_g

    depl_m_brown[i] <- if (k_m_brown[i-1] > 0) mat_brown[i]/k_m_brown[i-1] else 0.24
    depl_m_green[i] <- if (k_m_green[i-1] > 0) mat_green[i]/k_m_green[i-1] else 0.24
    depl_e_brown[i] <- if (k_e_brown[i-1] > 0) en_brown[i]/k_e_brown[i-1] else 0
    depl_e_green[i] <- if (k_e_green[i-1] > 0) en_green[i]/k_e_green[i-1] else 0

    # ----------------------- XIV. DAMAGES ---------------------------------- #

    Ta <- temp_at[i]
    if (Ta > 0) {
      d_t_brown[i] <- 1 - 1/(1 + dam1_brown*Ta + dam2_brown*Ta^2 + dam3_brown*Ta^dam4_brown)
      d_t_green[i] <- 1 - 1/(1 + dam1_green*Ta + dam2_green*Ta^2 + dam3_green*Ta^dam4_green)
    } else {
      d_t_brown[i] <- 1 - 1/(1 + dam1_brown*Ta)
      d_t_green[i] <- 1 - 1/(1 + dam1_green*Ta)
    }
    delta_brown[i] <- delta0_brown + (1 - delta0_brown)*(1 - ad_k_brown)*d_t_brown[i-1]
    delta_green[i] <- delta0_green + (1 - delta0_green)*(1 - ad_k_green)*d_t_green[i-1]

    # ----------------- XV. AGGREGATES & BALANCES --------------------------- #

    psbr_brown[i] <- gov_tot_brown[i] + r_brown*b_brown_s[i-1] - t_brown[i] - f_cb_brown[i]
    psbr_green[i] <- gov_tot_green[i] + r_green*b_green_s[i-1] - t_green[i] - f_cb_green[i]
    cab_brown[i]  <- x_brown[i] - im_brown[i] +
                     xr_green[i-1]*(r_green*b_browngreen_s[i-1] +
                                     r_e_green[i-1]*e_browngreen_s[i-1]) -
                     r_brown*b_greenbrown_s[i-1] -
                     r_e_brown[i-1]*e_greenbrown_s[i-1]
    cab_green[i]  <- -cab_brown[i]
    kabp_brown[i] <- -(b_browngreen_s[i] - b_browngreen_s[i-1])*xr_green[i] -
                      (e_browngreen_s[i] - e_browngreen_s[i-1])*xr_green[i] +
                      (b_greenbrown_s[i] - b_greenbrown_s[i-1]) +
                      (e_greenbrown_s[i] - e_greenbrown_s[i-1])
    kabp_green[i] <- -kabp_brown[i]
    bp_brown[i]   <- cab_brown[i] + kabp_brown[i]
    bp_green[i]   <- cab_green[i] + kabp_green[i]
    # FX reserves accumulate the BP. Under FIXED they're the buffer that
    # absorbs imbalances. Under GL they're a shadow variable (informative
    # but not driving xr).
    or_brown[i]   <- or_brown[i-1] + bp_brown[i]
    or_green[i]   <- or_green[i-1] + bp_green[i]
    nafa_brown[i] <- psbr_brown[i] + cab_brown[i]
    nafa_green[i] <- psbr_green[i] + cab_green[i]

    q_brown[i]     <- (e_brown_real_s[i]*p_e_brown[i] + l_firm_brown[i]) / max(k_brown[i], 1e-6)
    q_green[i]     <- (e_green_real_s[i]*p_e_green[i] + l_firm_green[i]) / max(k_green[i], 1e-6)
    lev_f_brown[i] <- l_firm_brown[i] / max(k_brown[i], 1e-6)
    lev_f_green[i] <- l_firm_green[i] / max(k_green[i], 1e-6)
    liq_b_brown[i] <- (a_s_brown[i] + dep_bank_brown[i] - l_s_brown[i]) / max(dep_bank_brown[i], 1e-6)
    liq_b_green[i] <- (a_s_green[i] + dep_bank_green[i] - l_s_green[i]) / max(dep_bank_green[i], 1e-6)

    inv[i] <- inv_green[i] + inv_brown[i]*xr_brown[i]
    gov[i] <- gov_con_green[i] + gov_con_brown[i]*xr_brown[i] +
              gov_gr_green[i]  + gov_gr_brown[i]*xr_brown[i]
    yd[i]  <- yd_brown[i]*xr_brown[i] + yd_green[i]
    k[i]   <- k_green[i] + k_brown[i]*xr_brown[i]
    v[i]   <- v_brown[i]*xr_brown[i] + v_green[i]
    y[i]   <- y_brown[i] + y_green[i]
  }
}

################################################################################
# 7) PLOT RESULTS
################################################################################

i_plot <- 2:nPeriods
periods <- i_plot

old.par <- par(no.readonly = TRUE)
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)

plot(periods, y_brown[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(y_brown[i_plot], y_green[i_plot])),
     main = "a) GDP by area", xlab = "Period", ylab = "Trillion USD")
lines(periods, y_green[i_plot], lwd = 2, col = "forestgreen")
legend("topleft", c("Brownland","Greenland"),
       col = c("orange","forestgreen"), lwd = 2, bty = "n", cex = 0.85)

plot(periods, temp_at[i_plot], type = "l", lwd = 2, col = "blue",
     main = "b) Atmospheric & lower-ocean temperature",
     xlab = "Period", ylab = "Anomaly (deg C)",
     ylim = range(c(temp_at[i_plot], temp_lo[i_plot])))
lines(periods, temp_lo[i_plot], lwd = 2, col = "#18a8d1")
legend("topleft", c("Atmosphere","Lower ocean"),
       col = c("blue","#18a8d1"), lwd = 2, bty = "n", cex = 0.85)

plot(periods, emis[i_plot], type = "l", lwd = 2, col = "black",
     main = "c) Total CO2 emissions per year", xlab = "Period", ylab = "Gt CO2/yr")
lines(periods, emis_brown[i_plot], col = "orange", lwd = 2)
lines(periods, emis_green[i_plot], col = "forestgreen", lwd = 2)
lines(periods, emis_l[i_plot], col = "darkblue", lwd = 2)
legend("topright", c("Total","Brownland","Greenland","Land"),
       col = c("black","orange","forestgreen","darkblue"), lwd = 2,
       bty = "n", cex = 0.85)

plot(periods, co2_at[i_plot], type = "l", lwd = 2, col = "purple",
     main = "d) Atmospheric CO2 concentration", xlab = "Period", ylab = "Gt CO2")

plot(periods, k_e[i_plot], type = "l", lwd = 2, col = "deeppink",
     ylim = range(c(k_e[i_plot], k_m[i_plot])),
     main = "e) Reserves of energy & matter", xlab = "Period", ylab = "Stock")
lines(periods, k_m[i_plot], lwd = 2, col = "purple")
legend("topleft", c("Energy (Ej)","Matter (Gt)"),
       col = c("deeppink","purple"), lwd = 2, bty = "n", cex = 0.85)

plot(periods, d_t_brown[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(d_t_brown[i_plot], d_t_green[i_plot])),
     main = "f) Climate damage ratio", xlab = "Period", ylab = "d_t")
lines(periods, d_t_green[i_plot], lwd = 2, col = "forestgreen")
legend("topleft", c("Brownland","Greenland"),
       col = c("orange","forestgreen"), lwd = 2, bty = "n", cex = 0.85)

par(old.par)

################################################################################
# 8) DIAGNOSTICS
################################################################################

cat("\n--- Simulation summary ---\n")
cat(sprintf("Final period: %d\n", nPeriods))
cat(sprintf("World GDP (final):           %.2f T\n",  y[nPeriods]))
cat(sprintf("  Brownland GDP:             %.2f T\n",  y_brown[nPeriods]))
cat(sprintf("  Greenland GDP:             %.2f T\n",  y_green[nPeriods]))
cat(sprintf("CO2 emissions (final):       %.2f Gt/yr\n", emis[nPeriods]))
cat(sprintf("Atmospheric CO2 (final):     %.2f Gt\n", co2_at[nPeriods]))
cat(sprintf("Atmospheric temp (final):    %.3f C\n",  temp_at[nPeriods]))
cat(sprintf("Lower-ocean temp (final):    %.3f C\n",  temp_lo[nPeriods]))
cat(sprintf("Damage ratio Brown (final):  %.4f\n",    d_t_brown[nPeriods]))
cat(sprintf("Exchange rate Green (final): %.4f\n",    xr_green[nPeriods]))
