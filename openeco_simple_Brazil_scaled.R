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
#   * GL closure only (no BOP / FIXED)
#   * Cash + deposits structure preserved as in the paper
#
# To run Scenario 5 (RoW-only RoW MOIS from 2025): set run_s5 = TRUE
#
################################################################################

# 1) CLEAR ALL ------------------------------------------------------------------
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

################################################################################
# 2) TIME SPAN
################################################################################

nPeriods_report <- 44  # periods to report/plot (2017-2060)
burn_in         <- 5   # periods to discard from start for IC consistency
nPeriods        <- nPeriods_report + burn_in  # total sim length

run_s5    <- FALSE
s5_start  <- 9 + burn_in   # period 9 of report = period 14 of sim
s5_uplift <- 0.5

################################################################################
# 3) ALLOCATE MODEL VARIABLES
################################################################################

vnames <- c(
  "yd_Br","yd_RoW",
  "yd_hs_Br","yd_hs_RoW",
  "y_h_Br","y_h_RoW",
  "v_Br","v_RoW",
  "cg_b_Br","cg_e_Br","cg_b_RoW","cg_e_RoW",
  "t_Br","t_RoW",
  "cons","cons_Br","cons_RoW",
  "y_Br","y_RoW","y_w_Br","y_w_RoW",
  "f_Br","fu_Br","fd_Br","f_m_Br",
  "f_RoW","fu_RoW","fd_RoW","f_m_RoW",
  "f_d_BrBr","f_d_BrRoW","f_d_RoWBr","f_d_RoWRoW",
  "f_bank_Br","f_bank_RoW",
  "k_Br","k_gr_Br","k_con_Br",
  "k_RoW","k_gr_RoW","k_con_RoW",
  "da_Br","da_gr_Br","da_con_Br",
  "da_RoW","da_gr_RoW","da_con_RoW",
  "af_Br","af_RoW","inv_Br","inv_RoW",
  "inv_gr_Br","inv_gr_RoW","inv_gr_Br_t","inv_gr_RoW_t",
  "inv_con_Br","inv_con_RoW","l_firm_Br","l_firm_RoW",
  "x_Br","im_Br","x_RoW","im_RoW","tb_Br","tb_RoW",
  "b_BrBr_d","b_BrBr_s","b_BrRoW_d","b_BrRoW_s",
  "b_RoWBr_d","b_RoWBr_s","b_RoWRoW_d","b_RoWRoW_s",
  "b_Br_s","b_RoW_s","b_cb_BrBr_s","b_cb_RoWRoW_s",
  "b_Br_bank","b_RoW_bank","b_Br_bank_not","b_RoW_bank_not",
  "z_Br","z_RoW",
  "e_BrBr_d","e_BrBr_s","e_BrRoW_d","e_BrRoW_s",
  "e_RoWBr_d","e_RoWBr_s","e_RoWRoW_d","e_RoWRoW_s",
  "e_Br_real_s","e_RoW_real_s","p_e_Br","p_e_RoW",
  "r_e_Br","r_e_Br_t","r_e_RoW","r_e_RoW_t",
  "h_Br_h","h_Br_s",
  "h_RoW_h","h_RoW_s",
  "dep_Br","dep_bank_Br",
  "dep_RoW","dep_bank_RoW",
  "l_s_Br","l_s_RoW","a_d_Br","a_s_Br","a_d_RoW","a_s_RoW",
  "gov_tot_Br","gov_tot_RoW","gov_con_Br","gov_con_RoW",
  "gov_gr_Br","gov_gr_RoW","f_cb_Br","f_cb_RoW",
  "psbr_Br","psbr_RoW","nafa_Br","nafa_RoW",
  "cab_Br","cab_RoW","kabp_Br","kabp_RoW","bp_Br","bp_RoW",
  "ca_int_paid_fc_Br","ca_int_recv_Br","ca_int_paid_lc_Br","ca_div_net_Br",
  "ka_b_in_Br","ka_b_out_Br","ka_e_in_Br","ka_e_out_Br","ka_fc_in_Br",
  "bop_resid_Br","bop_resid_RoW",
  "gnp_Br","gnp_RoW","xr_Br","xr_RoW",
  "or_Br","or_RoW",
  "y_mat_Br","y_mat_RoW","mat_Br","mat_RoW",
  "rec_Br","rec_RoW","dis_Br","dis_RoW",
  "dc_Br","dc_RoW","k_se_Br","k_se_RoW","wa_Br","wa_RoW",
  "k_m_Br","k_m_RoW","k_m","conv_m_Br","conv_m_RoW",
  "res_m_Br","res_m_RoW","res_m",
  "cen_Br","cen_RoW","o2_Br","o2_RoW",
  "e_Br","e_RoW","er_Br","er_RoW","en_Br","en_RoW",
  "ed_Br","ed_RoW",
  "k_e_Br","k_e_RoW","k_e","conv_e_Br","conv_e_RoW",
  "res_e_Br","res_e_RoW","res_e",
  "emis_Br","emis_RoW","emis_l","emis_l_Br","emis_l_RoW","emis",
  "co2_at","co2_up","co2_lo","f","f_ex","temp_at","temp_lo",
  "mu_Br","mu_RoW","epsilon_Br","epsilon_RoW",
  "beta_Br","beta_RoW","eta_Br","eta_RoW",
  "depl_m_Br","depl_m_RoW","depl_e_Br","depl_e_RoW",
  "d_t_Br","d_t_RoW","delta_Br","delta_RoW",
  "q_Br","q_RoW","lev_f_Br","lev_f_RoW",
  "q_nalin_Br","k_target_Br","q_active_Br",
  "inv_d_Br","K_target_Br","lcd_Br","ldom_Br","fc_residual_Br",
  "per_Br","per_RoW","liq_b_Br","liq_b_RoW",
  "y","inv","gov","yd","k","v",
  "l_fc_Br_d","l_fc_Br_s","l_fc_Br","delta_l_fc_Br",
  "int_fc_Br"
)
for (vn in vnames) assign(vn, numeric(nPeriods))

################################################################################
# 4) PARAMETERS
################################################################################

omega_Br    <- 0.62      ; omega_RoW    <- 0.62
alpha1_Br   <- 0.676919  ; alpha1_RoW   <- 0.676919
alpha2_Br   <- 0.020834  ; alpha2_RoW   <- 0.020834

gamma0_Br <- (0.093272 - 0.05) * 0.07949 ; gamma0_RoW <- (0.093272 - 0.05) * 2.5703
gamma1_Br <- 1.008213        ; gamma1_RoW <- 1.008213
gamma2_Br <- 0.005           ; gamma2_RoW <- 0.005

chi1_Br <- 0.2  ; chi1_RoW <- 0.2
chi2_Br <- 0.02 ; chi2_RoW <- 0.02
chi3_Br <- 30 * 0.07949   ; chi3_RoW <- 30 * 2.5703

# --- Green Investment Finance differential (GIFdif) channel ----------------
# gifdif = (r_l_con - r_l_RoW) * L_RoW_outstanding
# Flow of interest cost reduction firms enjoy when the MDB lowers the RoW
# loan rate. Dimensionally consistent with G_gr (both are T USD/year flows).
# When r_l_RoW = r_l_con (baseline), gifdif = 0 (channel inactive).
# chi4 has same default value as chi1 (RoW-inv. push per unit of GIFdif).
chi4_Br      <- 0.2 ; chi4_RoW      <- 0.2
mdb_subsidy_br  <- 0   ; mdb_subsidy_gn  <- 0   # subtracted from r_l_RoW
xi_sub_Br    <- 1   ; xi_sub_RoW    <- 1   # additional multiplier on r_l_RoW

ret_Br   <- 0.02   ; ret_RoW   <- 0.02
pi_dy_Br <- 0.00555; pi_dy_RoW <- 0.00555

# --- Q-channel: Nalin & Yajima (2022) leverage feedback in target capital ---
# k_target_t = k_0 + k_1 * Q_{t-1}, where Q = (L_dom + L_FC*xr) / K
# Higher leverage today -> higher target capital tomorrow (Minskyan euphoria).
# Calibration of k_0, k_1 deferred until investment block is wired.
#
# Toggles for thesis sensitivity tests:
#   q_channel_on = FALSE  -> disables Q feedback (target k held constant);
#                            useful for stability runs / avoiding Minsky cycles
#   q_target_cap_on = TRUE -> freezes Q feedback once Br green capital share
#                            hits a target threshold (s_gr_target). Used to
#                            stop the boom once structural transformation
#                            has succeeded; the post-transition phase doesn't
#                            need Minskyan dynamics to keep amplifying.
q_channel_on    <- TRUE
q_target_cap_on <- TRUE
s_gr_target_Br  <- 0.40   # green capital share at which Q feedback freezes
k0_Br <- 1.67 ; k0_RoW <- 1.60   # K/Y target at baseline; Br ~ observed 2016 ratio
k1_Br <- 0.20 ; k1_RoW <- 0.20   # Q feedback strength; moderate Minsky channel

# --- Nalin investment-financing parameters ---------------------------------
# Speed of adjustment to target capital (Nalin eq. 11 analog):
#   I_d = g * (K_target - K_{t-1}) + DA
g_inv_Br      <- 0.30     # fraction of capital gap closed per period
# Domestic-loan share equation (Nalin eq. 8 analog):
#   L_dom = lambda_dom_0 + lambda_dom_1 * L_total_need
# Calibrated so domestic-loan share ~ 87% at baseline (Br empirical: domestic
# bank credit to NFC ~ 6-7x larger than NFC FC debt at end-2019, BIS data).
lambda_dom_0  <- 0        # autonomous component
lambda_dom_1  <- 0.87     # share of total financing met by domestic loans
# Blend factor: in transition periods between old and new investment scheme,
# use convex combination. inv_nalin_weight = 0 -> old gamma equation;
# = 1 -> pure Nalin. Set to 1 for full Nalin port.
inv_nalin_weight <- 1.0
xi_Br    <- 0.01   ; xi_RoW    <- 0.01

eps0 <- -2.1 ; eps1 <- 0.5 ; eps2 <- 1.228
mu0  <- -2.1 ; mu1  <- 0.5 ; mu2_par  <- 1.228

# --- Endogenous trade elasticities (Souza & Silva / structuralist channel) ---
# Trade income elasticities depend on the RoW capital share of total capital
# (proxy for technological sophistication / industrialization):
#   eps_x_b = zeta0 + zeta1 * share_gr_Br   (export elasticity rises with K_gr)
#   eta_m_b = phi0_m - phi1_m * share_gr_Br (import elasticity falls with K_gr)
# Set use_endog_elast = FALSE to recover the constant-elasticity baseline.
use_endog_elast <- TRUE
# --- Souza & Silva (2024) empirical calibration for Brazil-like periphery ---
# At s_gr_b ~= 0.144 (baseline): eps_x = 0.92 (slight commodity-exporter signature),
# eta_m = 1.05 (slight industrial-importer signature). Modest asymmetry,
# empirically grounded for a peripheral economy facing the world.
# The endogenous part captures structural transformation: as Br accumulates
# RoW capital (s_gr rises), eps_x rises and eta_m falls -> trade asymmetry
# softens, consistent with structuralist Kaldorian/Thirlwall logic.
zeta0_par   <- 0.85   ; zeta1_par   <- 0.45    # exports: weak at low share_gr
phi0_m_par  <- 1.10   ; phi1_m_par  <- 0.35    # imports: strong at low share_gr

# --- Confidence channel: foreign demand for Br bills responds to fundamentals ---
# lambda50_effective = lambda50 * confidence_br
# confidence_br = exp( kappa_tb * tb_Br/y_Br + kappa_res * (or_Br/or_target - 1) )
# When trade deteriorates or reserves fall below target, confidence drops,
# reducing foreign demand for Br bills. Set use_confidence = FALSE to disable.
use_confidence <- TRUE
kappa_tb       <- 5      # sensitivity to trade-balance/GDP ratio
kappa_res      <- 0.5    # sensitivity to reserves vs target
or_target_br   <- 3.9746  # reserve target Br (scaled: 50 * s_Br)
or_target_row  <- 128.5109 # reserve target RoW (scaled: 50 * s_RoW)

lambda10 <- 0.14707 ; lambda11 <- 1 ; lambda12 <- 1 ; lambda13 <- 0    ; lambda14 <- 0
lambda20 <- 0.04902 ; lambda21 <- 1 ; lambda22 <- 1 ; lambda23 <- 0    ; lambda24 <- 0
lambda40 <- 0.14707 ; lambda41 <- 1 ; lambda42 <- 1 ; lambda43 <- 0    ; lambda44 <- 0
# --- lambda50 endogenized to scale with Br's share of world output ---
# Approach B (Brainard-Tobin with size-proportional foreign allocation):
#   lambda50_t = lambda50_bar * (Y_Br_t / (Y_Br_t + Y_RoW_t))
# As Br's share of world GDP rises, RoW naturally wants more Br bills.
# Reference parameter calibrated so at period 1 (Br share ~3%), the resulting
# allocation matches a 15% foreign-held share of Br bills (realistic for EM).
#   target: b_RoWBr_d[1] = 0.15 * b_Br_s[1] = 0.15 * 10.7645 = 1.6147
#   lambda50[1] = 1.6147 / V_RoW[1] = 1.6147 / 452.05 = 0.003572
#   lambda50_bar = 0.003572 / 0.03 = 0.1191
lambda50_bar <- 0.1191
# Backward compatibility: lambda50 var still exists but is now computed dynamically
lambda50 <- 0.003572  # placeholder (will be overwritten in loop)
lambda51 <- 1 ; lambda52 <- 1 ; lambda53 <- 0    ; lambda54 <- 0
lambda70 <- 0.02451 ; lambda71 <- 0 ; lambda72 <- 0 ; lambda73 <- 0.01 ; lambda74 <- 0.01
lambda80 <- 0.02451 ; lambda81 <- 0 ; lambda82 <- 0 ; lambda83 <- 0.01 ; lambda84 <- 0.01
lambda90 <- 0.02451 ; lambda91 <- 0 ; lambda92 <- 0 ; lambda93 <- 0.01 ; lambda94 <- 0.01
lambda100<- 0.02451 ; lambda101<- 0 ; lambda102<- 0 ; lambda103<- 0.01 ; lambda104<- 0.01

# Bank-deposit share of HH liquid wealth (rest is cash held by HH)
depsh_Br <- 0.7  ; depsh_RoW <- 0.7

r_Br     <- 0.03 ; r_RoW     <- 0.03
r_l_Br   <- 0.035; r_l_RoW   <- 0.035

# --- Foreign-currency loans to Br firms (from RoW banks) -------------------
# Channel: RoW banks lend in FC to Br firms; loans denominated in RoW currency
# (foreign currency from Br perspective). Used to finance imports of capital
# goods for ISI. Supply is passive (banks accommodate demand) in this version
# -- credit rationing / spread response can be added later. Structure follows
# Nalin & Yajima (2022), where Mexican firms borrow from US banks in FC; the
# financing identity (eq. 7-10 in their paper) makes FC borrowing the residual
# of total firm financing need net of domestic loans and internal funds.
#
# Rate calibration anchored to 2016 Brazilian data, decomposed as:
#   r_l_fc = r_RoW_base + EMBI+Br sovereign spread + corporate premium
#          = 0.03 + 0.040 + 0.015 = 0.085
# Sources:
#   - r_RoW = 0.03 retained from Carnevali et al. (2021) as the stylized RoW
#     base rate (long-run risk-free anchor, not literal 2016 US Treasury).
#   - EMBI+Br 2016 annual average ~400 bps over UST (Banco Central do Brasil
#     2016, Country Risk FAQ Series #9, Chart 1). Brazil rated BB/Ba2/BB+
#     (speculative grade) after the 2015 downgrades.
#   - Corporate premium ~150 bps over sovereign for sub-investment-grade EM
#     corporates: Bevilaqua, Hale & Tallman (2020, J. Int. Economics 124)
#     document near-full pass-through with persistent positive gap; Li, Magud,
#     Werner & Witte (2021, IMF WP 21/155) confirm using a 2016-2019 sample.
r_l_fc     <- 0.085     # FC corporate borrowing rate, Brazil 2016 anchor
fc_supply_mode <- "passive"  # "passive" = supply = demand; "rationed" later

theta_Br  <- 0.144193    ; theta_RoW  <- 0.144193
delta0_Br <- 0.100609    ; delta0_RoW <- 0.100609
ad_k_Br   <- 0.75        ; ad_k_RoW   <- 0.75

mu_gr_Br   <- 0.71  ; mu_gr_RoW   <- 0.51
mu_con_Br  <- 0.86  ; mu_con_RoW  <- 0.66
eps_gr_Br  <- 7.65  ; eps_gr_RoW  <- 5.65
eps_con_Br <- 9.32  ; eps_con_RoW <- 7.32
beta_gr_Br  <- 0.048154 - 0.01
beta_gr_RoW  <- 0.048154 - 0.02
beta_con_Br <- 0.048154 + 0.01
beta_con_RoW <- 0.048154
eta_con_Br <- 0     ; eta_con_RoW <- 0.05
eta_gr_Br  <- 0.075 ; eta_gr_RoW  <- 0.15

rho_Br     <- 0.2     ; rho_RoW     <- 0.28
sigma_m_Br <- 0.00034 ; sigma_m_RoW <- 0.00034
sigma_e_Br <- 0.00177 ; sigma_e_RoW <- 0.00177
zeta_Br    <- 0.015   ; zeta_RoW    <- 0.015
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

dam1_Br <- 0 ; dam2_Br <- 0.00284 ; dam3_Br <- 5e-6 ; dam4_Br <- 6.6754
dam1_RoW <- 0 ; dam2_RoW <- 0.00284 ; dam3_RoW <- 5e-6 ; dam4_RoW <- 6.6754
ad_exp <- 0.05 ; ad_im <- 0.05

g_land <- 0.044                # RoW land-use emissions decline rate (paper-style)

# --- Br land-use emissions (Agriculture + LULUCF) endogenized ---------------
# Option C with sensitivity multiplier psi:
#   emis_l_Br_t = emis_l_Br_0 * [ (Y_t/Y_0) / (K_t/K_0) * (1-s_gr_t)/(1-s_gr_0) ]^psi
#
# Two structural channels (unit elasticities, anchored to period 1):
#   (i)  Y/K ratio: growth-vs-capital deepening — when Y rises faster than K,
#        land-extractive activities expand (World Bank evidence on formal
#        industrial jobs vs deforestation rates)
#   (ii) Conv-capital share: green capital substitutes for land-extractive
#        production; lower conv share -> lower land pressure
#
# psi >= 0 is an EXOGENOUS SENSITIVITY DIAL. Default psi=1 reproduces standard
# Option C. Raise psi for stress-test scenarios where deforestation responds
# more strongly to structural change; lower psi for more conservative results.
# Note: we deliberately do NOT include exports as a separate driver: under
# successful ISI, the export *composition* shifts toward manufactures, so gross
# exports rising do not imply more land pressure. Y/K and green-share already
# capture the relevant structural transformation channels.
psi_land       <- 1.0    # sensitivity dial for land emissions (default 1)
emis_l_Br_0    <- 1.004  # Br land emissions 2016 (Agric 567 + LULUCF 437 kt)
emis_l_RoW_0   <- 2.491  # RoW land emissions = global 3.495 - Br 1.004

g_beta_Br <- 0.02
g_beta_RoW <- 0.04
greening_starts <- 4 + burn_in
# Autonomous emissions component, scaled per area (original 6.904820/2 = 3.4524)
beta0_base_Br  <- 0.27444   # 3.4524 * s_Br
beta0_base_RoW <- 8.87342   # 3.4524 * s_RoW

xr_relax <- 0.15
nIter <- 150

# --- FX closure -----------------------------------------------------------
# "GL"    = Godley-Lavoie floating; xr clears Br bill market every period.
# "FIXED" = peg xr_RoW = 1; FX reserves (or_Br, or_RoW) absorb BP.
fx_closure <- "GL"
or_init_Br  <- 3.9746   # initial Br reserves (scaled: 50 * s_Br)
or_init_RoW <- 128.5109 # initial RoW reserves (scaled: 50 * s_RoW)

################################################################################
# 5) INITIAL CONDITIONS (period 1)
################################################################################

y[1]          <- 100.000 ; y_Br[1]    <- 3.000    ; y_RoW[1]    <- 97.000
cons[1]       <- 65.052  ; cons_Br[1] <- 1.951    ; cons_RoW[1] <- 63.100
y_w_Br[1]  <- 1.860   ; y_w_RoW[1]  <- 60.140
y_h_Br[1]  <- 2.795   ; y_h_RoW[1]  <- 90.376
yd[1]         <- 79.696
yd_Br[1]   <- 2.392   ; yd_RoW[1]   <- 77.345
yd_hs_Br[1]<- 2.392   ; yd_hs_RoW[1]<- 77.345
t_Br[1]    <- 0.403   ; t_RoW[1]    <- 13.030

inv[1]         <- 18.286
inv_Br[1]   <- 0.549  ; inv_RoW[1]   <- 17.745
k_Br[1]     <- 4.793  ; k_RoW[1]     <- 154.987
k_con_Br[1] <- 4.104  ; k_con_RoW[1] <- 132.731
k_gr_Br[1]  <- 0.688  ; k_gr_RoW[1]  <- 22.257
k[1]           <- 159.780
da_Br[1]    <- 0.413  ; da_RoW[1]    <- 13.371
da_gr_Br[1] <- 0.059  ; da_gr_RoW[1] <- 1.910
da_con_Br[1]<- 0.354  ; da_con_RoW[1]<- 11.461
af_Br[1]    <- 0.413  ; af_RoW[1]    <- 13.371
delta_Br[1] <- 0.0888 ; delta_RoW[1] <- 0.0888

f_Br[1]     <- 0.6413  ; f_RoW[1]     <- 20.7339
fu_Br[1]    <- 0.0128  ; fu_RoW[1]    <- 0.4138
fd_Br[1]    <- 0.0231  ; fd_RoW[1]    <- 0.7454
f_m_Br[1]   <- 0       ; f_m_RoW[1]   <- 0
f_bank_Br[1]<- 0.2274  ; f_bank_RoW[1]<- 7.3534
f_d_BrBr[1] <- 0.0231  ; f_d_BrRoW[1] <- 0
f_d_RoWBr[1] <- 0      ; f_d_RoWRoW[1] <- 0.7454

gov[1]           <- 16.654
gov_tot_Br[1] <- 0.500 ; gov_tot_RoW[1] <- 16.154
gov_con_Br[1] <- 0.420 ; gov_con_RoW[1] <- 13.591
gov_gr_Br[1]  <- 0.0795; gov_gr_RoW[1]  <- 2.5703

x_Br[1]  <- 0.8406 ; x_RoW[1]  <- 0.8407
im_Br[1] <- 0.8407 ; im_RoW[1] <- 0.8406
tb_Br[1] <- -0.0001 ; tb_RoW[1] <- 0.0001

v[1]          <- 466.031
v_Br[1]    <- 13.9809  ; v_RoW[1]    <- 452.0498
b_Br_s[1]  <- 10.7645  ; b_RoW_s[1]  <- 348.0511
b_BrBr_d[1] <- 2.0560  ; b_BrBr_s[1] <- 2.0560
b_BrRoW_d[1] <- 0.6853 ; b_BrRoW_s[1] <- 0.6854
b_RoWBr_d[1] <- 1.6147 ; b_RoWBr_s[1] <- 1.6147
b_RoWRoW_d[1] <- 66.4761 ; b_RoWRoW_s[1] <- 66.4761
b_Br_bank[1]   <- 4.2942 ; b_RoW_bank[1]   <- 157.0351
b_cb_BrBr_s[1] <- 2.7996 ; b_cb_RoWRoW_s[1] <- 102.3769

e_Br_real_s[1] <- 0.4613 ; e_RoW_real_s[1] <- 14.9150
e_BrBr_d[1] <- 0.3427 ; e_BrBr_s[1] <- 0.3427
e_BrRoW_d[1] <- 0     ; e_BrRoW_s[1] <- 0
e_RoWBr_d[1] <- 0     ; e_RoWBr_s[1] <- 0
e_RoWRoW_d[1] <- 11.0802 ; e_RoWRoW_s[1] <- 11.0802
p_e_Br[1]      <- 1.486 ; p_e_RoW[1]      <- 1.486
r_e_Br[1]      <- 0.0352; r_e_RoW[1]      <- 0.0352

# --- Money, deposits, exchange rate (cash + deposits, paper structure) ---
h_Br_s[1]  <- 3.1663 ; h_RoW_s[1]  <- 102.3769
h_Br_h[1]  <- 3.1663 ; h_RoW_h[1]  <- 102.3769
dep_Br[1]  <- 7.3879 ; dep_RoW[1]  <- 238.8760
dep_bank_Br[1] <- 7.3879 ; dep_bank_RoW[1] <- 238.8760
l_firm_Br[1]   <- 2.5312 ; l_firm_RoW[1]   <- 81.8434
l_s_Br[1]      <- 2.5312 ; l_s_RoW[1]      <- 81.8434
# Br firms' FC debt stock (passive supply; demand-driven). Empirically Brazilian
# NFC FCD was ~13-15% of GDP at end-2019 per BIS data (Avdjiev, McGuire & von
# Peter 2020, Graph 4). At our Br GDP ~3 T, that gives ~0.40 T.
l_fc_Br[1]    <- 0.40    ; l_fc_Br_d[1]   <- 0.40
l_fc_Br_s[1]  <- 0.40    ; delta_l_fc_Br[1] <- 0
int_fc_Br[1]  <- r_l_fc * 0.40   # interest cost on initial FC stock (FC units)
xr_Br[1] <- 1 ; xr_RoW[1] <- 1
or_Br[1] <- or_init_Br ; or_RoW[1] <- or_init_RoW

d_t_Br[1]    <- 0.00235; d_t_RoW[1]    <- 0.00235
dc_Br[1]     <- 38.4412 ; dc_RoW[1]     <- 1242.9314

mu_Br[1]      <- 0.8385 ; mu_RoW[1]      <- 0.6385
epsilon_Br[1] <- 9.080  ; epsilon_RoW[1] <- 7.080
beta_Br[1]    <- 0.0553 ; beta_RoW[1]    <- 0.0453
eta_Br[1]     <- 0.0108 ; eta_RoW[1]     <- 0.0644
k_se_Br[1]    <- 50.109   ; k_se_RoW[1]    <- 1223.732
k_m_Br[1]     <- 236.097  ; k_m_RoW[1]     <- 8216.214
k_m[1]           <- 8452.31
k_e_Br[1]     <- 1534.181 ; k_e_RoW[1]     <- 56483.866
k_e[1]           <- 58018.05

res_m_Br[1]   <- 15537.73  ; res_m_RoW[1] <- 502386.75
res_m[1]         <- 517924.48
res_e_Br[1]   <- 21810.94  ; res_e_RoW[1] <- 705220.33
res_e[1]         <- 727031.27

emis[1]          <- 46.797
emis_Br[1]    <- 1.2990 ; emis_RoW[1]  <- 42.0025
emis_l[1]        <- 3.495
emis_l_Br[1]     <- emis_l_Br_0    # 1.004 Gt (Br Agric + LULUCF, 2016 NIR)
emis_l_RoW[1]    <- emis_l_RoW_0   # 2.491 Gt (global - Br share)
co2_at[1]        <- 3088.89
co2_up[1]        <- 5359.56
co2_lo[1]        <- 36606.18
temp_at[1]       <- 0.91
temp_lo[1]       <- 0.081
f[1]             <- 2.536
f_ex[1]          <- 0.565

# --- Reference values for endogenous Br land emissions (computed post-burn-in) ---
# These will be re-anchored after burn-in so the formula uses "real period 1"
# (reported period 1 = absolute period burn_in+1) as the baseline.
# For now, set tentatively from initial conditions; reassigned at end of burn-in.
y_Br_p1_ref <- y_Br[1]
k_Br_p1_ref <- k_Br[1]
KY0_Br      <- k_Br[1] / y_Br[1]
s_gr_Br_0   <- k_gr_Br[1] / k_Br[1]

################################################################################
# 6) SIMULATION LOOP
################################################################################

for (i in 2:nPeriods) {
  
  # Re-anchor Br land-emission reference values at the start of reported time
  # (absolute period burn_in+1 = reported period 1). The formula then uses
  # the model's self-consistent post-burn-in state as the baseline rather than
  # the hand-set initial conditions.
  if (i == burn_in + 1) {
    y_Br_p1_ref <- y_Br[i-1]
    k_Br_p1_ref <- k_Br[i-1]
    KY0_Br      <- k_Br[i-1] / y_Br[i-1]
    s_gr_Br_0   <- k_gr_Br[i-1] / k_Br[i-1]
  }
  
  xr_RoW[i] <- xr_RoW[i-1]
  xr_Br[i] <- xr_Br[i-1]
  
  if (i >= greening_starts) {
    cint_b_mult <- (1 + g_beta_Br)^(-(i - greening_starts))
    cint_g_mult <- (1 + g_beta_RoW)^(-(i - greening_starts))
  } else {
    cint_b_mult <- 1
    cint_g_mult <- 1
  }
  beta0_b_now <- beta0_base_Br  * cint_b_mult
  beta0_g_now <- beta0_base_RoW * cint_g_mult
  
  for (iter in 1:nIter) {
    
    # ------------------------ I. INCOME AND WEALTH ------------------------ #
    
    cg_b_Br[i]    <- (xr_RoW[i] - xr_RoW[i-1]) * b_BrRoW_s[i-1]
    cg_e_Br[i]    <- (xr_RoW[i] - xr_RoW[i-1]) * e_BrRoW_s[i-1]
    cg_b_RoW[i]    <- (xr_Br[i] - xr_Br[i-1]) * b_RoWBr_s[i-1]
    cg_e_RoW[i]    <- (xr_Br[i] - xr_Br[i-1]) * e_RoWBr_s[i-1]
    
    y_h_Br[i] <- y_w_Br[i] + f_m_Br[i] + f_bank_Br[i] +
      r_Br*b_BrBr_s[i-1] +
      xr_RoW[i-1]*r_RoW*b_BrRoW_s[i-1] +
      f_d_BrBr[i-1] + f_d_BrRoW[i-1]
    y_h_RoW[i] <- y_w_RoW[i] + f_m_RoW[i] + f_bank_RoW[i] +
      r_RoW*b_RoWRoW_s[i-1] +
      xr_Br[i-1]*r_Br*b_RoWBr_s[i-1] +
      f_d_RoWRoW[i-1] + f_d_RoWBr[i-1]
    
    yd_Br[i]     <- y_h_Br[i] * (1 - theta_Br)
    yd_RoW[i]     <- y_h_RoW[i] * (1 - theta_RoW)
    yd_hs_Br[i]  <- yd_Br[i] + cg_b_Br[i] + cg_e_Br[i]
    yd_hs_RoW[i]  <- yd_RoW[i] + cg_b_RoW[i] + cg_e_RoW[i]
    
    v_Br[i] <- v_Br[i-1] + yd_hs_Br[i] - cons_Br[i]
    v_RoW[i] <- v_RoW[i-1] + yd_hs_RoW[i] - cons_RoW[i]
    
    t_Br[i] <- y_h_Br[i] * theta_Br
    t_RoW[i] <- y_h_RoW[i] * theta_RoW
    
    # ------------------- II. CONSUMPTION AND TOTAL OUTPUT ------------------ #
    
    cons_Br[i] <- (alpha1_Br*yd_Br[i] + alpha2_Br*v_Br[i-1]) *
      (1 - d_t_Br[i-1])
    cons_RoW[i] <- (alpha1_RoW*yd_RoW[i] + alpha2_RoW*v_RoW[i-1]) *
      (1 - d_t_RoW[i-1])
    cons[i]       <- cons_Br[i] + cons_RoW[i]
    
    y_Br[i] <- cons_Br[i] + gov_tot_Br[i] +
      x_Br[i] - im_Br[i] + inv_Br[i]
    y_RoW[i] <- cons_RoW[i] + gov_tot_RoW[i] +
      x_RoW[i] - im_RoW[i] + inv_RoW[i]
    y_w_Br[i] <- y_Br[i] * omega_Br
    y_w_RoW[i] <- y_RoW[i] * omega_RoW
    
    # Option B: no managerial salary residual. All non-retained profit
    # is distributed as dividends to domestic households.
    # Interest expense split by capital type. When rates are equal (baseline),
    # this reduces to r_l_Br * l_firm_Br[i-1] exactly.
    share_gr_b_lag <- if (k_Br[i-1] > 0) k_gr_Br[i-1] / k_Br[i-1] else 0
    r_l_RoW_br   <- max(r_l_Br * xi_sub_Br - mdb_subsidy_br, 0)
    r_l_con_br     <- r_l_Br
    interest_Br <- r_l_RoW_br * (l_firm_Br[i-1] * share_gr_b_lag) +
      r_l_con_br   * (l_firm_Br[i-1] * (1 - share_gr_b_lag))
    # FC interest cost — close the Minsky loop: FC debt service hits firm
    # cash flow, reducing retained profits and amplifying next-period
    # financing need (Nalin's central fragility mechanism).
    int_fc_cashflow_Br <- r_l_fc * l_fc_Br[i-1] * xr_Br[i]
    f_Br[i]        <- y_Br[i] - y_w_Br[i] - da_Br[i] - interest_Br -
      int_fc_cashflow_Br
    fu_Br[i]       <- f_Br[i] * ret_Br
    fd_Br[i]       <- f_Br[i] - fu_Br[i]
    f_m_Br[i]      <- 0
    f_d_BrBr[i] <- fd_Br[i]
    f_d_BrRoW[i] <- 0
    
    share_gr_g_lag <- if (k_RoW[i-1] > 0) k_gr_RoW[i-1] / k_RoW[i-1] else 0
    r_l_RoW_gn   <- max(r_l_RoW * xi_sub_RoW - mdb_subsidy_gn, 0)
    r_l_con_gn     <- r_l_RoW
    interest_RoW <- r_l_RoW_gn * (l_firm_RoW[i-1] * share_gr_g_lag) +
      r_l_con_gn   * (l_firm_RoW[i-1] * (1 - share_gr_g_lag))
    f_RoW[i]        <- y_RoW[i] - y_w_RoW[i] - da_RoW[i] - interest_RoW
    fu_RoW[i]       <- f_RoW[i] * ret_RoW
    fd_RoW[i]       <- f_RoW[i] - fu_RoW[i]
    f_m_RoW[i]      <- 0
    f_d_RoWRoW[i] <- fd_RoW[i]
    f_d_RoWBr[i] <- 0
    
    # ------------------- III. INVESTMENT AND CAPITAL ----------------------- #
    
    da_gr_Br[i]  <- delta_Br[i] * k_gr_Br[i-1]
    da_con_Br[i] <- delta_Br[i] * k_con_Br[i-1]
    da_Br[i]     <- da_gr_Br[i] + da_con_Br[i]
    af_Br[i]     <- da_Br[i]
    da_gr_RoW[i]  <- delta_RoW[i] * k_gr_RoW[i-1]
    da_con_RoW[i] <- delta_RoW[i] * k_con_RoW[i-1]
    da_RoW[i]     <- da_gr_RoW[i] + da_con_RoW[i]
    af_RoW[i]     <- da_RoW[i]
    
    # --- NALIN Q-RATIO & TARGET CAPITAL (computed now, used in next step) ---
    # Q = (domestic loans + FC loans*xr) / capital -- leverage ratio.
    # Both numerator and denominator in Br currency. FC stock revalued at xr.
    # Distinct from existing q_Br which is Tobin's-q (market value / capital).
    q_nalin_Br[i] <- (l_firm_Br[i-1] + l_fc_Br[i-1] * xr_Br[i]) /
      max(k_Br[i-1], 1e-6)
    # Determine whether the Q channel is active this period:
    s_gr_Br_now <- if (k_Br[i-1] > 0) k_gr_Br[i-1] / k_Br[i-1] else 0
    q_active_Br[i] <- if (!q_channel_on) 0
    else if (q_target_cap_on && s_gr_Br_now >= s_gr_target_Br) 0
    else 1
    # Target capital-output ratio (Nalin eq. 12 analog):
    k_target_Br[i] <- k0_Br + q_active_Br[i] * k1_Br * q_nalin_Br[i]
    
    # --- NALIN DESIRED INVESTMENT (eq. 11 analog) ---------------------------
    # Target capital level = ratio * lagged GDP
    K_target_Br[i] <- k_target_Br[i] * y_Br[i-1]
    # Desired gross investment: closes a fraction g_inv of the capital gap,
    # plus depreciation allowances to maintain existing capital
    inv_d_Br[i] <- g_inv_Br * (K_target_Br[i] - k_Br[i-1]) + da_Br[i]
    inv_d_Br[i] <- max(inv_d_Br[i], 0)   # no negative gross investment
    
    # Blend Nalin desired investment with original gamma equation:
    inv_gamma_Br <- (gamma0_Br + gamma1_Br*inv_Br[i-1] +
                       gamma2_Br*gov_tot_Br[i-1]) * (1 - d_t_Br[i-1])
    inv_Br[i] <- inv_nalin_weight * inv_d_Br[i] +
      (1 - inv_nalin_weight) * inv_gamma_Br
    
    # RoW investment unchanged (keep original gamma equation; we're focused on Br)
    inv_RoW[i] <- (gamma0_RoW + gamma1_RoW*inv_RoW[i-1] +
                     gamma2_RoW*gov_tot_RoW[i-1]) * (1 - d_t_RoW[i-1])
    
    gifdif_Br <- max(r_l_con_br - r_l_RoW_br, 0) *
      (l_firm_Br[i-1] * share_gr_b_lag)
    inv_gr_Br_t[i] <- (chi1_Br*gov_gr_Br[i] + chi2_Br*y_Br[i] +
                         chi3_Br*d_t_Br[i-1] +
                         chi4_Br*gifdif_Br) * (1 - d_t_Br[i-1])
    gifdif_RoW <- max(r_l_con_gn - r_l_RoW_gn, 0) *
      (l_firm_RoW[i-1] * share_gr_g_lag)
    inv_gr_RoW_t[i] <- (chi1_RoW*gov_gr_RoW[i] + chi2_RoW*y_RoW[i] +
                          chi3_RoW*d_t_RoW[i-1] +
                          chi4_RoW*gifdif_RoW) * (1 - d_t_RoW[i-1])
    inv_gr_Br[i]   <- min(inv_gr_Br_t[i], inv_Br[i])
    inv_gr_RoW[i]   <- min(inv_gr_RoW_t[i], inv_RoW[i])
    inv_con_Br[i]  <- inv_Br[i] - inv_gr_Br[i]
    inv_con_RoW[i]  <- inv_RoW[i] - inv_gr_RoW[i]
    
    k_gr_Br[i]  <- k_gr_Br[i-1]  + inv_gr_Br[i]  - da_gr_Br[i]
    k_con_Br[i] <- k_con_Br[i-1] + inv_con_Br[i] - da_con_Br[i]
    k_Br[i]     <- k_gr_Br[i] + k_con_Br[i]
    k_gr_RoW[i]  <- k_gr_RoW[i-1]  + inv_gr_RoW[i]  - da_gr_RoW[i]
    k_con_RoW[i] <- k_con_RoW[i-1] + inv_con_RoW[i] - da_con_RoW[i]
    k_RoW[i]     <- k_gr_RoW[i] + k_con_RoW[i]
    
    # --- Total Br firm financing need (Nalin eq. 16 analog) -----------------
    # The traditional Carnevali line computes l_firm_Br as the residual of
    # the firm budget constraint. Under Nalin, this same total need is then
    # SPLIT between domestic loans (l_firm_Br) and FC loans (l_fc_Br * xr).
    #
    # Step 1: compute total financing need in Br currency
    lcd_Br[i] <- l_firm_Br[i-1] + l_fc_Br[i-1] * xr_Br[i] +
      inv_Br[i] - af_Br[i] - fu_Br[i] -
      (e_RoWBr_s[i] - e_RoWBr_s[i-1]) -
      (e_BrBr_s[i] - e_BrBr_s[i-1])
    # Step 2: domestic-loan share (Nalin eq. 8 analog)
    ldom_Br[i] <- lambda_dom_0 + lambda_dom_1 * lcd_Br[i]
    ldom_Br[i] <- max(ldom_Br[i], 0)   # no negative domestic borrowing
    # Step 3: domestic loans = the share; total l_firm_Br
    l_firm_Br[i] <- ldom_Br[i]
    # Step 4: FC borrowing = residual of total need (in Br currency)
    fc_residual_Br[i] <- lcd_Br[i] - ldom_Br[i]
    # Step 5: convert FC residual back to FC units (divide by xr) and update
    # FC loan stock. This OVERRIDES the placeholder l_fc_Br_d[i] set earlier
    # in section IIIb. The structure is: placeholder rolls over by default;
    # if Nalin investment block is active (inv_nalin_weight > 0), the
    # residual mechanism overrides.
    if (inv_nalin_weight > 0) {
      fc_demand_fc_units <- fc_residual_Br[i] / xr_Br[i]
      fc_demand_fc_units <- max(fc_demand_fc_units, 0)   # no negative FC debt
      l_fc_Br_d[i] <- fc_demand_fc_units
      if (fc_supply_mode == "passive") {
        l_fc_Br_s[i] <- l_fc_Br_d[i]
      } else {
        l_fc_Br_s[i] <- l_fc_Br_d[i]   # branch reserved for rationing
      }
      l_fc_Br[i]       <- l_fc_Br_s[i]
      delta_l_fc_Br[i] <- l_fc_Br[i] - l_fc_Br[i-1]
      # Recompute interest cost based on updated stock
      int_fc_Br[i] <- r_l_fc * l_fc_Br[i-1] * xr_Br[i]
    }
    # RoW firm loans unchanged (no FC borrowing channel for RoW)
    l_firm_RoW[i] <- l_firm_RoW[i-1] + inv_RoW[i] - af_RoW[i] - fu_RoW[i] -
      (e_BrRoW_s[i] - e_BrRoW_s[i-1]) -
      (e_RoWRoW_s[i] - e_RoWRoW_s[i-1])
    
    # -------------------------- IV. TRADE ---------------------------------- #
    # When use_endog_elast = TRUE, Br's import-export elasticities respond to
    # its RoW-capital share (structuralist Thirlwall channel). Intercepts
    # are re-anchored at period-1 to preserve initial trade levels — only
    # the *evolution* of trade differs from the baseline.
    
    yb_i    <- max(y_Br[i], 1e-6)
    yg_i    <- max(y_RoW[i], 1e-6)
    xrb_lag <- max(xr_Br[i-1], 1e-6)
    
    if (use_endog_elast) {
      share_gr_b_now <- if (k_Br[i-1] > 0) k_gr_Br[i-1]/k_Br[i-1] else 0.144
      eps_x_b <- zeta0_par  + zeta1_par * share_gr_b_now
      eta_m_b <- phi0_m_par - phi1_m_par * share_gr_b_now
      # Re-anchor intercepts so period-1 trade levels match initial conditions.
      # At i=1: y_Br=3.00, y_RoW=97.00, xr=1, x_Br=0.8406, im_Br=0.8407.
      # Solve for X0_eff, M0_eff such that x_Br[1] equals initial.
      X0_eff <- log(0.8406) - eps_x_b * log(97.000)
      M0_eff <- log(0.8407) - eta_m_b * log(3.000)
    } else {
      eps_x_b <- eps2
      eta_m_b <- mu2_par
      X0_eff  <- eps0
      M0_eff  <- mu0
    }
    
    x_Br[i]  <- exp(X0_eff - eps1*log(xrb_lag) + eps_x_b*log(yg_i)) *
      (1 - ad_exp*d_t_Br[i-1])
    im_Br[i] <- exp(M0_eff + mu1*log(xrb_lag) + eta_m_b*log(yb_i)) *
      (1 + ad_im *d_t_RoW[i-1])
    x_RoW[i]  <- im_Br[i] * xr_Br[i]
    im_RoW[i] <- x_Br[i]  * xr_Br[i]
    tb_Br[i] <- x_Br[i] - im_Br[i]
    tb_RoW[i] <- x_RoW[i] - im_RoW[i]
    
    # ------------------- V. PORTFOLIO DEMANDS ------------------------------ #
    
    b_BrBr_d[i] <- v_Br[i]*(lambda10 + lambda11*r_Br - lambda12*r_RoW -
                              lambda13*r_e_Br[i-1] - lambda14*r_e_RoW[i-1])
    b_BrRoW_d[i] <- v_Br[i]*(lambda20 - lambda21*r_Br + lambda22*r_RoW -
                               lambda23*r_e_Br[i-1] - lambda24*r_e_RoW[i-1])
    e_BrRoW_d[i] <- 0
    e_BrBr_d[i] <- v_Br[i]*((lambda90 + lambda70) - lambda91*r_Br - lambda92*r_RoW +
                              lambda93*r_e_Br[i-1] - lambda94*r_e_RoW[i-1])
    b_RoWRoW_d[i] <- v_RoW[i]*(lambda40 - lambda41*r_Br + lambda42*r_RoW -
                                 lambda43*r_e_Br[i-1] - lambda44*r_e_RoW[i-1])
    if (use_confidence) {
      tb_ratio   <- if (y_Br[i-1] > 0) tb_Br[i-1] / y_Br[i-1] else 0
      res_ratio  <- or_Br[i-1] / or_target_br - 1
      conf_br    <- exp(kappa_tb * tb_ratio + kappa_res * res_ratio)
      conf_br    <- max(min(conf_br, 2), 0.1)   # clamp 0.1–2 for stability
    } else {
      conf_br    <- 1
    }
    # Endogenous lambda50: size-proportional foreign allocation (Approach B).
    # As Br's GDP share rises, RoW wants more Br bills; confidence multiplies.
    Y_tot_lag   <- y_Br[i-1] + y_RoW[i-1]
    Br_share_w  <- if (Y_tot_lag > 0) y_Br[i-1] / Y_tot_lag else 0.03
    lambda50_t  <- lambda50_bar * Br_share_w * conf_br
    b_RoWBr_d[i] <- v_RoW[i]*(lambda50_t + lambda51*r_Br - lambda52*r_RoW -
                                lambda53*r_e_Br[i-1] - lambda54*r_e_RoW[i-1])
    e_RoWBr_d[i] <- 0
    e_RoWRoW_d[i] <- v_RoW[i]*((lambda100 + lambda80) - lambda101*r_Br - lambda102*r_RoW -
                                 lambda103*r_e_Br[i-1] + lambda104*r_e_RoW[i-1])
    
    # --------- VI. FINANCIAL SUPPLIES, EQUITY ISSUANCE, RETURNS ------------ #
    
    b_BrBr_s[i] <- b_BrBr_d[i]
    b_RoWRoW_s[i] <- b_RoWRoW_d[i]
    b_BrRoW_s[i] <- b_BrRoW_d[i] * xr_Br[i]
    e_BrRoW_s[i] <- 0
    e_RoWRoW_s[i] <- e_RoWRoW_d[i]
    e_RoWBr_s[i] <- 0
    e_BrBr_s[i] <- e_BrBr_d[i]
    e_Br_real_s[i] <- e_Br_real_s[i-1] + xi_Br*inv_Br[i-1]/max(p_e_Br[i-1],1e-6)
    e_RoW_real_s[i] <- e_RoW_real_s[i-1] + xi_RoW*inv_RoW[i-1]/max(p_e_RoW[i-1],1e-6)
    p_e_Br[i]      <- e_BrBr_d[i] / max(e_Br_real_s[i], 1e-6)
    p_e_RoW[i]      <- e_RoWRoW_d[i] / max(e_RoW_real_s[i], 1e-6)
    r_e_Br_t[i]    <- f_Br[i] / max(e_Br_real_s[i-1]*p_e_Br[i-1], 1e-6)
    r_e_RoW_t[i]    <- f_RoW[i] / max(e_RoW_real_s[i-1]*p_e_RoW[i-1], 1e-6)
    r_e_Br[i]      <- (1 - pi_dy_Br)*r_Br + pi_dy_Br*r_e_Br_t[i]
    r_e_RoW[i]      <- (1 - pi_dy_RoW)*r_RoW + pi_dy_RoW*r_e_RoW_t[i]
    
    # ----------- VII. BANK BALANCE SHEETS AND ADVANCES --------------------- #
    # Cash + deposits split (paper structure): HH residual wealth is split
    # between bank deposits (depsh) and cash held by HH (1 - depsh).
    
    residual_Br <- v_Br[i] - b_BrBr_s[i] - e_BrBr_s[i] -
      (b_BrRoW_s[i] + e_BrRoW_s[i]) * xr_RoW[i]
    dep_Br[i]    <- residual_Br * depsh_Br
    h_Br_h[i]    <- residual_Br - dep_Br[i]
    
    residual_RoW <- v_RoW[i] - b_RoWRoW_s[i] - e_RoWRoW_s[i] -
      (b_RoWBr_s[i] + e_RoWBr_s[i]) * xr_Br[i]
    dep_RoW[i]    <- residual_RoW * depsh_RoW
    h_RoW_h[i]    <- residual_RoW - dep_RoW[i]
    
    dep_bank_Br[i] <- dep_Br[i]
    dep_bank_RoW[i] <- dep_RoW[i]
    l_s_Br[i]      <- l_firm_Br[i]
    l_s_RoW[i]      <- l_firm_RoW[i]
    b_Br_bank_not[i] <- dep_bank_Br[i] - l_s_Br[i]
    b_RoW_bank_not[i] <- dep_bank_RoW[i] - l_s_RoW[i]
    if (is.na(b_Br_bank_not[i])) b_Br_bank_not[i] <- 0
    if (is.na(b_RoW_bank_not[i])) b_RoW_bank_not[i] <- 0
    z_Br[i] <- if (b_Br_bank_not[i] > 0) 1 else 0
    z_RoW[i] <- if (b_RoW_bank_not[i] > 0) 1 else 0
    b_Br_bank[i] <- b_Br_bank_not[i] * z_Br[i]
    b_RoW_bank[i] <- b_RoW_bank_not[i] * z_RoW[i]
    a_d_Br[i] <- -b_Br_bank_not[i] * (1 - z_Br[i])
    a_d_RoW[i] <- -b_RoW_bank_not[i] * (1 - z_RoW[i])
    a_s_Br[i] <- a_d_Br[i] ; a_s_RoW[i] <- a_d_RoW[i]
    f_bank_Br[i] <- r_Br*b_Br_bank[i-1] + r_l_Br*l_s_Br[i-1]
    f_bank_RoW[i] <- r_RoW*b_RoW_bank[i-1] + r_l_RoW*l_s_RoW[i-1]
    
    # ----------- VIII. CENTRAL BANK AND GOVERNMENT (G&L floating) ---------- #
    
    h_Br_s[i]         <- h_Br_h[i]
    h_RoW_s[i]         <- h_RoW_h[i]
    b_cb_BrBr_s[i] <- h_Br_s[i] - a_s_Br[i]
    b_cb_RoWRoW_s[i] <- h_RoW_s[i] - a_s_RoW[i]
    f_cb_Br[i]        <- r_Br * b_cb_BrBr_s[i-1]
    f_cb_RoW[i]        <- r_RoW * b_cb_RoWRoW_s[i-1]
    
    gov_con_Br[i] <- 0.006054 + 1.003373 * gov_con_Br[i-1]
    gov_con_RoW[i] <- 0.195770 + 1.003373 * gov_con_RoW[i-1]
    gov_gr_Br[i]  <- 0.0795
    gov_gr_RoW[i]  <- if (run_s5 && i >= s5_start) 2.5703 * (1 + s5_uplift) else 2.5703
    gov_tot_Br[i] <- gov_con_Br[i] + gov_gr_Br[i]
    gov_tot_RoW[i] <- gov_con_RoW[i] + gov_gr_RoW[i]
    
    b_Br_s[i] <- b_Br_s[i-1] + gov_tot_Br[i] + r_Br*b_Br_s[i-1] -
      t_Br[i] - f_cb_Br[i]
    b_RoW_s[i] <- b_RoW_s[i-1] + gov_tot_RoW[i] + r_RoW*b_RoW_s[i-1] -
      t_RoW[i] - f_cb_RoW[i]
    b_RoWBr_s[i] <- b_Br_s[i] - b_BrBr_s[i] - b_cb_BrBr_s[i] -
      b_Br_bank[i]
    
    # --------------- IX. EXCHANGE RATE CLOSURE (Bortz 2014 style) ---------- #
    # Following Bortz (2014), the closure is symmetric in structure across
    # regimes — what differs is which variable is residual:
    #   FIXED: xr_Br = 1 fixed; Br CB absorbs the bill-market gap as a
    #          residual line; reserves accumulate the BoP per Section XV.
    #   GL:    Br CB reserves held fixed; xr_Br adjusts to clear the Br-bill
    #          market for RoW HHs.
    if (fx_closure == "GL") {
      xr_RoW_new <- if (abs(b_RoWBr_d[i]) > 1e-6)
        b_RoWBr_s[i] / b_RoWBr_d[i] else 1
      if (is.na(xr_RoW_new) || !is.finite(xr_RoW_new) ||
          xr_RoW_new < 0.1 || xr_RoW_new > 10) xr_RoW_new <- 1
      xr_RoW[i] <- (1 - xr_relax) * xr_RoW[i] + xr_relax * xr_RoW_new
      xr_Br[i] <- 1 / xr_RoW[i]
    } else if (fx_closure == "FIXED") {
      # Peg at 1. Br CB absorbs bill-market gap as a clean residual:
      # foreign HH demand is honoured (b_RoWBr_s = b_RoWBr_d), and CB
      # picks up whatever is left of Br bill supply. The reserve change
      # is recorded automatically by the BoP identity in Section XV.
      xr_RoW[i] <- 1
      xr_Br[i]  <- 1
      if (use_confidence) {
        b_RoWBr_s[i]    <- b_RoWBr_d[i]
        # Br CB residually holds what nobody else wants
        b_cb_BrBr_s[i]  <- b_Br_s[i] - b_BrBr_s[i] - b_RoWBr_s[i] -
          b_Br_bank[i]
      }
    }
    
    # ------------------- X. ECOSYSTEM: MATERIAL FLOWS ---------------------- #
    
    y_mat_Br[i] <- mu_Br[i] * y_Br[i]
    y_mat_RoW[i] <- mu_RoW[i] * y_RoW[i]
    dis_Br[i]   <- mu_Br[i] * (da_Br[i] + zeta_Br * dc_Br[i-1])
    dis_RoW[i]   <- mu_RoW[i] * (da_RoW[i] + zeta_RoW * dc_RoW[i-1])
    rec_Br[i]   <- rho_Br * dis_Br[i]
    rec_RoW[i]   <- rho_RoW * dis_RoW[i]
    mat_Br[i]   <- y_mat_Br[i] - rec_Br[i]
    mat_RoW[i]   <- y_mat_RoW[i] - rec_RoW[i]
    dc_Br[i]    <- dc_Br[i-1] + cons_Br[i] - tb_Br[i] - zeta_Br*dc_Br[i-1]
    dc_RoW[i]    <- dc_RoW[i-1] + cons_RoW[i] - tb_RoW[i] - zeta_RoW*dc_RoW[i-1]
    k_se_Br[i]  <- k_se_Br[i-1] + y_mat_Br[i] - dis_Br[i]
    k_se_RoW[i]  <- k_se_RoW[i-1] + y_mat_RoW[i] - dis_RoW[i]
    wa_Br[i]    <- mat_Br[i] - (k_se_Br[i] - k_se_Br[i-1])
    wa_RoW[i]    <- mat_RoW[i] - (k_se_RoW[i] - k_se_RoW[i-1])
    
    conv_m_Br[i] <- sigma_m_Br * res_m_Br[i-1]
    conv_m_RoW[i] <- sigma_m_RoW * res_m_RoW[i-1]
    res_m_Br[i]  <- res_m_Br[i-1] - conv_m_Br[i]
    res_m_RoW[i]  <- res_m_RoW[i-1] - conv_m_RoW[i]
    k_m_Br[i]    <- k_m_Br[i-1] + conv_m_Br[i] - mat_Br[i]
    k_m_RoW[i]    <- k_m_RoW[i-1] + conv_m_RoW[i] - mat_RoW[i]
    k_m[i]          <- k_m_Br[i] + k_m_RoW[i]
    res_m[i]        <- res_m_Br[i] + res_m_RoW[i]
    cen_Br[i]    <- emis_Br[i] / car
    cen_RoW[i]    <- emis_RoW[i] / car
    o2_Br[i]     <- emis_Br[i] - cen_Br[i]
    o2_RoW[i]     <- emis_RoW[i] - cen_RoW[i]
    
    # ------------------- XI. ECOSYSTEM: ENERGY FLOWS ----------------------- #
    
    e_Br[i]    <- epsilon_Br[i] * y_Br[i]
    e_RoW[i]    <- epsilon_RoW[i] * y_RoW[i]
    er_Br[i]   <- eta_Br[i] * e_Br[i]
    er_RoW[i]   <- eta_RoW[i] * e_RoW[i]
    en_Br[i]   <- e_Br[i] - er_Br[i]
    en_RoW[i]   <- e_RoW[i] - er_RoW[i]
    ed_Br[i]   <- er_Br[i] + en_Br[i]
    ed_RoW[i]   <- er_RoW[i] + en_RoW[i]
    conv_e_Br[i] <- sigma_e_Br * res_e_Br[i-1]
    conv_e_RoW[i] <- sigma_e_RoW * res_e_RoW[i-1]
    res_e_Br[i]  <- res_e_Br[i-1] - conv_e_Br[i]
    res_e_RoW[i]  <- res_e_RoW[i-1] - conv_e_RoW[i]
    k_e_Br[i]    <- k_e_Br[i-1] + conv_e_Br[i] - en_Br[i]
    k_e_RoW[i]    <- k_e_RoW[i-1] + conv_e_RoW[i] - en_RoW[i]
    k_e[i]          <- k_e_Br[i] + k_e_RoW[i]
    res_e[i]        <- res_e_Br[i] + res_e_RoW[i]
    
    # ------------------- XII. EMISSIONS AND CLIMATE ------------------------ #
    
    # Land-use emissions: Br endogenous (Y/K + green share with psi exponent), RoW exogenous.
    K_ratio_Br  <- if (k_Br_p1_ref > 0) k_Br[i-1] / k_Br_p1_ref else 1
    Y_ratio_Br  <- y_Br[i] / y_Br_p1_ref
    s_gr_Br_lag <- if (k_Br[i-1] > 0) k_gr_Br[i-1] / k_Br[i-1] else s_gr_Br_0
    land_driver <- (Y_ratio_Br / K_ratio_Br) *
      ((1 - s_gr_Br_lag) / (1 - s_gr_Br_0))
    emis_l_Br[i] <- max(emis_l_Br_0 * (land_driver ^ psi_land), 0)
    emis_l_RoW[i] <- emis_l_RoW[i-1] * (1 - g_land)
    emis_l[i]     <- emis_l_Br[i] + emis_l_RoW[i]
    
    emis_Br[i] <- max(beta0_b_now + beta_Br[i]*en_Br[i], 0)
    emis_RoW[i] <- max(beta0_g_now - 4 + beta_RoW[i]*en_RoW[i], 0)
    emis[i]       <- emis_Br[i] + emis_RoW[i] + emis_l[i]
    
    co2_at[i] <- emis[i] + phi11*co2_at[i-1] + phi21*co2_up[i-1]
    co2_up[i] <- phi12*co2_at[i-1] + phi22*co2_up[i-1] + phi32*co2_lo[i-1]
    co2_lo[i] <- phi23*co2_up[i-1] + phi33*co2_lo[i-1]
    f_ex[i]   <- f_ex[i-1] + fex_incr
    f[i]      <- f2 * log(co2_at[i] / co2_at_pre, base = 2) + f_ex[i]
    temp_at[i] <- temp_at[i-1] +
      t1*(f[i] - (f2/sens)*temp_at[i-1] - t2*(temp_at[i-1] - temp_lo[i-1]))
    temp_lo[i] <- temp_lo[i-1] + t3*(temp_at[i-1] - temp_lo[i-1])
    
    # ------------------- XIII. ECOLOGICAL EFFICIENCY ----------------------- #
    
    wgr_b <- if (k_Br[i] > 0) k_gr_Br[i] / k_Br[i] else 0
    wco_b <- 1 - wgr_b
    mu_Br[i]      <- mu_gr_Br*wgr_b  + mu_con_Br*wco_b
    epsilon_Br[i] <- eps_gr_Br*wgr_b + eps_con_Br*wco_b
    beta_Br[i]    <- (beta_gr_Br*wgr_b + beta_con_Br*wco_b) * cint_b_mult
    eta_Br[i]     <- eta_gr_Br*wgr_b + eta_con_Br*wco_b
    
    wgr_g <- if (k_RoW[i] > 0) k_gr_RoW[i] / k_RoW[i] else 0
    wco_g <- 1 - wgr_g
    mu_RoW[i]      <- mu_gr_RoW*wgr_g  + mu_con_RoW*wco_g
    epsilon_RoW[i] <- eps_gr_RoW*wgr_g + eps_con_RoW*wco_g
    beta_RoW[i]    <- (beta_gr_RoW*wgr_g + beta_con_RoW*wco_g) * cint_g_mult
    eta_RoW[i]     <- eta_gr_RoW*wgr_g + eta_con_RoW*wco_g
    
    depl_m_Br[i] <- if (k_m_Br[i-1] > 0) mat_Br[i]/k_m_Br[i-1] else 0.24
    depl_m_RoW[i] <- if (k_m_RoW[i-1] > 0) mat_RoW[i]/k_m_RoW[i-1] else 0.24
    depl_e_Br[i] <- if (k_e_Br[i-1] > 0) en_Br[i]/k_e_Br[i-1] else 0
    depl_e_RoW[i] <- if (k_e_RoW[i-1] > 0) en_RoW[i]/k_e_RoW[i-1] else 0
    
    # ----------------------- XIV. DAMAGES ---------------------------------- #
    
    Ta <- temp_at[i]
    if (Ta > 0) {
      d_t_Br[i] <- 1 - 1/(1 + dam1_Br*Ta + dam2_Br*Ta^2 + dam3_Br*Ta^dam4_Br)
      d_t_RoW[i] <- 1 - 1/(1 + dam1_RoW*Ta + dam2_RoW*Ta^2 + dam3_RoW*Ta^dam4_RoW)
    } else {
      d_t_Br[i] <- 1 - 1/(1 + dam1_Br*Ta)
      d_t_RoW[i] <- 1 - 1/(1 + dam1_RoW*Ta)
    }
    delta_Br[i] <- delta0_Br + (1 - delta0_Br)*(1 - ad_k_Br)*d_t_Br[i-1]
    delta_RoW[i] <- delta0_RoW + (1 - delta0_RoW)*(1 - ad_k_RoW)*d_t_RoW[i-1]
    
    # ----------------- XV. AGGREGATES & BALANCES --------------------------- #
    # Public sector borrowing requirement
    psbr_Br[i] <- gov_tot_Br[i] + r_Br*b_Br_s[i-1] - t_Br[i] - f_cb_Br[i]
    psbr_RoW[i] <- gov_tot_RoW[i] + r_RoW*b_RoW_s[i-1] - t_RoW[i] - f_cb_RoW[i]
    
    # --- Br BALANCE OF PAYMENTS (Bortz 2014 / Nalin & Yajima 2022 style) ---
    # All items expressed in Br currency. FC stocks/flows multiplied by xr_Br
    # when entering the Br BoP, following the SFC convention that foreign-
    # currency items are revalued at the prevailing exchange rate.
    #
    # CURRENT ACCOUNT — decomposed for transparency:
    #   trade balance (already in Br currency)
    #   + interest received on FC assets (FC units * xr -> Br currency)
    #   - interest paid on Br LC assets held by RoW (already in Br currency)
    #   - interest paid on FC loans (FC units * xr -> Br currency)
    #   + dividends net received (already entering with xr where applicable)
    ca_int_recv_Br[i]    <- xr_Br[i] * (r_RoW * b_BrRoW_s[i-1] +
                                          r_e_RoW[i-1] * e_BrRoW_s[i-1])
    ca_int_paid_lc_Br[i] <- r_Br * b_RoWBr_s[i-1] +
      r_e_Br[i-1] * e_RoWBr_s[i-1]
    ca_int_paid_fc_Br[i] <- int_fc_Br[i]   # already xr-multiplied in line 583
    ca_div_net_Br[i]     <- 0              # placeholder; cross-border dividends zeroed
    cab_Br[i] <- (x_Br[i] - im_Br[i]) +
      ca_int_recv_Br[i] -
      ca_int_paid_lc_Br[i] -
      ca_int_paid_fc_Br[i] +
      ca_div_net_Br[i]
    cab_RoW[i] <- -cab_Br[i]
    
    # CAPITAL ACCOUNT — decomposed by item:
    #   + RoW HHs increase Br LC bill holdings (inflow, Br-currency native)
    #   + RoW HHs increase Br LC equity holdings (inflow)
    #   + Br firms increase FC borrowing (inflow, FC * xr -> Br currency)
    #   - Br HHs increase FC bill holdings (outflow, FC * xr -> Br currency)
    #   - Br HHs increase FC equity holdings (outflow)
    ka_b_in_Br[i]   <- (b_RoWBr_s[i] - b_RoWBr_s[i-1])
    ka_e_in_Br[i]   <- (e_RoWBr_s[i] - e_RoWBr_s[i-1])
    ka_fc_in_Br[i]  <- (l_fc_Br[i]   - l_fc_Br[i-1])   * xr_Br[i]
    ka_b_out_Br[i]  <- (b_BrRoW_s[i] - b_BrRoW_s[i-1]) * xr_Br[i]
    ka_e_out_Br[i]  <- (e_BrRoW_s[i] - e_BrRoW_s[i-1]) * xr_Br[i]
    kabp_Br[i] <- ka_b_in_Br[i] + ka_e_in_Br[i] + ka_fc_in_Br[i] -
      ka_b_out_Br[i] - ka_e_out_Br[i]
    kabp_RoW[i] <- -kabp_Br[i]
    
    # Overall BoP (sums to reserves change under clean accounting)
    bp_Br[i]  <- cab_Br[i] + kabp_Br[i]
    bp_RoW[i] <- cab_RoW[i] + kabp_RoW[i]
    
    # Reserves: residual of BoP (Bortz 2014 convention)
    or_Br[i]  <- or_Br[i-1] + bp_Br[i]
    or_RoW[i] <- or_RoW[i-1] + bp_RoW[i]
    
    # NAFA (Net Acquisition of Financial Assets by domestic private sector)
    nafa_Br[i]  <- psbr_Br[i] + cab_Br[i]
    nafa_RoW[i] <- psbr_RoW[i] + cab_RoW[i]
    
    # BoP identity check (should be ~0 to machine precision under clean SFC)
    bop_resid_Br[i]  <- bp_Br[i]  - (or_Br[i]  - or_Br[i-1])
    bop_resid_RoW[i] <- bp_RoW[i] - (or_RoW[i] - or_RoW[i-1])
    
    q_Br[i]     <- (e_Br_real_s[i]*p_e_Br[i] + l_firm_Br[i]) / max(k_Br[i], 1e-6)
    q_RoW[i]     <- (e_RoW_real_s[i]*p_e_RoW[i] + l_firm_RoW[i]) / max(k_RoW[i], 1e-6)
    lev_f_Br[i] <- l_firm_Br[i] / max(k_Br[i], 1e-6)
    lev_f_RoW[i] <- l_firm_RoW[i] / max(k_RoW[i], 1e-6)
    liq_b_Br[i] <- (a_s_Br[i] + dep_bank_Br[i] - l_s_Br[i]) / max(dep_bank_Br[i], 1e-6)
    liq_b_RoW[i] <- (a_s_RoW[i] + dep_bank_RoW[i] - l_s_RoW[i]) / max(dep_bank_RoW[i], 1e-6)
    
    inv[i] <- inv_RoW[i] + inv_Br[i]*xr_Br[i]
    gov[i] <- gov_con_RoW[i] + gov_con_Br[i]*xr_Br[i] +
      gov_gr_RoW[i]  + gov_gr_Br[i]*xr_Br[i]
    yd[i]  <- yd_Br[i]*xr_Br[i] + yd_RoW[i]
    k[i]   <- k_RoW[i] + k_Br[i]*xr_Br[i]
    v[i]   <- v_Br[i]*xr_Br[i] + v_RoW[i]
    y[i]   <- y_Br[i] + y_RoW[i]
  }
}

################################################################################
# 7) PLOT RESULTS
################################################################################

i_plot <- (burn_in + 1):nPeriods   # discard burn-in transient
periods <- 1:length(i_plot)         # relabel as 1..nPeriods_report for display

old.par <- par(no.readonly = TRUE)
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)

plot(periods, y_Br[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(y_Br[i_plot], y_RoW[i_plot])),
     main = "a) GDP by area", xlab = "Period", ylab = "Trillion USD")
lines(periods, y_RoW[i_plot], lwd = 2, col = "forestgreen")
legend("topleft", c("Br","RoW"),
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
lines(periods, emis_Br[i_plot], col = "orange", lwd = 2)
lines(periods, emis_RoW[i_plot], col = "forestgreen", lwd = 2)
lines(periods, emis_l[i_plot], col = "darkblue", lwd = 2)
legend("topright", c("Total","Br","RoW","Land"),
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

plot(periods, d_t_Br[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(d_t_Br[i_plot], d_t_RoW[i_plot])),
     main = "f) Climate damage ratio", xlab = "Period", ylab = "d_t")
lines(periods, d_t_RoW[i_plot], lwd = 2, col = "forestgreen")
legend("topleft", c("Br","RoW"),
       col = c("orange","forestgreen"), lwd = 2, bty = "n", cex = 0.85)

par(old.par)

################################################################################
# 8) DIAGNOSTICS
################################################################################

cat("\n--- Simulation summary ---\n")
cat(sprintf("Reported periods: %d (after %d burn-in periods discarded)\n",
            nPeriods_report, burn_in))
cat(sprintf("World GDP (final):           %.2f T\n",  y[nPeriods]))
cat(sprintf("  Br GDP:             %.2f T\n",  y_Br[nPeriods]))
cat(sprintf("  RoW GDP:             %.2f T\n",  y_RoW[nPeriods]))
cat(sprintf("CO2 emissions (final):       %.2f Gt/yr\n", emis[nPeriods]))
cat(sprintf("Atmospheric CO2 (final):     %.2f Gt\n", co2_at[nPeriods]))
cat(sprintf("Atmospheric temp (final):    %.3f C\n",  temp_at[nPeriods]))
cat(sprintf("Lower-ocean temp (final):    %.3f C\n",  temp_lo[nPeriods]))
cat(sprintf("Damage ratio Br (final):  %.4f\n",    d_t_Br[nPeriods]))
cat(sprintf("Exchange rate RoW (final): %.4f\n",    xr_RoW[nPeriods]))

# --- SFC identity checks (Bortz 2014 style) ---------------------------------
cat("\n--- BoP identity checks (final period) ---\n")
cat(sprintf("Br  CA + KA - dor : %+.6f  (should be ~0)\n",
            bop_resid_Br[nPeriods]))
cat(sprintf("RoW CA + KA - dor : %+.6f  (should be ~0)\n",
            bop_resid_RoW[nPeriods]))
cat(sprintf("Br  CA: %+.4f (trade %+.4f, +int %+.4f, -int-LC %+.4f, -int-FC %+.4f)\n",
            cab_Br[nPeriods],
            x_Br[nPeriods] - im_Br[nPeriods],
            ca_int_recv_Br[nPeriods],
            ca_int_paid_lc_Br[nPeriods],
            ca_int_paid_fc_Br[nPeriods]))
cat(sprintf("Br  KA: %+.4f (b_in %+.4f, e_in %+.4f, fc_in %+.4f, b_out %+.4f, e_out %+.4f)\n",
            kabp_Br[nPeriods],
            ka_b_in_Br[nPeriods], ka_e_in_Br[nPeriods], ka_fc_in_Br[nPeriods],
            ka_b_out_Br[nPeriods], ka_e_out_Br[nPeriods]))
cat(sprintf("World: CA_Br + CA_RoW = %+.6f  (should be ~0)\n",
            cab_Br[nPeriods] + cab_RoW[nPeriods]))
cat(sprintf("World: KA_Br + KA_RoW = %+.6f  (should be ~0)\n",
            kabp_Br[nPeriods] + kabp_RoW[nPeriods]))

################################################################################
# 9) ADDITIONAL STANDALONE PLOTS
################################################################################

# Re-use i_plot / periods from Section 7 (post-burn-in: 1..nPeriods_report)

# --- Plot 1: FX / gold reserves — Br only ------------------------------------
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)
plot(periods, or_Br[i_plot], type = "l", lwd = 2, col = "orange",
     main = "Br FX reserves",
     xlab = "Period", ylab = "Reserves (T USD)")
abline(h = or_init_Br, lty = 2, col = "grey50")
legend("topleft",
       legend = c("Br", "Br initial"),
       col    = c("orange", "grey50"),
       lwd    = c(2, 1),
       lty    = c(1, 2),
       bty    = "n", cex = 0.85)

# --- Plot 2: CO2 emissions — Br only -----------------------------------------
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)
plot(periods, emis_Br[i_plot], type = "l", lwd = 2, col = "orange",
     main = "Br CO2 emissions",
     xlab = "Period", ylab = "Gt CO2 / yr")
legend("topright",
       legend = "Br",
       col    = "orange",
       lwd    = 2, bty = "n", cex = 0.85)

# --- Plot 3: International flows (Br-focused, except cross-border bills) -----
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1, oma = c(0, 0, 2, 0))

# (a) Br trade flows (real): exports and imports
plot(periods, x_Br[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(x_Br[i_plot], im_Br[i_plot])),
     main = "a) Br trade flows (real)",
     xlab = "Period", ylab = "Trillion USD")
lines(periods, im_Br[i_plot], lwd = 2, col = "deeppink")
legend("topleft",
       legend = c("Br exports", "Br imports"),
       col = c("orange", "deeppink"), lwd = 2, bty = "n", cex = 0.8)

# (b) Trade balance — Br
plot(periods, tb_Br[i_plot], type = "l", lwd = 2, col = "purple",
     main = "b) Trade balance (Br)",
     xlab = "Period", ylab = "Trillion USD")
abline(h = 0, lty = 2, col = "grey50")

# (c) Cross-border bill holdings (both areas — kept bilateral by request)
plot(periods, b_BrRoW_s[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(b_BrRoW_s[i_plot], b_RoWBr_s[i_plot])),
     main = "c) Cross-border bill holdings",
     xlab = "Period", ylab = "Trillion USD")
lines(periods, b_RoWBr_s[i_plot], lwd = 2, col = "forestgreen")
legend("topleft",
       legend = c("Br holdings of RoW bills", "RoW holdings of Br bills"),
       col = c("orange", "forestgreen"), lwd = 2, bty = "n", cex = 0.8)

# (d) Balance of payments components — Br
plot(periods, cab_Br[i_plot], type = "l", lwd = 2, col = "darkblue",
     ylim = range(c(cab_Br[i_plot], kabp_Br[i_plot], bp_Br[i_plot])),
     main = "d) Balance of payments (Br)",
     xlab = "Period", ylab = "Trillion USD")
lines(periods, kabp_Br[i_plot], lwd = 2, col = "deeppink")
lines(periods, bp_Br[i_plot],   lwd = 2, col = "black")
abline(h = 0, lty = 2, col = "grey50")
legend("topleft",
       legend = c("Current account", "Capital account", "Overall BoP"),
       col = c("darkblue", "deeppink", "black"), lwd = 2, bty = "n", cex = 0.8)

mtext("Br international flows",
      outer = TRUE, cex = 1.05, font = 2)
layout(1)  # reset layout so any later plots aren't stuck in 2x2