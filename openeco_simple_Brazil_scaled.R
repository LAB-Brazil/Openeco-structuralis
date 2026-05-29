################################################################################
#
# OPENECO-UNIFIED model — pure post-2017 simulation in R
# Simplified single-household variant of Carnevali, Deleidi, Pariboni,
# Veronese Passarella (2021).
#
# ---------------------------------------------------------------------------
# BORTZ-FAITHFUL FIXED-ER BILL-CLEARING PATCH (see the five "# >>> BORTZ FIX"
# blocks below). Summary of what changed vs. the original:
#   (1) New variable b_cb_BrRoW_s = Br CB holdings of RoW (US) bills. Under the
#       peg this is the residual of the RoW bill market (Bortz eq. 72uFX),
#       which CLOSES that market (the original left it open -> latent hole).
#   (2) Reserves or_Br ARE that holding (set in the FIXED closure, NOT cumulated
#       as the BoP in Section XV). This turns the BoP identity check from a
#       tautology into a genuine test.
#   (3) Br CB DOMESTIC bill holding from its balance sheet (Bortz eq. 70a),
#       toggled by cb_domestic_rule. Domestic-market clearing then becomes the
#       redundant equation (Bortz 71a), reported in Section 8.
#   (4) Current account now includes interest earned ON RESERVES (Bortz 75a),
#       not just on HH holdings of RoW bills.
#   (5) New parameter b_RoW_other: exogenous institutional/MDB placeholder
#       holder of RoW bills. Calibrated so the period-1 Br CB residual equals
#       the balance-sheet-consistent reserve level. It only shifts the LEVEL of
#       reserves (its first difference is 0), so it does not affect the BoP
#       identity. Set to 0 to recover the pure two-country Bortz mechanism.
# ---------------------------------------------------------------------------
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

nPeriods_report <- 100  # periods to report/plot (2017-2060 -> initially 44 periods) -> Suggestion 80 periods (reserves of energy are still positive)
# Burn-in: discarded from start so GL xr dynamics + augmented-model
# (FC loans, Bortz BoP, Nalin investment) can converge to a self-consistent
# state before reporting. Methodological note: this is a workaround for the
# fact that we have not yet re-solved initial conditions under the augmented
# equations. Nalin, Bortz, and Souza & Silva all use steady-state-solved IC
# instead of burn-in. To be addressed before thesis finalisation; for now,
# read trajectory shapes and policy differences rather than absolute levels.
burn_in         <- 15
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
  "b_cb_BrRoW_s",                                  # >>> BORTZ FIX (1): Br CB reserves held as RoW bills
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
  "cb_nw_Br",                                      # >>> BORTZ FIX (3): implied Br CB net worth diagnostic
  "br_bill_resid",                                 # >>> BORTZ FIX (3): redundant Br-bill-market check (Bortz 71a)
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
  "int_fc_Br",
  "l_fc_gr_Br","l_fc_con_Br",
  "delta_l_fc_gr_Br","delta_l_fc_con_Br",
  "int_fc_gr_Br","int_fc_con_Br",
  "psi_gr_share_Br"
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
chi4_Br      <- 0.2 ; chi4_RoW      <- 0.2
mdb_subsidy_br  <- 0   ; mdb_subsidy_gn  <- 0   # subtracted from r_l_RoW
xi_sub_Br    <- 1   ; xi_sub_RoW    <- 1   # additional multiplier on r_l_RoW

ret_Br   <- 0.02   ; ret_RoW   <- 0.02
pi_dy_Br <- 0.00555; pi_dy_RoW <- 0.00555

# --- Q-channel: Nalin & Yajima (2022) leverage feedback in target capital ---
q_channel_on    <- TRUE
q_target_cap_on <- TRUE
s_gr_target_Br  <- 0.40   # green capital share at which Q feedback freezes
k0_Br <- 1.67 ; k0_RoW <- 1.60   # K/Y target at baseline; Br ~ observed 2016 ratio
k1_Br <- 0.20 ; k1_RoW <- 0.20   # Q feedback strength; moderate Minsky channel

# --- Nalin investment-financing parameters ---------------------------------
g_inv_Br      <- 0.30     # fraction of capital gap closed per period
lambda_dom_0  <- 0        # autonomous component
lambda_dom_1  <- 0.87 #87    # share of total financing met by domestic loans
inv_nalin_weight <- 1.0
xi_Br    <- 0.01   ; xi_RoW    <- 0.01

eps0 <- -2.1 ; eps1 <- 0.5 ; eps2 <- 1.228
mu0  <- -2.1 ; mu1  <- 0.5 ; mu2_par  <- 1.228

# --- Endogenous trade elasticities (Souza & Silva / structuralist channel) ---
use_endog_elast <- TRUE
zeta0_par   <- 0.85   ; zeta1_par   <- 0.45    # exports: weak at low share_gr
phi0_m_par  <- 1.10   ; phi1_m_par  <- 0.35    # imports: strong at low share_gr

or_target_br   <- 3.9746  # reserve target Br (scaled: 50 * s_Br)
or_target_row  <- 128.5109 # reserve target RoW (scaled: 50 * s_RoW)

lambda10 <- 0.14707 ; lambda11 <- 1 ; lambda12 <- 1 ; lambda13 <- 0    ; lambda14 <- 0
lambda20 <- 0.04902 ; lambda21 <- 1 ; lambda22 <- 1 ; lambda23 <- 0    ; lambda24 <- 0
lambda40 <- 0.14707 ; lambda41 <- 1 ; lambda42 <- 1 ; lambda43 <- 0    ; lambda44 <- 0
lambda50 <- 0.022
lambda51 <- 1 ; lambda52 <- 0.25 ; lambda53 <- 0    ; lambda54 <- 0
lambda70 <- 0.02451 ; lambda71 <- 0 ; lambda72 <- 0 ; lambda73 <- 0.01 ; lambda74 <- 0.01
lambda80 <- 0.02451 ; lambda81 <- 0 ; lambda82 <- 0 ; lambda83 <- 0.01 ; lambda84 <- 0.01
lambda90 <- 0.02451 ; lambda91 <- 0 ; lambda92 <- 0 ; lambda93 <- 0.01 ; lambda94 <- 0.01
lambda100<- 0.02451 ; lambda101<- 0 ; lambda102<- 0 ; lambda103<- 0.01 ; lambda104<- 0.01

# Bank-deposit share of HH liquid wealth (rest is cash held by HH)
depsh_Br <- 0.7  ; depsh_RoW <- 0.7

r_Br     <- 0.03 ; r_RoW     <- 0.03
r_l_Br   <- 0.035; r_l_RoW   <- 0.035

# --- Foreign-currency loans to Br firms (from RoW banks) -------------------
r_l_fc     <- 0.085     # FC corporate borrowing rate, Brazil 2016 anchor (legacy aggregate)
fc_supply_mode <- "passive"  # "passive" = supply = demand; "rationed" later

# --- Split FC borrowing: green vs. conventional ----------------------------
r_l_fc_gr  <- r_l_fc    # green FC borrowing rate (initially identical)
r_l_fc_con <- r_l_fc    # conventional FC borrowing rate
kappa_fc   <- 10        # 1 pp rate gap -> 10 pp shift in green share
chi5_Br    <- 1
shock_mdb       <- FALSE
mdb_green_cut   <- 0.02   # -200 bp on r_l_fc_gr when activated
t_shock_mdb     <- 22     # period of activation (reported scale)

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

psi_land       <- 1.0    # sensitivity dial for land emissions (default 1)
emis_l_Br_0    <- 1.004  # Br land emissions 2016 (Agric 567 + LULUCF 437 kt)
emis_l_RoW_0   <- 2.491  # RoW land emissions = global 3.495 - Br 1.004

g_beta_Br <- 0.02
g_beta_RoW <- 0.04
greening_starts <- 4 + burn_in
beta0_base_Br  <- 0.27444   # 3.4524 * s_Br
beta0_base_RoW <- 8.87342   # 3.4524 * s_RoW

xr_relax <- 0.15
nIter <- 150

# --- FX closure -----------------------------------------------------------
# "GL"    = Godley-Lavoie floating; xr clears Br bill market every period.
# "FIXED" = peg xr_RoW = 1; FX reserves (or_Br, or_RoW) absorb BP.
fx_closure <- "FIXED"
or_init_Br  <- 3.9746   # initial Br reserves (scaled: 50 * s_Br) -- LEGACY anchor; see note below
or_init_RoW <- 128.5109 # initial RoW reserves (scaled: 50 * s_RoW)

# >>> BORTZ FIX (5): RoW-bill residual / reserve mechanism (Bortz 2014 eq. 72uFX)
# ---------------------------------------------------------------------------
# Under the peg the Br CB is the residual holder of RoW (US) bills, and that
# holding IS its reserves. b_RoW_other is an EXOGENOUS placeholder for
# institutional / MDB / other-foreign holders of RoW bills not yet modelled as
# a sector. It only shifts the LEVEL of reserves (first difference 0), so it
# leaves the BoP identity Delta(reserves)=BoP untouched.
#
# It is calibrated so the period-1 Br CB residual equals the BALANCE-SHEET-
# CONSISTENT reserve level, i.e. or_Br[1] = cash - advances - domestic bills
#                                         = 3.1663 - 0 - 0.5971 = 2.5692.
# (Your legacy or_init_Br = 3.9746 was NOT balance-sheet-consistent: it exceeds
#  cash, which would force the CB to hold NEGATIVE domestic bills. The patch
#  uses 2.5692 instead. The 1.4 gap is the original CB-balance-sheet hole that
#  closing the RoW bill market surfaces.)
#
# Raw RoW-bill residual at IC with b_RoW_other = 0:
#   348.0511 - 66.4761 - 157.0351 - 102.3769 - 0.6854 = 21.4776  (~7x Br GDP!)
# Target reserve level: 2.5692  =>  b_RoW_other = 21.4776 - 2.5692 = 18.9084.
# Set b_RoW_other = 0 to recover the PURE two-country Bortz mechanism (the Br
# CB then absorbs the entire ~21.5 residual; watch the CB balance sheet break).
b_RoW_other <- 18.9084

# >>> BORTZ FIX (3): how the Br CB sets its DOMESTIC (peso) bill holding under FIXED
#   "balance_sheet" (Bortz 70a, faithful): b_cb_BrBr_s = cash - advances -
#       reserves. Domestic-market clearing is then the REDUNDANT check (71a).
#       NOTE: with b_RoWBr_d LOCKED at IC (your eq. 75 hack), this redundant
#       check will NOT stay at zero -- the lock makes the domestic market fail
#       to clear once RoW wealth grows. br_bill_resid reports the gap.
#   "residual" (pragmatic): b_cb_BrBr_s = Br-bill-market residual (as in your
#       original). Both bill markets then clear by construction and the CB net
#       worth (cb_nw_Br) absorbs the difference. Use this while b_RoWBr_d is
#       locked; it keeps the model clean at the cost of an endogenous CB NW.
cb_domestic_rule <- "balance_sheet"

# --- Shock toggles --------------------------------------------------------
shock_fed     <- FALSE
fed_uplift    <- 0.02     # +200 bp on RoW base rate
t_shock_fed   <- 22       # period at which the shock hits (reported scale)

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

v[1]          <- 462.45     # World wealth — recomputed below from sub-totals
v_Br[1]    <- 11.9069
v_RoW[1]    <- 450.5351   # was 452.0498
b_Br_s[1]  <- 1.7000   ; b_RoW_s[1]  <- 348.0511
b_BrBr_d[1] <- 0.3247  ; b_BrBr_s[1] <- 0.3247
b_BrRoW_d[1] <- 0.6853 ; b_BrRoW_s[1] <- 0.6854
b_RoWBr_d[1] <- 0.1000 ; b_RoWBr_s[1] <- 0.1000
b_RoWRoW_d[1] <- 66.4761 ; b_RoWRoW_s[1] <- 66.4761
b_Br_bank[1]   <- 0.6782 ; b_RoW_bank[1]   <- 157.0351
b_cb_BrBr_s[1] <- 0.5971 ; b_cb_RoWRoW_s[1] <- 102.3769

e_Br_real_s[1] <- 0.4613 ; e_RoW_real_s[1] <- 14.9150
e_BrBr_d[1] <- 0.3427 ; e_BrBr_s[1] <- 0.3427
e_BrRoW_d[1] <- 0     ; e_BrRoW_s[1] <- 0
e_RoWBr_d[1] <- 0     ; e_RoWBr_s[1] <- 0
e_RoWRoW_d[1] <- 11.0802 ; e_RoWRoW_s[1] <- 11.0802
p_e_Br[1]      <- 1.486 ; p_e_RoW[1]      <- 1.486
r_e_Br[1]      <- 0.0352; r_e_RoW[1]      <- 0.0352

h_Br_s[1]  <- 3.1663 ; h_RoW_s[1]  <- 102.3769
h_Br_h[1]  <- 3.1663 ; h_RoW_h[1]  <- 102.3769
dep_Br[1]  <- 7.3879 ; dep_RoW[1]  <- 238.8760
dep_bank_Br[1] <- 7.3879 ; dep_bank_RoW[1] <- 238.8760
l_firm_Br[1]   <- 2.5312 ; l_firm_RoW[1]   <- 81.8434
l_s_Br[1]      <- 2.5312 ; l_s_RoW[1]      <- 81.8434
l_fc_Br[1]    <- 0.40    ; l_fc_Br_d[1]   <- 0.40
l_fc_Br_s[1]  <- 0.40    ; delta_l_fc_Br[1] <- 0
int_fc_Br[1]  <- r_l_fc * 0.40   # interest cost on initial FC stock (FC units)
l_fc_gr_Br[1]    <- 0.40 * (0.688 / 4.793)
l_fc_con_Br[1]   <- 0.40 - l_fc_gr_Br[1]
delta_l_fc_gr_Br[1]  <- 0
delta_l_fc_con_Br[1] <- 0
int_fc_gr_Br[1]      <- r_l_fc_gr  * l_fc_gr_Br[1]
int_fc_con_Br[1]     <- r_l_fc_con * l_fc_con_Br[1]
psi_gr_share_Br[1]   <- 0.5   # placeholder; recomputed from t=2
xr_Br[1] <- 1 ; xr_RoW[1] <- 1
or_Br[1] <- or_init_Br ; or_RoW[1] <- or_init_RoW

# >>> BORTZ FIX (1)+(2): Br CB reserves = RoW-bill-market residual (Bortz 72uFX)
# at IC. With b_RoW_other = 18.9084 this gives or_Br[1] = 2.5692, which is the
# balance-sheet-consistent reserve level (cash 3.1663 - advances 0 - domestic
# bills 0.5971). It supersedes the legacy or_init_Br = 3.9746 set just above.
b_cb_BrRoW_s[1] <- b_RoW_s[1] - b_RoWRoW_s[1] - b_RoW_bank[1] -
  b_cb_RoWRoW_s[1] - b_BrRoW_s[1] - b_RoW_other
or_Br[1] <- b_cb_BrRoW_s[1] * xr_RoW[1]

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

y_Br_p1_ref <- y_Br[1]
k_Br_p1_ref <- k_Br[1]
KY0_Br      <- k_Br[1] / y_Br[1]
s_gr_Br_0   <- k_gr_Br[1] / k_Br[1]

################################################################################
# 6) SIMULATION LOOP
################################################################################

xr_guard_fired <- logical(nPeriods)

for (i in 2:nPeriods) {
  
  if (shock_fed && i == burn_in + t_shock_fed) {
    r_RoW       <- r_RoW       + fed_uplift
    r_l_fc      <- r_l_fc      + fed_uplift
    r_l_fc_gr   <- r_l_fc_gr   + fed_uplift
    r_l_fc_con  <- r_l_fc_con  + fed_uplift
  }
  
  if (shock_mdb && i == burn_in + t_shock_mdb) {
    r_l_fc_gr <- r_l_fc_gr - mdb_green_cut
  }
  
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
    
    share_gr_b_lag <- if (k_Br[i-1] > 0) k_gr_Br[i-1] / k_Br[i-1] else 0
    r_l_RoW_br   <- max(r_l_Br * xi_sub_Br - mdb_subsidy_br, 0)
    r_l_con_br     <- r_l_Br
    interest_Br <- r_l_RoW_br * (l_firm_Br[i-1] * share_gr_b_lag) +
      r_l_con_br   * (l_firm_Br[i-1] * (1 - share_gr_b_lag))
    int_fc_cashflow_Br <- r_l_fc_gr  * l_fc_gr_Br[i-1]  * xr_Br[i] +
      r_l_fc_con * l_fc_con_Br[i-1] * xr_Br[i]
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
    
    q_nalin_Br[i] <- (l_firm_Br[i-1] + l_fc_Br[i-1] * xr_Br[i]) /
      max(k_Br[i-1], 1e-6)
    s_gr_Br_now <- if (k_Br[i-1] > 0) k_gr_Br[i-1] / k_Br[i-1] else 0
    q_active_Br[i] <- if (!q_channel_on) 0
    else if (q_target_cap_on && s_gr_Br_now >= s_gr_target_Br) 0
    else 1
    k_target_Br[i] <- k0_Br + q_active_Br[i] * k1_Br * q_nalin_Br[i]
    
    K_target_Br[i] <- k_target_Br[i] * y_Br[i-1]
    inv_d_Br[i] <- g_inv_Br * (K_target_Br[i] - k_Br[i-1]) + da_Br[i]
    
    inv_gamma_Br <- (gamma0_Br + gamma1_Br*inv_Br[i-1] +
                       gamma2_Br*gov_tot_Br[i-1]) * (1 - d_t_Br[i-1])
    inv_Br[i] <- inv_nalin_weight * inv_d_Br[i] +
      (1 - inv_nalin_weight) * inv_gamma_Br
    
    inv_RoW[i] <- (gamma0_RoW + gamma1_RoW*inv_RoW[i-1] +
                     gamma2_RoW*gov_tot_RoW[i-1]) * (1 - d_t_RoW[i-1])
    
    gifdif_Br <- max(r_l_con_br - r_l_RoW_br, 0) *
      (l_firm_Br[i-1] * share_gr_b_lag)
    inv_gr_Br_t[i] <- (chi1_Br*gov_gr_Br[i] + chi2_Br*y_Br[i] +
                         chi3_Br*d_t_Br[i-1] +
                         chi4_Br*gifdif_Br +
                         chi5_Br * delta_l_fc_gr_Br[i-1] * xr_Br[i]) *
      (1 - d_t_Br[i-1])
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
    
    lcd_Br[i] <- l_firm_Br[i-1] + l_fc_Br[i-1] * xr_Br[i] +
      inv_Br[i] - af_Br[i] - fu_Br[i] -
      (e_RoWBr_s[i] - e_RoWBr_s[i-1]) -
      (e_BrBr_s[i] - e_BrBr_s[i-1])
    ldom_Br[i] <- lambda_dom_0 + lambda_dom_1 * lcd_Br[i]
    ldom_Br[i] <- max(ldom_Br[i], 0)
    l_firm_Br[i] <- ldom_Br[i]
    fc_residual_Br[i] <- lcd_Br[i] - ldom_Br[i]
    if (inv_nalin_weight > 0) {
      fc_demand_fc_units <- fc_residual_Br[i] / xr_Br[i]
      fc_demand_fc_units <- max(fc_demand_fc_units, 0)
      l_fc_Br_d[i] <- fc_demand_fc_units
      if (fc_supply_mode == "passive") {
        l_fc_Br_s[i] <- l_fc_Br_d[i]
      } else {
        l_fc_Br_s[i] <- l_fc_Br_d[i]   # branch reserved for rationing
      }
      l_fc_Br[i]       <- l_fc_Br_s[i]
      delta_l_fc_Br[i] <- l_fc_Br[i] - l_fc_Br[i-1]
      int_fc_Br[i] <- r_l_fc * l_fc_Br[i-1] * xr_Br[i]
      
      psi_gr_share_Br[i] <- 0.5 + kappa_fc * (r_l_fc_con - r_l_fc_gr)
      delta_l_fc_gr_Br[i]  <- delta_l_fc_Br[i] * psi_gr_share_Br[i]
      delta_l_fc_con_Br[i] <- delta_l_fc_Br[i] - delta_l_fc_gr_Br[i]
      l_fc_gr_Br[i]  <- l_fc_gr_Br[i-1]  + delta_l_fc_gr_Br[i]
      l_fc_con_Br[i] <- l_fc_con_Br[i-1] + delta_l_fc_con_Br[i]
      int_fc_gr_Br[i]  <- r_l_fc_gr  * l_fc_gr_Br[i-1]
      int_fc_con_Br[i] <- r_l_fc_con * l_fc_con_Br[i-1]
    }
    l_firm_RoW[i] <- l_firm_RoW[i-1] + inv_RoW[i] - af_RoW[i] - fu_RoW[i] -
      (e_BrRoW_s[i] - e_BrRoW_s[i-1]) -
      (e_RoWRoW_s[i] - e_RoWRoW_s[i-1])
    
    # -------------------------- IV. TRADE ---------------------------------- #
    
    yb_i    <- max(y_Br[i], 1e-6)
    yg_i    <- max(y_RoW[i], 1e-6)
    xrb_lag <- max(xr_Br[i-1], 1e-6)
    
    if (use_endog_elast) {
      share_gr_b_now <- if (k_Br[i-1] > 0) k_gr_Br[i-1]/k_Br[i-1] else 0.144
      eps_x_b <- zeta0_par  + zeta1_par * share_gr_b_now
      eta_m_b <- phi0_m_par - phi1_m_par * share_gr_b_now
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
    b_BrRoW_d_notional <- v_Br[i]*(lambda20 - lambda21*r_Br + lambda22*r_RoW -
                                     lambda23*r_e_Br[i-1] - lambda24*r_e_RoW[i-1])
    if (b_BrRoW_d_notional < 1e-10) {
      b_BrRoW_d[i] <- 1e-10
    } else {
      b_BrRoW_d[i] <- b_BrRoW_d_notional
    }
    e_BrRoW_d[i] <- 0
    e_BrBr_d[i] <- v_Br[i]*((lambda90 + lambda70) - lambda91*r_Br - lambda92*r_RoW +
                              lambda93*r_e_Br[i-1] - lambda94*r_e_RoW[i-1])
    b_RoWRoW_d[i] <- v_RoW[i]*(lambda40 - lambda41*r_Br + lambda42*r_RoW -
                                 lambda43*r_e_Br[i-1] - lambda44*r_e_RoW[i-1])
    b_RoWBr_d[i] <- b_RoWBr_d[1]
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
    if (fx_closure == "GL") {
      xr_RoW_new <- b_RoWBr_s[i] / b_RoWBr_d[i]
      xr_RoW[i] <- (1 - xr_relax) * xr_RoW[i] + xr_relax * xr_RoW_new
      xr_Br[i] <- 1 / xr_RoW[i]
    } else if (fx_closure == "FIXED") {
      # >>> BORTZ FIX (1)+(2)+(3): Bortz-faithful fixed-ER bill clearing -----
      xr_RoW[i] <- 1
      xr_Br[i]  <- 1
      
      # Foreign demand for Br (peso) bills honoured at the peg (Bortz 66ai).
      b_RoWBr_s[i] <- b_RoWBr_d[i]
      
      # (1)+(2) Reserves = residual of the RoW (US) bill market (Bortz 72uFX),
      # in RoW (FC) units. Holders netted out: RoW HH, RoW banks, RoW CB (Fed),
      # Br HH, plus the exogenous institutional/MDB placeholder b_RoW_other.
      # This CLOSES the RoW bill market that the original left open.
      b_cb_BrRoW_s[i] <- b_RoW_s[i] - b_RoWRoW_s[i] - b_RoW_bank[i] -
        b_cb_RoWRoW_s[i] - b_BrRoW_s[i] - b_RoW_other
      or_Br[i] <- b_cb_BrRoW_s[i] * xr_RoW[i]   # reserves in Br currency
      
      # (3) Br CB DOMESTIC (peso) bill holding.
      if (cb_domestic_rule == "balance_sheet") {
        # Bortz 70a in levels: domestic bills + reserves + advances = cash.
        # Domestic-market clearing is then the redundant check (Section XV).
        b_cb_BrBr_s[i] <- h_Br_s[i] - a_s_Br[i] - or_Br[i]
      } else {
        # Pragmatic: domestic-market residual (as in the original). Both
        # bill markets then clear by construction; CB net worth absorbs gap.
        b_cb_BrBr_s[i] <- b_Br_s[i] - b_BrBr_s[i] - b_RoWBr_s[i] -
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
    psbr_Br[i] <- gov_tot_Br[i] + r_Br*b_Br_s[i-1] - t_Br[i] - f_cb_Br[i]
    psbr_RoW[i] <- gov_tot_RoW[i] + r_RoW*b_RoW_s[i-1] - t_RoW[i] - f_cb_RoW[i]
    
    # >>> BORTZ FIX (4): CA now includes interest earned on RESERVES
    # (b_cb_BrRoW_s), per Bortz 75a -- not just HH holdings of RoW bills.
    ca_int_recv_Br[i]    <- xr_Br[i] * (r_RoW * (b_BrRoW_s[i-1] +
                                                   b_cb_BrRoW_s[i-1]) +
                                          r_e_RoW[i-1] * e_BrRoW_s[i-1])
    ca_int_paid_lc_Br[i] <- r_Br * b_RoWBr_s[i-1] +
      r_e_Br[i-1] * e_RoWBr_s[i-1]
    ca_int_paid_fc_Br[i] <- r_l_fc_gr  * l_fc_gr_Br[i-1]  * xr_Br[i] +
      r_l_fc_con * l_fc_con_Br[i-1] * xr_Br[i]
    ca_div_net_Br[i]     <- 0              # placeholder; cross-border dividends zeroed
    cab_Br[i] <- (x_Br[i] - im_Br[i]) +
      ca_int_recv_Br[i] -
      ca_int_paid_lc_Br[i] -
      ca_int_paid_fc_Br[i] +
      ca_div_net_Br[i]
    cab_RoW[i] <- -cab_Br[i]
    
    ka_b_in_Br[i]   <- (b_RoWBr_s[i] - b_RoWBr_s[i-1])
    ka_e_in_Br[i]   <- (e_RoWBr_s[i] - e_RoWBr_s[i-1])
    ka_fc_in_Br[i]  <- (l_fc_Br[i]   - l_fc_Br[i-1])   * xr_Br[i]
    ka_b_out_Br[i]  <- (b_BrRoW_s[i] - b_BrRoW_s[i-1]) * xr_Br[i]
    ka_e_out_Br[i]  <- (e_BrRoW_s[i] - e_BrRoW_s[i-1]) * xr_Br[i]
    kabp_Br[i] <- ka_b_in_Br[i] + ka_e_in_Br[i] + ka_fc_in_Br[i] -
      ka_b_out_Br[i] - ka_e_out_Br[i]
    kabp_RoW[i] <- -kabp_Br[i]
    
    bp_Br[i]  <- cab_Br[i] + kabp_Br[i]
    bp_RoW[i] <- cab_RoW[i] + kabp_RoW[i]
    
    # >>> BORTZ FIX (2): reserves are NO LONGER cumulated BoP under FIXED.
    #   FIXED: or_Br already SET in Section IX (RoW-bill residual, Bortz
    #          72uFX). Leaving it untouched is what makes the BoP identity
    #          below a GENUINE, non-tautological check.
    #   GL:    keep the legacy cumulated-BoP convention.
    if (fx_closure != "FIXED") {
      or_Br[i] <- or_Br[i-1] + bp_Br[i]
    }
    # or_RoW kept as a diagnostic mirror only: in the asymmetric Bortz setup
    # the RoW (US) is the reserve-issuing / dominant country and does NOT
    # settle its BoP in reserves, so its reserve identity is not expected to
    # bind. The meaningful check is or_Br.
    or_RoW[i] <- or_RoW[i-1] + bp_RoW[i]
    
    nafa_Br[i]  <- psbr_Br[i] + cab_Br[i]
    nafa_RoW[i] <- psbr_RoW[i] + cab_RoW[i]
    
    bop_resid_Br[i]  <- bp_Br[i]  - (or_Br[i]  - or_Br[i-1])
    bop_resid_RoW[i] <- bp_RoW[i] - (or_RoW[i] - or_RoW[i-1])
    
    # >>> BORTZ FIX (3): CB-balance-sheet diagnostics.
    # Implied Br CB net worth = (domestic bills + reserves + advances) - cash.
    # Under cb_domestic_rule == "balance_sheet" this is ~0 by construction;
    # under "residual" it absorbs the gap the b_RoWBr_d lock would otherwise
    # dump into the redundant market-clearing condition below.
    cb_nw_Br[i] <- (b_cb_BrBr_s[i] + or_Br[i] + a_s_Br[i]) - h_Br_s[i]
    # Redundant Br-bill-market clearing (Bortz 71a): ~0 iff watertight.
    br_bill_resid[i] <- b_Br_s[i] -
      (b_BrBr_s[i] + b_RoWBr_s[i] + b_Br_bank[i] + b_cb_BrBr_s[i])
    
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

guard_periods_rep <- which(xr_guard_fired[i_plot])
add_guard_lines <- function() {
  if (length(guard_periods_rep) > 0) {
    abline(v = guard_periods_rep, col = "red", lwd = 1, lty = 1)
  }
}

old.par <- par(no.readonly = TRUE)
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)

plot(periods, y_Br[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(y_Br[i_plot], y_RoW[i_plot])),
     main = "a) GDP by area", xlab = "Period", ylab = "Trillion USD")
add_guard_lines()
lines(periods, y_RoW[i_plot], lwd = 2, col = "forestgreen")
legend("topleft", c("Br","RoW"),
       col = c("orange","forestgreen"), lwd = 2, bty = "n", cex = 0.85)

plot(periods, temp_at[i_plot], type = "l", lwd = 2, col = "blue",
     main = "b) Atmospheric & lower-ocean temperature",
     xlab = "Period", ylab = "Anomaly (deg C)",
     ylim = range(c(temp_at[i_plot], temp_lo[i_plot])))
add_guard_lines()
lines(periods, temp_lo[i_plot], lwd = 2, col = "#18a8d1")
legend("topleft", c("Atmosphere","Lower ocean"),
       col = c("blue","#18a8d1"), lwd = 2, bty = "n", cex = 0.85)

plot(periods, emis[i_plot], type = "l", lwd = 2, col = "black",
     main = "c) Total CO2 emissions per year", xlab = "Period", ylab = "Gt CO2/yr")
add_guard_lines()
lines(periods, emis_Br[i_plot], col = "orange", lwd = 2)
lines(periods, emis_RoW[i_plot], col = "forestgreen", lwd = 2)
lines(periods, emis_l[i_plot], col = "darkblue", lwd = 2)
legend("topright", c("Total","Br","RoW","Land"),
       col = c("black","orange","forestgreen","darkblue"), lwd = 2,
       bty = "n", cex = 0.85)

plot(periods, co2_at[i_plot], type = "l", lwd = 2, col = "purple",
     main = "d) Atmospheric CO2 concentration", xlab = "Period", ylab = "Gt CO2")
add_guard_lines()

plot(periods, k_e[i_plot], type = "l", lwd = 2, col = "deeppink",
     ylim = range(c(k_e[i_plot], k_m[i_plot])),
     main = "e) Reserves of energy & matter", xlab = "Period", ylab = "Stock")
add_guard_lines()
lines(periods, k_m[i_plot], lwd = 2, col = "purple")
legend("topleft", c("Energy (Ej)","Matter (Gt)"),
       col = c("deeppink","purple"), lwd = 2, bty = "n", cex = 0.85)

plot(periods, d_t_Br[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(d_t_Br[i_plot], d_t_RoW[i_plot])),
     main = "f) Climate damage ratio", xlab = "Period", ylab = "d_t")
add_guard_lines()
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
cat(sprintf("RoW CA + KA - dor : %+.6f  (mirror only; not expected to bind)\n",
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

# >>> BORTZ FIX: reserve / bill-clearing diagnostics ------------------------
cat("\n--- Bortz fixed-ER bill-clearing diagnostics ---\n")
cat(sprintf("cb_domestic_rule        : %s\n", cb_domestic_rule))
cat(sprintf("b_RoW_other (param)     : %.4f\n", b_RoW_other))
cat(sprintf("Br reserves or_Br [IC]   : %.4f  (legacy or_init_Br = %.4f)\n",
            or_Br[1], or_init_Br))
cat(sprintf("Br reserves or_Br [final]: %.4f\n", or_Br[nPeriods]))
win <- (burn_in + 1):nPeriods
cat(sprintf("max |BoP resid Br| (reported window) : %.3e   <- genuine now\n",
            max(abs(bop_resid_Br[win]))))
cat(sprintf("max |Br-bill mkt resid| (Bortz 71a)  : %.3e\n",
            max(abs(br_bill_resid[win]))))
cat(sprintf("Br-bill mkt resid  [IC / final]      : %+.4e / %+.4e\n",
            br_bill_resid[1], br_bill_resid[nPeriods]))
cat(sprintf("implied Br CB net worth [IC / final] : %+.4e / %+.4e\n",
            cb_nw_Br[1], cb_nw_Br[nPeriods]))
cat(sprintf("Br CB domestic bills b_cb_BrBr_s [IC / final] : %+.4f / %+.4f\n",
            b_cb_BrBr_s[1], b_cb_BrBr_s[nPeriods]))
cat(sprintf("Br CB reserve bills  b_cb_BrRoW_s [IC / final]: %+.4f / %+.4f\n",
            b_cb_BrRoW_s[1], b_cb_BrRoW_s[nPeriods]))
if (max(abs(br_bill_resid[win])) > 1e-6) {
  cat("  NOTE: Br-bill market does not clear to machine precision. Under\n")
  cat("  cb_domestic_rule='balance_sheet' this is the b_RoWBr_d LOCK surfacing\n")
  cat("  (RoW HH Br-bill demand frozen at IC). Either un-lock b_RoWBr_d or set\n")
  cat("  cb_domestic_rule='residual' to absorb the gap into CB net worth.\n")
}

# --- Hard-guard firing diagnostic ---
cat("\n--- Numerical guard triggers ---\n")
n_fired <- sum(xr_guard_fired)
if (n_fired == 0) {
  cat("b_RoWBr_d floor (1e-10): never triggered\n")
} else {
  fired_abs <- which(xr_guard_fired)
  fired_rep <- fired_abs - burn_in
  fired_rep_in_view <- fired_rep[fired_rep >= 1]
  cat(sprintf("b_RoWBr_d floor (1e-10): triggered in %d period(s).\n", n_fired))
  if (length(fired_rep_in_view) > 0) {
    cat(sprintf("  First fire: reported period %d  (absolute %d).\n",
                fired_rep_in_view[1], fired_abs[which(fired_rep == fired_rep_in_view[1])]))
    cat(sprintf("  All reported-period fires: %s\n",
                paste(fired_rep_in_view, collapse=", ")))
  }
  cat("  ECONOMIC INTERPRETATION: notional RoW demand for Br bills fell\n")
  cat("  below 1e-10, indicating foreign appetite for the Br currency\n")
  cat("  has vanished. The FX-clearing ratio supply/demand then drives\n")
  cat("  xr toward zero -- the peso effectively crashes to worthless.\n")
}

# --- Split-FC borrowing diagnostic ---
cat("\n--- Split FC borrowing (green / conventional) ---\n")
cat(sprintf("Final stocks: l_fc_gr_Br=%.4f, l_fc_con_Br=%.4f, total=%.4f\n",
            l_fc_gr_Br[nPeriods], l_fc_con_Br[nPeriods],
            l_fc_gr_Br[nPeriods] + l_fc_con_Br[nPeriods]))
cat(sprintf("Initial   stocks: l_fc_gr_Br=%.4f, l_fc_con_Br=%.4f, total=%.4f\n",
            l_fc_gr_Br[1], l_fc_con_Br[1], l_fc_gr_Br[1] + l_fc_con_Br[1]))
cat(sprintf("Rates at end: r_l_fc_gr=%.4f, r_l_fc_con=%.4f, gap=%+.4f\n",
            r_l_fc_gr, r_l_fc_con, r_l_fc_con - r_l_fc_gr))
psi_post_burn <- psi_gr_share_Br[(burn_in+1):nPeriods]
cat(sprintf("psi_gr_share (reported window): min=%.3f, max=%.3f, mean=%.3f\n",
            min(psi_post_burn), max(psi_post_burn), mean(psi_post_burn)))
neg_psi <- sum(psi_post_burn < 0)
gt1_psi <- sum(psi_post_burn > 1)
if (neg_psi > 0) cat(sprintf("  psi < 0 in %d period(s) -- firms net-repaid green\n", neg_psi))
if (gt1_psi > 0) cat(sprintf("  psi > 1 in %d period(s) -- firms over-rotated into green\n", gt1_psi))
if (neg_psi == 0 && gt1_psi == 0) cat("  psi stayed within [0,1] naturally (no extreme allocations)\n")
neg_stock_gr  <- sum(l_fc_gr_Br[(burn_in+1):nPeriods]  < 0)
neg_stock_con <- sum(l_fc_con_Br[(burn_in+1):nPeriods] < 0)
if (neg_stock_gr  > 0) cat(sprintf("  l_fc_gr_Br  went NEGATIVE in %d period(s) -- model says firms accumulated FC green ASSETS\n", neg_stock_gr))
if (neg_stock_con > 0) cat(sprintf("  l_fc_con_Br went NEGATIVE in %d period(s) -- model says firms accumulated FC conv ASSETS\n", neg_stock_con))

################################################################################
# 9) ADDITIONAL STANDALONE PLOTS
################################################################################

par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)
plot(periods, or_Br[i_plot], type = "l", lwd = 2, col = "orange",
     main = "Br FX reserves (= Br CB holdings of RoW bills, Bortz 72uFX)",
     xlab = "Period", ylab = "Reserves (T USD)")
add_guard_lines()
abline(h = or_Br[burn_in + 1], lty = 2, col = "grey50")
legend("topleft",
       legend = c("Br", "Br initial"),
       col    = c("orange", "grey50"),
       lwd    = c(2, 1),
       lty    = c(1, 2),
       bty    = "n", cex = 0.85)

par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)
plot(periods, emis_Br[i_plot], type = "l", lwd = 2, col = "orange",
     main = "Br CO2 emissions",
     xlab = "Period", ylab = "Gt CO2 / yr")
add_guard_lines()
legend("topright", legend = "Br", col = "orange", lwd = 2, bty = "n", cex = 0.85)

layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1, oma = c(0, 0, 2, 0))

plot(periods, x_Br[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(x_Br[i_plot], im_Br[i_plot])),
     main = "a) Br trade flows (real)",
     xlab = "Period", ylab = "Trillion USD")
add_guard_lines()
lines(periods, im_Br[i_plot], lwd = 2, col = "deeppink")
legend("topleft",
       legend = c("Br exports", "Br imports"),
       col = c("orange", "deeppink"), lwd = 2, bty = "n", cex = 0.8)

plot(periods, tb_Br[i_plot], type = "l", lwd = 2, col = "purple",
     main = "b) Trade balance (Br)",
     xlab = "Period", ylab = "Trillion USD")
add_guard_lines()
abline(h = 0, lty = 2, col = "grey50")

plot(periods, b_BrRoW_s[i_plot], type = "l", lwd = 2, col = "orange",
     ylim = range(c(b_BrRoW_s[i_plot], b_RoWBr_s[i_plot])),
     main = "c) Cross-border bill holdings",
     xlab = "Period", ylab = "Trillion USD")
add_guard_lines()
lines(periods, b_RoWBr_s[i_plot], lwd = 2, col = "forestgreen")
legend("topleft",
       legend = c("Br holdings of RoW bills", "RoW holdings of Br bills"),
       col = c("orange", "forestgreen"), lwd = 2, bty = "n", cex = 0.8)

plot(periods, cab_Br[i_plot], type = "l", lwd = 2, col = "darkblue",
     ylim = range(c(cab_Br[i_plot], kabp_Br[i_plot], bp_Br[i_plot])),
     main = "d) Balance of payments (Br)",
     xlab = "Period", ylab = "Trillion USD")
add_guard_lines()
lines(periods, kabp_Br[i_plot], lwd = 2, col = "deeppink")
lines(periods, bp_Br[i_plot],   lwd = 2, col = "black")
abline(h = 0, lty = 2, col = "grey50")
legend("topleft",
       legend = c("Current account", "Capital account", "Overall BoP"),
       col = c("darkblue", "deeppink", "black"), lwd = 2, bty = "n", cex = 0.8)

mtext("Br international flows", outer = TRUE, cex = 1.05, font = 2)
layout(1)

gdp_gr_Br_pct  <- 100 * (y_Br[i_plot]  / y_Br[i_plot - 1]  - 1)
gdp_gr_RoW_pct <- 100 * (y_RoW[i_plot] / y_RoW[i_plot - 1] - 1)
kgr_gr_Br_pct  <- 100 * (k_gr_Br[i_plot]  / pmax(k_gr_Br[i_plot - 1], 1e-9)  - 1)
kgr_gr_RoW_pct <- 100 * (k_gr_RoW[i_plot] / pmax(k_gr_RoW[i_plot - 1], 1e-9) - 1)

old.par2 <- par(no.readonly = TRUE)
layout(matrix(1:3, nrow = 3, ncol = 1, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)

plot(periods, gdp_gr_Br_pct, type = "l", lwd = 2, col = "orange",
     ylim = range(c(gdp_gr_Br_pct, gdp_gr_RoW_pct), na.rm = TRUE),
     main = "GDP growth rate (year-over-year)",
     xlab = "Period", ylab = "Growth rate (%)")
add_guard_lines()
lines(periods, gdp_gr_RoW_pct, lwd = 2, col = "forestgreen")
abline(h = 0, lty = 2, col = "grey50")
if (shock_fed) abline(v = t_shock_fed, lty = 3, col = "red")
legend("topright",
       legend = c("Br", "RoW", if (shock_fed) "Fed shock"),
       col    = c("orange", "forestgreen", if (shock_fed) "red"),
       lwd    = c(2, 2, if (shock_fed) 1),
       lty    = c(1, 1, if (shock_fed) 3),
       bty = "n", cex = 0.85)

plot(periods, k_gr_Br[i_plot], type = "l", lwd = 2, col = "forestgreen",
     main = "Br green capital stock",
     xlab = "Period", ylab = "k_gr_Br (T USD)")
add_guard_lines()
if (shock_fed) abline(v = t_shock_fed, lty = 3, col = "red")
legend("topleft",
       legend = c("Br green capital", if (shock_fed) "Fed shock"),
       col    = c("forestgreen", if (shock_fed) "red"),
       lwd    = c(2, if (shock_fed) 1),
       lty    = c(1, if (shock_fed) 3),
       bty = "n", cex = 0.85)

plot(periods, kgr_gr_Br_pct, type = "l", lwd = 2, col = "orange",
     ylim = range(c(kgr_gr_Br_pct, kgr_gr_RoW_pct), na.rm = TRUE),
     main = "Green capital growth rate (year-over-year)",
     xlab = "Period", ylab = "Growth rate (%)")
add_guard_lines()
lines(periods, kgr_gr_RoW_pct, lwd = 2, col = "forestgreen")
abline(h = 0, lty = 2, col = "grey50")
if (shock_fed) abline(v = t_shock_fed, lty = 3, col = "red")
legend("topright",
       legend = c("Br", "RoW", if (shock_fed) "Fed shock"),
       col    = c("orange", "forestgreen", if (shock_fed) "red"),
       lwd    = c(2, 2, if (shock_fed) 1),
       lty    = c(1, 1, if (shock_fed) 3),
       bty = "n", cex = 0.85)

par(old.par2)
layout(1)