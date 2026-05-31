################################################################################
#
# OPENECO-UNIFIED model — pure post-2017 simulation in R
# Simplified single-household variant of Carnevali, Deleidi, Pariboni,
# Veronese Passarella (2021).
#
# ---------------------------------------------------------------------------
# BORTZ (2014) FIXED-ER CLOSURE. Single closure, no toggle. Key elements:
#   (1) b_cb_BrRoW_s = Br CB holdings of RoW (US) bills = its FX reserves.
#       These CUMULATE FROM THE BALANCE OF PAYMENTS (Bortz eq. 76a / KABOSA:
#       reserves are the official-settlement item). bp_Br drains/fills them, so
#       imports and interest flows now actually move reserves.
#   (2) US-bill market clears on the ISSUER's central bank (the Fed) as the
#       residual holder. In the asymmetric two-country world the tiny periphery
#       cannot absorb the US-bill residual (that gave 13.6x Br GDP in reserves),
#       so the Fed clears its own market. Fed net worth (cb_nw_RoW) then equals
#       the share of RoW debt RoW does not hold domestically -- a diagnostic.
#   (3) Br CB DOMESTIC (peso) bill holding is the residual of the peso-bill
#       market (Bortz eq. 71a); that market clearing is the redundant check.
#   (4) Current account includes interest earned ON RESERVES (Bortz 75a).
#   (5) Peg xr_RoW = xr_Br = 1 (Bortz 73uFX/73a); all CB profit remitted to the
#       Treasury via f_cb_Br, holding CB net worth ~constant.
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

# No burn-in. t=1 is the (consistent) initial condition; t>=2 is generated
# entirely by the behavioral equations. Shocks are applied at absolute periods
# inside the loop (see the shock blocks), in the style of G&L/Passarella.
nPeriods <- 100  # periods to simulate and report

run_s5    <- FALSE
s5_start  <- 9             # absolute period at which Scenario 5 activates
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
  "bop_resid_Br",
  "cb_nw_Br","cb_nw_RoW",                          # implied CB net worth diagnostics (V_cba; Fed gap)
  "br_bill_resid","row_bill_resid",                # redundant bill-market checks (Bortz 71a; US-bill)
  "gnp_Br","gnp_RoW","xr_Br","xr_RoW",
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

r_Br     <- 0.01 ; r_RoW     <- 0.01
r_l_Br   <- 0.035; r_l_RoW   <- 0.035

# --- Central-bank advances rate (Bortz 2014 eq. 62a: bill rate + penalty) ---
# raa = rba * (1 + upsilon_a); the penalty is smaller than the banks' loan
# mark-up (here 0.035/0.03 - 1 = 16.7%), per Bortz's description.
upsilon_Br <- 0.10                     # advances penalty over the bill rate
r_a_Br     <- r_Br * (1 + upsilon_Br)  # = 0.033

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

g_beta_Br <- 0.01 #original from Carnevali 0.02
g_beta_RoW <- 0.02 #original from Carnevali 0.04
greening_starts <- 4
beta0_base_Br  <- 0.27444   # 3.4524 * s_Br
beta0_base_RoW <- 8.87342   # 3.4524 * s_RoW

nIter <- 150

# --- FX closure: single Bortz (2014) fixed exchange rate peg ---------------
# Peg xr_RoW = xr_Br = 1 (Bortz 73uFX/73a). Br CB reserves = its stock of RoW
# (US) bills, cumulated from the balance of payments (Bortz 76a / KABOSA: the
# official-settlement item). The US-bill market clears on the Fed (residual);
# the peso-bill market clears on the Br CB (residual, Bortz 71a). No toggle.

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

f_Br[1]     <- 0.251449 ; f_RoW[1]     <- 7.624353   # consistent profit IC (fixed point: f[2]=f[1], no t=2 jump under lagged-dividend rule)
fu_Br[1]    <- ret_Br * f_Br[1]    ; fu_RoW[1]    <- ret_RoW * f_RoW[1]     # retained (stationary IC: fu = ret*f)
fd_Br[1]    <- (1-ret_Br) * f_Br[1]; fd_RoW[1]    <- (1-ret_RoW) * f_RoW[1] # distributed (= (1-ret)*f, consistent with new rule)
f_m_Br[1]   <- 0       ; f_m_RoW[1]   <- 0
f_bank_Br[1]<- 0.2274  ; f_bank_RoW[1]<- 7.3534
f_d_BrBr[1] <- fd_Br[1]  ; f_d_BrRoW[1] <- 0
f_d_RoWBr[1] <- 0        ; f_d_RoWRoW[1] <- fd_RoW[1]

gov[1]           <- 16.654
gov_tot_Br[1] <- 0.500 ; gov_tot_RoW[1] <- 16.154
gov_con_Br[1] <- 0.420 ; gov_con_RoW[1] <- 13.591
gov_gr_Br[1]  <- 0.0795; gov_gr_RoW[1]  <- 2.5703

x_Br[1]  <- 0.8406 ; x_RoW[1]  <- 0.8407
im_Br[1] <- 0.8407 ; im_RoW[1] <- 0.8406
tb_Br[1] <- -0.0001 ; tb_RoW[1] <- 0.0001

# ============================================================================
# CONSISTENT FINANCIAL INITIAL CONDITIONS (data-anchored; FIXED peg, Bortz)
# Stocks are chosen from data ratios; household wealth is the SUM of household
# assets (closes the HH balance sheet exactly); banks park (deposits - loans)
# in bills; the Br CB holds the peso-bill-market residual (Bortz eq. 71a -- the
# RESIDUAL closure), so its net worth equals V_cba = reserves + net domestic
# bills (Bortz's "sort of net wealth"), held constant by the f_cb remittance.
# Every instrument market and balance sheet closes to < 1e-9 at t=1 (verified
# by check_consistency() in Section 8). Single CB rule = RESIDUAL; no toggle.
# ----------------------------------------------------------------------------

## (1) calibration ratios (data-anchored; tune here) -------------------------
Y_Br  <- 3 ; Y_RoW <- 97                          # GDP normalisation (world=100)
cash_gdp_Br   <- 0.04 ; cash_gdp_RoW   <- 0.08    # currency / GDP
dep_gdp_Br    <- 0.60 ; dep_gdp_RoW    <- 1.17    # bank deposits / GDP
loan_gdp_Br   <- 0.50 ; loan_gdp_RoW   <- 0.90    # bank loans to firms / GDP
debt_gdp_Br   <- 0.78 ; debt_gdp_RoW   <- 1.00    # govt bill supply / GDP
res_gdp_Br    <- 0.15                             # FX reserves / GDP (Br only)
foreign_share_Br_debt <- 0.10                     # share of Br bills held abroad
r_e_ic        <- 0.0352                           # equity return at IC
p_e_ic        <- 1.486                            # equity price at IC

## (2) levels from ratios ----------------------------------------------------
h_Br_s[1]    <- cash_gdp_Br * Y_Br ; h_RoW_s[1]    <- cash_gdp_RoW * Y_RoW
dep_Br[1]    <- dep_gdp_Br  * Y_Br ; dep_RoW[1]    <- dep_gdp_RoW  * Y_RoW
l_firm_Br[1] <- loan_gdp_Br * Y_Br ; l_firm_RoW[1] <- loan_gdp_RoW * Y_RoW
b_Br_s[1]    <- debt_gdp_Br * Y_Br ; b_RoW_s[1]    <- debt_gdp_RoW * Y_RoW
b_cb_BrRoW_s[1] <- res_gdp_Br  * Y_Br                  # Br CB reserves (US bills), sane target
b_RoWBr_d[1] <- foreign_share_Br_debt * b_Br_s[1]       # foreign holding of Br bills
b_RoWBr_s[1] <- b_RoWBr_d[1]                            # honoured at the peg (66ai)
xr_Br[1] <- 1 ; xr_RoW[1] <- 1
h_Br_h[1] <- h_Br_s[1] ; h_RoW_h[1] <- h_RoW_s[1]       # cash supply = HH demand
a_s_Br[1] <- 0 ; a_d_Br[1] <- 0 ; a_s_RoW[1] <- 0 ; a_d_RoW[1] <- 0  # no advances

## (3) household wealth = sum of assets (fixed point of the demand block) -----
# Shares from the model's lambdas at the IC rates (interest terms cancel since
# lambda x1 = x2 = 1), so the t=1 levels equal what Section-V produces at t=2.
sh_b_BrBr   <- lambda10 + lambda11*r_Br - lambda12*r_RoW - lambda13*r_e_ic - lambda14*r_e_ic
sh_b_BrRoW  <- lambda20 - lambda21*r_Br + lambda22*r_RoW - lambda23*r_e_ic - lambda24*r_e_ic
sh_e_BrBr   <- (lambda90 + lambda70) - lambda91*r_Br - lambda92*r_RoW + lambda93*r_e_ic - lambda94*r_e_ic
sh_b_RoWRoW <- lambda40 - lambda41*r_Br + lambda42*r_RoW - lambda43*r_e_ic - lambda44*r_e_ic
sh_e_RoWRoW <- (lambda100 + lambda80) - lambda101*r_Br - lambda102*r_RoW - lambda103*r_e_ic + lambda104*r_e_ic
liquid_Br  <- dep_Br[1]  + h_Br_s[1]
liquid_RoW <- dep_RoW[1] + h_RoW_s[1]
v_Br[1]  <- liquid_Br  / (1 - sh_b_BrBr - sh_b_BrRoW - sh_e_BrBr)
v_RoW[1] <- (b_RoWBr_d[1] + liquid_RoW) / (1 - sh_b_RoWRoW - sh_e_RoWRoW)
b_BrBr_d[1]   <- v_Br[1]  * sh_b_BrBr  ; b_BrBr_s[1]   <- b_BrBr_d[1]
b_BrRoW_d[1]  <- v_Br[1]  * sh_b_BrRoW ; b_BrRoW_s[1]  <- b_BrRoW_d[1]
e_BrBr_d[1]   <- v_Br[1]  * sh_e_BrBr  ; e_BrBr_s[1]   <- e_BrBr_d[1]
b_RoWRoW_d[1] <- v_RoW[1] * sh_b_RoWRoW; b_RoWRoW_s[1] <- b_RoWRoW_d[1]
e_RoWRoW_d[1] <- v_RoW[1] * sh_e_RoWRoW; e_RoWRoW_s[1] <- e_RoWRoW_d[1]
depsh_Br  <- dep_Br[1]  / liquid_Br        # SUPERSEDES Section-4 depsh_Br (~0.9375)
depsh_RoW <- dep_RoW[1] / liquid_RoW       # SUPERSEDES Section-4 depsh_RoW (~0.9360)
dep_bank_Br[1] <- dep_Br[1] ; dep_bank_RoW[1] <- dep_RoW[1]
lambda50 <- b_RoWBr_d[1] / v_RoW[1]        # SUPERSEDES Section-4 lambda50 (~0.00155)
e_BrRoW_d[1] <- 0 ; e_BrRoW_s[1] <- 0 ; e_RoWBr_d[1] <- 0 ; e_RoWBr_s[1] <- 0

## (4) banks: park (deposits - loans) in bills, advances = 0 -----------------
l_fc_Br[1]    <- 0.40                              # initial FC loan stock (set early; used in bank residual)
l_s_Br[1]     <- l_firm_Br[1]  ; l_s_RoW[1]     <- l_firm_RoW[1]
b_Br_bank[1]  <- dep_bank_Br[1]  - l_s_Br[1]
b_RoW_bank[1] <- dep_bank_RoW[1] - l_s_RoW[1] - l_fc_Br[1]   # FC loan funded by bills (Bortz 53u)

## (5) central banks ---------------------------------------------------------
# Br CB reserves (b_cb_BrRoW_s[1]) set to the sane target in block (2) above.
# Fed = residual holder of US bills (it clears its own market in the asymmetric
# two-country world). Its NW = bills + advances - cash is non-zero and equals
# the share of RoW debt not held by RoW HH/banks, Br HH, or Br reserves -- the
# quantity the old b_RoW_other plug used to hide.
b_cb_RoWRoW_s[1] <- b_RoW_s[1] - b_RoWRoW_s[1] - b_RoW_bank[1] -
  b_BrRoW_s[1] - b_cb_BrRoW_s[1]    # Fed residual (US-bill market clears)
b_cb_BrBr_s[1]   <- b_Br_s[1] - b_BrBr_s[1] - b_RoWBr_s[1] - b_Br_bank[1]  # Br CB residual (Bortz 71a)

## (6) equity real/price split + world wealth --------------------------------
p_e_Br[1] <- p_e_ic ; p_e_RoW[1] <- p_e_ic
e_Br_real_s[1]  <- e_BrBr_s[1]   / p_e_Br[1]
e_RoW_real_s[1] <- e_RoWRoW_s[1] / p_e_RoW[1]
r_e_Br[1] <- r_e_ic ; r_e_RoW[1] <- r_e_ic
v[1] <- v_Br[1] * xr_Br[1] + v_RoW[1]                  # world wealth from sub-totals
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
# (xr, reserves, b_cb_* and world wealth are all set in the consistent
#  IC block above; the old or_init_Br / RoW-bill-residual lines are gone.)

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

# Growth-normalisation references for the land-emissions driver. Anchored ONCE
# at t=1 from the initial condition and never reset during the run.
# ===========================================================================
# CONSISTENT INITIAL CONDITIONS (override the hand-calibrated seeds above).
# The block above sets economically-motivated *seed* levels, but those levels
# do not jointly satisfy the model's own behavioural equations, so starting
# from them directly produces a large one-off adjustment over the first ~14
# periods (a GDP-growth swing of about +/-28% that then rings down). The values
# below are a SELF-CONSISTENT state of the model (every variable equals what
# the equations produce given the others), so period 1 already sits on the
# model's own dynamics and the reported run is a single clean pass from t=1
# with no start-up artifact and nothing hidden. They were obtained once by
# running the model forward from the seed block until the equations were
# mutually consistent, then recording that state; they are pasted here as
# literal numbers so the simulation needs only ONE pass. Long-run behaviour is
# unchanged. To inspect the raw cold-start instead, comment out this block.
# ===========================================================================
a_d_Br[1] <- 0
a_d_RoW[1] <- 0
a_s_Br[1] <- 0
a_s_RoW[1] <- 0
af_Br[1] <- 0.512277302389064
af_RoW[1] <- 20.4985575134221
b_Br_bank[1] <- 6.76079318160528
b_Br_bank_not[1] <- 6.76079318160528
b_Br_s[1] <- 9.40422327396341
b_BrBr_d[1] <- 1.6761877432253
b_BrBr_s[1] <- 1.6761877432253
b_BrRoW_d[1] <- 0.558691257040213
b_BrRoW_s[1] <- 0.558691257040213
b_cb_BrBr_s[1] <- 0.733242349132828
b_cb_BrRoW_s[1] <- 1.82881378254592
b_cb_RoWRoW_s[1] <- 62.54512116383
b_RoW_bank[1] <- 209.901264203815
b_RoW_bank_not[1] <- 209.901264203815
b_RoW_s[1] <- 337.460282969907
b_RoWBr_d[1] <- 0.234
b_RoWBr_s[1] <- 0.234
b_RoWRoW_d[1] <- 62.6263925626755
b_RoWRoW_s[1] <- 62.6263925626755
beta_Br[1] <- 0.040155857138441
beta_RoW[1] <- 0.0243263266111545
bop_resid_Br[1] <- 5.55111512312578e-17
bp_Br[1] <- 0.0564951008692201
bp_RoW[1] <- -0.0564951008692201
br_bill_resid[1] <- 0
ca_div_net_Br[1] <- 0
ca_int_paid_fc_Br[1] <- 0.0164805000505782
ca_int_paid_lc_Br[1] <- 0.00702
ca_int_recv_Br[1] <- 0.0691129866146375
cab_Br[1] <- 0.0826420212838503
cab_RoW[1] <- -0.0826420212838503
cb_nw_Br[1] <- 2.02434326350913
cb_nw_RoW[1] <- 40.6506300330343
cen_Br[1] <- 0.35700937976672
cen_RoW[1] <- 4.82894646830253
cg_b_Br[1] <- 0
cg_b_RoW[1] <- 0
cg_e_Br[1] <- 0
cg_e_RoW[1] <- 0
co2_at[1] <- 3309.34446079725
co2_lo[1] <- 36588.8788780176
co2_up[1] <- 5658.08304773727
cons[1] <- 62.8485991398187
cons_Br[1] <- 1.82602308297106
cons_RoW[1] <- 61.0225760568476
conv_e_Br[1] <- 37.393728229153
conv_e_RoW[1] <- 1209.06377082756
conv_m_Br[1] <- 5.25059055852642
conv_m_RoW[1] <- 169.769144288051
d_t_Br[1] <- 0.00685387095237522
d_t_RoW[1] <- 0.00685387095237522
da_Br[1] <- 0.512277302389064
da_con_Br[1] <- 0.435450390396656
da_con_RoW[1] <- 17.8561956243699
da_gr_Br[1] <- 0.0768269119924084
da_gr_RoW[1] <- 2.64236188905217
da_RoW[1] <- 20.4985575134221
dc_Br[1] <- 50.6545219861254
dc_RoW[1] <- 1730.24852749241
delta_Br[1] <- 0.102092382127545
delta_l_fc_Br[1] <- 0.00109679781436611
delta_l_fc_con_Br[1] <- 0.000548398907183056
delta_l_fc_gr_Br[1] <- 0.000548398907183056
delta_RoW[1] <- 0.102092382127545
dep_bank_Br[1] <- 8.06569302254423
dep_bank_RoW[1] <- 320.206932787886
dep_Br[1] <- 8.06569302254423
dep_RoW[1] <- 320.206932787886
depl_e_Br[1] <- 0.0151175738403146
depl_e_RoW[1] <- 0.0102080114455454
depl_m_Br[1] <- 0.00796839923932081
depl_m_RoW[1] <- 0.0056548539773555
dis_Br[1] <- 1.0519841362543
dis_RoW[1] <- 29.4132874159386
e_Br[1] <- 27.967644519477
e_Br_real_s[1] <- 0.111256590171206
e_BrBr_d[1] <- 0.558925574957262
e_BrBr_s[1] <- 0.558925574957262
e_BrRoW_d[1] <- 0
e_BrRoW_s[1] <- 0
e_RoW[1] <- 745.109308254315
e_RoW_real_s[1] <- 6.77212380659016
e_RoWBr_d[1] <- 0
e_RoWBr_s[1] <- 0
e_RoWRoW_d[1] <- 20.8652900847376
e_RoWRoW_s[1] <- 20.8652900847376
ed_Br[1] <- 27.967644519477
ed_RoW[1] <- 745.109308254315
emis[1] <- 21.0705313092706
emis_Br[1] <- 1.31022442374386
emis_l[1] <- 2.03807334685645
emis_l_Br[1] <- 0.978639897396638
emis_l_RoW[1] <- 1.05943344945981
emis_RoW[1] <- 17.7222335386703
en_Br[1] <- 27.6500075891852
en_RoW[1] <- 698.200033480925
epsilon_Br[1] <- 9.0671107857186
epsilon_RoW[1] <- 7.10363091893059
er_Br[1] <- 0.317636930291853
er_RoW[1] <- 46.9092747733896
eta_Br[1] <- 0.0113573000425777
eta_RoW[1] <- 0.0629562323993661
f[1] <- 3.00860695991274
f_bank_Br[1] <- 0.236658497678189
f_bank_RoW[1] <- 9.77613205786095
f_Br[1] <- 0.597943528804415
f_cb_Br[1] <- 0.0760750111729902
f_cb_RoW[1] <- 1.84989093473596
f_d_BrBr[1] <- 0.570257903841437
f_d_BrRoW[1] <- 0
f_d_RoWBr[1] <- 0
f_d_RoWRoW[1] <- 14.7193457846967
f_ex[1] <- 0.66
f_m_Br[1] <- 0
f_m_RoW[1] <- 0
f_RoW[1] <- 15.5362081205358
fc_residual_Br[1] <- 0.194985033703521
fd_Br[1] <- 0.570257903841437
fd_RoW[1] <- 14.7193457846967
fu_Br[1] <- 0.0276856249629782
fu_RoW[1] <- 0.816862335839195
gnp_Br[1] <- 0
gnp_RoW[1] <- 0
gov[1] <- 21.5398292845408
gov_con_Br[1] <- 0.56633497204685
gov_con_RoW[1] <- 18.323694312494
gov_gr_Br[1] <- 0.0795
gov_gr_RoW[1] <- 2.5703
gov_tot_Br[1] <- 0.64583497204685
gov_tot_RoW[1] <- 20.893994312494
h_Br_h[1] <- 0.537712868169617
h_Br_s[1] <- 0.537712868169617
h_RoW_h[1] <- 21.8944911307956
h_RoW_s[1] <- 21.8944911307956
im_Br[1] <- 0.865811277359424
im_RoW[1] <- 0.902840812079215
int_fc_Br[1] <- 0.0164805000505782
int_fc_con_Br[1] <- 0.0203597993680807
int_fc_gr_Br[1] <- -0.00387929931750247
inv[1] <- 23.587419037249
inv_Br[1] <- 0.575627994160121
inv_con_Br[1] <- 0.481885244251036
inv_con_RoW[1] <- 19.9117826590489
inv_d_Br[1] <- 0.575627994160121
inv_gr_Br[1] <- 0.0937427499090852
inv_gr_Br_t[1] <- 0.0937427499090852
inv_gr_RoW[1] <- 3.10000838403999
inv_gr_RoW_t[1] <- 3.10000838403999
inv_RoW[1] <- 23.0117910430888
k[1] <- 208.378764344554
k_Br[1] <- 5.08113254496623
k_con_Br[1] <- 4.31169325337573
k_con_RoW[1] <- 176.957918161226
k_e[1] <- 70746.8671589407
k_e_Br[1] <- 1838.74140778537
k_e_RoW[1] <- 68908.1257511554
k_gr_Br[1] <- 0.769439291590502
k_gr_RoW[1] <- 26.3397136383623
k_m[1] <- 10836.7883023579
k_m_Br[1] <- 300.582205608728
k_m_RoW[1] <- 10536.2060967491
k_RoW[1] <- 203.297631799588
k_se_Br[1] <- 72.2963598897014
k_se_RoW[1] <- 1791.64680273354
k_target_Br[1] <- 1.7294465049304
K_target_Br[1] <- 5.22895082576536
ka_b_in_Br[1] <- 0
ka_b_out_Br[1] <- 0.0272437182289963
ka_e_in_Br[1] <- 0
ka_e_out_Br[1] <- 0
ka_fc_in_Br[1] <- 0.00109679781436611
kabp_Br[1] <- -0.0261469204146302
kabp_RoW[1] <- 0.0261469204146302
l_fc_Br[1] <- 0.194985033703521
l_fc_Br_d[1] <- 0.194985033703521
l_fc_Br_s[1] <- 0.194985033703521
l_fc_con_Br[1] <- 0.240075450296367
l_fc_gr_Br[1] <- -0.045090416592846
l_firm_Br[1] <- 1.30489984093895
l_firm_RoW[1] <- 110.110683550367
l_s_Br[1] <- 1.30489984093895
l_s_RoW[1] <- 110.110683550367
lcd_Br[1] <- 1.49988487464247
ldom_Br[1] <- 1.30489984093895
lev_f_Br[1] <- 0.256812793091116
lev_f_RoW[1] <- 0.541623050773729
liq_b_Br[1] <- 0.838216029634198
liq_b_RoW[1] <- 0.656126484858753
mat_Br[1] <- 2.3722230369566
mat_RoW[1] <- 58.9540638540962
mu_Br[1] <- 0.837285399914845
mu_RoW[1] <- 0.640565651400951
nafa_Br[1] <- 0.5201023389242
nafa_RoW[1] <- 15.4489821882165
o2_Br[1] <- 0.953215043977143
o2_RoW[1] <- 12.8932870703677
p_e_Br[1] <- 5.02375251746586
p_e_RoW[1] <- 3.08105561573357
per_Br[1] <- 0
per_RoW[1] <- 0
psbr_Br[1] <- 0.43746031764035
psbr_RoW[1] <- 15.5316242095003
psi_gr_share_Br[1] <- 0.5
q_active_Br[1] <- 1
q_Br[1] <- 0.366812988915761
q_nalin_Br[1] <- 0.297232524651985
q_RoW[1] <- 0.64425725216623
r_e_Br[1] <- 0.0360749946826945
r_e_Br_t[1] <- 1.12459363652153
r_e_RoW[1] <- 0.0341395959534115
r_e_RoW_t[1] <- 0.775873144758824
rec_Br[1] <- 0.210396827250861
rec_RoW[1] <- 8.23572047646281
res_e[1] <- 702966.81880417
res_e_Br[1] <- 21089.0064012358
res_e_RoW[1] <- 681877.812402935
res_m[1] <- 514588.906284498
res_m_Br[1] <- 15437.6628168721
res_m_RoW[1] <- 499151.243467626
row_bill_resid[1] <- -5.6843418860808e-14
t_Br[1] <- 0.401302531923202
t_RoW[1] <- 13.1703389310699
tb_Br[1] <- 0.037029534719791
tb_RoW[1] <- -0.037029534719791
temp_at[1] <- 1.54835323531467
temp_lo[1] <- 0.185646278106274
v[1] <- 437.224317032031
v_Br[1] <- 11.3972104659366
v_RoW[1] <- 425.827106566094
wa_Br[1] <- 0.841587309003441
wa_RoW[1] <- 21.1775669394759
x_Br[1] <- 0.902840812079215
x_RoW[1] <- 0.865811277359424
xr_Br[1] <- 1
xr_RoW[1] <- 1
y[1] <- 107.975847461608
y_Br[1] <- 3.08451558389782
y_h_Br[1] <- 2.78309302062654
y_h_RoW[1] <- 91.3382683699616
y_mat_Br[1] <- 2.58261986420746
y_mat_RoW[1] <- 67.189784330559
y_RoW[1] <- 104.891331877711
y_w_Br[1] <- 1.91239966201665
y_w_RoW[1] <- 65.0326257641806
yd[1] <- 80.549719927595
yd_Br[1] <- 2.38179048870333
yd_hs_Br[1] <- 2.38179048870333
yd_hs_RoW[1] <- 78.1679294388917
yd_RoW[1] <- 78.1679294388917
z_Br[1] <- 1
z_RoW[1] <- 1

y_Br_p1_ref <- y_Br[1]
k_Br_p1_ref <- k_Br[1]
KY0_Br      <- k_Br[1] / y_Br[1]
s_gr_Br_0   <- k_gr_Br[1] / k_Br[1]

################################################################################
# 6) SIMULATION LOOP
################################################################################

xr_guard_fired <- logical(nPeriods)

for (i in 2:nPeriods) {
  
  if (shock_fed && i == t_shock_fed) {
    r_RoW       <- r_RoW       + fed_uplift
    r_l_fc      <- r_l_fc      + fed_uplift
    r_l_fc_gr   <- r_l_fc_gr   + fed_uplift
    r_l_fc_con  <- r_l_fc_con  + fed_uplift
  }
  
  if (shock_mdb && i == t_shock_mdb) {
    r_l_fc_gr <- r_l_fc_gr - mdb_green_cut
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
      f_d_BrBr[i] + f_d_BrRoW[i]
    y_h_RoW[i] <- y_w_RoW[i] + f_m_RoW[i] + f_bank_RoW[i] +
      r_RoW*b_RoWRoW_s[i-1] +
      xr_Br[i-1]*r_Br*b_RoWBr_s[i-1] +
      f_d_RoWRoW[i] + f_d_RoWBr[i]
    
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
    # Fix A: dividend PAID this period is based on LAGGED profit, so it is known
    # at the top of the period (no circularity) and is both paid by the firm and
    # received by households in the SAME period -> the TFM profit row closes.
    # The firm retains the residual of CURRENT profit (fu = f[i] - fd[i]); this
    # is what funds investment and grows firm net worth. (Lagging fu as well
    # would leave current profit unallocated and merely move the leak to df.)
    fd_Br[i]       <- (1 - ret_Br) * f_Br[i-1]
    fu_Br[i]       <- f_Br[i] - fd_Br[i]
    f_m_Br[i]      <- 0
    f_d_BrBr[i] <- fd_Br[i]
    f_d_BrRoW[i] <- 0
    
    share_gr_g_lag <- if (k_RoW[i-1] > 0) k_gr_RoW[i-1] / k_RoW[i-1] else 0
    r_l_RoW_gn   <- max(r_l_RoW * xi_sub_RoW - mdb_subsidy_gn, 0)
    r_l_con_gn     <- r_l_RoW
    interest_RoW <- r_l_RoW_gn * (l_firm_RoW[i-1] * share_gr_g_lag) +
      r_l_con_gn   * (l_firm_RoW[i-1] * (1 - share_gr_g_lag))
    f_RoW[i]        <- y_RoW[i] - y_w_RoW[i] - da_RoW[i] - interest_RoW
    fd_RoW[i]       <- (1 - ret_RoW) * f_RoW[i-1]   # lagged-basis dividend (Fix A)
    fu_RoW[i]       <- f_RoW[i] - fd_RoW[i]         # retained = current profit - dividend paid
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
    # RoW (US) banks also hold the dollar loan to Br firms (l_fc_Br) as an asset.
    # Bortz eq. 53u: bills = deposits + bank NW - loans, with the FC loan to the
    # foreign firm (Luas) entering with a MINUS sign exactly like domestic loans.
    # So the dollar loan is funded by drawing down the bank's US-bill holding,
    # not by creating deposits. l_fc_Br is already in FC (dollar) units = the
    # bank's own currency, so it enters the dollar residual with no conversion
    # (under the peg xr_RoW = 1 in any case).
    b_RoW_bank_not[i] <- dep_bank_RoW[i] - l_s_RoW[i] - l_fc_Br[i]
    if (is.na(b_Br_bank_not[i])) b_Br_bank_not[i] <- 0
    if (is.na(b_RoW_bank_not[i])) b_RoW_bank_not[i] <- 0
    z_Br[i] <- if (b_Br_bank_not[i] > 0) 1 else 0
    z_RoW[i] <- if (b_RoW_bank_not[i] > 0) 1 else 0
    b_Br_bank[i] <- b_Br_bank_not[i] * z_Br[i]
    b_RoW_bank[i] <- b_RoW_bank_not[i] * z_RoW[i]
    a_d_Br[i] <- -b_Br_bank_not[i] * (1 - z_Br[i])
    a_d_RoW[i] <- -b_RoW_bank_not[i] * (1 - z_RoW[i])
    a_s_Br[i] <- a_d_Br[i] ; a_s_RoW[i] <- a_d_RoW[i]
    f_bank_Br[i] <- r_Br*b_Br_bank[i-1] + r_l_Br*l_s_Br[i-1] -
      r_a_Br*a_s_Br[i-1]                         # advances cost (Bortz 58a)
    f_bank_RoW[i] <- r_RoW*b_RoW_bank[i-1] + r_l_RoW*l_s_RoW[i-1] +
      int_fc_cashflow_Br / xr_Br[i]   # FC loan interest received from Br firms,
    # in the RoW bank's own currency (dollars). int_fc_cashflow_Br is the Br
    # firm's payment in pesos (= dollar interest x xr_Br), so dividing by
    # xr_Br[i] recovers the dollar amount and guarantees firm-pays = bank-
    # receives exactly. Already a Br CA outflow (ca_int_paid_fc_Br) and a RoW
    # CA inflow (cab_RoW = -cab_Br); this credits the receiving RoW sector.
    
    # ----------- VIII. CENTRAL BANK AND GOVERNMENT (G&L floating) ---------- #
    
    h_Br_s[i]         <- h_Br_h[i]
    h_RoW_s[i]         <- h_RoW_h[i]
    f_cb_Br[i]        <- r_Br * b_cb_BrBr_s[i-1] +
      xr_Br[i] * r_RoW * b_cb_BrRoW_s[i-1] +     # reserve interest (Bortz 74a)
      r_a_Br * a_s_Br[i-1]                        # advances interest (Bortz 74a)
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
    
    # --------------- IX. EXCHANGE RATE CLOSURE (Bortz 2014 fixed ER) ------- #
    # Peg (Bortz 73uFX/73a). Single closure: no toggle.
    xr_RoW[i] <- 1
    xr_Br[i]  <- 1
    
    # Foreign demand for Br (peso) bills honoured at the peg (Bortz 66ai).
    b_RoWBr_s[i] <- b_RoWBr_d[i]
    
    # Br CB reserves (its stock of RoW/US bills) cumulate from the balance of
    # payments: Delta(reserves . xr) = BoP (Bortz: reserves are the official-
    # settlement item of the BoP, eq. 76a / KABOSA). bp_Br is built below in
    # section XV; the within-period solver iterates this to a fixed point.
    b_cb_BrRoW_s[i] <- b_cb_BrRoW_s[i-1] + bp_Br[i] / xr_Br[i]
    
    # RoW (US) bill market clears on the ISSUER's central bank: the Fed holds
    # the residual of its own bills after RoW HH, RoW banks, Br HH and the Br
    # CB (reserves). In Bortz's closed two-country world every US bill is held
    # by a US sector or as foreign reserves -- the periphery's CB does NOT
    # absorb US issuance. The RoW CB balance sheet (cash = bills + advances)
    # is then the redundant check.
    b_cb_RoWRoW_s[i] <- b_RoW_s[i] - b_RoWRoW_s[i] - b_RoW_bank[i] -
      b_BrRoW_s[i] - b_cb_BrRoW_s[i]
    
    # Br CB DOMESTIC (peso) bill holding: the peso-bill-market residual
    # (Bortz eq. 71a). The government supplies to the CB whatever bills HH,
    # banks, and foreigners do not buy. CB net worth is then V_cba (Bortz's
    # "sort of net wealth"); domestic-market clearing is the redundant check.
    b_cb_BrBr_s[i] <- b_Br_s[i] - b_BrBr_s[i] - b_RoWBr_s[i] - b_Br_bank[i]
    
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
    
    nafa_Br[i]  <- psbr_Br[i] + cab_Br[i]
    nafa_RoW[i] <- psbr_RoW[i] + cab_RoW[i]
    
    # BoP identity check (Bortz: reserves are the official-settlement item of
    # the BoP). Reserves b_cb_BrRoW_s cumulate from bp_Br in section IX, so
    # Delta(reserves . xr) = bp_Br holds by construction and this prints ~0.
    bop_resid_Br[i]  <- bp_Br[i] - (b_cb_BrRoW_s[i] - b_cb_BrRoW_s[i-1]) * xr_Br[i]
    
    # Implied Br CB net worth = (domestic bills + reserves + advances) - cash
    # = V_cba (Bortz's "sort of net wealth"). Non-zero and ~constant when CB
    # profit (incl. reserve + advances interest) is fully remitted via f_cb_Br.
    cb_nw_Br[i] <- (b_cb_BrBr_s[i] + b_cb_BrRoW_s[i] * xr_Br[i] + a_s_Br[i]) - h_Br_s[i]
    # Implied RoW CB (Fed) net worth = bills + advances - cash. With the Fed as
    # the residual holder of US bills (it clears its own market), this is the
    # amount of its own debt RoW does NOT absorb domestically -- exactly the
    # old b_RoW_other gap, now surfaced as a diagnostic instead of a phantom.
    cb_nw_RoW[i] <- (b_cb_RoWRoW_s[i] + a_s_RoW[i]) - h_RoW_s[i]
    # Redundant peso-bill-market clearing (Bortz 71a): ~0 iff watertight.
    br_bill_resid[i] <- b_Br_s[i] -
      (b_BrBr_s[i] + b_RoWBr_s[i] + b_Br_bank[i] + b_cb_BrBr_s[i])
    # Redundant US-bill-market clearing (Fed is residual): ~0 iff watertight.
    row_bill_resid[i] <- b_RoW_s[i] -
      (b_RoWRoW_s[i] + b_RoW_bank[i] + b_cb_RoWRoW_s[i] + b_BrRoW_s[i] + b_cb_BrRoW_s[i])
    
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

i_plot <- 1:nPeriods   # full run; t=1 is the IC, no transient to discard
periods <- 1:length(i_plot)

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
cat(sprintf("Simulated periods: %d (no burn-in; t=1 is the initial condition)\n",
            nPeriods))
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
cat(sprintf("Br  CA + KA - dReserves : %+.6f  (should be ~0; reserves cumulate from BoP)\n",
            bop_resid_Br[nPeriods]))
cat(sprintf("RoW CB (Fed) net worth  : %+.6f  (the RoW under-holding gap; 0 if RoW holds its own debt)\n",
            cb_nw_RoW[nPeriods]))
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

# Reserve / bill-clearing diagnostics (single Bortz fixed-ER closure) -------
cat("\n--- Bortz fixed-ER closure diagnostics ---\n")
cat(sprintf("Peso-bill rule: Br CB residual (71a) | US-bill rule: Fed residual | reserves: BoP-cumulated\n"))
win <- 1:nPeriods
cat(sprintf("Br reserves b_cb_BrRoW_s [IC / final]: %.4f / %.4f\n",
            b_cb_BrRoW_s[1], b_cb_BrRoW_s[nPeriods]))
cat(sprintf("max |BoP resid Br| (full run): %.3e   <- now tautological (reserves = BoP), verifies cumulation\n",
            max(abs(bop_resid_Br[win]))))
cat(sprintf("max |peso-bill mkt resid| (Bortz 71a): %.3e\n", max(abs(br_bill_resid[win]))))
cat(sprintf("max |US-bill mkt resid|   (Fed residual): %.3e\n", max(abs(row_bill_resid[win]))))
cat(sprintf("implied Br CB net worth (V_cba) [IC / final]: %+.4f / %+.4f\n",
            cb_nw_Br[1], cb_nw_Br[nPeriods]))
cat(sprintf("implied RoW CB (Fed) net worth [IC / final]: %+.4f / %+.4f\n",
            cb_nw_RoW[1], cb_nw_RoW[nPeriods]))
cat(sprintf("Br CB domestic bills b_cb_BrBr_s [IC / final]: %+.4f / %+.4f\n",
            b_cb_BrBr_s[1], b_cb_BrBr_s[nPeriods]))
cat(sprintf("Fed bill holding b_cb_RoWRoW_s   [IC / final]: %+.4f / %+.4f\n",
            b_cb_RoWRoW_s[1], b_cb_RoWRoW_s[nPeriods]))
if (abs(cb_nw_RoW[1]) > 1e-6) {
  cat("  NOTE: Fed net worth is non-zero at the IC. It equals the share of RoW\n")
  cat("  debt NOT held by RoW HH/banks, Br HH, or Br reserves -- what b_RoW_other\n")
  cat("  used to hide. Recalibrate RoW to hold more of its own debt (step 5) to\n")
  cat("  shrink it. If it GROWS over the run, that is the r>g issuance spiral.\n")
}

# --- Stock-flow-consistency auditor (Bortz balance-sheet matrix) -----------
# Every instrument-market and balance-sheet row MUST be ~0 (else ORPHAN). The
# Br CB net-worth line is informational (= V_cba, non-zero, constant). The FC
# loan is a known unmatched simplification (a RoW-bank asset not carried on a
# modelled balance-sheet row here). The reserve alias must match exactly.
check_consistency <- function(p = 1, tol = 1e-9) {
  g <- function(v) get(v)[p]
  line <- function(label, resid, kind = "instrument") {
    flag <- if (kind == "instrument") {
      if (abs(resid) < tol) "OK" else "*** ORPHAN ***"
    } else if (kind == "nw") "net worth (informational)" else "informational"
    cat(sprintf("  %-34s % .6e   %s\n", label, resid, flag))
  }
  cat(sprintf("\n=== check_consistency(period = %d) ===\n", p))
  cat("-- instrument-market clearing (must be ~0) --\n")
  line("Br bill market",
       g("b_Br_s") - (g("b_BrBr_s") + g("b_RoWBr_s") + g("b_Br_bank") + g("b_cb_BrBr_s")))
  line("RoW bill market",
       g("b_RoW_s") - (g("b_RoWRoW_s") + g("b_RoW_bank") + g("b_cb_RoWRoW_s") +
                         g("b_BrRoW_s") + g("b_cb_BrRoW_s")))
  line("Br equity (s = d)",  g("e_BrBr_s")   - g("e_BrBr_d"))
  line("RoW equity (s = d)", g("e_RoWRoW_s") - g("e_RoWRoW_d"))
  line("Br equity (real*p)",  g("e_Br_real_s")*g("p_e_Br")   - g("e_BrBr_s"))
  line("RoW equity (real*p)", g("e_RoW_real_s")*g("p_e_RoW") - g("e_RoWRoW_s"))
  cat("-- balance-sheet closure (must be ~0) --\n")
  line("Br household",
       g("v_Br") - (g("b_BrBr_d") + g("b_BrRoW_d") + g("e_BrBr_d") + g("dep_Br") + g("h_Br_h")))
  line("RoW household",
       g("v_RoW") - (g("b_RoWRoW_d") + g("b_RoWBr_d") + g("e_RoWRoW_d") + g("dep_RoW") + g("h_RoW_h")))
  line("Br banks",  (g("l_s_Br")  + g("b_Br_bank"))  - (g("dep_bank_Br")  + g("a_s_Br")))
  line("RoW banks", (g("l_s_RoW") + g("b_RoW_bank") + g("l_fc_Br")) - (g("dep_bank_RoW") + g("a_s_RoW")))
  cat("-- informational (NOT orphans) --\n")
  line("Br CB net worth (V_cba)",
       (g("b_cb_BrBr_s") + g("b_cb_BrRoW_s")*g("xr_Br") + g("a_s_Br")) - g("h_Br_s"), kind = "nw")
  line("RoW CB net worth (Fed gap)",
       (g("b_cb_RoWRoW_s") + g("a_s_RoW")) - g("h_RoW_s"), kind = "nw")
}
cat("\n--- SFC consistency audit ---\n")
check_consistency(1)
check_consistency(nPeriods)

# --- Hard-guard firing diagnostic ---
cat("\n--- Numerical guard triggers ---\n")
n_fired <- sum(xr_guard_fired)
if (n_fired == 0) {
  cat("b_RoWBr_d floor (1e-10): never triggered\n")
} else {
  fired_abs <- which(xr_guard_fired)
  fired_rep <- fired_abs
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
psi_post_burn <- psi_gr_share_Br[1:nPeriods]
cat(sprintf("psi_gr_share (full run): min=%.3f, max=%.3f, mean=%.3f\n",
            min(psi_post_burn), max(psi_post_burn), mean(psi_post_burn)))
neg_psi <- sum(psi_post_burn < 0)
gt1_psi <- sum(psi_post_burn > 1)
if (neg_psi > 0) cat(sprintf("  psi < 0 in %d period(s) -- firms net-repaid green\n", neg_psi))
if (gt1_psi > 0) cat(sprintf("  psi > 1 in %d period(s) -- firms over-rotated into green\n", gt1_psi))
if (neg_psi == 0 && gt1_psi == 0) cat("  psi stayed within [0,1] naturally (no extreme allocations)\n")
neg_stock_gr  <- sum(l_fc_gr_Br[1:nPeriods]  < 0)
neg_stock_con <- sum(l_fc_con_Br[1:nPeriods] < 0)
if (neg_stock_gr  > 0) cat(sprintf("  l_fc_gr_Br  went NEGATIVE in %d period(s) -- model says firms accumulated FC green ASSETS\n", neg_stock_gr))
if (neg_stock_con > 0) cat(sprintf("  l_fc_con_Br went NEGATIVE in %d period(s) -- model says firms accumulated FC conv ASSETS\n", neg_stock_con))

################################################################################
# 9) ADDITIONAL STANDALONE PLOTS
################################################################################

par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)
plot(periods, b_cb_BrRoW_s[i_plot], type = "l", lwd = 2, col = "orange",
     main = "Br FX reserves (= Br CB holdings of RoW bills, BoP-cumulated)",
     xlab = "Period", ylab = "Reserves (T USD)")
add_guard_lines()
abline(h = b_cb_BrRoW_s[1], lty = 2, col = "grey50")
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

# Year-over-year growth rates. NOTE: index the lag with a length-preserving
# helper, NOT `series[i_plot - 1]`. When i_plot starts at 1, `i_plot - 1`
# contains 0; R DROPS index 0 (it is not NA), so the lagged vector is one
# element short, R recycles it, and the final period divides by the wrapped-
# around first element -> a spurious terminal spike. lag1() prepends NA and
# drops the last element, keeping length == length(i_plot) and aligning t-1
# correctly (first in-window period is NA, no predecessor).
lag1 <- function(x) c(NA_real_, x[-length(x)])
gdp_gr_Br_pct  <- 100 * (y_Br[i_plot]  / lag1(y_Br[i_plot])  - 1)
gdp_gr_RoW_pct <- 100 * (y_RoW[i_plot] / lag1(y_RoW[i_plot]) - 1)
kgr_gr_Br_pct  <- 100 * (k_gr_Br[i_plot]  / pmax(lag1(k_gr_Br[i_plot]), 1e-9)  - 1)
kgr_gr_RoW_pct <- 100 * (k_gr_RoW[i_plot] / pmax(lag1(k_gr_RoW[i_plot]), 1e-9) - 1)

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