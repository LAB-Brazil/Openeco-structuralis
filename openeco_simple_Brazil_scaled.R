################################################################################
#
# OPENECO-UNIFIED model — pure post-2017 simulation in R
# Simplified single-household variant of Carnevali, Deleidi, Pariboni,
# Veronese Passarella (2021).
#r_l_LC_MDB -> MDB funding costs is 1% before, the markdown was set to reduce Firms interest rates from 5% to 3% This means that the cost of finance for the MDB was positive regardles
#bk_sens -> How sensitive our green investment is to the markdown

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
#   (3) Br CB DOMESTIC (BRL) bill holding is the residual of the BRL-bill
#       market (Bortz eq. 71a); that market clearing is the redundant check.
#   (4) Current account includes interest earned ON RESERVES (Bortz 75a).
#   (5) Peg xr_RoW = xr_Br = 1 (Bortz 73uFX/73a); all CB profit remitted to the
#       Treasury via f_cb_Br, holding CB net worth ~constant.
# ---------------------------------------------------------------------------
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
  "cons_Br","cons_RoW",
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
  # per-sector net lending (current-account surplus) + global redundant sum.
  # Sum over all 13 sectors must be 0 every period (Walras' law / horizontal
  # consistency). This is an INDEPENDENT consistency check on the whole model.
  "nl_hh_Br","nl_fc_Br","nl_fk_Br","nl_bank_Br","nl_gov_Br","nl_cb_Br",
  "nl_hh_RoW","nl_fc_RoW","nl_fk_RoW","nl_bank_RoW","nl_gov_RoW","nl_cb_RoW",
  "nl_mdb","nl_sum_check",
  "cab_Br","cab_RoW","kabp_Br","kabp_RoW","bp_Br","bp_RoW",
  "ca_int_paid_fc_Br","ca_int_recv_Br","ca_int_paid_lc_Br","ca_div_net_Br",
  "ka_b_in_Br","ka_b_out_Br","ka_e_in_Br","ka_e_out_Br","ka_fc_in_Br",
  "bop_resid_Br",
  "cb_nw_Br","cb_nw_RoW",                          # implied CB net worth diagnostics (V_cba; Fed gap)
  "br_bill_resid","row_bill_resid",                # redundant bill-market checks (Bortz 71a; US-bill)
  "xr_Br","xr_RoW",
  "y_mat_Br","y_mat_RoW","mat_Br","mat_RoW",
  "rec_Br","rec_RoW","dis_Br","dis_RoW",
  "dc_Br","dc_RoW","k_se_Br","k_se_RoW","wa_Br","wa_RoW",
  "k_m_Br","k_m_RoW","k_m","conv_m_Br","conv_m_RoW",
  "res_m_Br","res_m_RoW","res_m",
  "e_Br","e_RoW","er_Br","er_RoW","en_Br","en_RoW",
  "ed_Br","ed_RoW",
  "k_e_Br","k_e_RoW","k_e","conv_e_Br","conv_e_RoW",
  "res_e_Br","res_e_RoW","res_e",
  "emis_Br","emis_RoW","emis_l","emis_l_Br","emis_l_RoW","emis",
  "emis_Br_total","emis_RoW_total",   # total area emissions = productive + land use
  "co2_at","co2_up","co2_lo","f","f_ex","temp_at","temp_lo",
  "mu_Br","mu_RoW","epsilon_Br","epsilon_RoW",
  "beta_Br","beta_RoW","eta_Br","eta_RoW",
  "depl_m_Br","depl_m_RoW","depl_e_Br","depl_e_RoW",
  "d_t_Br","d_t_RoW","delta_Br","delta_RoW",
  "q_Br","q_RoW","lev_f_Br","lev_f_RoW",
  "q_nalin_Br","k_target_Br",
  "inv_d_Br","K_target_Br","lcd_Br","ldom_Br","fc_residual_Br",
  "liq_b_Br","liq_b_RoW",
  "y",
  "l_fc_Br_d","l_fc_Br_s","l_fc_Br","delta_l_fc_Br",
  "int_fc_Br",
  "l_fc_gr_Br","l_fc_con_Br",
  "delta_l_fc_gr_Br","delta_l_fc_con_Br",
  "int_fc_gr_Br","int_fc_con_Br",
  "psi_gr_share_Br",
  # --- MDB (multilateral development bank) sector -----------------------------
  # Naming standardised to the model's Br/RoW casing. MDB appears only as holder
  # (bills) or issuer (bonds/loans), so the _MDB suffix unambiguously means
  # "the MDB's". Currencies: b_RoW_MDB & Bond_MDB in FC; b_Br_MDB & l_lc_MDB in
  # LC (reais). Net worth V_MDB is reported in FC (BRL assets converted at xr,
  # an accounting convention only). reval_MDB is the FX revaluation of net worth
  # (a valuation change, NOT a transaction; zero under the peg).
  "cap_MDB",                                   # capitalization from RoW gov (FC, policy flow)
  "b_RoW_MDB",                                 # MDB holdings of RoW bills (FC asset; the carry/safe asset)
  "b_Br_MDB",                                  # MDB holdings of Br bills (LC asset; residual BRL buffer)
  "l_lc_MDB",                                  # MDB loans to Br banks, in LC (BRL asset)
  "green_loan_pool",                           # Brazil's demand for MDB concessional (green) loans, BRL (drives MDB issuance, 1-period lag)
  "Bond_MDB_s","Bond_MDB_d",                   # MDB bonds: issued (s) / held by RoW HH (d); FC liability
  "Bond_LC_MDB_s","Bond_LC_MDB_d",             # MDB BRL bonds: issued (s) / held by Br HH (d); BRL liability
  "int_bond_LC_MDB",                           # BRL-bond coupon paid by MDB to Br HH (BRL flow)
  "q_c_Br","q_d_Br",                            # Kaldor-Verdoorn productivity indices: green / conventional capital (Br)
  "q_c_RoW","q_d_RoW",                          # productivity indices: green / conventional capital (RoW)
  "V_MDB",                                     # MDB net worth in FC = b_RoW_MDB + (b_Br_MDB + l_lc_MDB)*xr - Bond_MDB_s
  "f_MDB_RoW",                                 # MDB profit remitted to RoW gov (=0 under 100% retention baseline)
  "reval_MDB",                                 # FX revaluation of MDB net worth (valuation change, not a flow; 0 under peg)
  "int_row_MDB","int_bond_MDB","int_lc_MDB","int_br_MDB",   # MDB interest flows (RoW-bill in, bond out, LC-loan in, Br-bill in)
  "ca_int_paid_mdb_Br","ka_mdb_in_Br"          # MDB's footprint in Brazil's BoP (CA interest out, KA claims in)
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
# Bueno-Kesse relative-rate sensitivity (Channel 1 price channel): desired green
# investment takes the relative-rate form chi4 * I2C * (1 - mu) * pool,
# where mu = effective green rate / conventional rate (= r_l_RoW_br / r_l_Br).
# At mu = 1 (no MDB markdown) the term is 0, so the no-policy baseline is
# unchanged; as the markdown deepens (mu falls) it raises desired green inv.
bk_i2_Br     <- 1.0
bk_sens_Br   <- 1.0   # sensitivity of the green/conventional split to the MDB rate gap (1 - mu); placeholder, calibrate after
# --- Kaldor-Verdoorn productivity loop (Carnevali et al. 2021 mechanism; green/
#     conventional application is this model's adaptation, as for the elasticities) ---
# Per-type productivity GROWTH = verdoorn_coef * that type's CAPITAL-stock growth.
# verdoorn_coef = 0.5 is Kaldor's (1966) classic estimate (Verdoorn's was 0.45).
# This is also the GAIN on the green cumulative-causation loop, so it is left at
# the citable value (not tuned) and the green share is checked for convergence.
verdoorn_coef <- 0.5
# bk_i3 = sensitivity of the green/conventional investment split to the
# productivity ratio q_C/q_D (the I_{3C} term of design eq. inv_br_clean).
bk_i3_Br     <- 0.2
# Channel 2 (capital deepening via Tobin's Q): the MDB's ADDITIONAL green lending
# l_lc_MDB(-1) enters the firm leverage ratio q_nalin directly as extra credit
# against capital (additionality principle). This raises q_nalin -> raises
# k_target -> opens a gap -> the accelerator (g_inv UNCHANGED) closes it,
# deepening K/Y to a higher level. Permanent: gated by capitalisation (cap=0 ->
# l_lc_MDB=0 -> no contribution), so no separate on/off toggle is needed.
mdb_subsidy_gn  <- 0   # subtracted from r_l_RoW
xi_sub_RoW    <- 1   # additional multiplier on r_l_RoW

ret_Br   <- 0.02   ; ret_RoW   <- 0.02
pi_dy_Br <- 0.00555; pi_dy_RoW <- 0.00555

# --- Q-channel: Nalin & Yajima (2022) leverage feedback in target capital ---
# Permanent feature (no toggle): leverage always feeds the K/Y target.
k0_Br <- 1.67   # K/Y target at baseline; Br ~ observed 2016 ratio
k1_Br <- 0.20   # Q feedback strength; moderate Minsky channel

# --- Nalin investment-financing parameters ---------------------------------
g_inv_Br      <- 0.30     # fraction of capital gap closed per period
Ia_Br         <- 0.25     # autonomous (constant) investment desire [experimental]
lambda_dom_0  <- 0        # autonomous component
lambda_dom_1  <- 0.87 #87    # share of total financing met by domestic loans
inv_nalin_weight <- 1
xi_Br    <- 0.01   ; xi_RoW    <- 0.01

eps0 <- -2.1 ; eps1 <- 0.5 ; eps2 <- 1.228
mu0  <- -2.1 ; mu1  <- 0.5 ; mu2_par  <- 1.228

# --- Endogenous trade elasticities (Souza & Silva / Botta structuralist channel) ---
# DRIVER: the capital-output ratio K/Y (degree of industrialisation / development,
# Botta 2009), NOT the green capital share. Development relaxes the Thirlwall BoP
# constraint regardless of green vs conventional capital, which decouples the
# greening goal (Channel 1) from the BoP-relaxation goal (Channel 2 deepens K/Y).
# We keep S&S's export:import sensitivity RATIO (0.45:0.35) and calibrate the
# magnitude so Thirlwall balance (eps = eta) is reached at a target K/Y; the
# intercepts are pinned so that at the baseline K/Y the elasticities equal their
# prior baseline values (so the validated baseline run is preserved).
use_endog_elast <- TRUE
ky_base_el  <- 1.67          # reference (baseline) K/Y, = k0_Br
eps_base_el <- 0.922         # export income elasticity at baseline K/Y (prior baseline)
eta_base_el <- 1.044         # import income elasticity at baseline K/Y (prior baseline)
ky_cross_el <- 1.74          # K/Y at which eps = eta (Thirlwall balance) -- KEY CALIBRATION
sens_ratio  <- 0.45 / 0.35   # S&S export:import sensitivity ratio (preserved)
# solve slopes so the crossover lands exactly at ky_cross_el, keeping sens_ratio
phi1_m_par <- (eta_base_el - eps_base_el) /
  ((ky_cross_el - ky_base_el) * (1 + sens_ratio))
zeta1_par  <- sens_ratio * phi1_m_par
zeta0_par  <- eps_base_el - zeta1_par  * ky_base_el     # exports: eps = zeta0 + zeta1*(K/Y)
phi0_m_par <- eta_base_el + phi1_m_par * ky_base_el     # imports: eta = phi0  - phi1 *(K/Y)


lambda10 <- 0.14707 ; lambda11 <- 1 ; lambda12 <- 1 ; lambda13 <- 0    ; lambda14 <- 0
lambda20 <- 0.04902 ; lambda21 <- 1 ; lambda22 <- 1 ; lambda23 <- 0    ; lambda24 <- 0
lambda40 <- 0.14707 ; lambda41 <- 1 ; lambda42 <- 1 ; lambda43 <- 0    ; lambda44 <- 0
lambda70 <- 0.02451
lambda80 <- 0.02451
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
r_a_Br     <- r_Br * (1 + upsilon_Br)  # = 0.011 (bill rate + advances penalty)

# --- Foreign-currency loans to Br firms (from RoW banks) -------------------
r_l_fc     <- 0.085     # FC corporate borrowing rate, Brazil 2016 anchor (legacy aggregate)
# Capital-controls toggle: when FALSE, Br firms cannot borrow abroad in FC (the
# financing gap is met entirely by domestic loans, l_fc_Br -> 0). TRUE reproduces
# the open-account baseline. Used to make the capital-controls argument: it severs
# the FC income drain that keeps the BoP in deficit despite a trade surplus.
fc_loans_on <- TRUE

# --- Split FC borrowing: green vs. conventional ----------------------------
r_l_fc_gr  <- r_l_fc    # green FC borrowing rate (initially identical)
r_l_fc_con <- r_l_fc    # conventional FC borrowing rate
kappa_fc   <- 10        # 1 pp rate gap -> 10 pp shift in green share
chi5_Br    <- 1

# --- MDB sector parameters --------------------------------------------------
# The MDB is a permanent sector. The policy lever is the capitalization
# cap_MDB (size cap_mdb_amt, one-shot at period cap_mdb_start). With
# cap_mdb_amt = 0 the sector stays dormant (every stock is zero) and the model
# reproduces the no-MDB economy exactly; there is no separate on/off flag.
# Structure:
#  - RoW gov capitalizes the MDB (cap_MDB, FC), funding it by issuing bills and
#    holding the MDB equity as an asset; the MDB parks the capital in RoW bills.
#  - MDB issues FC bonds to RoW HH at r_MDB = r_RoW (AAA indifference), capped
#    so bonds never exceed RoW-bill holdings: Bond_MDB_s <= lambda_mdb*b_RoW_MDB,
#    lambda_mdb in [0,1]. RoW-bill interest (FC) services bond interest (FC);
#    the FC book is RoW-resident on both sides (invisible to Brazil's BoP).
#  - MDB lends to Br banks in LC at the concessional rate r_l_LC_MDB; the bank
#    interest is a clean BRL flow (no xr). MDB holds the FX mismatch.
#  - The concessional loan marks down the green LC rate via mdb_subsidy_br
#    (endogenous to l_lc_MDB through the phi-blend in the loop).
#  - capitalization is a one-shot injection of cap_mdb_amt at period cap_mdb_start.
cap_mdb_amt   <- 0.1      # capitalization amount (FC); 0 = dormant baseline
cap_mdb_start <- 20       # period capitalization begins (or the one-shot date)
# Funding mix (FIXED split): a constant share lambda_fc of the lagged concessional-
# loan demand is funded by FC bonds (proceeds converted FC -> BRL) and the rest by
# BRL bonds, with lambda_fc + lambda_lc = 1 so the demand is fully funded. The fixed
# FC share keeps the MDB converting fresh hard currency in every period, so it bears
# a genuine, persistent FX mismatch (FC liabilities against reais assets) -- the
# local-currency-finance mechanism in its purest form. lambda_fc is a benchmark, not
# an estimate: defend it via a sensitivity sweep over [0, 1]. When the FC cushion is
# exhausted the MDB sells Br bills to service the FC coupon (the floor block in
# section VI-b), so the FC book need not be self-sustaining.
lambda_fc     <- 0.5      # FC share of lagged loan demand funded by FC bonds (MDB bears the FX mismatch)
lambda_lc     <- 0.5      # BRL share funded by BRL bonds (currency-matched); lambda_fc + lambda_lc = 1
r_l_LC_MDB    <- 0.008     # concessional LC rate on MDB loans to Br banks (placeholder, < r_l_Br)
r_MDB         <- r_RoW    # MDB bond coupon = RoW bill rate (RoW HH indifferent)
omega_mdb     <- 0.005    # bank intermediation spread on the MDB-funded green slice (a few bp).
#   firms pay r_l_LC_MDB + omega_mdb on that slice; the bank keeps omega_mdb
#   (passed to HH, so bank NL stays 0). omega governs subsidy INCIDENCE only.

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

co2_at_pre <- 2156.2
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

g_beta_Br <- 0.01
g_beta_RoW <- 0.02
greening_starts <- 4
beta0_base_Br  <- 0.27444   # 3.4524 * s_Br
beta0_base_RoW <- 8.87342   # 3.4524 * s_RoW
# Brazil productive-emission scale: the energy-based productive intensities
# (beta0, beta) are inherited from the global OPENECO calibration and overstate
# Brazil's industrial/energy emissions ~3x relative to its GDP scale. This
# factor rescales them so that at t=1 land use is ~63% of Br total emissions
# (Brazil NIR-2024), the empirically realistic split. Applied only to Br's
# productive coefficients; the land anchor (emis_l_Br_0) is already empirical.
prod_emis_scale_Br <- 0.31620

nIter <- 150

# ===========================================================================
# EXCHANGE-RATE CONVENTION  (read before touching anything that uses xr)
# ---------------------------------------------------------------------------
# There is ONE quote in this model, used identically by the trade block and the
# balance-sheet block. Stated once, plainly:
#
#   xr_Br = REAIS PER DOLLAR = how many reais it takes to buy ONE US dollar.
#
#   * xr_Br = 1 at the baseline peg (one real buys one dollar).
#   * HIGHER xr_Br (e.g. 1.5): it now takes MORE reais to buy a dollar.
#         -> the REAL has DEPRECIATED (the real is WEAKER, worth FEWER dollars);
#            equivalently the DOLLAR has APPRECIATED (worth MORE reais).
#   * LOWER  xr_Br (e.g. 0.5): it now takes FEWER reais to buy a dollar.
#         -> the REAL has APPRECIATED (the real is STRONGER, worth MORE dollars);
#            this is the OVERVALUATION case. The DOLLAR has depreciated.
#
#   xr_RoW = DOLLARS PER REAL = 1 / xr_Br (the same rate seen from the US side,
#   the dollar price of one real). It moves OPPOSITE to xr_Br: when the real
#   weakens (xr_Br up), xr_RoW falls. At the peg xr_RoW = xr_Br = 1.
#
# What a DEPRECIATION (xr_Br UP) does, now consistently in BOTH blocks:
#   - Trade: Brazilian goods are cheaper abroad -> exports RISE, imports FALL
#     (the +eps1 / -mu1 signs in the trade block). Overvaluation (xr_Br DOWN) is
#     the opposite and is the New-Developmentalist competitiveness loss.
#   - Balance sheet: the reais value of the DOLLAR-denominated FC debt
#     (l_fc_Br * xr_Br) RISES, a heavier burden (original sin); the reais value
#     of the dollar ASSETS Brazil holds also rises (the cg_* capital gains).
#   So one move (xr_Br up = depreciation) is at once a competitiveness gain AND a
#   heavier dollar-debt burden: the contractionary-depreciation tension.
# ===========================================================================

# --- FX closure: single Bortz (2014) fixed exchange rate peg ---------------
# Peg xr_RoW = xr_Br = 1 (Bortz 73uFX/73a). Br CB reserves = its stock of RoW
# (US) bills, cumulated from the balance of payments (Bortz 76a / KABOSA: the
# official-settlement item). The US-bill market clears on the Fed (residual);
# the BRL-bill market clears on the Br CB (residual, Bortz 71a). No toggle.

# --- Exogenous exchange-rate (re-peg) shock --------------------------------
# Independent of the MDB / capital-controls / baseline settings: a one-off,
# PERMANENT re-peg of the BRL at period xr_shock_start. xr_Br is the BRL/USD
# peg (1 = baseline), xr_RoW = 1/xr_Br its mirror. xr_shock_delta > 0 is a
# depreciation (more reais per dollar), < 0 an appreciation. The revaluation of
# cross-border holdings is booked through the cg_* terms, so the re-peg is
# stock-flow consistent. Off by default; set xr_shock_on <- TRUE and choose the
# sign of xr_shock_delta to run the up / down scenarios.
xr_shock_on    <- FALSE  # master toggle (orthogonal to every other switch)
xr_shock_start <- 40      # period the re-peg occurs (permanent thereafter)
xr_shock_delta <- 0.5     # change in the BRL/USD peg: +0.5 -> 1.5 (deprec.), -0.5 -> 0.5 (apprec.)

# --- Exogenous external-demand shock: RoW cuts its imports -----------------
# Independent of every other toggle. In this two-country model RoW's imports ARE
# Brazil's exports (im_RoW = x_Br * xr_RoW), so a fall in RoW import demand is a
# permanent fractional cut to Brazil's exports x_Br from rowimp_shock_start: a
# global / commodity demand shock to the periphery's export market. Off by default.
rowimp_shock_on    <- FALSE   # master toggle (orthogonal to the xr and capital-controls switches)
rowimp_shock_start <- 40      # period the import fall begins (permanent thereafter)
rowimp_shock_size  <- 0.5     # fractional fall in RoW imports (= Br exports): 0.2 = a 20% drop

################################################################################
# 5) INITIAL CONDITIONS (period 1)
################################################################################

y[1]          <- 100.000 ; y_Br[1]    <- 3.000    ; y_RoW[1]    <- 97.000
cons_Br[1] <- 1.951    ; cons_RoW[1] <- 63.100
y_w_Br[1]  <- 1.860   ; y_w_RoW[1]  <- 60.140
y_h_Br[1]  <- 2.795   ; y_h_RoW[1]  <- 90.376
yd_Br[1]   <- 2.392   ; yd_RoW[1]   <- 77.345
yd_hs_Br[1]<- 2.392   ; yd_hs_RoW[1]<- 77.345
t_Br[1]    <- 0.403   ; t_RoW[1]    <- 13.030

inv_Br[1]   <- 0.549  ; inv_RoW[1]   <- 17.745
k_Br[1]     <- 4.793  ; k_RoW[1]     <- 154.987
k_con_Br[1] <- 4.104  ; k_con_RoW[1] <- 132.731
k_gr_Br[1]  <- 0.688  ; k_gr_RoW[1]  <- 22.257
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
# in bills; the Br CB holds the BRL-bill-market residual (Bortz eq. 71a -- the
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

# --- MDB initial conditions (the sector starts empty and is built up by ------
# capitalization once cap_mdb_amt > 0). All zero; explicit here so the starting
# state is visible and editable.
cap_MDB[1]      <- 0
b_RoW_MDB[1]    <- 0
b_Br_MDB[1]     <- 0
l_lc_MDB[1]     <- 0
Bond_MDB_s[1]   <- 0 ; Bond_MDB_d[1] <- 0
Bond_LC_MDB_s[1] <- 0 ; Bond_LC_MDB_d[1] <- 0 ; int_bond_LC_MDB[1] <- 0
V_MDB[1]        <- b_RoW_MDB[1] + (b_Br_MDB[1] + l_lc_MDB[1] - Bond_LC_MDB_s[1]) / xr_Br[1] - Bond_MDB_s[1]  # = 0; BRL assets -> FC via /xr_Br
f_MDB_RoW[1]    <- 0
reval_MDB[1]    <- 0
int_row_MDB[1]  <- 0 ; int_bond_MDB[1] <- 0 ; int_lc_MDB[1] <- 0 ; int_br_MDB[1] <- 0
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
af_Br[1] <- 0.653433261380905
af_RoW[1] <- 24.9976478176183
b_Br_bank[1] <- 13.0006922961507
b_Br_bank_not[1] <- 13.0006922961507
b_Br_s[1] <- 17.2044117140581
b_BrBr_d[1] <- 3.05948944879654
b_BrBr_s[1] <- 3.05948944879654
b_BrRoW_d[1] <- 1.01976047310809
b_BrRoW_s[1] <- 1.01976047310809
b_cb_BrBr_s[1] <- 0.910229969110858
b_cb_BrRoW_s[1] <- 2.09559678190542
b_cb_RoWRoW_s[1] <- 77.4305640343647
b_RoW_bank[1] <- 411.880267237103
b_RoW_bank_not[1] <- 411.880267237103
b_RoW_s[1] <- 597.602808982198
b_RoWBr_d[1] <- 0.234
b_RoWBr_s[1] <- 0.234
b_RoWRoW_d[1] <- 105.176620455717
b_RoWRoW_s[1] <- 105.176620455717
beta_Br[1] <- 0.0173412635
beta_RoW[1] <- 0.0330098265726167
bop_resid_Br[1] <- -1.04083408558608e-16
bp_Br[1] <- -0.0152923549663414
bp_RoW[1] <- 0.0152923549663414
br_bill_resid[1] <- 0
ca_div_net_Br[1] <- 0
ca_int_paid_fc_Br[1] <- 0.0215158646958872
ca_int_paid_lc_Br[1] <- 0.00234
ca_int_recv_Br[1] <- 0.0310492865162668
cab_Br[1] <- 0.0063118231783399
cab_RoW[1] <- -0.0063118231783399
cb_nw_Br[1] <- 2.02434326350916
cb_nw_RoW[1] <- 40.6506300330351
cg_b_Br[1] <- 0
cg_b_RoW[1] <- 0
cg_e_Br[1] <- 0
cg_e_RoW[1] <- 0
cons_Br[1] <- 2.31497232385835
cons_RoW[1] <- 76.0551673223652
conv_e_Br[1] <- 36.1560104236993
conv_e_RoW[1] <- 1169.04423204524
conv_m_Br[1] <- 5.21677533539218
conv_m_RoW[1] <- 168.675785087515
d_t_Br[1] <- 0.0130627367030944
d_t_RoW[1] <- 0.0130627367030944
da_Br[1] <- 0.653433261380905
da_con_Br[1] <- 0.545519887993178
da_con_RoW[1] <- 21.4776238714506
da_gr_Br[1] <- 0.107913373387727
da_gr_RoW[1] <- 3.52002394616776
da_RoW[1] <- 24.9976478176183
dc_Br[1] <- 70.9924014105701
dc_RoW[1] <- 2405.13344713823
delta_Br[1] <- 0.103460466398434
delta_l_fc_Br[1] <- 0.00411678020849315
delta_l_fc_con_Br[1] <- 0.00205839010424658
delta_l_fc_gr_Br[1] <- 0.00205839010424658
delta_RoW[1] <- 0.103460466398434
dep_bank_Br[1] <- 14.7222523126068
dep_bank_RoW[1] <- 537.906534769446
dep_Br[1] <- 14.7222523126068
dep_RoW[1] <- 537.906534769446
depl_e_Br[1] <- 0.017534399388887
depl_e_RoW[1] <- 0.0111106181811156
depl_m_Br[1] <- 0.00840604101559453
depl_m_RoW[1] <- 0.00573012667778014
dis_Br[1] <- 1.41917049856001
dis_RoW[1] <- 38.6201868339554
e_Br[1] <- 34.8104560338252
e_Br_real_s[1] <- 0.130130035200454
e_BrBr_d[1] <- 1.0199615058086
e_BrBr_s[1] <- 1.0199615058086
e_BrRoW_d[1] <- 0
e_BrRoW_s[1] <- 0
e_RoW[1] <- 910.577236124086
e_RoW_real_s[1] <- 8.06290954045559
e_RoWBr_d[1] <- 0
e_RoWBr_s[1] <- 0
e_RoWRoW_d[1] <- 35.0495787256175
e_RoWRoW_s[1] <- 35.0495787256175
ed_Br[1] <- 34.8104560338252
ed_RoW[1] <- 910.577236124086
emis[1] <- 32.717502
emis_Br[1] <- 0.6829393924
emis_l[1] <- 1.44227038335001
emis_l_Br[1] <- 0.991688595379593
emis_l_RoW[1] <- 0.45058178797042
emis_RoW[1] <- 30.5922920312913
en_Br[1] <- 34.3782022752289
en_RoW[1] <- 852.124633768816
epsilon_Br[1] <- 9.04350691560243
epsilon_RoW[1] <- 7.0829784068751
er_Br[1] <- 0.432253758596338
er_RoW[1] <- 58.4526023552699
eta_Br[1] <- 0.0124173540897114
eta_RoW[1] <- 0.0641929097679578
f[1] <- 3.68938589187745
f_bank_Br[1] <- 0.185859304588024
f_bank_RoW[1] <- 8.42022728358624
f_Br[1] <- 0.72846450055589
f_cb_Br[1] <- 0.0298107041799611
f_cb_RoW[1] <- 0.765954468957963
f_d_BrBr[1] <- 0.705986370788939
f_d_BrRoW[1] <- 0
f_d_RoWBr[1] <- 0
f_d_RoWRoW[1] <- 18.6795816208822
f_ex[1] <- 0.755
f_m_Br[1] <- 0
f_m_RoW[1] <- 0
f_RoW[1] <- 19.4525520608623
fc_residual_Br[1] <- 0.257244600160108
fd_Br[1] <- 0.705986370788939
fd_RoW[1] <- 18.6795816208822
fu_Br[1] <- 0.0224781297669508
fu_RoW[1] <- 0.772970439980035
gov_con_Br[1] <- 0.722338322228999
gov_con_RoW[1] <- 23.3690785963047
gov_gr_Br[1] <- 0.0795
gov_gr_RoW[1] <- 2.5703
gov_tot_Br[1] <- 0.801838322228999
gov_tot_RoW[1] <- 25.9393785963047
h_Br_h[1] <- 0.981483487507122
h_Br_s[1] <- 0.981483487507122
h_RoW_h[1] <- 36.7799340013296
h_RoW_s[1] <- 36.7799340013296
im_Br[1] <- 1.0907769096369
im_RoW[1] <- 1.08989531099486
int_fc_Br[1] <- 0.0215158646958872
int_fc_con_Br[1] <- 0.0228774816907352
int_fc_gr_Br[1] <- -0.00136161699484794
inv_Br[1] <- 0.733291875222204
inv_con_Br[1] <- 0.609526526567174
inv_con_RoW[1] <- 22.5515132990528
inv_d_Br[1] <- 0.733291875222204
inv_gr_Br[1] <- 0.12376534865503
inv_gr_Br_t[1] <- 0.12376534865503
inv_gr_RoW[1] <- 4.01158129351948
inv_gr_RoW_t[1] <- 4.01158129351948
inv_RoW[1] <- 26.5630945925723
# --- Brazil capital placed on the leveraged Tobin-Q target it actually pursues ---
# k_Br[1] is set to K_target[2], the fixed point of
#   k = (k0_Br + k1_Br*(l_firm_Br[1] + l_fc_Br[1]*xr_Br + l_lc_MDB[1])/k) * y_Br[1],
# i.e. the leveraged target ratio K/Y = 1.7295 (vs the unleveraged k0_Br = 1.67,
# which left a t=2 accelerator gap). The extra capital is a real asset with no
# financial counterpart, so it is absorbed entirely by firm net worth -- no other
# sector's balance sheet moves and the SFC audit still closes. k_con_Br[1] and
# k_gr_Br[1] are scaled by the same factor so that k_Br = k_gr + k_con holds and
# the baseline green capital ratio (k_gr/k_con = 0.1984) is preserved.
k_Br[1] <- 6.6570352282119
k_con_Br[1] <- 5.5548650466600
k_con_RoW[1] <- 208.666457092421
k_e[1] <- 78973.9316641407
k_e_Br[1] <- 1962.39256961085
k_e_RoW[1] <- 77011.5390945299
k_gr_Br[1] <- 1.1021701815519
# Kaldor-Verdoorn productivity indices start at 1 in both countries, so the
# q_C/q_D ratio is exactly 1 at t=1 and the productivity loop is INERT until
# green and conventional capital growth diverge (no baseline effect).
q_c_Br[1] <- 1 ; q_d_Br[1] <- 1 ; q_c_RoW[1] <- 1 ; q_d_RoW[1] <- 1
k_gr_RoW[1] <- 34.5144461734265
k_m[1] <- 12890.9940904518
k_m_Br[1] <- 350.95198301113
k_m_RoW[1] <- 12540.0421074407
k_RoW[1] <- 243.180903265847
k_se_Br[1] <- 102.782185290017
k_se_RoW[1] <- 2534.24860933472
k_target_Br[1] <- 1.73165945995951
K_target_Br[1] <- 6.58197264326648
ka_b_in_Br[1] <- 0
ka_b_out_Br[1] <- 0.0257209583531745
ka_e_in_Br[1] <- 0
ka_e_out_Br[1] <- 0
ka_fc_in_Br[1] <- 0.00411678020849315
kabp_Br[1] <- -0.0216041781446813
kabp_RoW[1] <- 0.0216041781446813
l_fc_Br[1] <- 0.257244600160108
l_fc_Br_d[1] <- 0.257244600160108
l_fc_Br_s[1] <- 0.257244600160108
l_fc_con_Br[1] <- 0.27120523352466
l_fc_gr_Br[1] <- -0.0139606333645527
l_firm_Br[1] <- 1.7215600164561
l_firm_RoW[1] <- 125.769022932182
l_s_Br[1] <- 1.7215600164561
l_s_RoW[1] <- 125.769022932182
lcd_Br[1] <- 1.97880461661621
ldom_Br[1] <- 1.7215600164561
lev_f_Br[1] <- 0.269177302991213
lev_f_RoW[1] <- 0.517182974662656
liq_b_Br[1] <- 0.883064086941241
liq_b_RoW[1] <- 0.766187962401147
mat_Br[1] <- 2.93090161544949
mat_RoW[1] <- 71.2980430227047
mu_Br[1] <- 0.835165291820577
mu_RoW[1] <- 0.638710635348063
nafa_Br[1] <- 0.46732286870359
nafa_RoW[1] <- 15.4394394486898
p_e_Br[1] <- 7.83801759707076
p_e_RoW[1] <- 4.34701376094529
psbr_Br[1] <- 0.46101104552525
psbr_RoW[1] <- 15.4457512718682
psi_gr_share_Br[1] <- 0.5
q_Br[1] <- 0.428655035201552
q_nalin_Br[1] <- 0.308297299797546
q_RoW[1] <- 0.661312625695743
r_e_Br[1] <- 0.014010865427542
r_e_Br_t[1] <- 0.732678455412974
r_e_RoW[1] <- 0.0130963344428863
r_e_RoW_t[1] <- 0.567898097817355
rec_Br[1] <- 0.283834099712002
rec_RoW[1] <- 10.8136523135075
res_e[1] <- 679698.891547891
res_e_Br[1] <- 20390.9685227398
res_e_RoW[1] <- 659307.923025151
res_m[1] <- 511274.814565776
res_m_Br[1] <- 15338.2400934651
res_m_RoW[1] <- 495936.574472311
row_bill_resid[1] <- -1.13686837721616e-13
t_Br[1] <- 0.478450579209117
t_RoW[1] <- 15.5492434325819
tb_Br[1] <- -0.000881598642039627
tb_RoW[1] <- 0.000881598642039627
temp_at[1] <- 2.10024167460171
temp_lo[1] <- 0.333748813029728
v_Br[1] <- 20.8029472278271
v_RoW[1] <- 715.14666795211
wa_Br[1] <- 1.13533639884801
wa_RoW[1] <- 27.806534520448
x_Br[1] <- 1.08989531099486
x_RoW[1] <- 1.0907769096369
xr_Br[1] <- 1
xr_RoW[1] <- 1
y[1] <- 132.407743032552
y_Br[1] <- 3.84922092266752
y_h_Br[1] <- 3.31812625584541
y_h_RoW[1] <- 107.836326538611
y_mat_Br[1] <- 3.21473571516149
y_mat_RoW[1] <- 82.1116953362122
y_RoW[1] <- 128.558522109884
y_w_Br[1] <- 2.38651697205386
y_w_RoW[1] <- 79.7062837081282
yd_Br[1] <- 2.83967567663629
yd_hs_Br[1] <- 2.83967567663629
yd_hs_RoW[1] <- 92.2870831060289
yd_RoW[1] <- 92.2870831060289
z_Br[1] <- 1
z_RoW[1] <- 1

y_Br_p1_ref <- y_Br[1]
k_Br_p1_ref <- k_Br[1]
# total area emissions at the IC (productive + land), so the t=1 plot point is correct
emis_Br_total[1]  <- emis_Br[1]  + emis_l_Br[1]
emis_RoW_total[1] <- emis_RoW[1] + emis_l_RoW[1]

################################################################################
# 6) SIMULATION LOOP
################################################################################

xr_guard_fired <- logical(nPeriods)

for (i in 2:nPeriods) {
  
  xr_RoW[i] <- xr_RoW[i-1]
  xr_Br[i] <- xr_Br[i-1]
  
  if (i >= greening_starts) {
    cint_b_mult <- (1 + g_beta_Br)^(-(i - greening_starts))
    cint_g_mult <- (1 + g_beta_RoW)^(-(i - greening_starts))
  } else {
    cint_b_mult <- 1
    cint_g_mult <- 1
  }
  beta0_b_now <- beta0_base_Br  * cint_b_mult * prod_emis_scale_Br
  beta0_g_now <- beta0_base_RoW * cint_g_mult
  
  for (iter in 1:nIter) {
    
    # ------------------------ I. INCOME AND WEALTH ------------------------ #
    # SOURCE: transactions-flow accounting (Godley & Lavoie 2007, ch.11; Bortz 2014).
    # cg_* are FX capital gains on cross-border holdings -- Bortz eq.70a (bill demands
    # appear in differences precisely because exchange-rate moves revalue them).
    
    cg_b_Br[i]    <- (xr_Br[i] - xr_Br[i-1]) * b_BrRoW_s[i-1]
    cg_e_Br[i]    <- (xr_Br[i] - xr_Br[i-1]) * e_BrRoW_s[i-1]
    cg_b_RoW[i]    <- (xr_RoW[i] - xr_RoW[i-1]) * b_RoWBr_s[i-1]
    cg_e_RoW[i]    <- (xr_RoW[i] - xr_RoW[i-1]) * e_RoWBr_s[i-1]
    
    y_h_Br[i] <- y_w_Br[i] + f_m_Br[i] + f_bank_Br[i] +
      r_Br*b_BrBr_s[i-1] +
      r_Br*Bond_LC_MDB_d[i-1] +
      xr_Br[i]*r_RoW*b_BrRoW_s[i-1] +
      f_d_BrBr[i] + f_d_BrRoW[i]
    y_h_RoW[i] <- y_w_RoW[i] + f_m_RoW[i] + f_bank_RoW[i] +
      r_RoW*b_RoWRoW_s[i-1] +
      r_MDB*Bond_MDB_d[i-1] +                 # coupon on MDB bonds held by RoW HH
      xr_RoW[i]*r_Br*b_RoWBr_s[i-1] +
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
    # SOURCE: Godley & Lavoie (2007) consumption function -- spending out of current
    # disposable income (alpha1) and lagged net wealth (alpha2); output is demand-led
    # (Kaleckian closure).
    
    cons_Br[i] <- (alpha1_Br*yd_Br[i] + alpha2_Br*v_Br[i-1]) *
      (1 - d_t_Br[i-1])
    cons_RoW[i] <- (alpha1_RoW*yd_RoW[i] + alpha2_RoW*v_RoW[i-1]) *
      (1 - d_t_RoW[i-1])
    
    y_Br[i] <- cons_Br[i] + gov_tot_Br[i] +
      x_Br[i] - im_Br[i] + inv_Br[i]
    y_RoW[i] <- cons_RoW[i] + gov_tot_RoW[i] +
      x_RoW[i] - im_RoW[i] + inv_RoW[i]
    y_w_Br[i] <- y_Br[i] * omega_Br
    y_w_RoW[i] <- y_RoW[i] * omega_RoW
    
    share_gr_b_lag <- if (k_Br[i-1] > 0) k_gr_Br[i-1] / k_Br[i-1] else 0
    # Endogenous green-rate markdown from MDB concessional funding (phi-blend).
    # phi = MDB-funded share of the green domestic loan pool that the markdown
    # applies to (= l_firm_Br * green capital share). The marked-down green rate
    # blends the concessional MDB rate (r_l_LC_MDB) into that share, so the
    # reduction vs the unsubsidised rate is phi*(r_l_Br - r_l_LC_MDB), self-
    # bounded by the blend. With no capitalization, l_lc_MDB = 0 -> phi = 0 -> the
    # markdown is zero and the green rate is unchanged (no policy = no subsidy).
    green_loan_pool[i] <- l_firm_Br[i-1] * share_gr_b_lag
    phi_mdb <- if (green_loan_pool[i] > 1e-9)
      min(l_lc_MDB[i-1] / green_loan_pool[i], 1) else 0
    # Design eqs. mdb_reff / mdb_mu: the effective (blended) green rate firms pay
    # is the bank's ordinary rate on the (1-phi) own-funded slice and the
    # concessional rate plus a small bank spread on the phi MDB-funded slice.
    # Self-bounded in [r_l_LC_MDB+omega_mdb, r_l_Br]; phi=0 -> r_l_RoW_br = r_l_Br
    # (no markdown, baseline). The markdown factor is mu = r_l_RoW_br / r_l_Br.
    r_l_RoW_br   <- (1 - phi_mdb) * r_l_Br + phi_mdb * (r_l_LC_MDB + omega_mdb)
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
    # SOURCE: flexible (capital-stock-adjustment) accelerator -- Harrod (1939), Domar
    # (1946), Godley & Lavoie (2007, ch.11), Bortz (2014): inv closes fraction g_inv of
    # the gap to a target capital stock, whose K/Y rises with the firm leverage ratio
    # q_nalin (debt/capital) -- the leveraged Tobin's-Q channel of Nalin & Yajima (2022),
    # Minskyan (more leverage -> more desired capital; cf. Dafermos et al. 2018). RoW is
    # pure-gamma (Kaleckian autonomous+government), a deliberate core-periphery asymmetry.
    
    da_gr_Br[i]  <- delta_Br[i] * k_gr_Br[i-1]
    da_con_Br[i] <- delta_Br[i] * k_con_Br[i-1]
    da_Br[i]     <- da_gr_Br[i] + da_con_Br[i]
    af_Br[i]     <- da_Br[i]
    da_gr_RoW[i]  <- delta_RoW[i] * k_gr_RoW[i-1]
    da_con_RoW[i] <- delta_RoW[i] * k_con_RoW[i-1]
    da_RoW[i]     <- da_gr_RoW[i] + da_con_RoW[i]
    af_RoW[i]     <- da_RoW[i]
    
    # Channel 2: MDB additional green lending l_lc_MDB(-1) enters the firm
    # leverage ratio as extra credit against capital (additionality), lifting
    # q_nalin -> k_target.
    q_nalin_Br[i] <- (l_firm_Br[i-1] + l_fc_Br[i-1] * xr_Br[i] + l_lc_MDB[i-1]) /
      k_Br[i-1]
    k_target_Br[i] <- k0_Br + k1_Br * q_nalin_Br[i]
    
    K_target_Br[i] <- k_target_Br[i] * y_Br[i-1]
    inv_d_Br[i] <- Ia_Br + g_inv_Br * (K_target_Br[i] - k_Br[i-1]) + da_Br[i]
    
    inv_gamma_Br <- (gamma0_Br + gamma1_Br*inv_Br[i-1] +
                       gamma2_Br*gov_tot_Br[i-1]) * (1 - d_t_Br[i-1])
    inv_Br[i] <- inv_nalin_weight * inv_d_Br[i] +
      (1 - inv_nalin_weight) * inv_gamma_Br
    
    inv_RoW[i] <- (gamma0_RoW + gamma1_RoW*inv_RoW[i-1] +
                     gamma2_RoW*gov_tot_RoW[i-1]) * (1 - d_t_RoW[i-1])
    
    # --- Green/conventional split: balanced-growth allocation of the Nalin total ---
    # SOURCE: Bueno & Kesse cost-of-finance channel. The split is centred on the
    # CURRENT (lagged) green capital share, so under balanced rates each type is
    # the same fraction of its own stock -> both grow at the same rate -> the
    # green/conventional ratio is static (only the initial LEVELS differ). The MDB
    # markdown mu = r_green/r_con < 1 tilts the split toward green through the
    # interest channel; shares sum to 1 so the total stays the Nalin accelerator
    # (no double-count). mu = 1 -> tilt 0 -> share = stock share -> static ratio.
    mu_gr_Br <-  r_l_RoW_br / r_l_con_br 
    s_gr_base_Br <-  k_gr_Br[i-1] / k_Br[i-1] 
    s_gr_base_Br <- min(max(s_gr_base_Br, 1e-9), 1 - 1e-9)
    share_gr_Br  <- 1 / (1 + exp(-(log(s_gr_base_Br/(1 - s_gr_base_Br)) +
                                     bk_sens_Br * (1 - mu_gr_Br))))
    inv_gr_Br[i]  <- share_gr_Br * inv_Br[i]
    inv_con_Br[i] <- inv_Br[i] - inv_gr_Br[i]
    # RoW: no MDB, split left on its chi form for now (pending the same treatment)
    inv_gr_RoW_t[i] <- (chi1_RoW*gov_gr_RoW[i] + chi2_RoW*y_RoW[i] +
                          chi3_RoW*d_t_RoW[i-1]) * (1 - d_t_RoW[i-1])
    inv_gr_RoW[i]   <- min(inv_gr_RoW_t[i], inv_RoW[i])
    inv_con_RoW[i]  <- inv_RoW[i] - inv_gr_RoW[i]
    
    k_gr_Br[i]  <- k_gr_Br[i-1]  + inv_gr_Br[i]  - da_gr_Br[i]
    k_con_Br[i] <- k_con_Br[i-1] + inv_con_Br[i] - da_con_Br[i]
    k_Br[i]     <- k_gr_Br[i] + k_con_Br[i]
    k_gr_RoW[i]  <- k_gr_RoW[i-1]  + inv_gr_RoW[i]  - da_gr_RoW[i]
    k_con_RoW[i] <- k_con_RoW[i-1] + inv_con_RoW[i] - da_con_RoW[i]
    k_RoW[i]     <- k_gr_RoW[i] + k_con_RoW[i]
    
    # ---- Kaldor-Verdoorn productivity loop (dynamic Verdoorn law, per capital type) ----
    # Each type's productivity grows at verdoorn_coef times that type's capital-
    # stock growth rate (Carnevali et al. 2021 mechanism; green/conventional split
    # is this model's adaptation). The q_C/q_D RATIO is what enters the investment
    # split, so the common autonomous drift cancels and only DIFFERENTIAL capital
    # growth moves it: green accumulating faster -> q_C/q_D rises -> more green
    # investment -> faster green accumulation (cumulative causation).
    gK_gr_Br  <- if (k_gr_Br[i-1]  > 1e-9) (k_gr_Br[i]  - k_gr_Br[i-1])  / k_gr_Br[i-1]  else 0
    gK_con_Br <- if (k_con_Br[i-1] > 1e-9) (k_con_Br[i] - k_con_Br[i-1]) / k_con_Br[i-1] else 0
    q_c_Br[i] <- q_c_Br[i-1] * (1 + verdoorn_coef * gK_gr_Br)
    q_d_Br[i] <- q_d_Br[i-1] * (1 + verdoorn_coef * gK_con_Br)
    gK_gr_RoW  <- if (k_gr_RoW[i-1]  > 1e-9) (k_gr_RoW[i]  - k_gr_RoW[i-1])  / k_gr_RoW[i-1]  else 0
    gK_con_RoW <- if (k_con_RoW[i-1] > 1e-9) (k_con_RoW[i] - k_con_RoW[i-1]) / k_con_RoW[i-1] else 0
    q_c_RoW[i] <- q_c_RoW[i-1] * (1 + verdoorn_coef * gK_gr_RoW)
    q_d_RoW[i] <- q_d_RoW[i-1] * (1 + verdoorn_coef * gK_con_RoW)
    
    lcd_Br[i] <- l_firm_Br[i-1] + l_fc_Br[i-1] * xr_Br[i] +
      inv_Br[i] - af_Br[i] - fu_Br[i] -
      (e_RoWBr_s[i] - e_RoWBr_s[i-1]) -
      (e_BrBr_s[i] - e_BrBr_s[i-1])
    # Capital controls (fc_loans_on = FALSE): firms fund the WHOLE financing gap
    # domestically, so the FC residual is zero and l_fc_Br -> 0. TRUE keeps the
    # lambda_dom_1 split (a share borrowed abroad in FC).
    ldom_Br[i] <- if (fc_loans_on) lambda_dom_0 + lambda_dom_1 * lcd_Br[i] else lcd_Br[i]
    l_firm_Br[i] <- ldom_Br[i]
    fc_residual_Br[i] <- lcd_Br[i] - ldom_Br[i]
    # SOURCE: Bortz (2014) eq.31ai -- the financing need not met by domestic loans is
    # borrowed abroad in FC (l_fc); loan and interest convert at xr, so a real depreciation
    # raises the BRL burden -- the foreign-currency mismatch on the firm balance sheet.
    fc_demand_fc_units <- fc_residual_Br[i] / xr_Br[i]
    fc_demand_fc_units <- max(fc_demand_fc_units, 0)
    l_fc_Br_d[i] <- fc_demand_fc_units
    l_fc_Br_s[i] <- l_fc_Br_d[i]
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
    l_firm_RoW[i] <- l_firm_RoW[i-1] + inv_RoW[i] - af_RoW[i] - fu_RoW[i] -
      (e_BrRoW_s[i] - e_BrRoW_s[i-1]) -
      (e_RoWRoW_s[i] - e_RoWRoW_s[i-1])
    
    # -------------------------- IV. TRADE ---------------------------------- #
    # SOURCE: imperfect-substitutes trade (Houthakker-Magee 1969; Bortz 2014) -- exports
    # fall in the real exchange rate / rise with foreign income, imports the reverse
    # (price terms = MARSHALL-LERNER). The export/import income-elasticity ratio is the
    # engine of THIRLWALL'S LAW (balance-of-payments-constrained growth, Thirlwall 1979).
    # Endogenous elasticities on K/Y = Souza & Silva (2024)/Botta structuralist channel
    # (this model's adaptation). Trade & import-price conversion: Bortz (2014) eq.6a/6u.
    
    yb_i    <- max(y_Br[i], 1e-6)
    yg_i    <- max(y_RoW[i], 1e-6)
    xrb_lag <- max(xr_Br[i-1], 1e-6)
    
    if (use_endog_elast) {
      ky_dev_b <- if (y_Br[i-1] > 0) k_Br[i-1]/y_Br[i-1] else ky_base_el
      eps_x_b <- zeta0_par  + zeta1_par * ky_dev_b
      eta_m_b <- phi0_m_par - phi1_m_par * ky_dev_b
      X0_eff <- log(0.8406) - eps_x_b * log(97.000)
      M0_eff <- log(0.8407) - eta_m_b * log(3.000)
    } else {
      eps_x_b <- eps2
      eta_m_b <- mu2_par
      X0_eff  <- eps0
      M0_eff  <- mu0
    }
    
    # xr quoted as reais per dollar (consistent with the FC-debt / reserve /
    # revaluation side): a HIGHER xr_Br is a DEPRECIATION, so exports rise (+eps1)
    # and imports fall (-mu1). Overvaluation is a LOW xr_Br (fewer exports, more
    # imports). Signs aligned to the balance-sheet convention so a depreciation is
    # competitive AND raises the dollar-debt burden (contractionary-depreciation
    # tension). Moot at the peg (log(1)=0); only the off-peg shock response differs.
    x_Br[i]  <- exp(X0_eff + eps1*log(xrb_lag) + eps_x_b*log(yg_i)) *
      (1 - ad_exp*d_t_Br[i-1])
    # External-demand shock (independent toggle): RoW cuts its imports from Brazil
    # exogenously at rowimp_shock_start. RoW imports = Brazil's exports here
    # (im_RoW = x_Br * xr_RoW), so this scales x_Br down by rowimp_shock_size and
    # im_RoW falls with it. Brazil's export revenue, trade balance, and reserves
    # take the hit; a global / commodity-demand shock to the periphery.
    if (rowimp_shock_on && i >= rowimp_shock_start) {
      x_Br[i] <- x_Br[i] * (1 - rowimp_shock_size)
    }
    im_Br[i] <- exp(M0_eff - mu1*log(xrb_lag) + eta_m_b*log(yb_i)) *
      (1 + ad_im *d_t_RoW[i-1])
    x_RoW[i]  <- im_Br[i] * xr_RoW[i]
    im_RoW[i] <- x_Br[i]  * xr_RoW[i]
    tb_Br[i] <- x_Br[i] - im_Br[i]
    tb_RoW[i] <- x_RoW[i] - im_RoW[i]
    
    # ------------------- V. PORTFOLIO DEMANDS ------------------------------ #
    # SOURCE: Tobinesque portfolio choice (Tobin 1969; Brainard & Tobin 1968; Godley &
    # Lavoie 2007, ch.11; Bortz 2014) -- wealth split across assets in shares that respond
    # to relative expected returns, with Brainard-Tobin adding-up constraints on the
    # lambdas. Foreign demand for Br bills (b_RoWBr_d) is fixed at the peg (Bortz 66ai).
    
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
    
    b_BrBr_s[i] <- b_BrBr_d[i] - Bond_LC_MDB_d[i]
    # RoW HH are indifferent between RoW bills and MDB bonds (both pay r_RoW), so
    # the MDB bonds they hold (Bond_MDB_d) come out of their RoW-bill demand
    # one-for-one: same total RoW-currency safe-asset holding, reallocated. This
    # keeps RoW HH wealth allocation unchanged and is zero in the baseline (Bond_MDB_d=0).
    b_RoWRoW_s[i] <- b_RoWRoW_d[i] - Bond_MDB_d[i]
    b_BrRoW_s[i] <- b_BrRoW_d[i] * xr_RoW[i]
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
    
    # ----------- VI-b. MDB (MULTILATERAL DEVELOPMENT BANK) ----------------- #
    # The MDB is a permanent sector; the policy lever is the capitalization
    # cap_MDB. With no capitalization (cap_MDB = 0 throughout) every stock below
    # stays at zero, so the sector is dormant automatically -- no on/off flag.
    # Sequence (all flows on LAGGED stocks -> no circularity with the bank/bill
    # blocks below). Two currency books:
    #  FC book  : RoW bills (asset) = the MDB's equity/capital cushion, funded by
    #             capitalization + retained FC surplus. The FC bond is NOT levered off
    #             these bills any more -- it funds the FC share of the LC loan (below),
    #             so the RoW-bill book is the FC cushion that absorbs the FX losses on
    #             the BRL lending. Both counterparties RoW-resident -> invisible to Brazil's BoP.
    #  BRL book: Br bills + LC loans to Br banks (assets), funded by converting
    #             bond proceeds FC->BRL (the one reserve-moving inflow) plus
    #             retained BRL interest. BRL interest is xr-free and recycled
    #             into Br bills. MDB carries the FX mismatch (FC liab, BRL assets).
    # capitalization (policy): one-shot injection of cap_mdb_amt at cap_mdb_start
    cap_MDB[i] <- if (i == cap_mdb_start) cap_mdb_amt else 0
    
    # FC book: interest on lagged RoW bills (income) and lagged FC bonds (expense);
    # surplus is retained FC profit. The capital-funded RoW-bill book dwarfs the small
    # demand-driven FC bond, so fc_surplus >= 0 (the cushion out-earns the bond coupon).
    int_row_MDB[i]  <- r_RoW * b_RoW_MDB[i-1]
    int_bond_MDB[i] <- r_MDB * Bond_MDB_s[i-1]
    fc_surplus_MDB  <- int_row_MDB[i] - int_bond_MDB[i]
    
    # RoW-bill cushion grows by new capital + retained FC surplus, FLOORED AT ZERO.
    # If the coupon deficit would push the cushion negative (an impossible negative
    # FC-bill holding), the MDB raises the shortfall by selling Br bills (charged to
    # the BRL buffer below) and converting reais -> FC at the peg. That reais sale is
    # a non-resident running down its Br claims, so it drains Brazil's reserves through
    # ka_mdb_in: the MDB's FX-absorption capacity is bounded by its cushion.
    b_RoW_tent_MDB      <- b_RoW_MDB[i-1] + cap_MDB[i] + fc_surplus_MDB
    fc_shortfall_MDB    <- if (b_RoW_tent_MDB < 0) -b_RoW_tent_MDB else 0   # FC the cushion cannot cover
    b_RoW_MDB[i]        <- b_RoW_tent_MDB + fc_shortfall_MDB                # floored at 0
    brl_used_for_fc_MDB <- fc_shortfall_MDB * xr_Br[i]                      # reais sold to buy that FC
    
    # DEMAND-DRIVEN ISSUANCE (design eq. mdb_issue): bond issuance is PULLED by the
    # lagged demand for concessional loans (green_loan_pool(-1)), not by the MDB's own
    # stock -- so it issues nothing when nobody wants a loan. The MDB is active once
    # capitalized (b_RoW_MDB>0) or once it has built an LC book (b_Br_MDB>0).
    gate_active <- (b_RoW_MDB[i-1] > 0) || (b_Br_MDB[i-1] > 0)
    demand_MDB  <- if (gate_active) green_loan_pool[i-1] else 0
    # FIXED SPLIT: a constant share lambda_fc of the lagged demand is funded by FC
    # bonds (proceeds converted FC -> BRL), so the FC leg grows in lockstep with demand
    # and the MDB keeps bringing fresh hard currency in -- a persistent FX mismatch it
    # carries on purpose (the local-currency-finance mechanism), not throttled away.
    Bond_MDB_s[i]    <- lambda_fc * demand_MDB / xr_Br[i]
    Bond_MDB_d[i]    <- Bond_MDB_s[i]                           # RoW HH absorb at r_MDB=r_RoW
    # LC bond funds the complementary share lambda_lc of the demand (currency-matched,
    # held by Br HH); with lambda_fc + lambda_lc = 1 the loan demand is fully funded.
    Bond_LC_MDB_s[i] <- lambda_lc * demand_MDB   # BRL bond, held by Br HH
    Bond_LC_MDB_d[i] <- Bond_LC_MDB_s[i]
    
    # BRL-book retained earnings: concessional-loan spread + Br-bill interest - BRL coupon
    int_lc_MDB[i]      <- r_l_LC_MDB * l_lc_MDB[i-1]   # LC loan interest from Br banks (BRL, no xr)
    int_br_MDB[i]      <- r_Br       * b_Br_MDB[i-1]   # Br-bill interest (BRL)
    int_bond_LC_MDB[i] <- r_Br       * Bond_LC_MDB_d[i-1]
    sigma_lc_MDB       <- int_lc_MDB[i] + int_br_MDB[i] - int_bond_LC_MDB[i]
    
    # DEMAND-DETERMINED loan (design eq. mdb_loan): lend this period's green demand,
    # capped by the BRL raised from both bonds. If demand outpaces the lagged issuance
    # the excess is simply unmet; if it slackens the surplus parks in Br bills. That
    # demand/issuance gap is real, but it is one period of demand change, not a runaway.
    BRL_funds_MDB <- Bond_MDB_s[i] * xr_Br[i] + Bond_LC_MDB_s[i]   # total BRL raised (FC converted + BRL bond)
    l_lc_MDB[i]   <- min(BRL_funds_MDB, green_loan_pool[i])
    
    # Br-bill buffer = residual, STOCK-FLOW CONSISTENT: it accrues only the NEW bond
    # proceeds (FLOWS) not lent this period, plus retained BRL earnings -- never the
    # whole bond stock (that stock-as-flow double-count was the b_Br_MDB blow-up).
    new_brl_funds_MDB <- (Bond_MDB_s[i] - Bond_MDB_s[i-1]) * xr_Br[i] +
      (Bond_LC_MDB_s[i] - Bond_LC_MDB_s[i-1])
    b_Br_MDB[i] <- b_Br_MDB[i-1] + new_brl_funds_MDB -
      (l_lc_MDB[i] - l_lc_MDB[i-1]) + sigma_lc_MDB - brl_used_for_fc_MDB
    
    f_MDB_RoW[i] <- 0                               # 100% retention: no remittance to RoW gov
    # net worth in FC (BRL assets -> FC via /xr_Br) and its FX revaluation
    # (a valuation change, NOT a transaction; identically 0 under the peg)
    V_MDB[i]     <- b_RoW_MDB[i] + (b_Br_MDB[i] + l_lc_MDB[i] - Bond_LC_MDB_s[i]) / xr_Br[i] - Bond_MDB_s[i]
    reval_MDB[i] <- (1/xr_Br[i] - 1/xr_Br[i-1]) * (b_Br_MDB[i-1] + l_lc_MDB[i-1] - Bond_LC_MDB_s[i-1])
    
    # ----------- VII. BANK BALANCE SHEETS AND ADVANCES --------------------- #
    # SOURCE: Bortz (2014) eq.53u -- banks hold bills as the residual (bills = deposits +
    # bank net worth - loans); loans create deposits (horizontalist endogenous money).
    
    residual_Br <- v_Br[i] - b_BrBr_s[i] - Bond_LC_MDB_d[i] - e_BrBr_s[i] -
      (b_BrRoW_s[i] + e_BrRoW_s[i]) * xr_Br[i]
    dep_Br[i]    <- residual_Br * depsh_Br
    h_Br_h[i]    <- residual_Br - dep_Br[i]
    
    residual_RoW <- v_RoW[i] - b_RoWRoW_s[i] - Bond_MDB_d[i] - e_RoWRoW_s[i] -
      (b_RoWBr_s[i] + e_RoWBr_s[i]) * xr_RoW[i]
    # − Bond_MDB_d: the MDB bonds RoW HH hold are part of their RoW-currency safe
    # assets (b_RoWRoW_s + Bond_MDB_d = their notional demand), so they must be
    # netted out before the rest is split into deposits/cash. Omitting this let
    # the bond value inflate deposits. Zero in the baseline (Bond_MDB_d = 0).
    dep_RoW[i]    <- residual_RoW * depsh_RoW
    h_RoW_h[i]    <- residual_RoW - dep_RoW[i]
    
    dep_bank_Br[i] <- dep_Br[i]
    dep_bank_RoW[i] <- dep_RoW[i]
    l_s_Br[i]      <- l_firm_Br[i]
    l_s_RoW[i]      <- l_firm_RoW[i]
    b_Br_bank_not[i] <- dep_bank_Br[i] - l_s_Br[i] + l_lc_MDB[i]
    # The MDB's LC loan to Br banks (l_lc_MDB) is an extra BRL funding source
    # (a bank liability), so it enters with a PLUS sign: cheap MDB money lets the
    # bank hold more bills / lean less on CB advances. Zero in the baseline (l_lc_MDB=0).
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
    f_bank_Br[i] <- r_Br*b_Br_bank[i-1] + interest_Br -   # loan income = what the
      r_a_Br*a_s_Br[i-1] -                       # firm actually pays (interest_Br,
      r_l_LC_MDB*l_lc_MDB[i-1]                    # marked down), NOT r_l_Br*l_s.
    # interest_Br is the firm's domestic-loan payment on l_firm_Br[i-1] (= l_s_Br[i-1])
    # at the current blended/marked-down rate; the bank receives exactly that. Using
    # r_l_Br*l_s_Br instead credited the bank the markdown as phantom income (a leak
    # into bank profit -> Br HH income). The two remaining terms are the advances
    # cost (Bortz 58a) and the interest paid to the MDB on its concessional LC loan
    # (a BRL, xr-free funding cost). Both MDB-linked terms are zero with no policy.
    f_bank_RoW[i] <- r_RoW*b_RoW_bank[i-1] + r_l_RoW*l_s_RoW[i-1] +
      int_fc_cashflow_Br / xr_Br[i]   # FC loan interest received from Br firms,
    # in the RoW bank's own currency (dollars). int_fc_cashflow_Br is the Br
    # firm's payment in reais (= dollar interest x xr_Br), so dividing by
    # xr_Br[i] recovers the dollar amount and guarantees firm-pays = bank-
    # receives exactly. Already a Br CA outflow (ca_int_paid_fc_Br) and a RoW
    # CA inflow (cab_RoW = -cab_Br); this credits the receiving RoW sector.
    
    # ----------- VIII. CENTRAL BANK AND GOVERNMENT (G&L floating) ---------- #
    # SOURCE: Godley & Lavoie (2007). Policy interest rate is exogenous and money/credit
    # endogenous -- HORIZONTALISM (Kaldor; Moore 1988); government bills residually finance
    # the deficit.
    
    h_Br_s[i]         <- h_Br_h[i]
    h_RoW_s[i]         <- h_RoW_h[i]
    f_cb_Br[i]        <- r_Br * b_cb_BrBr_s[i-1] +
      xr_Br[i] * r_RoW * b_cb_BrRoW_s[i-1] +     # reserve interest (Bortz 74a)
      r_a_Br * a_s_Br[i-1]                        # advances interest (Bortz 74a)
    f_cb_RoW[i]        <- r_RoW * b_cb_RoWRoW_s[i-1]
    
    gov_con_Br[i] <- 0.006054 + 1.003373 * gov_con_Br[i-1]
    gov_con_RoW[i] <- 0.195770 + 1.003373 * gov_con_RoW[i-1]
    gov_gr_Br[i]  <- 0.0795
    gov_gr_RoW[i]  <- 2.5703
    gov_tot_Br[i] <- gov_con_Br[i] + gov_gr_Br[i]
    gov_tot_RoW[i] <- gov_con_RoW[i] + gov_gr_RoW[i]
    
    b_Br_s[i] <- b_Br_s[i-1] + gov_tot_Br[i] + r_Br*b_Br_s[i-1] -
      t_Br[i] - f_cb_Br[i]
    b_RoW_s[i] <- b_RoW_s[i-1] + gov_tot_RoW[i] + r_RoW*b_RoW_s[i-1] -
      t_RoW[i] - f_cb_RoW[i] + cap_MDB[i]
    # + cap_MDB: the RoW gov funds the MDB capitalization by issuing bills (and
    # acquires the MDB equity as an asset, so its net worth is unchanged). The
    # new bills are exactly absorbed by the MDB's RoW-bill purchase, leaving the
    # Fed residual unchanged. cap_MDB = 0 in the baseline, so this is inert.
    
    # --------------- IX. EXCHANGE RATE CLOSURE (Bortz 2014 fixed ER) ------- #
    # SOURCE: Bortz (2014) eq.72u -- under the peg the central bank absorbs all excess
    # foreign-bill supply, so reserves move one-for-one with the balance of payments.
    # Peg (Bortz 73uFX/73a). Single closure: no toggle.
    # Peg, with an optional one-off permanent re-peg at xr_shock_start (orthogonal
    # to the MDB / capital-controls settings). xr_RoW is the mirror 1/xr_Br.
    xr_Br[i]  <- if (xr_shock_on && i >= xr_shock_start) 1 + xr_shock_delta else 1
    xr_RoW[i] <- 1 / xr_Br[i]
    
    # Foreign demand for Br (BRL) bills honoured at the peg (Bortz 66ai).
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
      b_BrRoW_s[i] - b_cb_BrRoW_s[i] - b_RoW_MDB[i]
    # (the MDB holds RoW bills too; the Fed clears the residual of what remains)
    
    # Br CB DOMESTIC (BRL) bill holding: the BRL-bill-market residual
    # (Bortz eq. 71a). The government supplies to the CB whatever bills HH,
    # banks, and foreigners do not buy. CB net worth is then V_cba (Bortz's
    # "sort of net wealth"); domestic-market clearing is the redundant check.
    b_cb_BrBr_s[i] <- b_Br_s[i] - b_BrBr_s[i] - b_RoWBr_s[i] - b_Br_bank[i] - b_Br_MDB[i]
    # (the MDB holds Br bills too; the Br CB clears the residual of what remains)
    
    # ------------------- X. ECOSYSTEM: MATERIAL FLOWS ---------------------- #
    # SOURCE (blocks X-XIV): ecological stock-flow-consistent macro -- Carnevali et al.
    # (2021); Dafermos, Nikolaidi & Galanis (2017, DEFINE). Material/energy throughput and
    # emissions accumulate into a temperature stock whose damage function feeds back onto
    # output and consumption.
    
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
    
    # Both channels are CONTEMPORANEOUS: the land-use pressure this period
    # reflects the economy's structure this period (capital deepening = formal-
    # industrial labour absorption -> less deforestation NOW, and the current
    # green share). k_Br[i], k_gr_Br[i] are already resolved earlier in this same
    # Gauss-Seidel iteration (capital block ~l.911-926), so using [i] introduces
    # no simultaneity. Y and K are now dated identically (both [i]).
    # Land-use emissions respond ONLY to capital-output deepening (the Y/K
    # channel): rising K/Y proxies formal-industrial labour absorption, which
    # draws the workforce away from land-extractive activity and lowers
    # deforestation. The green/conventional capital composition is NOT used here
    # -- that effect is already carried by industrial (productive) emissions
    # through the blended intensity beta. Both Y and K are contemporaneous [i].
    K_ratio_Br  <- if (k_Br_p1_ref > 0) k_Br[i] / k_Br_p1_ref else 1
    Y_ratio_Br  <- y_Br[i] / y_Br_p1_ref
    land_driver <- Y_ratio_Br / K_ratio_Br
    # No max(.,0) floor: the driver is a ratio of strictly positive quantities
    # raised to a power and scaled by a positive anchor, so land emissions are
    # positive by construction (unlike productive emissions, which are an
    # additive beta0 + beta*EN and genuinely can cross zero).
    emis_l_Br[i] <- emis_l_Br_0 * (land_driver ^ psi_land)
    emis_l_RoW[i] <- emis_l_RoW[i-1] * (1 - g_land)
    emis_l[i]     <- emis_l_Br[i] + emis_l_RoW[i]
    
    emis_Br[i] <- max(beta0_b_now + beta_Br[i]*en_Br[i], 0)
    emis_RoW[i] <- max(beta0_g_now - 4 + beta_RoW[i]*en_RoW[i], 0)
    # total area emissions = productive (energy/industry) + land use. emis_Br
    # alone is productive-only; for Brazil land use is the larger share (~63%,
    # Brazil NIR 2024), so the *_total series are the right object to report/plot.
    emis_Br_total[i]  <- emis_Br[i]  + emis_l_Br[i]
    emis_RoW_total[i] <- emis_RoW[i] + emis_l_RoW[i]
    emis[i]       <- emis_Br[i] + emis_RoW[i] + emis_l[i]   # = emis_Br_total + emis_RoW_total
    
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
    beta_Br[i]    <- (beta_gr_Br*wgr_b + beta_con_Br*wco_b) * cint_b_mult * prod_emis_scale_Br
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
    # Interest Brazil pays the MDB (a non-resident): on its Br-bill holding and
    # on its LC loans to Br banks. Both are BRL flows (xr-free) and CA outflows.
    # Zero in the baseline (the stocks are zero).
    ca_int_paid_mdb_Br[i] <- r_Br * b_Br_MDB[i-1] + r_l_LC_MDB * l_lc_MDB[i-1]
    ca_div_net_Br[i]     <- 0              # placeholder; cross-border dividends zeroed
    cab_Br[i] <- (x_Br[i] - im_Br[i]) +
      ca_int_recv_Br[i] -
      ca_int_paid_lc_Br[i] -
      ca_int_paid_fc_Br[i] -
      ca_int_paid_mdb_Br[i] +
      int_bond_LC_MDB[i] +
      ca_div_net_Br[i]
    cab_RoW[i] <- -cab_Br[i]
    
    ka_b_in_Br[i]   <- (b_RoWBr_s[i] - b_RoWBr_s[i-1])
    ka_e_in_Br[i]   <- (e_RoWBr_s[i] - e_RoWBr_s[i-1])
    ka_fc_in_Br[i]  <- (l_fc_Br[i]   - l_fc_Br[i-1])   * xr_Br[i]
    ka_b_out_Br[i]  <- (b_BrRoW_s[i] - b_BrRoW_s[i-1]) * xr_Br[i]
    ka_e_out_Br[i]  <- (e_BrRoW_s[i] - e_BrRoW_s[i-1]) * xr_Br[i]
    # MDB (non-resident) net acquisition of Br claims = capital inflow to Brazil:
    # its Br-bill holding plus its LC loans to Br banks (both BRL). This is the
    # FC the MDB converts in to fund BRL lending; it relieves reserves. Reinvested
    # BRL interest self-cancels against the CA outflow above. Zero when mdb off.
    ka_mdb_in_Br[i] <- (b_Br_MDB[i] - b_Br_MDB[i-1]) + (l_lc_MDB[i] - l_lc_MDB[i-1]) -
      (Bond_LC_MDB_d[i] - Bond_LC_MDB_d[i-1])
    kabp_Br[i] <- ka_b_in_Br[i] + ka_e_in_Br[i] + ka_fc_in_Br[i] + ka_mdb_in_Br[i] -
      ka_b_out_Br[i] - ka_e_out_Br[i]
    kabp_RoW[i] <- -kabp_Br[i]
    
    bp_Br[i]  <- cab_Br[i] + kabp_Br[i]
    bp_RoW[i] <- cab_RoW[i] + kabp_RoW[i]
    
    nafa_Br[i]  <- psbr_Br[i] + cab_Br[i]
    nafa_RoW[i] <- psbr_RoW[i] + cab_RoW[i]
    
    # --------- REDUNDANT EQUATION: sum of all sectors' net lending = 0 --------
    # Each sector's net lending (NL) = current-account income - current outlays,
    # i.e. the surplus it has available to acquire net financial assets. By
    # horizontal consistency (Walras' law) the 13 sector NLs must sum to zero
    # every period. This is built INDEPENDENTLY from the flows (not from the
    # financial-stock side), so a non-zero sum would expose a leak. Everything is
    # expressed in FC (BRL items divided by xr_Br) so the global sum is comparable.
    # Interest the firm actually pays on domestic loans (marked down) = interest_Br.
    ip_dom_Br   <- interest_Br                                  # firm -> bank (BRL)
    ip_fc_Br    <- r_l_fc_gr*l_fc_gr_Br[i-1] + r_l_fc_con*l_fc_con_Br[i-1]  # firm -> RoW bank (FC)
    int_BrBills <- r_Br*b_Br_s[i-1]                             # total coupon on Br bills (BRL)
    int_RoWBills<- r_RoW*b_RoW_s[i-1]                           # total coupon on RoW bills (FC)
    
    # --- Brazil (BRL flows; /xr_Br to FC) ---
    nl_hh_Br[i]   <- ( y_w_Br[i] + fd_Br[i] + f_bank_Br[i]
                       + r_Br*b_BrBr_s[i-1] + r_RoW*b_BrRoW_s[i-1]*xr_Br[i]
                       + r_Br*Bond_LC_MDB_d[i-1]
                       - t_Br[i] - cons_Br[i] ) / xr_Br[i]
    nl_fc_Br[i]   <- ( f_Br[i] - fu_Br[i] - fd_Br[i] ) / xr_Br[i]          # = 0 (profit fully allocated)
    nl_fk_Br[i]   <- ( fu_Br[i] + af_Br[i] - inv_Br[i] ) / xr_Br[i]
    nl_bank_Br[i] <- ( ip_dom_Br + r_Br*b_Br_bank[i-1]
                       - r_a_Br*a_s_Br[i-1] - r_l_LC_MDB*l_lc_MDB[i-1]
                       - f_bank_Br[i] ) / xr_Br[i]                          # bank NL (retains 0; = 0)
    nl_gov_Br[i]  <- ( t_Br[i] + f_cb_Br[i] - gov_tot_Br[i]
                       - r_Br*b_Br_s[i-1] ) / xr_Br[i]
    nl_cb_Br[i]   <- ( r_Br*b_cb_BrBr_s[i-1] + r_RoW*b_cb_BrRoW_s[i-1]*xr_Br[i]
                       - f_cb_Br[i] ) / xr_Br[i]
    
    # --- RoW (FC flows already) ---
    nl_hh_RoW[i]   <- ( y_w_RoW[i] + fd_RoW[i] + f_bank_RoW[i]
                        + r_RoW*b_RoWRoW_s[i-1] + r_Br*b_RoWBr_s[i-1]*xr_RoW[i]
                        + r_MDB*Bond_MDB_d[i-1]
                        - t_RoW[i] - cons_RoW[i] )
    nl_fc_RoW[i]   <- ( f_RoW[i] - fu_RoW[i] - fd_RoW[i] )
    nl_fk_RoW[i]   <- ( fu_RoW[i] + af_RoW[i] - inv_RoW[i] )
    nl_bank_RoW[i] <- ( r_l_RoW*l_s_RoW[i-1] + ip_fc_Br + r_RoW*b_RoW_bank[i-1]
                        - r_a_Br*0 - f_bank_RoW[i] )                        # RoW bank NL (= 0)
    nl_gov_RoW[i]  <- ( t_RoW[i] + f_cb_RoW[i] - gov_tot_RoW[i]
                        - r_RoW*b_RoW_s[i-1] )
    # NB: cap_MDB is NOT here. Capitalization is a capital-account transaction
    # (the gov acquires MDB equity, an asset), not current expenditure, so it
    # does not enter the current-account net-lending surplus. The MDB likewise
    # books the received capital as equity (capital account), so it is absent
    # from nl_mdb too. Both sides' net financial ACQUISITION reflects cap_MDB,
    # but their net LENDING (current surplus) does not.
    nl_cb_RoW[i]   <- ( r_RoW*b_cb_RoWRoW_s[i-1] + r_Br*b_cb_BrRoW_s[i-1]*0
                        - f_cb_RoW[i] )
    
    # --- MDB (own currency books; BRL interest /xr_Br to FC) ---
    nl_mdb[i] <- ( int_row_MDB[i] - int_bond_MDB[i] )            # sigma^FC (FC book surplus)
    nl_mdb[i] <- nl_mdb[i] + ( int_lc_MDB[i] + int_br_MDB[i] - int_bond_LC_MDB[i] ) / xr_Br[i] - f_MDB_RoW[i]
    
    nl_sum_check[i] <- nl_hh_Br[i] + nl_fc_Br[i] + nl_fk_Br[i] + nl_bank_Br[i] +
      nl_gov_Br[i] + nl_cb_Br[i] + nl_hh_RoW[i] + nl_fc_RoW[i] + nl_fk_RoW[i] +
      nl_bank_RoW[i] + nl_gov_RoW[i] + nl_cb_RoW[i] + nl_mdb[i]
    
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
    # Redundant BRL-bill-market clearing (Bortz 71a): ~0 iff watertight.
    br_bill_resid[i] <- b_Br_s[i] -
      (b_BrBr_s[i] + b_RoWBr_s[i] + b_Br_bank[i] + b_cb_BrBr_s[i] + b_Br_MDB[i])
    # Redundant US-bill-market clearing (Fed is residual): ~0 iff watertight.
    row_bill_resid[i] <- b_RoW_s[i] -
      (b_RoWRoW_s[i] + b_RoW_bank[i] + b_cb_RoWRoW_s[i] + b_BrRoW_s[i] + b_cb_BrRoW_s[i] + b_RoW_MDB[i])
    
    q_Br[i]     <- (e_Br_real_s[i]*p_e_Br[i] + l_firm_Br[i]) / k_Br[i]
    q_RoW[i]     <- (e_RoW_real_s[i]*p_e_RoW[i] + l_firm_RoW[i]) / k_RoW[i]
    lev_f_Br[i] <- l_firm_Br[i] / k_Br[i]
    lev_f_RoW[i] <- l_firm_RoW[i] / k_RoW[i]
    liq_b_Br[i] <- (a_s_Br[i] + dep_bank_Br[i] - l_s_Br[i]) / max(dep_bank_Br[i], 1e-6)
    liq_b_RoW[i] <- (a_s_RoW[i] + dep_bank_RoW[i] - l_s_RoW[i]) / max(dep_bank_RoW[i], 1e-6)
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
lines(periods, emis_Br_total[i_plot], col = "orange", lwd = 2)
lines(periods, emis_RoW_total[i_plot], col = "forestgreen", lwd = 2)
lines(periods, emis_l[i_plot], col = "darkblue", lwd = 2, lty = 3)
legend("topright", c("Total","Br (prod.+land)","RoW (prod.+land)","Land (global)"),
       col = c("black","orange","forestgreen","darkblue"),
       lwd = 2, lty = c(1,1,1,3), bty = "n", cex = 0.85)

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
cat(sprintf("BRL-bill rule: Br CB residual (71a) | US-bill rule: Fed residual | reserves: BoP-cumulated\n"))
win <- 1:nPeriods
cat(sprintf("Br reserves b_cb_BrRoW_s [IC / final]: %.4f / %.4f\n",
            b_cb_BrRoW_s[1], b_cb_BrRoW_s[nPeriods]))
cat(sprintf("max |BoP resid Br| (full run): %.3e   <- now tautological (reserves = BoP), verifies cumulation\n",
            max(abs(bop_resid_Br[win]))))
cat(sprintf("max |BRL-bill mkt resid| (Bortz 71a): %.3e\n", max(abs(br_bill_resid[win]))))
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
       g("b_Br_s") - (g("b_BrBr_s") + g("b_RoWBr_s") + g("b_Br_bank") + g("b_cb_BrBr_s") + g("b_Br_MDB")))
  line("RoW bill market",
       g("b_RoW_s") - (g("b_RoWRoW_s") + g("b_RoW_bank") + g("b_cb_RoWRoW_s") +
                         g("b_BrRoW_s") + g("b_cb_BrRoW_s") + g("b_RoW_MDB")))
  line("Br equity (s = d)",  g("e_BrBr_s")   - g("e_BrBr_d"))
  line("RoW equity (s = d)", g("e_RoWRoW_s") - g("e_RoWRoW_d"))
  line("Br equity (real*p)",  g("e_Br_real_s")*g("p_e_Br")   - g("e_BrBr_s"))
  line("RoW equity (real*p)", g("e_RoW_real_s")*g("p_e_RoW") - g("e_RoWRoW_s"))
  cat("-- balance-sheet closure (must be ~0) --\n")
  line("Br household",
       g("v_Br") - (g("b_BrBr_d") + g("b_BrRoW_d") + g("e_BrBr_d") + g("dep_Br") + g("h_Br_h")))
  line("RoW household",
       g("v_RoW") - (g("b_RoWRoW_d") + g("b_RoWBr_d")*g("xr_RoW") + g("e_RoWRoW_d") + g("dep_RoW") + g("h_RoW_h")))
  line("Br banks",  (g("l_s_Br")  + g("b_Br_bank"))  - (g("dep_bank_Br")  + g("a_s_Br") + g("l_lc_MDB")))
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
  cat("  xr toward zero -- the BRL effectively crashes to worthless.\n")
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
plot(periods, emis_Br_total[i_plot], type = "l", lwd = 2, col = "black",
     ylim = range(c(emis_Br_total[i_plot], emis_Br[i_plot], emis_l_Br[i_plot]), na.rm = TRUE),
     main = "Br CO2 emissions (total = productive + land use)",
     xlab = "Period", ylab = "Gt CO2 / yr")
add_guard_lines()
lines(periods, emis_Br[i_plot],   col = "orange",      lwd = 2)
lines(periods, emis_l_Br[i_plot], col = "forestgreen", lwd = 2)
legend("topright",
       legend = c("Br total", "Br productive (energy/industry)", "Br land use (Agric+LULUCF)"),
       col    = c("black", "orange", "forestgreen"),
       lwd = 2, bty = "n", cex = 0.8)

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

# --- Br firm leverage vs government debt (all in BRL) ----------------------
# Firm leverage = DOMESTIC firm loans / capital stock (l_firm_Br/k_Br, ~0.3 and
# stable). Government debt = the stock of Br bills (b_Br_s): the sovereign here
# issues ONLY in local currency, so -- unlike firms, which also carry the FC loan
# l_fc_Br (a dollar liability -> FX mismatch) -- it bears no currency mismatch.
# Twin axis: firm leverage (left), government BRL debt (right).
par(mar = c(4, 4.5, 3, 4.5), cex.main = 1.0, font.main = 1)
plot(periods, lev_f_Br[i_plot], type = "l", lwd = 2, col = "orange",
     main = "Br firm leverage vs government debt (all in BRL)",
     xlab = "Period", ylab = "Firm leverage (L_firm / K)")
add_guard_lines()
par(new = TRUE)
plot(periods, b_Br_s[i_plot], type = "l", lwd = 2, col = "steelblue",
     axes = FALSE, xlab = "", ylab = "")
axis(4, col = "steelblue", col.axis = "steelblue")
mtext("Government debt (BRL, all in LC)", side = 4, line = 3, col = "steelblue")
legend("topleft",
       legend = c("Firm leverage (L_firm/K, left)", "Govt debt (BRL, right)"),
       col = c("orange", "steelblue"), lwd = 2, bty = "n", cex = 0.85)

# --- MDB sector: bill holdings & bonds issued ------------------------------
# Populated only when cap_mdb_amt > 0 (default 0 = dormant baseline). FC quantities
# are US$, LC quantities are reais; under the peg (xr = 1) they coincide unit-for-unit,
# so both panels use a SHARED axis (no twin axis, no scale distortion). With the 50/50
# split the FC and BRL bonds are numerically EQUAL at the peg, so the BRL series is
# drawn as MARKERS riding on the FC line: you see both series and that they coincide;
# off the peg the markers lift off the line. Markers thinned to every 5th period.
# Dotted vertical line = capitalisation onset (cap_mdb_start).
mk <- seq(2, length(periods), by = 5)   # marker subset (disambiguates coincident series)

layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1, oma = c(0, 0, 2, 0))

# (a) bill holdings on ONE axis: RoW bills (FC cushion) and Br bills (BRL buffer)
ymax_a <- max(c(b_RoW_MDB[i_plot], b_Br_MDB[i_plot]), na.rm = TRUE) * 1.05
if (!is.finite(ymax_a) || ymax_a <= 0) ymax_a <- 1
plot(periods, b_RoW_MDB[i_plot], type = "l", lwd = 2.5, col = "forestgreen",
     main = "a) MDB bill holdings", ylim = c(0, ymax_a),
     xlab = "Period", ylab = "Holding (US$ / reais)")
add_guard_lines()
abline(v = cap_mdb_start, lty = 3, col = "grey45")
lines(periods, b_Br_MDB[i_plot], lwd = 1.6, col = "darkorange")
points(periods[mk], (b_Br_MDB[i_plot])[mk], pch = 1, cex = 0.85, col = "darkorange")
legend("topleft",
       legend = c("RoW bills = FC cushion (US$)", "Br bills = BRL buffer (reais)"),
       col = c("forestgreen", "darkorange"), lwd = c(2.5, 1.6), pch = c(NA, 1),
       bty = "n", cex = 0.76)

# (b) bonds on ONE axis: FC bond -> RoW HH (line) and BRL bond -> Br HH (markers on it)
ymax_b <- max(c(Bond_MDB_s[i_plot], Bond_LC_MDB_s[i_plot]), na.rm = TRUE) * 1.05
if (!is.finite(ymax_b) || ymax_b <= 0) ymax_b <- 1
plot(periods, Bond_MDB_s[i_plot], type = "l", lwd = 2.5, col = "forestgreen",
     main = "b) MDB bonds issued", ylim = c(0, ymax_b),
     xlab = "Period", ylab = "Bond issued (US$ face / reais)")
add_guard_lines()
abline(v = cap_mdb_start, lty = 3, col = "grey45")
lines(periods, Bond_LC_MDB_s[i_plot], lwd = 1.6, col = "darkorange")
points(periods[mk], (Bond_LC_MDB_s[i_plot])[mk], pch = 1, cex = 0.85, col = "darkorange")
legend("topleft",
       legend = c("FC bond -> RoW HH (US$ face)", "BRL bond -> Br HH (reais)"),
       col = c("forestgreen", "darkorange"), lwd = c(2.5, 1.6), pch = c(NA, 1),
       bty = "n", cex = 0.76)

mtext("MDB sector", outer = TRUE, cex = 1.05, font = 2)
layout(1)
par(oma = c(0, 0, 0, 0))

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
legend("topright",
       legend = c("Br", "RoW"),
       col    = c("orange", "forestgreen"),
       lwd    = c(2, 2),
       lty    = c(1, 1),
       bty = "n", cex = 0.85)

plot(periods, k_gr_Br[i_plot], type = "l", lwd = 2, col = "forestgreen",
     main = "Br green capital stock",
     xlab = "Period", ylab = "k_gr_Br (T USD)")
add_guard_lines()
legend("topleft",
       legend = c("Br green capital"),
       col    = c("forestgreen"),
       lwd    = c(2),
       lty    = c(1),
       bty = "n", cex = 0.85)

plot(periods, kgr_gr_Br_pct, type = "l", lwd = 2, col = "orange",
     ylim = range(c(kgr_gr_Br_pct, kgr_gr_RoW_pct), na.rm = TRUE),
     main = "Green capital growth rate (year-over-year)",
     xlab = "Period", ylab = "Growth rate (%)")
add_guard_lines()
lines(periods, kgr_gr_RoW_pct, lwd = 2, col = "forestgreen")
abline(h = 0, lty = 2, col = "grey50")
legend("topright",
       legend = c("Br", "RoW"),
       col    = c("orange", "forestgreen"),
       lwd    = c(2, 2),
       lty    = c(1, 1),
       bty = "n", cex = 0.85)

par(old.par2)
layout(1)

# --- Redundant equation: sum of all sectors' net lending (must be 0) ---------
# Independent stock-flow-consistency check, built from the current-account flow
# side. By Walras' law the 13 sector net-lending positions sum to zero every
# period; any departure from zero (beyond ~1e-12 numerical noise) signals a leak.
par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1)
plot(periods, nl_sum_check[i_plot], type = "l", lwd = 2, col = "blue",
     main = "Redundant eq.: sum of net lending (full scale)",
     xlab = "Period", ylab = "Sum of sector net lending (FC)")
abline(h = 0, lty = 2, col = "red")
plot(periods, nl_sum_check[i_plot], type = "l", lwd = 2, col = "blue",
     ylim = c(-1, 1),
     main = "Redundant eq.: sum of net lending (restricted scale)",
     xlab = "Period", ylab = "Deviation from zero (FC)")
abline(h = 0, lty = 2, col = "red")
par(mfrow = c(1, 1))
cat(sprintf("Redundant equation (sum of 13 sector NLs): max |dev| = %.2e over t=2..%d\n",
            max(abs(nl_sum_check[2:nPeriods])), nPeriods))

################################################################################
# 10) CURATED THESIS PANELS
#     Eight-panel set used in the write-up, regenerated from the simulated series
#     so one run of this script reproduces them. They reflect whatever scenario
#     the file is configured for: with cap_mdb_amt = 0 the MDB is dormant (the
#     bond/bill panels sit at zero, the macro panels are the no-MDB baseline) and
#     no onset line is drawn; with cap_mdb_amt > 0 the bank activates at
#     cap_mdb_start, marked by a dotted vertical line. Variables are taken
#     directly from the model; nothing is recomputed by a separate routine.
################################################################################

pp_panel <- 1:nPeriods

ky_panel      <- k_Br / y_Br                        # capital-output ratio
lev_dom_panel <- l_firm_Br / k_Br                   # domestic firm leverage
lev_fc_panel  <- l_fc_Br * xr_Br / k_Br             # foreign-currency leverage (reais value)
# year-over-year green capital growth (first period has no predecessor -> NA)
kgr_growth_panel <- c(NA_real_,
                      100 * (k_gr_Br[-1] / pmax(k_gr_Br[-nPeriods], 1e-9) - 1))

# onset marker drawn only when the MDB is actually capitalized
mdb_onset_line <- function() if (cap_mdb_amt > 0) abline(v = cap_mdb_start, lty = 3, col = "grey45")
# guard ranges that are identically zero in the dormant baseline
safe_range <- function(...) { r <- range(c(...), na.rm = TRUE)
if (!all(is.finite(r)) || diff(r) < 1e-9) c(0, 1) else r }

## --- Figure A: real economy and external position ----------------------------
old.parA <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1, oma = c(0, 0, 2, 0))

plot(pp_panel, ky_panel, type = "l", lwd = 2, col = "steelblue",
     main = "a) Capital-output ratio  (k_Br / Y_Br)",
     xlab = "Period", ylab = "Ratio")
mdb_onset_line()

plot(pp_panel, lev_dom_panel, type = "l", lwd = 2, col = "forestgreen",
     ylim = safe_range(lev_dom_panel, lev_fc_panel),
     main = "b) Firm leverage (domestic and FC)",
     xlab = "Period", ylab = "Loans / K")
lines(pp_panel, lev_fc_panel, lwd = 2, col = "firebrick")
mdb_onset_line()
legend("left", c("Domestic  (L_firm / K)", "Foreign-currency  (L_fc . xr / K)"),
       col = c("forestgreen", "firebrick"), lwd = 2, bty = "n", cex = 0.78)

plot(pp_panel, tb_Br, type = "l", lwd = 2, col = "purple",
     main = "c) Trade balance  (x_Br - im_Br)",
     xlab = "Period", ylab = "Trillion USD")
abline(h = 0, lty = 2, col = "grey60")
mdb_onset_line()

plot(pp_panel, b_cb_BrRoW_s, type = "l", lwd = 2, col = "darkorange",
     main = "d) Central-bank FX reserves  (b_cb_BrRoW)",
     xlab = "Period", ylab = "Trillion USD")
abline(h = 0, lty = 2, col = "grey60")
mdb_onset_line()

mtext("Brazil: real economy and external position", outer = TRUE, cex = 1.05, font = 2)
par(old.parA); layout(1)

## --- Figure B: green transition and MDB sector -------------------------------
old.parB <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar = c(4, 4.5, 3, 1.5), cex.main = 1.0, font.main = 1, oma = c(0, 0, 2, 0))

plot(pp_panel, kgr_growth_panel, type = "l", lwd = 2, col = "forestgreen",
     main = "a) Green capital growth rate (YoY)",
     xlab = "Period", ylab = "Growth rate (%)")
abline(h = 0, lty = 2, col = "grey60")
mdb_onset_line()

plot(pp_panel, emis_Br_total, type = "l", lwd = 2, col = "black",
     ylim = safe_range(emis_Br_total, emis_Br, emis_l_Br),
     main = "b) Brazil CO2 emissions",
     xlab = "Period", ylab = "Gt CO2 / yr")
lines(pp_panel, emis_Br,   lwd = 2, col = "darkorange")
lines(pp_panel, emis_l_Br, lwd = 2, col = "forestgreen")
mdb_onset_line()
legend("topright", c("Total (productive + land)", "Productive (energy/industry)", "Land use (Agric + LULUCF)"),
       col = c("black", "darkorange", "forestgreen"), lwd = 2, bty = "n", cex = 0.72)

plot(pp_panel, Bond_MDB_s, type = "l", lwd = 2, col = "steelblue",
     ylim = safe_range(Bond_MDB_s, Bond_LC_MDB_s),
     main = "c) MDB bonds issued (FC and LC)",
     xlab = "Period", ylab = "Face value (US$ / reais)")
lines(pp_panel, Bond_LC_MDB_s, lwd = 2, col = "darkorange")
mdb_onset_line()
legend("topleft", c("FC bond -> RoW HH  (Bond_MDB)", "LC bond -> Br HH  (Bond_LC_MDB)"),
       col = c("steelblue", "darkorange"), lwd = 2, bty = "n", cex = 0.72)

plot(pp_panel, b_RoW_MDB, type = "l", lwd = 2, col = "steelblue",
     ylim = safe_range(b_RoW_MDB, b_Br_MDB),
     main = "d) MDB bill holdings (FC cushion and BRL buffer)",
     xlab = "Period", ylab = "Holding (US$ / reais)")
lines(pp_panel, b_Br_MDB, lwd = 2, col = "darkorange")
mdb_onset_line()
legend("topleft", c("RoW bills = FC cushion  (b_RoW_MDB)", "Br bills = BRL buffer  (b_Br_MDB)"),
       col = c("steelblue", "darkorange"), lwd = 2, bty = "n", cex = 0.72)

mtext("Green transition and MDB sector", outer = TRUE, cex = 1.05, font = 2)
par(old.parB); layout(1)

cat("\n[Curated thesis panels: Figure A (real/external) and Figure B (green/MDB) rendered]\n")