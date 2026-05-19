# =============================================================================
# SOFI - Paramètres législatifs 2025
# Source : Maquette Sofi 2025.xlsx, onglet "Données 2025"
# =============================================================================

parametres_2025 <- function() {
  list(
    annee = 2025,

    # =========================================================================
    # SMIC et contexte (Rows 3-6)
    # =========================================================================
    smic_net = 1426.3,                    # Row 3, B
    smic_net_n1 = 1398.7,                 # Row 3, D
    smic_net_n2 = 1353.07,                # Row 4, D
    smic_horaire_net = 9.192744,           # Row 5, B
    smic_horaire_brut = 11.88,             # Row 6, B
    evolution_smic = 1.0197326088510759,   # Row 4, B
    plafond_ss = 47100,                    # Row 20, B
    plafond_ss_n1 = 46368,                 # Row 21, B
    plafond_ss_n2 = 43992,                 # Row 22, B
    seuil_pauvrete = 1336.4891518737672,   # Row 22, F (estimation 2024)

    # Coefficients (Rows 13-18)
    evolution_n1_n = 1.0197326088510759,   # Row 13, B
    evolution_n2_n = 1.0541213684436135,   # Row 14, B
    evolution_n2_n1 = 1.0337233106934602,  # Row 15, B
    coeff_ressources_n2 = 11.803060571387338,  # Row 16, B
    coeff_revenu_imposable = 12.44185836133368, # Row 17, B
    coeff_al_aah = 11.19767252520031,      # Row 18, B
    ratio_assiette_csg_crds = 0.9825,      # Row 13, F
    taux_CSG_deductible = 0.068,           # Row 14, F
    taux_CSG_imposable = 0.024,            # Row 15, F
    taux_cotis_sal_total = 0.2262,         # Row 16, F
    taux_cotis_sal_sous_plafond = 0.2262,  # Row 17, F
    taux_CRDS = 0.005,                     # Row 18, F

    # =========================================================================
    # Allocations Familiales (AF) - Rows 26-37
    # =========================================================================
    bmaf = 474.37,                          # Row 26, B
    # Montants AF après CRDS (Rows 29-34)
    af_1 = 0,                               # Row 29, B (1 enfant = 0)
    af_2 = 151.039408,                      # Row 30, B
    af_3 = 344.5586495,                     # Row 31, B
    af_4 = 538.077891,                      # Row 32, B
    af_5 = 731.5971325,                     # Row 33, B
    af_6 = 921.4583883375001,               # Row 34, B
    af_majo_14 = 75.519704,                 # Row 36, B
    af_forfait = 95.504105671,              # Row 37, B
    # Plafonds de ressources AF (Row 29)
    af_plaf_tranche1 = 78565,               # Row 29, D
    af_plaf_tranche2 = 104719,              # Row 29, E
    af_plaf_sup_enfant = 6546,              # Row 29, F
    # Taux de modulation
    af_taux_modulation_t1 = 0.5,
    af_taux_modulation_t2 = 0.25,

    # =========================================================================
    # ASF (Allocation de Soutien Familial) - Rows 40-42
    # =========================================================================
    asf = 199.1596193925,                   # Row 41, B
    asf_orphelin_double = 265.498959375,    # Row 41, G
    asf_deduit_rsa = 106.19958375,          # Row 42, B

    # =========================================================================
    # Complément familial (CF) - Rows 45-48
    # =========================================================================
    cf = 196.587229475,                     # Row 46, B
    cf_majo = 98.317214645,                 # Row 47, B
    cf_majore = 196.587229475 + 98.317214645, # CF + majoration = 294.90
    cf_deduit_rsa = 196.587229475,          # Row 48, B
    # Plafonds CF (Row 46-47)
    cf_plaf_couple_1rev = 43946,            # Row 46, D
    cf_plaf_couple_2rev = 53758,            # Row 46, E
    cf_plaf_isole = 53758,                  # = couple biactif
    cf_plaf_sup_enfant = 7324,              # Row 46, F
    cf_seuil_activite = 5982.912,           # Row 46, G (13.6% plafond SS n-2)
    # Plafonds CF majoré (Row 47)
    cf_plaf_majo_couple_1rev = 21978,       # Row 47, D
    cf_plaf_majo_couple_2rev = 26884,       # Row 47, E
    cf_plaf_majo_isole = 26884,
    cf_plaf_majo_sup_enfant = 3663,         # Row 47, F

    # =========================================================================
    # ARS (Allocation de Rentrée Scolaire) - Rows 50-53
    # =========================================================================
    ars_6_10 = 423.47674018000004,          # Row 51, B
    ars_11_14 = 446.84064860499996,         # Row 52, B
    ars_15_18 = 462.32218792500004,         # Row 53, B
    ars_plaf_base = 28444,                  # Row 51, D
    ars_plaf_sup_enfant = 6564,             # Row 51, E

    # =========================================================================
    # PAJE - Allocation de Base (AB) - Rows 56-58
    # =========================================================================
    ab_taux_plein = 196.587229475,          # Row 57, B
    ab_taux_reduit = 98.2936147375,         # Row 58, B
    # Plafonds AB taux plein (Row 57)
    ab_plaf_tp_couple_1rev = 36461,         # Row 57, D
    ab_plaf_tp_couple_2rev = 43753,         # Row 57, E
    ab_plaf_tp_isole = 48186,               # Row 57, F
    ab_plaf_tp_isole_2 = 55478,             # Row 57, G
    ab_plaf_tp_sup_enfant = 8751,           # Row 57, H
    # Plafonds AB taux réduit (Row 58)
    ab_plaf_tr_couple_1rev = 30518,         # Row 58, D
    ab_plaf_tr_couple_2rev = 36621,         # Row 58, E
    ab_plaf_tr_isole = 40330,               # Row 58, F
    ab_plaf_tr_isole_2 = 46434,             # Row 58, G
    ab_plaf_tr_sup_enfant = 7324,           # Row 58, H

    # =========================================================================
    # PREPARE (ex-CLCA) - Rows 61-64
    # =========================================================================
    prepare_taux_plein = 456.04461253,          # Row 62, B
    prepare_taux_partiel_50 = 294.81004449,     # Row 63, B
    prepare_taux_partiel_80 = 170.060933445,    # Row 64, B

    # =========================================================================
    # CMG - Réforme 2025 - Rows 66-77
    # =========================================================================
    # Taux d'effort (Rows 68-71)
    cmg_effort_am_1 = 0.000619,             # Row 68, B
    cmg_effort_am_2 = 0.000516,             # Row 69, B
    cmg_effort_am_3 = 0.000413,             # Row 70, B
    cmg_effort_am_4plus = 0.00031,          # Row 71, B
    cmg_effort_dom_1 = 0.001238,            # Row 68, C
    cmg_effort_dom_2 = 0.001032,            # Row 69, C
    cmg_effort_dom_3 = 0.000826,            # Row 70, C
    cmg_effort_dom_4plus = 0.00062,         # Row 71, C
    # Coûts horaires de référence (Row 73)
    cmg_cout_ref_am = 4.85,                 # Row 73, B
    cmg_cout_ref_dom = 10.38,               # Row 73, C
    # Plafonds horaires (Row 74)
    cmg_plaf_h_am = 8,                      # Row 74, B
    cmg_plaf_h_dom = 15,                    # Row 74, C
    # Plafond/plancher de ressources (Rows 75-76)
    cmg_plafond_ressources = 8500,           # Row 75, B
    cmg_plancher_ressources = 814.02,        # Row 76, B
    # Prise en charge max cotisations garde domicile (Row 77)
    cmg_prise_en_charge_cotis_dom = 496,     # Row 77, C (valeur 2024)
    # Heures max
    cmg_heures_max_mois = 250,

    # =========================================================================
    # AAH (déconjugalisée) - Rows 79-87
    # =========================================================================
    aah = 1033.32,                           # Row 80, B
    aah_plafond_isole = 12399.84,            # Row 81, B
    aah_plafond_sup_enfant = 0.50,           # Row 82, B (+50% par enfant)
    aah_seuil_abattement_1 = 540.54,         # Row 83, B (seuil cumul AAH-salaire 6 mois)
    aah_abattement_bas = 0.80,               # Row 84, B
    aah_abattement_haut = 0.40,              # Row 85, B
    aah_min_salaire_pro = 344.52,            # Row 86, B (29 * smic horaire brut)

    # =========================================================================
    # RSA - Rows 89-102
    # =========================================================================
    # Forfait logement RSA (Rows 91-93)
    rsa_fl_1 = 77.58239999999999,            # Row 91, B
    rsa_fl_2 = 155.16479999999999,           # Row 92, B
    rsa_fl_3 = 192.01644,                    # Row 93, B
    # Montant de base RSA (Row 95, C = 568.9376)
    rsa_montant_base = 568.9376,             # Row 95, C
    # Montants forfaitaires RSA (Rows 97-100)
    rsa_I0 = 646.52,                         # Row 97, B
    rsa_I1 = 969.78,                         # Row 98, B
    rsa_I2 = 1163.7359999999999,             # Row 99, B
    rsa_I_sup = 258.608,                     # Row 100, B
    rsa_C0 = 969.78,                         # Row 97, C
    rsa_C1 = 1163.7359999999999,             # Row 98, C
    rsa_C2 = 1357.6919999999998,             # Row 99, C
    rsa_C_sup = 258.608,                     # Row 100, C
    # RSA majoré (Rows 97-100, col D)
    rsa_M0 = 830.2092624,                   # Row 97, D
    rsa_M1 = 1106.9456831999998,             # Row 98, D
    rsa_M2 = 1383.6821039999998,             # Row 99, D
    rsa_M_sup = 276.73642079999996,          # Row 100, D
    # Seuil de versement (Row 102)
    rsa_seuil_versement = 6,                 # Row 102, B

    # =========================================================================
    # Prime d'activité - Rows 104-121
    # =========================================================================
    # Forfait logement PA (Rows 105-107)
    pa_fl_1 = 75.9852,                      # Row 105, B
    pa_fl_2 = 151.9704,                     # Row 106, B
    pa_fl_3 = 188.06337000000002,            # Row 107, B
    # Montants forfaitaires PA (Rows 110-113)
    pa_I0 = 633.21,                          # Row 110, B
    pa_I1 = 949.815,                         # Row 111, B
    pa_I2 = 1139.778,                        # Row 112, B
    pa_I_sup = 253.28400000000002,           # Row 113, B
    pa_C0 = 949.815,                         # Row 110, C
    pa_C1 = 1139.778,                        # Row 111, C
    pa_C2 = 1329.741,                        # Row 112, C
    pa_C_sup = 253.28400000000002,           # Row 113, C
    # PA majoré (Rows 110-113, col D)
    pa_M0 = 813.1176252,                    # Row 110, D
    pa_M1 = 1084.1568336,                   # Row 111, D
    pa_M2 = 1355.196042,                    # Row 112, D
    pa_M_sup = 271.0392084,                 # Row 113, D
    # Coefficient revenus d'activité (Row 114)
    pa_coeff_activite = 0.5985,              # Row 114, B
    # Bonus (Rows 117-119)
    pa_bonus_seuil = 696.2,                 # Row 117, B
    pa_bonus_plafond = 1416,                # Row 118, B
    pa_bonus_max = 184.27,                  # Row 119, B
    # Seuil versement (Row 121)
    pa_seuil_versement = 15,                 # Row 121, B

    # =========================================================================
    # Aides au logement (AL) - Rows 124-154
    # =========================================================================
    # Plafonds de loyer par zone (Rows 125-128, cols G/H/I)
    al_loy_plaf_I0_z1 = 319.30, al_loy_plaf_I0_z2 = 278.28, al_loy_plaf_I0_z3 = 260.82,
    al_loy_plaf_C0_z1 = 385.09, al_loy_plaf_C0_z2 = 340.62, al_loy_plaf_C0_z3 = 316.17,
    al_loy_plaf_1_z1 = 435.24,  al_loy_plaf_1_z2 = 383.28,  al_loy_plaf_1_z3 = 354.51,
    al_loy_plaf_sup_z1 = 63.15, al_loy_plaf_sup_z2 = 55.79,  al_loy_plaf_sup_z3 = 50.81,
    # Forfait charges (Rows 129-131, mêmes pour toutes zones)
    al_charges_0 = 58.08,                    # Row 129
    al_charges_1 = 71.25,                    # Row 130
    al_charges_sup = 13.17,                  # Row 131
    # R0 (Rows 132-140, col H)
    al_R0_I = 5186,                          # Row 132
    al_R0_C = 7430,                          # Row 133
    al_R0_1pac = 8862,                       # Row 134
    al_R0_2pac = 9061,                       # Row 135
    al_R0_3pac = 9408,                       # Row 136
    al_R0_4pac = 9758,                       # Row 137
    al_R0_5pac = 10105,                      # Row 138
    al_R0_6pac = 10454,                      # Row 139
    al_R0_sup = 343,                         # Row 140 (par pac sup après 6)
    # Participation personnelle minimale (Row 141)
    al_p0 = 37.91,                           # Row 141, G
    # Taux appliqué à loyer + charges (Row 142)
    al_taux_loyer_charges = 0.085,           # Row 142, G
    # Taux de participation Tf par composition (Rows 143-149, col H)
    al_TF_I0 = 0.0283,                      # Row 143 (isolé sans PAC)
    al_TF_C0 = 0.0315,                      # Row 144 (couple sans PAC)
    al_TF_1pac = 0.0270,                    # Row 145
    al_TF_2pac = 0.0238,                    # Row 146
    al_TF_3pac = 0.0201,                    # Row 147
    al_TF_4pac = 0.0185,                    # Row 148
    al_TF_sup_pac = -0.0006,                # Row 149 (diminution par PAC après 4)
    # Taux complémentaire TL (Row 150, cols G/H/I)
    al_TL_z1 = 0.004052357337933019,        # Row 150, G
    al_TL_z2 = 0.0030500000000000006,       # Row 150, H
    al_TL_z3 = 0.0026233505821474778,       # Row 150, I
    # Seuils ratio loyer/loyer plafond (Rows 151-152)
    al_seuil_ratio_1_z1 = 3.4, al_seuil_ratio_1_z2 = 2.5, al_seuil_ratio_1_z3 = 2.5,
    al_seuil_ratio_2_z1 = 4.0, al_seuil_ratio_2_z2 = 3.1, al_seuil_ratio_2_z3 = 3.1,
    # Réduction forfaitaire et seuil versement (Rows 153-154)
    al_reduction_5 = 5,                      # Row 153
    al_seuil_versement = 10,                 # Row 154

    # =========================================================================
    # Impôt sur le revenu (IRPP) - Rows 156-194
    # =========================================================================
    # Barème (Rows 159-166, col G)
    irpp_tranche1 = 11497,
    irpp_tranche2 = 29315,
    irpp_tranche3 = 83823,
    irpp_tranche4 = 180294,
    irpp_taux1 = 0.11,
    irpp_taux2 = 0.30,
    irpp_taux3 = 0.41,
    irpp_taux4 = 0.45,
    # Déductions par tranche (Rows 167-170, col G)
    irpp_deduction1 = 1264.67,
    irpp_deduction2 = 6834.52,
    irpp_deduction3 = 16055.05,
    irpp_deduction4 = 23266.81,
    # CEHR (Rows 171-174)
    irpp_cehr_seuil1 = 250000,               # Row 171 (seuil unique, à diviser selon situation)
    irpp_cehr_seuil2 = 500000,               # Row 172
    irpp_cehr_taux1 = 0.03,                  # Row 173
    irpp_cehr_taux2 = 0.04,                  # Row 174
    # Abattement frais pro (Rows 175-177)
    irpp_abattement_fp_taux = 0.10,          # Row 175
    irpp_abattement_fp_max = 14426,          # Row 176
    irpp_abattement_fp_min = 504,            # Row 177
    # Abattement pensions/retraites (Rows 178-180)
    irpp_abattement_pension_taux = 0.10,     # Row 178
    irpp_abattement_pension_max = 4399,      # Row 179
    irpp_abattement_pension_min = 450,       # Row 180
    # Quotient familial (Rows 181-182)
    irpp_qf_plafond_pi = 4224,              # Row 181 (parent isolé)
    irpp_qf_plafond = 1791,                 # Row 182 (couple)
    # Décote (Rows 183-185)
    irpp_decote_coeff = 0.4525,             # Row 183
    irpp_decote_seuil_I = 1964,             # Row 184
    irpp_decote_seuil_C = 3249,             # Row 185
    # Réductions/crédits d'impôt (Rows 186-194)
    irpp_credit_college = 61,               # Row 186
    irpp_credit_lycee = 153,                # Row 187
    irpp_credit_superieur = 183,            # Row 188
    irpp_credit_garde_taux_am = 0.50,       # Row 189
    irpp_credit_garde_plafond_am = 3500,    # Row 190
    irpp_credit_garde_taux_dom = 0.50,      # Row 191
    irpp_credit_garde_seuil_dom = 12000,    # Row 192
    irpp_credit_garde_sup_enfant_dom = 1500, # Row 193
    irpp_credit_garde_plafond_dom = 15000,  # Row 194

    # =========================================================================
    # Frais de garde - Rows 199-234
    # =========================================================================
    # Assistante maternelle (Rows 200-210)
    garde_am_horaire_net = 3.91,             # Row 200, B
    garde_am_entretien_jour = 4.149,         # Row 201, B
    garde_am_entretien_forfait_ci = 2.65,    # Row 202, B
    garde_am_repas = 3.798,                  # Row 203, B
    garde_am_jours = 18,                     # Row 204, B
    garde_am_heures_jour = 9,                # Row 205, B
    garde_am_repas_tp = 68.364,              # Row 206, B
    garde_am_cout_net_cmg = 776.466,         # Row 207, B
    garde_am_cout_net_ci = 681.12,           # Row 208, B
    garde_am_cotis_sal = 529.20,             # Row 209, B
    garde_am_cout_brut_total = 1305.67,      # Row 210, B
    garde_am_taux_cotis_sal = 0.2188025,     # Row 200, E
    garde_am_taux_cotis_pat = 0.43386,       # Row 200, F
    # Garde à domicile (Rows 213-220)
    garde_dom_horaire_net = 10,              # Row 213, B
    garde_dom_jours = 18,                    # Row 214, B
    garde_dom_heures_travail = 8,            # Row 215, B
    garde_dom_heures_presence = 1,           # Row 216, B
    garde_dom_cout_net = 1560,               # Row 217, B
    garde_dom_abattement_cotis = 282,        # Row 218, B
    garde_dom_cotisations = 1047.48,         # Row 219, B
    garde_dom_cout_brut = 2607.48,           # Row 220, B
    garde_dom_taux_cotis_sal = 0.2188025,    # Row 213, E
    garde_dom_taux_cotis_pat = 0.44696,      # Row 213, F
    # Crèche (Rows 224-234)
    garde_creche_coeff_1 = 0.0006119,        # Row 225, B
    garde_creche_coeff_2 = 0.000516,         # Row 226, B
    garde_creche_coeff_3 = 0.000413,         # Row 227, B
    garde_creche_coeff_4plus = 0.00031,      # Row 228, B
    garde_creche_coeff_annuel_1 = 1.1895336, # Row 225, C
    garde_creche_coeff_annuel_2 = 1.003104,  # Row 226, C
    garde_creche_coeff_annuel_3 = 0.802872,  # Row 227, C
    garde_creche_coeff_annuel_4plus = 0.60264, # Row 228, C
    garde_creche_heures_jour = 8.64,         # Row 230, B
    garde_creche_jours_semaine = 5,          # Row 231, B
    garde_creche_semaines = 45,              # Row 232, B
    garde_creche_plancher = 801,             # Row 233, B
    garde_creche_plafond = 7000,             # Row 234, B

    # =========================================================================
    # Bourses de collège - Rows 243-248
    # =========================================================================
    # Montants annuels et plafonds (Rows 246-248)
    bourse_college_echelon = c(516, 330, 120),             # Col A
    bourse_college_plaf_1enf = c(3458, 9691, 18130),       # Col B
    bourse_college_sup_enfant = c(798, 2262, 4183),        # Col C

    # =========================================================================
    # Bourses de lycée - Rows 251-269
    # =========================================================================
    # Montants annuels par échelon (Rows 254-259)
    bourse_lycee_montant = c(495, 609, 720, 831, 939, 1053),  # Echelons 1 à 6
    # Plafonds par nombre d'enfants et échelon (Rows 262-269)
    # Matrice: lignes = nb enfants (1 à 8+), colonnes = échelons (1 à 6)
    bourse_lycee_plaf = matrix(
      c(21611, 17107, 14529, 11718,  7282,  2846,   # 1 enfant
        23272, 18664, 15849, 12781,  8092,  3401,   # 2 enfants
        26596, 21774, 18491, 14913,  9710,  4507,   # 3 enfants
        30753, 24887, 21133, 17044, 11327,  5612,   # 4 enfants
        34908, 29553, 25095, 20240, 13755,  7271,   # 5 enfants
        39897, 34217, 29059, 23437, 16184,  8929,   # 6 enfants
        44883, 38884, 33022, 26631, 18611, 10590,   # 7 enfants
        49870, 43552, 36985, 29826, 21039, 12248),  # 8+ enfants
      nrow = 8, byrow = TRUE,
      dimnames = list(paste0("enf", 1:8), paste0("ech", 1:6))
    ),

    # =========================================================================
    # Bourses du supérieur - Rows 274-308
    # =========================================================================
    # Montants annuels sur 10 mois (Rows 301-308, col C)
    bourse_sup_montant = c(1454, 2163, 3071, 3828, 4587, 5212, 5506, 6335),
    # Echelon labels: 0bis, 1, 2, 3, 4, 5, 6, 7
    bourse_sup_montant_vacances = c(1744.8, 2596, 3685, 4594, 5504, 6254, 6607, 7602),
    # Plafonds par points de charge (Rows 280-297) et échelon (cols B-I)
    # Matrice: lignes = points de charge (0 à 17), colonnes = échelons (0.5 à 7)
    bourse_sup_plaf = matrix(
      c(35086, 23850, 19281, 17034, 14829, 12667,  7992,   265,   # 0 points
        38966, 26500, 21423, 18921, 16472, 14077,  8872,   530,   # 1 point
        42877, 29150, 23564, 20818, 18126, 15476,  9773,   795,   # 2 points
        46767, 31800, 25705, 22716, 19758, 16875, 10653,  1060,   # 3 points
        50668, 34450, 27846, 24603, 21412, 18285, 11533,  1325,   # 4 points
        54569, 37111, 29998, 26500, 23066, 19695, 12434,  1590,   # 5 points
        58459, 39761, 32139, 28376, 24709, 21105, 13324,  1855,   # 6 points
        62360, 42411, 34280, 30274, 26352, 22514, 14215,  2120,   # 7 points
        66261, 45061, 36422, 32171, 29648, 23914, 15094,  2385,   # 8 points
        70151, 47700, 38563, 34058, 29648, 25323, 15985,  2650,   # 9 points
        74052, 50361, 40704, 35955, 31291, 26733, 16685,  2915,   # 10 points
        77952, 53011, 42835, 37853, 32955, 28132, 17755,  3180,   # 11 points
        81843, 55650, 44976, 39739, 34588, 29542, 18645,  3445,   # 12 points
        85743, 58300, 47117, 41637, 36231, 30952, 19525,  3710,   # 13 points
        89634, 60971, 49269, 43513, 37895, 32362, 20426,  3975,   # 14 points
        93545, 63611, 51410, 45410, 39538, 33772, 21317,  4240,   # 15 points
        97435, 66261, 53551, 47308, 41170, 35181, 22196,  4505,   # 16 points
       101347, 68911, 55692, 49195, 42824, 36581, 23087,  4770),  # 17 points
      nrow = 18, byrow = TRUE,
      dimnames = list(paste0("pts", 0:17), paste0("ech", c("0bis", 1:7)))
    ),

    # =========================================================================
    # Pension alimentaire (CEEE) - Rows 331-356
    # =========================================================================
    # Table 2010 (Rows 332-342)
    ceee_2010_abattement = 646.52,           # Row 333, C
    # Taux: colonnes = classique / réduit / alterné (Rows 337-342)
    ceee_2010_taux = matrix(
      c(0.135, 0.180, 0.090,   # 1 enfant
        0.115, 0.155, 0.078,   # 2 enfants
        0.100, 0.133, 0.067,   # 3 enfants
        0.088, 0.117, 0.059,   # 4 enfants
        0.080, 0.106, 0.053,   # 5 enfants
        0.075, 0.095, 0.048),  # 6 enfants
      nrow = 6, byrow = TRUE,
      dimnames = list(paste0("enf", 1:6), c("classique", "reduit", "alterne"))
    ),
    # Table 2018 (Rows 345-356)
    ceee_2018_abattement = 0,                # Row 346, C
    ceee_2018_rav = 646.52,                  # Row 347, C (reste à vivre minimum)
    ceee_2018_taux = matrix(
      c(0.140, 0.187, 0.093,   # 1 enfant
        0.118, 0.157, 0.079,   # 2 enfants
        0.102, 0.136, 0.068,   # 3 enfants
        0.090, 0.120, 0.060,   # 4 enfants
        0.080, 0.107, 0.053,   # 5 enfants
        0.072, 0.097, 0.048),  # 6 enfants
      nrow = 6, byrow = TRUE,
      dimnames = list(paste0("enf", 1:6), c("classique", "reduit", "alterne"))
    ),

    # =========================================================================
    # Unités de consommation - Rows 359-368
    # =========================================================================
    uc_adulte = 1,
    uc_conjoint = 0.5,
    # Échelle standard (Row 360-361, col B)
    uc_enfant = 0.3,                         # Row 360, B
    uc_ado = 0.5,                            # Row 361, B
    # Coût solo +1/3 (col C)
    uc_enfant_solo_tiers = 0.4,              # Row 360, C
    uc_ado_solo_tiers = 0.6667,              # Row 361, C
    # Coût solo +50% (col D)
    uc_enfant_solo_moitie = 0.45,            # Row 360, D
    uc_ado_solo_moitie = 0.75,               # Row 361, D
    # Parent gardien (col C-D, Rows 363-364)
    uc_enfant_pg = 0.3,                      # Row 363, C
    uc_ado_pg = 0.5,                         # Row 364, C
    uc_enfant_pg_partage = 0.3375,           # Row 363, D
    uc_ado_pg_partage = 0.5625,              # Row 364, D
    # Parent non gardien (Rows 365-366)
    uc_enfant_png = 0.1,                     # Row 365, C
    uc_ado_png = 0.1667,                     # Row 366, C
    uc_enfant_png_partage = 0.1125,          # Row 365, D
    uc_ado_png_partage = 0.1875,             # Row 366, D
    # Garde alternée (Rows 367-368)
    uc_enfant_ga = 0.2,                      # Row 367, C
    uc_ado_ga = 0.3333,                      # Row 368, C
    uc_enfant_ga_partage = 0.225,            # Row 367, D
    uc_ado_ga_partage = 0.375,               # Row 368, D
    # Barème intermédiaire (Rows 361-364)
    bareme_intermediaire_seuil1 = 1426.3,    # Row 361, H
    bareme_intermediaire_seuil2 = 2852.6,    # Row 362, H
    bareme_intermediaire_coeff_a = 1.4532847227091072  # Row 364, H
  )
}
