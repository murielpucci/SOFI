# =============================================================================
# SOFI - Impôt sur le Revenu des Personnes Physiques (IRPP)
# =============================================================================

#' Calcul du nombre de parts fiscales
#' @param situation "isolé" ou "couple"
#' @param nb_enfants Nombre d'enfants à charge
#' @param parent_isole TRUE si parent isolé (case T)
#' @return Nombre de parts fiscales
calcul_parts_fiscales <- function(situation, nb_enfants, parent_isole = FALSE) {
  if (situation == "couple") {
    parts <- 2
  } else {
    parts <- 1
  }

  # Enfants: 0.5 pour les 2 premiers, 1 à partir du 3ème
  if (nb_enfants >= 1) parts <- parts + 0.5
  if (nb_enfants >= 2) parts <- parts + 0.5
  if (nb_enfants >= 3) parts <- parts + (nb_enfants - 2)

  # Parent isolé: demi-part supplémentaire
  if (parent_isole && situation == "isolé" && nb_enfants > 0) {
    parts <- parts + 0.5
  }

  parts
}


#' Calcul de l'impôt brut selon le barème progressif
#' @param revenu_imposable_par_part Revenu imposable par part
#' @param p Paramètres législatifs
#' @return Impôt par part
impot_bareme <- function(revenu_imposable_par_part, p) {
  R <- revenu_imposable_par_part

  if (R <= p$irpp_tranche1) return(0)

  impot <- 0
  if (R > p$irpp_tranche1) {
    impot <- impot + min(R, p$irpp_tranche2) * p$irpp_taux1 -
      p$irpp_tranche1 * p$irpp_taux1
  }
  if (R > p$irpp_tranche2) {
    impot <- impot + (min(R, p$irpp_tranche3) - p$irpp_tranche2) * p$irpp_taux2
  }
  if (R > p$irpp_tranche3) {
    impot <- impot + (min(R, p$irpp_tranche4) - p$irpp_tranche3) * p$irpp_taux3
  }
  if (R > p$irpp_tranche4) {
    impot <- impot + (R - p$irpp_tranche4) * p$irpp_taux4
  }

  impot
}


#' Calcul complet de l'IRPP annuel
#' @param situation "isolé" ou "couple"
#' @param nb_enfants Nombre d'enfants à charge fiscale
#' @param salaire_annuel_1 Salaire annuel net imposable adulte 1
#' @param salaire_annuel_2 Salaire annuel net imposable adulte 2
#' @param autres_revenus_annuels Autres revenus annuels imposables
#' @param pension_payee_annuelle Pension alimentaire déduite annuelle
#' @param ages_enfants Vecteur des âges des enfants
#' @param frais_garde_annuels Frais de garde annuels (enfants < 6 ans)
#' @param parent_isole TRUE si parent isolé (case T)
#' @param p Paramètres législatifs
#' @return Impôt annuel (positif = à payer)
calcul_irpp <- function(situation, nb_enfants, salaire_annuel_1, salaire_annuel_2 = 0,
                        autres_revenus_annuels = 0, pension_payee_annuelle = 0,
                        ages_enfants = c(), frais_garde_annuels = 0,
                        parent_isole = FALSE, p) {

  # 1. Revenu net imposable
  # Abattement 10% frais professionnels
  abat1 <- min(max(salaire_annuel_1 * p$irpp_abattement_fp_taux, p$irpp_abattement_fp_min),
               p$irpp_abattement_fp_max)
  abat2 <- if (salaire_annuel_2 > 0) {
    min(max(salaire_annuel_2 * p$irpp_abattement_fp_taux, p$irpp_abattement_fp_min),
        p$irpp_abattement_fp_max)
  } else {
    0
  }

  revenu_net <- salaire_annuel_1 - abat1 + salaire_annuel_2 - abat2 +
    autres_revenus_annuels - pension_payee_annuelle
  revenu_net <- max(0, revenu_net)

  # 2. Quotient familial
  parts <- calcul_parts_fiscales(situation, nb_enfants, parent_isole)

  # Impôt avec QF
  R_par_part <- revenu_net / parts
  impot_qf <- impot_bareme(R_par_part, p) * parts

  # 3. Plafonnement du QF
  # Impôt sans avantage QF (avec parts de base)
  parts_base <- if (situation == "couple") 2 else 1
  R_base <- revenu_net / parts_base
  impot_base <- impot_bareme(R_base, p) * parts_base

  # Avantage maximal
  demi_parts_sup <- parts - parts_base
  if (parent_isole && situation == "isolé" && nb_enfants > 0) {
    # Première demi-part de parent isolé: plafond spécifique
    avantage_max <- p$irpp_qf_plafond_pi + max(0, demi_parts_sup - 0.5) * 2 * p$irpp_qf_plafond
  } else {
    avantage_max <- demi_parts_sup * 2 * p$irpp_qf_plafond
  }

  avantage_reel <- impot_base - impot_qf
  if (avantage_reel > avantage_max) {
    impot <- impot_base - avantage_max
  } else {
    impot <- impot_qf
  }

  # 4. Décote
  if (situation == "couple") {
    seuil_decote <- p$irpp_decote_seuil_C
  } else {
    seuil_decote <- p$irpp_decote_seuil_I
  }

  if (impot > 0 && impot < seuil_decote) {
    decote <- seuil_decote * p$irpp_decote_coeff - impot * p$irpp_decote_coeff
    impot <- max(0, impot - decote)
  }

  # 5. Réductions et crédits d'impôt
  ages <- ages_enfants[ages_enfants >= 0]

  # Crédit pour frais de garde (enfants < 6 ans)
  nb_moins_6 <- sum(ages >= 0 & ages < 6)
  credit_garde <- min(frais_garde_annuels, nb_moins_6 * p$irpp_credit_garde_plafond_am) *
    p$irpp_credit_garde_taux_am

  # Réductions pour scolarité
  nb_college <- sum(ages >= 11 & ages <= 14)
  nb_lycee <- sum(ages >= 15 & ages <= 17)
  nb_sup <- sum(ages >= 18 & ages <= 25)  # approximation
  reduction_scolarite <- nb_college * p$irpp_credit_college +
    nb_lycee * p$irpp_credit_lycee +
    nb_sup * p$irpp_credit_superieur

  # Appliquer réductions (limitées à l'impôt)
  impot <- max(0, impot - reduction_scolarite)

  # Appliquer crédits (peuvent créer un impôt négatif = remboursement)
  impot <- impot - credit_garde

  # 6. Contribution exceptionnelle hauts revenus (CEHR)
  # Seuils: 250k/500k pour isolé, 500k/1M pour couple
  revenu_fiscal_ref <- revenu_net
  if (situation == "couple") {
    seuil1_cehr <- p$irpp_cehr_seuil1 * 2  # 500000
    seuil2_cehr <- p$irpp_cehr_seuil2 * 2  # 1000000
  } else {
    seuil1_cehr <- p$irpp_cehr_seuil1       # 250000
    seuil2_cehr <- p$irpp_cehr_seuil2       # 500000
  }

  if (revenu_fiscal_ref > seuil2_cehr) {
    cehr <- (seuil2_cehr - seuil1_cehr) * p$irpp_cehr_taux1 +
      (revenu_fiscal_ref - seuil2_cehr) * p$irpp_cehr_taux2
  } else if (revenu_fiscal_ref > seuil1_cehr) {
    cehr <- (revenu_fiscal_ref - seuil1_cehr) * p$irpp_cehr_taux1
  } else {
    cehr <- 0
  }

  impot + cehr
}
