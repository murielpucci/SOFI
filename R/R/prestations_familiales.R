# =============================================================================
# SOFI - Prestations Familiales (AF, ASF, ARS, CF)
# =============================================================================

#' Calcul des Allocations Familiales (AF)
#' @param ages_enfants Vecteur des âges des enfants (-1 = pas d'enfant)
#' @param ressources Ressources annuelles du ménage (base PF)
#' @param type_parent "pg", "png", "solo", "ga_m", "ga_p"
#' @param p Paramètres législatifs
#' @return Montant mensuel des AF
calcul_af <- function(ages_enfants, ressources, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_enfants_af <- sum(ages <= 20)

  if (nb_enfants_af < 2) return(0)

  # Montant de base selon le nombre d'enfants (valeurs exactes après CRDS)
  montant_base <- switch(as.character(nb_enfants_af),
    "2" = p$af_2,
    "3" = p$af_3,
    "4" = p$af_4,
    "5" = p$af_5,
    "6" = p$af_6,
    # Au-delà de 6, extrapolation
    p$af_6 + (nb_enfants_af - 6) * (p$af_6 - p$af_5)
  )

  # Majoration enfants > 14 ans (sauf aîné si famille de 2 enfants)
  enfants_14_plus <- sum(ages > 14 & ages <= 20)
  if (nb_enfants_af == 2 && enfants_14_plus > 0) {
    majo <- max(0, enfants_14_plus - 1) * p$af_majo_14
  } else {
    majo <- enfants_14_plus * p$af_majo_14
  }

  # Forfait pour enfants de 20 ans
  forfait <- sum(ages == 20) * p$af_forfait

  montant_total <- montant_base + majo + forfait

  # Modulation selon les ressources
  plafond_t1 <- p$af_plaf_tranche1 + (nb_enfants_af - 2) * p$af_plaf_sup_enfant
  plafond_t2 <- p$af_plaf_tranche2 + (nb_enfants_af - 2) * p$af_plaf_sup_enfant

  if (ressources <= plafond_t1) {
    taux <- 1
  } else if (ressources <= plafond_t2) {
    taux <- p$af_taux_modulation_t1
  } else {
    taux <- p$af_taux_modulation_t2
  }

  montant_total * taux
}


#' Calcul de l'ASF (Allocation de Soutien Familial)
calcul_asf <- function(droit_asf, situation, nb_enfants, pension_recue, type_parent, p) {
  if (droit_asf != 1 || situation != "isolé") return(0)
  if (type_parent == "ga_p") return(0)

  asf_brut <- p$asf * nb_enfants
  max(0, asf_brut - pension_recue)
}


#' Calcul de l'ARS (Allocation de Rentrée Scolaire)
#' @return Montant ANNUEL de l'ARS
calcul_ars <- function(ages_enfants, ressources, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_enfants_charge <- length(ages)
  if (nb_enfants_charge == 0) return(0)

  # Plafond de ressources
  plafond <- p$ars_plaf_base + nb_enfants_charge * p$ars_plaf_sup_enfant
  if (ressources > plafond) return(0)

  # Montant par tranche d'âge
  montant <- 0
  for (age in ages) {
    if (age >= 6 && age <= 10) montant <- montant + p$ars_6_10
    else if (age >= 11 && age <= 14) montant <- montant + p$ars_11_14
    else if (age >= 15 && age <= 18) montant <- montant + p$ars_15_18
  }

  # ARS différentielle si ressources proches du plafond
  ars_pleine <- montant
  depassement <- ressources - (plafond - ars_pleine)
  if (depassement > 0) {
    montant <- max(0, ars_pleine - depassement)
  }

  montant
}


#' Calcul du Complément Familial (CF)
calcul_cf <- function(ages_enfants, situation, nb_revenus, ressources, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_enfants_cf <- sum(ages >= 3 & ages <= 21)
  if (nb_enfants_cf < 3) return(0)

  # Vérifier qu'aucun enfant < 3 ans
  if (any(ages >= 0 & ages < 3)) return(0)

  nb_enfants_total <- length(ages)

  # Plafond CF majoré (ressources basses)
  if (situation == "couple") {
    if (nb_revenus >= 2) {
      plaf_majo <- p$cf_plaf_majo_couple_2rev
    } else {
      plaf_majo <- p$cf_plaf_majo_couple_1rev
    }
  } else {
    plaf_majo <- p$cf_plaf_majo_isole
  }
  plaf_majo <- plaf_majo + max(0, nb_enfants_total - 3) * p$cf_plaf_majo_sup_enfant

  if (ressources <= plaf_majo) {
    return(p$cf_majore)  # CF + majoration
  }

  # Plafond CF de base (ressources moyennes)
  if (situation == "couple") {
    if (nb_revenus >= 2) {
      plaf_base <- p$cf_plaf_couple_2rev
    } else {
      plaf_base <- p$cf_plaf_couple_1rev
    }
  } else {
    plaf_base <- p$cf_plaf_isole
  }
  plaf_base <- plaf_base + max(0, nb_enfants_total - 3) * p$cf_plaf_sup_enfant

  if (ressources <= plaf_base) {
    return(p$cf)  # CF de base seulement
  }

  0
}
