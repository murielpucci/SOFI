# =============================================================================
# SOFI - PAJE (Allocation de Base + PREPARE)
# =============================================================================

#' Calcul de l'Allocation de Base de la PAJE (AB)
#' @param ages_enfants Vecteur des âges des enfants
#' @param situation "isolé" ou "couple"
#' @param nb_revenus 1 ou 2
#' @param ressources Ressources annuelles du ménage (base PF)
#' @param type_parent Type de parent
#' @param p Paramètres législatifs
#' @return Montant mensuel de l'AB
calcul_ab <- function(ages_enfants, situation, nb_revenus, ressources, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_moins_3 <- sum(ages >= 0 & ages < 3)
  if (nb_moins_3 == 0) return(0)

  nb_enfants <- length(ages)

  # Déterminer les plafonds selon situation et nombre de revenus
  if (situation == "couple") {
    if (nb_revenus >= 2) {
      plaf_tp <- p$ab_plaf_tp_couple_2rev
      plaf_tr <- p$ab_plaf_tr_couple_2rev
      sup_enfant_tp <- p$ab_plaf_tp_sup_enfant
      sup_enfant_tr <- p$ab_plaf_tr_sup_enfant
    } else {
      plaf_tp <- p$ab_plaf_tp_couple_1rev
      plaf_tr <- p$ab_plaf_tr_couple_1rev
      sup_enfant_tp <- p$ab_plaf_tp_sup_enfant
      sup_enfant_tr <- p$ab_plaf_tr_sup_enfant
    }
  } else {
    plaf_tp <- p$ab_plaf_tp_isole
    plaf_tr <- p$ab_plaf_tr_isole
    sup_enfant_tp <- p$ab_plaf_tp_sup_enfant
    sup_enfant_tr <- p$ab_plaf_tr_sup_enfant
  }

  plaf_tp <- plaf_tp + max(0, nb_enfants - 1) * sup_enfant_tp
  plaf_tr <- plaf_tr + max(0, nb_enfants - 1) * sup_enfant_tr

  if (ressources <= plaf_tr) {
    return(p$ab_taux_plein)
  } else if (ressources <= plaf_tp) {
    return(p$ab_taux_reduit)
  }

  0
}


#' Calcul de la PREPARE (Prestation Partagée d'Éducation de l'Enfant)
calcul_prepare <- function(ages_enfants, quotite_travail, quotite_prepare, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_moins_3 <- sum(ages >= 0 & ages < 3)
  if (nb_moins_3 == 0) return(0)

  if (quotite_prepare == 0 || quotite_travail >= 1) return(0)

  if (quotite_travail == 0) {
    return(p$prepare_taux_plein)
  } else if (quotite_travail <= 0.5) {
    return(p$prepare_taux_partiel_50)
  } else if (quotite_travail <= 0.8) {
    return(p$prepare_taux_partiel_80)
  }

  0
}
