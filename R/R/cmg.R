# =============================================================================
# SOFI - CMG (Complément libre choix du Mode de Garde) - Réforme 2025
# =============================================================================

#' Calcul du CMG (réforme septembre 2025)
#' @param ages_enfants Vecteur des âges des enfants
#' @param ressources_mensuelles Ressources mensuelles du ménage
#' @param heures_am Heures mensuelles chez l'assistante maternelle
#' @param heures_dom Heures mensuelles en garde à domicile
#' @param cout_horaire_am Coût horaire réel assistante maternelle (NULL = valeur par défaut)
#' @param cout_horaire_dom Coût horaire réel garde à domicile (NULL = valeur par défaut)
#' @param type_parent Type de parent
#' @param p Paramètres législatifs
#' @return Liste avec CMG cotisations et CMG prestation
calcul_cmg <- function(ages_enfants, ressources_mensuelles, heures_am = 0, heures_dom = 0,
                       cout_horaire_am = NULL, cout_horaire_dom = NULL,
                       type_parent, p) {
  if (type_parent == "ga_p") return(list(cmg_cotis = 0, cmg_prestation = 0))

  ages <- ages_enfants[ages_enfants >= 0]
  nb_moins_6 <- sum(ages >= 0 & ages < 6)
  if (nb_moins_6 == 0) return(list(cmg_cotis = 0, cmg_prestation = 0))

  if (heures_am == 0 && heures_dom == 0) return(list(cmg_cotis = 0, cmg_prestation = 0))

  # Ressources bornées
  ressources <- max(p$cmg_plancher_ressources, min(ressources_mensuelles, p$cmg_plafond_ressources))

  # Taux d'effort selon nombre d'enfants gardés
  if (nb_moins_6 == 1) {
    effort_am <- p$cmg_effort_am_1
    effort_dom <- p$cmg_effort_dom_1
  } else if (nb_moins_6 == 2) {
    effort_am <- p$cmg_effort_am_2
    effort_dom <- p$cmg_effort_dom_2
  } else if (nb_moins_6 == 3) {
    effort_am <- p$cmg_effort_am_3
    effort_dom <- p$cmg_effort_dom_3
  } else {
    effort_am <- p$cmg_effort_am_4plus
    effort_dom <- p$cmg_effort_dom_4plus
  }

  cmg_total <- 0

  # CMG Assistante maternelle
  if (heures_am > 0) {
    cout_h_am <- if (!is.null(cout_horaire_am)) cout_horaire_am else p$garde_am_horaire_net
    cout_h_am <- min(cout_h_am, p$cmg_plaf_h_am)
    cout_ref <- p$cmg_cout_ref_am
    reste_a_charge_h <- effort_am * ressources
    prise_en_charge_h <- max(0, min(cout_ref, cout_h_am) - reste_a_charge_h)
    cmg_am <- prise_en_charge_h * min(heures_am, p$cmg_heures_max_mois)
    cmg_total <- cmg_total + cmg_am
  }

  # CMG Garde à domicile
  if (heures_dom > 0) {
    cout_h_dom <- if (!is.null(cout_horaire_dom)) cout_horaire_dom else p$garde_dom_horaire_net
    cout_h_dom <- min(cout_h_dom, p$cmg_plaf_h_dom)
    cout_ref <- p$cmg_cout_ref_dom
    reste_a_charge_h <- effort_dom * ressources
    prise_en_charge_h <- max(0, min(cout_ref, cout_h_dom) - reste_a_charge_h)
    cmg_dom <- prise_en_charge_h * min(heures_dom, p$cmg_heures_max_mois)
    cmg_total <- cmg_total + cmg_dom
  }

  list(cmg_cotis = 0, cmg_prestation = cmg_total)
}
