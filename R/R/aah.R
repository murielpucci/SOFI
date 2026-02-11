# =============================================================================
# SOFI - AAH (Allocation Adulte Handicapé) - Déconjugalisée
# =============================================================================

#' Calcul de l'AAH
#' @param droit_aah 0 ou 1 (droit reconnu)
#' @param nb_enfants Nombre d'enfants à charge
#' @param revenus_activite_annuels Revenus d'activité annuels de la personne
#' @param autres_revenus_annuels Autres revenus annuels de la personne (hors couple)
#' @param p Paramètres législatifs
#' @return Montant mensuel de l'AAH
calcul_aah <- function(droit_aah, nb_enfants, revenus_activite_annuels,
                       autres_revenus_annuels = 0, p) {
  if (droit_aah != 1) return(0)

  # Plafond annuel (individuel, déconjugalisé)
  plafond <- p$aah_plafond_isole * (1 + p$aah_plafond_sup_enfant * nb_enfants)

  # Abattement sur les revenus d'activité
  # Seuil = valeur mensuelle * 12 (Row 83: 540.54 mensuel)
  seuil_abattement <- p$aah_seuil_abattement_1 * 12

  if (revenus_activite_annuels <= seuil_abattement) {
    revenus_apres_abat <- revenus_activite_annuels * (1 - p$aah_abattement_bas)
  } else {
    revenus_apres_abat <- seuil_abattement * (1 - p$aah_abattement_bas) +
      (revenus_activite_annuels - seuil_abattement) * (1 - p$aah_abattement_haut)
  }

  # Ressources prises en compte
  ressources <- revenus_apres_abat + autres_revenus_annuels

  # AAH différentielle
  aah_annuelle <- max(0, plafond - ressources)
  aah_mensuelle <- min(p$aah, aah_annuelle / 12)

  aah_mensuelle
}
