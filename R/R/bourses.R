# =============================================================================
# SOFI - Bourses (Collège, Lycée, Supérieur)
# =============================================================================

#' Calcul de la bourse de collège
#' @param ages_enfants Vecteur des âges des enfants
#' @param ressources Ressources annuelles (base PF)
#' @param nb_enfants_charge Nombre total d'enfants à charge
#' @param type_parent Type de parent
#' @param p Paramètres législatifs
#' @return Montant ANNUEL de la bourse de collège
calcul_bourse_college <- function(ages_enfants, ressources, nb_enfants_charge, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_collegiens <- sum(ages >= 11 & ages <= 14)
  if (nb_collegiens == 0) return(0)

  # 3 échelons (du plus généreux au moins)
  # Plafonds = base 1 enfant + supplément par enfant au-delà
  montant_par_collegien <- 0
  for (i in seq_along(p$bourse_college_echelon)) {
    plafond <- p$bourse_college_plaf_1enf[i] +
      max(0, nb_enfants_charge - 1) * p$bourse_college_sup_enfant[i]
    if (ressources <= plafond) {
      montant_par_collegien <- p$bourse_college_echelon[i]
      break
    }
  }

  montant_par_collegien * nb_collegiens
}


#' Calcul de la bourse de lycée
#' @param ages_enfants Vecteur des âges des enfants
#' @param ressources Ressources annuelles (base PF)
#' @param nb_enfants_charge Nombre total d'enfants à charge
#' @param type_parent Type de parent
#' @param p Paramètres législatifs
#' @return Montant ANNUEL de la bourse de lycée
calcul_bourse_lycee <- function(ages_enfants, ressources, nb_enfants_charge, type_parent, p) {
  if (type_parent == "ga_p") return(0)

  ages <- ages_enfants[ages_enfants >= 0]
  nb_lyceens <- sum(ages >= 15 & ages <= 17)
  if (nb_lyceens == 0) return(0)

  # Déterminer la ligne de plafond selon le nb d'enfants (1 à 8+)
  idx_enf <- min(nb_enfants_charge, 8)

  # 6 échelons, du plus haut au plus bas
  # On parcourt dans l'ordre décroissant des montants (échelon 6 = le plus élevé)
  montant_par_lyceen <- 0
  for (ech in 6:1) {
    plafond <- p$bourse_lycee_plaf[idx_enf, ech]
    if (ressources <= plafond) {
      montant_par_lyceen <- p$bourse_lycee_montant[ech]
      break
    }
  }

  montant_par_lyceen * nb_lyceens
}


#' Calcul de la bourse du supérieur
#' @param nb_enfants_sup Nombre d'enfants dans le supérieur (18+)
#' @param ressources Ressources annuelles du ménage
#' @param points_de_charge Points de charge du ménage (0 à 17)
#' @param type_parent Type de parent
#' @param p Paramètres législatifs
#' @return Montant ANNUEL de la bourse du supérieur
calcul_bourse_superieur <- function(nb_enfants_sup, ressources, points_de_charge = 0,
                                    type_parent, p) {
  if (type_parent == "ga_p") return(0)
  if (nb_enfants_sup == 0) return(0)

  # Points de charge (0 à 17)
  pts <- min(max(points_de_charge, 0), 17)
  idx_pts <- pts + 1  # index dans la matrice (lignes 1 à 18)

  # Trouver l'échelon le plus élevé dont le plafond est respecté
  # Echelons: 0bis, 1, 2, 3, 4, 5, 6, 7 (colonnes 1 à 8)
  montant_par_etudiant <- 0
  for (ech in 8:1) {
    plafond <- p$bourse_sup_plaf[idx_pts, ech]
    if (ressources <= plafond) {
      montant_par_etudiant <- p$bourse_sup_montant[ech]
      break
    }
  }

  montant_par_etudiant * nb_enfants_sup
}
