# =============================================================================
# SOFI - Aides au Logement (AL)
# Formule : AL = L + C - PP, avec PP = p0 + Tf*R/12 + Tl*(L+C)
# =============================================================================

#' Calcul des Aides au Logement
#' @param situation "isolé" ou "couple"
#' @param nb_enfants Nombre d'enfants (personnes à charge)
#' @param est_locataire TRUE si locataire
#' @param zone Zone de logement (1, 2 ou 3)
#' @param loyer Loyer mensuel (ou "plaf" pour loyer plafond)
#' @param ressources_mensuelles Ressources mensuelles du ménage
#' @param p Paramètres législatifs
#' @return Montant mensuel de l'aide au logement
calcul_al <- function(situation, nb_enfants, est_locataire, zone, loyer,
                      ressources_mensuelles, p) {
  if (!est_locataire) return(0)

  nb_pac <- nb_enfants
  zone_str <- paste0("z", zone)

  # --- Plafond de loyer ---
  if (nb_pac == 0) {
    if (situation == "isolé") {
      loy_plaf <- p[[paste0("al_loy_plaf_I0_", zone_str)]]
    } else {
      loy_plaf <- p[[paste0("al_loy_plaf_C0_", zone_str)]]
    }
  } else {
    loy_plaf <- p[[paste0("al_loy_plaf_1_", zone_str)]]
    if (nb_pac > 1) {
      loy_plaf <- loy_plaf + (nb_pac - 1) * p[[paste0("al_loy_plaf_sup_", zone_str)]]
    }
  }

  # Loyer pris en compte (plafonné)
  if (is.character(loyer) && loyer == "plaf") {
    L <- loy_plaf
  } else {
    L <- min(as.numeric(loyer), loy_plaf)
  }

  # --- Forfait charges ---
  if (nb_pac == 0) {
    C <- p$al_charges_0
  } else if (nb_pac == 1) {
    C <- p$al_charges_1
  } else {
    C <- p$al_charges_1 + (nb_pac - 1) * p$al_charges_sup
  }

  # --- R0 (ressources annuelles de référence) ---
  if (nb_pac == 0) {
    if (situation == "isolé") {
      R0 <- p$al_R0_I
    } else {
      R0 <- p$al_R0_C
    }
  } else if (nb_pac == 1) {
    R0 <- p$al_R0_1pac
  } else if (nb_pac == 2) {
    R0 <- p$al_R0_2pac
  } else if (nb_pac == 3) {
    R0 <- p$al_R0_3pac
  } else if (nb_pac == 4) {
    R0 <- p$al_R0_4pac
  } else if (nb_pac == 5) {
    R0 <- p$al_R0_5pac
  } else if (nb_pac == 6) {
    R0 <- p$al_R0_6pac
  } else {
    R0 <- p$al_R0_6pac + (nb_pac - 6) * p$al_R0_sup
  }

  # --- Taux de participation Tf (selon composition du ménage) ---
  if (nb_pac == 0) {
    if (situation == "isolé") {
      Tf <- p$al_TF_I0
    } else {
      Tf <- p$al_TF_C0
    }
  } else if (nb_pac == 1) {
    Tf <- p$al_TF_1pac
  } else if (nb_pac == 2) {
    Tf <- p$al_TF_2pac
  } else if (nb_pac == 3) {
    Tf <- p$al_TF_3pac
  } else if (nb_pac == 4) {
    Tf <- p$al_TF_4pac
  } else {
    Tf <- p$al_TF_4pac + (nb_pac - 4) * p$al_TF_sup_pac
  }

  # --- Taux complémentaire Tl (par zone) ---
  Tl <- p[[paste0("al_TL_", zone_str)]]

  # --- Ressources annuelles ---
  R <- ressources_mensuelles * 12

  # --- Participation personnelle ---
  # PP = p0 + Tf * (R - R0) / 12 + Tl * (L + C)
  # Si R < R0, le terme Tf est nul
  PP <- p$al_p0 + Tf * max(0, R - R0) / 12 + Tl * (L + C)

  # --- Aide = L + C - PP ---
  aide <- L + C - PP

  # --- Réduction forfaitaire de 5€ ---
  aide <- aide - p$al_reduction_5

  # --- Seuil de versement ---
  if (aide < p$al_seuil_versement) return(0)

  max(0, aide)
}
