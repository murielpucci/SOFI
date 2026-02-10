# =============================================================================
# SOFI - RSA et Prime d'Activité
# =============================================================================

#' Calcul du forfait logement
#' @param nb_personnes Nombre de personnes dans le foyer
#' @param est_locataire Logement locatif (TRUE/FALSE)
#' @param al Montant des aides au logement
#' @param type "rsa" ou "pa"
#' @param p Paramètres législatifs
#' @return Montant du forfait logement
calcul_forfait_logement <- function(nb_personnes, est_locataire, al, type = "rsa", p) {
  if (!est_locataire && al == 0) return(0)

  prefix <- if (type == "rsa") "rsa" else "pa"
  if (nb_personnes == 1) {
    fl <- p[[paste0(prefix, "_fl_1")]]
  } else if (nb_personnes == 2) {
    fl <- p[[paste0(prefix, "_fl_2")]]
  } else {
    fl <- p[[paste0(prefix, "_fl_3")]]
  }

  fl
}


#' Montant forfaitaire du RSA
#' @param situation "isolé" ou "couple"
#' @param nb_enfants Nombre d'enfants à charge
#' @param majoration_pi RSA majoré parent isolé (0 ou 1)
#' @param p Paramètres législatifs
#' @return Montant forfaitaire mensuel
montant_forfaitaire_rsa <- function(situation, nb_enfants, majoration_pi, p) {
  if (majoration_pi == 1 && situation == "isolé") {
    # RSA majoré
    if (nb_enfants == 0) return(p$rsa_M0)
    if (nb_enfants == 1) return(p$rsa_M1)
    return(p$rsa_M1 + (nb_enfants - 1) * p$rsa_M_sup)
  }

  if (situation == "isolé") {
    if (nb_enfants == 0) return(p$rsa_I0)
    if (nb_enfants == 1) return(p$rsa_I1)
    if (nb_enfants == 2) return(p$rsa_I2)
    return(p$rsa_I2 + (nb_enfants - 2) * p$rsa_I_sup)
  }

  # Couple

  if (nb_enfants == 0) return(p$rsa_C0)
  if (nb_enfants == 1) return(p$rsa_C1)
  if (nb_enfants == 2) return(p$rsa_C2)
  return(p$rsa_C2 + (nb_enfants - 2) * p$rsa_C_sup)
}


#' Calcul du RSA
#' @param situation "isolé" ou "couple"
#' @param nb_enfants Nombre d'enfants à charge RSA
#' @param majoration_pi RSA majoré parent isolé (0 ou 1)
#' @param revenus_activite Revenus d'activité mensuels du foyer
#' @param autres_ressources Autres ressources (pensions, etc.)
#' @param asf Montant ASF perçu
#' @param al Aides au logement
#' @param est_locataire TRUE/FALSE
#' @param pension_recue Pension alimentaire reçue
#' @param p Paramètres législatifs
#' @return Montant mensuel du RSA
calcul_rsa <- function(situation, nb_enfants, majoration_pi, revenus_activite,
                       autres_ressources, asf, al, est_locataire, pension_recue, p) {
  nb_personnes <- ifelse(situation == "couple", 2, 1) + nb_enfants

  # Montant forfaitaire
  mf <- montant_forfaitaire_rsa(situation, nb_enfants, majoration_pi, p)

  # Forfait logement
  fl <- calcul_forfait_logement(nb_personnes, est_locataire, al, "rsa", p)

  # Base de ressources RSA
  # RSA: revenus d'activité sont neutralisés (pas de cumul intégral),

  # mais 62% des revenus d'activité sont pris en compte après les 3 premiers mois
  base_ressources <- revenus_activite + autres_ressources + pension_recue

  # ASF prise en compte dans les ressources RSA (montant réduit)
  asf_rsa <- if (situation == "isolé") min(asf, p$asf_deduit_rsa * nb_enfants) else 0

  # RSA = montant forfaitaire - forfait logement - base ressources
  rsa <- mf - fl - base_ressources + asf_rsa

  # Seuil de versement
  if (rsa < p$rsa_seuil_versement) return(0)

  max(0, rsa)
}


#' Montant forfaitaire de la Prime d'Activité
montant_forfaitaire_pa <- function(situation, nb_enfants, p) {
  if (situation == "isolé") {
    if (nb_enfants == 0) return(p$pa_I0)
    if (nb_enfants == 1) return(p$pa_I1)
    if (nb_enfants == 2) return(p$pa_I2)
    return(p$pa_I2 + (nb_enfants - 2) * p$pa_I_sup)
  }

  # Couple
  if (nb_enfants == 0) return(p$pa_C0)
  if (nb_enfants == 1) return(p$pa_C1)
  if (nb_enfants == 2) return(p$pa_C2)
  return(p$pa_C2 + (nb_enfants - 2) * p$pa_C_sup)
}


#' Calcul du bonus individuel Prime d'Activité
#' @param revenu_activite Revenu d'activité individuel mensuel
#' @param p Paramètres législatifs
#' @return Montant du bonus individuel
calcul_bonus_pa <- function(revenu_activite, p) {
  if (revenu_activite <= p$pa_bonus_seuil) return(0)

  if (revenu_activite >= p$pa_bonus_plafond) return(p$pa_bonus_max)

  # Bonus proportionnel entre le seuil et le plafond
  p$pa_bonus_max * (revenu_activite - p$pa_bonus_seuil) / (p$pa_bonus_plafond - p$pa_bonus_seuil)
}


#' Calcul de la Prime d'Activité
#' @param situation "isolé" ou "couple"
#' @param nb_enfants Nombre d'enfants à charge
#' @param salaire1 Salaire mensuel adulte 1
#' @param salaire2 Salaire mensuel adulte 2
#' @param autres_ressources Autres ressources (hors activité)
#' @param al Aides au logement
#' @param est_locataire TRUE/FALSE
#' @param pension_recue Pension alimentaire reçue
#' @param p Paramètres législatifs
#' @return Montant mensuel de la Prime d'Activité
calcul_prime_activite <- function(situation, nb_enfants, salaire1, salaire2,
                                  autres_ressources, al, est_locataire, pension_recue, p) {
  nb_personnes <- ifelse(situation == "couple", 2, 1) + nb_enfants
  revenus_activite <- salaire1 + salaire2

  # Condition d'activité: au moins un revenu > 0
  if (revenus_activite == 0) return(0)

  # Montant forfaitaire
  mf <- montant_forfaitaire_pa(situation, nb_enfants, p)

  # Forfait logement PA
  fl <- calcul_forfait_logement(nb_personnes, est_locataire, al, "pa", p)

  # 61% des revenus d'activité
  part_activite <- p$pa_coeff_activite * revenus_activite

  # Bonus individuel
  bonus1 <- calcul_bonus_pa(salaire1, p)
  bonus2 <- if (situation == "couple") calcul_bonus_pa(salaire2, p) else 0

  # Revenu garanti
  revenu_garanti <- mf + part_activite + bonus1 + bonus2

  # Base de ressources
  base_ressources <- revenus_activite + autres_ressources + pension_recue + al

  # Si pas locataire et pas d'AL, pas de FL mais pas d'AL dans la base non plus
  if (!est_locataire && al == 0) {
    base_ressources_totale <- base_ressources
  } else {
    base_ressources_totale <- base_ressources
  }

  # Prime d'activité = revenu garanti - base ressources - forfait logement
  pa <- revenu_garanti - base_ressources_totale - fl

  # Application CRDS (0.5%)
  if (pa >= p$pa_seuil_versement) {
    return(pa * (1 - p$taux_CRDS))
  }

  0
}
