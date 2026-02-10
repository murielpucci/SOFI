# =============================================================================
# SOFI - Fonction principale de simulation
# =============================================================================

# Charger tous les modules
source_sofi <- function() {
  dir <- dirname(sys.frame(1)$ofile)
  if (is.null(dir)) dir <- "R"
  source(file.path(dir, "parametres_2025.R"), local = parent.frame())
  source(file.path(dir, "prestations_familiales.R"), local = parent.frame())
  source(file.path(dir, "paje.R"), local = parent.frame())
  source(file.path(dir, "cmg.R"), local = parent.frame())
  source(file.path(dir, "rsa_pa.R"), local = parent.frame())
  source(file.path(dir, "aides_logement.R"), local = parent.frame())
  source(file.path(dir, "irpp.R"), local = parent.frame())
  source(file.path(dir, "aah.R"), local = parent.frame())
  source(file.path(dir, "bourses.R"), local = parent.frame())
}


#' Simuler un cas type SOFI
#'
#' @param situation "isolé" ou "couple"
#' @param ages_enfants Vecteur des âges des enfants (-1 = pas d'enfant)
#' @param type_parent "pg" (parent gardien), "png" (non gardien), "solo", "ga_m", "ga_p"
#' @param salaire1 Salaire mensuel net adulte 1
#' @param salaire2 Salaire mensuel net adulte 2 (0 si isolé)
#' @param statut1 "salarié" ou "chomeur"
#' @param statut2 "salarié" ou "chomeur"
#' @param quotite1 Quotité de travail adulte 1 (0 à 1)
#' @param quotite2 Quotité de travail adulte 2
#' @param are1 Allocation chômage mensuelle adulte 1 (si chômeur)
#' @param are2 Allocation chômage mensuelle adulte 2
#' @param droit_asf 0 ou 1
#' @param majoration_rsa 0 ou 1 (parent isolé majoré)
#' @param est_locataire TRUE/FALSE
#' @param zone Zone logement (1, 2, 3)
#' @param loyer Loyer mensuel (ou "plaf")
#' @param droit_aah1 0 ou 1
#' @param droit_aah2 0 ou 1
#' @param heures_am Heures mensuelles assistante maternelle
#' @param heures_dom Heures mensuelles garde à domicile
#' @param heures_creche Heures mensuelles crèche
#' @param quotite_prepare Quotité PREPARE (0, 0.5, 0.8, 1)
#' @param pension_recue Pension alimentaire reçue par mois
#' @param pension_payee Pension alimentaire payée par mois
#' @param nb_enfants_sup Nombre d'enfants dans le supérieur (18+)
#' @param p Paramètres législatifs (par défaut: 2025)
#' @return Liste avec tous les résultats
simuler_cas <- function(
  situation = "isolé",
  ages_enfants = c(-1, -1, -1, -1, -1, -1),
  type_parent = "solo",
  salaire1 = 0,
  salaire2 = 0,
  statut1 = "salarié",
  statut2 = "salarié",
  quotite1 = NULL,
  quotite2 = NULL,
  are1 = 0,
  are2 = 0,
  droit_asf = 0,
  majoration_rsa = 0,
  est_locataire = TRUE,
  zone = 2,
  loyer = "plaf",
  droit_aah1 = 0,
  droit_aah2 = 0,
  heures_am = 0,
  heures_dom = 0,
  heures_creche = 0,
  quotite_prepare = 1,
  pension_recue = 0,
  pension_payee = 0,
  nb_enfants_sup = 0,
  p = NULL
) {
  # Paramètres par défaut
  if (is.null(p)) p <- parametres_2025()

  # Nettoyage
  ages <- ages_enfants[ages_enfants >= 0]
  nb_enfants <- length(ages)

  # Quotité de travail
  if (is.null(quotite1)) quotite1 <- min(1, salaire1 / p$smic_net)
  if (is.null(quotite2)) quotite2 <- min(1, salaire2 / p$smic_net)

  # Nb de revenus d'activité dans le couple

  nb_revenus <- (salaire1 > 0) + (situation == "couple" & salaire2 > 0)

  # Revenus d'activité
  rev_salariaux <- salaire1 + ifelse(situation == "couple", salaire2, 0)
  rev_chomage <- ifelse(statut1 == "chomeur", are1, 0) +
    ifelse(situation == "couple" & statut2 == "chomeur", are2, 0)
  revenus_primaires <- rev_salariaux + rev_chomage

  # =========================================================================
  # Base ressources PF (n-2, approximée par revenus actuels * 12)
  # =========================================================================
  ressources_pf <- revenus_primaires * 12

  # =========================================================================
  # PRESTATIONS FAMILIALES
  # =========================================================================
  af <- calcul_af(ages_enfants, ressources_pf, type_parent, p)
  asf <- calcul_asf(droit_asf, situation, nb_enfants, pension_recue, type_parent, p)
  ars_annuel <- calcul_ars(ages_enfants, ressources_pf, type_parent, p)
  ars <- ars_annuel / 12
  cf_result <- calcul_cf(ages_enfants, situation, nb_revenus, ressources_pf, type_parent, p)
  cf <- cf_result

  # =========================================================================
  # PAJE
  # =========================================================================
  ab <- calcul_ab(ages_enfants, situation, nb_revenus, ressources_pf, type_parent, p)
  prepare <- calcul_prepare(ages_enfants, quotite1, quotite_prepare, type_parent, p)

  # =========================================================================
  # CMG
  # =========================================================================
  cmg_result <- calcul_cmg(ages_enfants, revenus_primaires, heures_am, heures_dom,
                           type_parent = type_parent, p = p)
  cmg_cotis <- cmg_result$cmg_cotis
  cmg_prestation <- cmg_result$cmg_prestation

  total_pf <- af + cf + asf + ars + ab + prepare

  # =========================================================================
  # AIDES AU LOGEMENT
  # =========================================================================
  al <- calcul_al(situation, nb_enfants, est_locataire, zone, loyer,
                  revenus_primaires, p)

  # =========================================================================
  # BOURSES
  # =========================================================================
  bourse_college_an <- calcul_bourse_college(ages_enfants, ressources_pf, nb_enfants, type_parent, p)
  bourse_lycee_an <- calcul_bourse_lycee(ages_enfants, ressources_pf, nb_enfants, type_parent, p)
  bourse_sup_an <- calcul_bourse_superieur(nb_enfants_sup, ressources_pf, 0, type_parent, p)
  bourse_college <- bourse_college_an / 12
  bourse_lycee <- bourse_lycee_an / 12
  bourse_sup <- bourse_sup_an / 12
  total_bourses <- bourse_college + bourse_lycee + bourse_sup

  # =========================================================================
  # AAH
  # =========================================================================
  aah1 <- calcul_aah(droit_aah1, nb_enfants, salaire1 * 12, 0, p)
  aah2 <- if (situation == "couple") calcul_aah(droit_aah2, nb_enfants, salaire2 * 12, 0, p) else 0
  aah_total <- aah1 + aah2

  # =========================================================================
  # RSA
  # =========================================================================
  autres_ress_rsa <- rev_chomage + af + cf + asf
  rsa <- calcul_rsa(situation, nb_enfants, majoration_rsa, rev_salariaux,
                    rev_chomage, asf, al, est_locataire, pension_recue, p)

  # =========================================================================
  # PRIME D'ACTIVITÉ
  # =========================================================================
  pa <- calcul_prime_activite(situation, nb_enfants, salaire1,
                              ifelse(situation == "couple", salaire2, 0),
                              rev_chomage, al, est_locataire, pension_recue, p)

  # =========================================================================
  # TOTAL PRESTATIONS
  # =========================================================================
  total_prestations <- total_pf + al + total_bourses + aah_total + rsa + pa

  # =========================================================================
  # IRPP
  # =========================================================================
  sal_annuel_imposable_1 <- salaire1 * 12 * p$coeff_revenu_imposable / 12
  sal_annuel_imposable_2 <- salaire2 * 12 * p$coeff_revenu_imposable / 12

  irpp_annuel <- calcul_irpp(
    situation = situation,
    nb_enfants = nb_enfants,
    salaire_annuel_1 = sal_annuel_imposable_1,
    salaire_annuel_2 = sal_annuel_imposable_2,
    pension_payee_annuelle = pension_payee * 12,
    ages_enfants = ages_enfants,
    frais_garde_annuels = 0,  # simplifié
    parent_isole = (situation == "isolé" & nb_enfants > 0),
    p = p
  )
  irpp_mensuel <- irpp_annuel / 12

  # =========================================================================
  # REVENU DISPONIBLE
  # =========================================================================
  revenu_disponible <- revenus_primaires + total_prestations - irpp_mensuel +
    pension_recue - pension_payee

  # =========================================================================
  # NIVEAUX DE VIE
  # =========================================================================
  # UC selon le type de parent
  uc <- 1 + ifelse(situation == "couple", p$uc_conjoint, 0)
  if (type_parent %in% c("pg", "solo")) {
    uc <- uc + sum(ages < 14) * p$uc_enfant + sum(ages >= 14) * p$uc_ado
  } else if (type_parent == "png") {
    uc <- uc + sum(ages < 14) * p$uc_enfant_png + sum(ages >= 14) * p$uc_ado_png
  } else if (type_parent %in% c("ga_m", "ga_p")) {
    uc <- uc + sum(ages < 14) * p$uc_enfant_ga + sum(ages >= 14) * p$uc_ado_ga
  } else {
    uc <- uc + sum(ages < 14) * p$uc_enfant + sum(ages >= 14) * p$uc_ado
  }

  niveau_de_vie <- revenu_disponible / uc

  # =========================================================================
  # RÉSULTATS
  # =========================================================================
  list(
    # Revenus primaires
    salaires = rev_salariaux,
    chomage = rev_chomage,
    revenus_primaires = revenus_primaires,

    # Prestations familiales
    af = af,
    cf = cf,
    asf = asf,
    ars = ars,
    ab = ab,
    prepare = prepare,
    cmg_cotis = cmg_cotis,
    cmg_prestation = cmg_prestation,
    total_pf = total_pf,

    # Aides au logement
    al = al,

    # Bourses
    bourse_college = bourse_college,
    bourse_lycee = bourse_lycee,
    bourse_sup = bourse_sup,
    total_bourses = total_bourses,

    # Minima sociaux
    aah = aah_total,
    rsa = rsa,
    prime_activite = pa,

    # Total prestations
    total_prestations = total_prestations,

    # Impôt
    irpp_mensuel = irpp_mensuel,
    irpp_annuel = irpp_annuel,

    # Pensions alimentaires
    pension_recue = pension_recue,
    pension_payee = pension_payee,

    # Revenu disponible
    revenu_disponible = revenu_disponible,

    # Niveau de vie
    uc = uc,
    niveau_de_vie = niveau_de_vie,
    sous_seuil_pauvrete = (niveau_de_vie < p$seuil_pauvrete)
  )
}
