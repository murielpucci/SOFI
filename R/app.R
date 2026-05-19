# =============================================================================
# SOFI - Application Shiny
# Simulateur socio-fiscal français — avec mode Réforme
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(scales)
library(tidyr)

# Charger les modules SOFI
source("R/parametres_2025.R")
source("R/prestations_familiales.R")
source("R/paje.R")
source("R/cmg.R")
source("R/rsa_pa.R")
source("R/aides_logement.R")
source("R/irpp.R")
source("R/aah.R")
source("R/bourses.R")
source("R/simuler_cas.R")

# Paramètres de référence
p_ref <- parametres_2025()

# Catégories de paramètres modifiables pour la réforme
reforme_categories <- list(
  "RSA" = list(
    "Montant forfaitaire isolé 0 enfant (rsa_I0)" = "rsa_I0",
    "Montant forfaitaire isolé 1 enfant (rsa_I1)" = "rsa_I1",
    "Montant forfaitaire isolé 2 enfants (rsa_I2)" = "rsa_I2",
    "Montant forfaitaire couple 0 enfant (rsa_C0)" = "rsa_C0",
    "Montant forfaitaire couple 1 enfant (rsa_C1)" = "rsa_C1",
    "Montant forfaitaire couple 2 enfants (rsa_C2)" = "rsa_C2",
    "Supplément par enfant (rsa_I_sup)" = "rsa_I_sup",
    "Forfait logement 1 pers (rsa_fl_1)" = "rsa_fl_1",
    "Forfait logement 2 pers (rsa_fl_2)" = "rsa_fl_2",
    "Forfait logement 3+ pers (rsa_fl_3)" = "rsa_fl_3"
  ),
  "Prime d'activité" = list(
    "Montant forfaitaire isolé 0 enfant (pa_I0)" = "pa_I0",
    "Montant forfaitaire isolé 1 enfant (pa_I1)" = "pa_I1",
    "Montant forfaitaire couple 0 enfant (pa_C0)" = "pa_C0",
    "Coefficient revenus d'activité (pa_coeff_activite)" = "pa_coeff_activite",
    "Seuil bonus (pa_bonus_seuil)" = "pa_bonus_seuil",
    "Plafond bonus (pa_bonus_plafond)" = "pa_bonus_plafond",
    "Montant max bonus (pa_bonus_max)" = "pa_bonus_max"
  ),
  "Allocations familiales" = list(
    "AF 2 enfants (af_2)" = "af_2",
    "AF 3 enfants (af_3)" = "af_3",
    "Majoration >14 ans (af_majo_14)" = "af_majo_14",
    "Plafond tranche 1 (af_plaf_tranche1)" = "af_plaf_tranche1",
    "Plafond tranche 2 (af_plaf_tranche2)" = "af_plaf_tranche2",
    "BMAF (bmaf)" = "bmaf"
  ),
  "Complément familial" = list(
    "Montant CF (cf)" = "cf",
    "Majoration CF (cf_majo)" = "cf_majo",
    "Plafond couple monoactif (cf_plaf_couple_1rev)" = "cf_plaf_couple_1rev",
    "Plafond biactif/isolé (cf_plaf_couple_2rev)" = "cf_plaf_couple_2rev"
  ),
  "ASF" = list(
    "Montant ASF (asf)" = "asf"
  ),
  "ARS" = list(
    "ARS 6-10 ans (ars_6_10)" = "ars_6_10",
    "ARS 11-14 ans (ars_11_14)" = "ars_11_14",
    "ARS 15-18 ans (ars_15_18)" = "ars_15_18",
    "Plafond base (ars_plaf_base)" = "ars_plaf_base"
  ),
  "PAJE" = list(
    "AB taux plein (ab_taux_plein)" = "ab_taux_plein",
    "AB taux réduit (ab_taux_reduit)" = "ab_taux_reduit",
    "PREPARE taux plein (prepare_taux_plein)" = "prepare_taux_plein"
  ),
  "AAH" = list(
    "Montant AAH (aah)" = "aah",
    "Plafond isolé (aah_plafond_isole)" = "aah_plafond_isole",
    "Taux abattement bas (aah_abattement_bas)" = "aah_abattement_bas",
    "Taux abattement haut (aah_abattement_haut)" = "aah_abattement_haut"
  ),
  "Aides au logement" = list(
    "Participation minimale p0 (al_p0)" = "al_p0",
    "Réduction forfaitaire (al_reduction_5)" = "al_reduction_5",
    "Seuil versement (al_seuil_versement)" = "al_seuil_versement"
  ),
  "Impôt sur le revenu" = list(
    "Seuil tranche 1 (irpp_tranche1)" = "irpp_tranche1",
    "Seuil tranche 2 (irpp_tranche2)" = "irpp_tranche2",
    "Taux tranche 1 (irpp_taux1)" = "irpp_taux1",
    "Taux tranche 2 (irpp_taux2)" = "irpp_taux2",
    "Décote seuil isolé (irpp_decote_seuil_I)" = "irpp_decote_seuil_I",
    "Décote seuil couple (irpp_decote_seuil_C)" = "irpp_decote_seuil_C",
    "QF plafond parent isolé (irpp_qf_plafond_pi)" = "irpp_qf_plafond_pi",
    "QF plafond couple (irpp_qf_plafond)" = "irpp_qf_plafond"
  )
)

# Liste plate de tous les paramètres modifiables
all_reforme_params <- unlist(reforme_categories, use.names = FALSE)

# =============================================================================
# UI
# =============================================================================
ui <- page_sidebar(
  title = "SOFI - Simulateur socio-fiscal",
  theme = bs_theme(bootswatch = "flatly", version = 5),

  sidebar = sidebar(
    width = 380,
    title = "Paramètres du ménage",

    accordion(
      open = TRUE,
      # --- Situation familiale ---
      accordion_panel(
        "Situation familiale",
        icon = bsicons::bs_icon("people-fill"),
        selectInput("situation", "Situation maritale",
                    choices = c("Isolé" = "isolé", "Couple" = "couple")),
        selectInput("type_parent", "Type de parent",
                    choices = c("Solo" = "solo",
                                "Parent gardien" = "pg",
                                "Parent non gardien" = "png",
                                "Garde alternée (mère)" = "ga_m",
                                "Garde alternée (père)" = "ga_p")),
        numericInput("nb_enfants", "Nombre d'enfants", value = 1, min = 0, max = 6),
        conditionalPanel("input.nb_enfants >= 1",
                         numericInput("age1", "Âge enfant 1", value = 6, min = 0, max = 25)),
        conditionalPanel("input.nb_enfants >= 2",
                         numericInput("age2", "Âge enfant 2", value = -1, min = 0, max = 25)),
        conditionalPanel("input.nb_enfants >= 3",
                         numericInput("age3", "Âge enfant 3", value = -1, min = 0, max = 25)),
        conditionalPanel("input.nb_enfants >= 4",
                         numericInput("age4", "Âge enfant 4", value = -1, min = 0, max = 25)),
        conditionalPanel("input.nb_enfants >= 5",
                         numericInput("age5", "Âge enfant 5", value = -1, min = 0, max = 25)),
        conditionalPanel("input.nb_enfants >= 6",
                         numericInput("age6", "Âge enfant 6", value = -1, min = 0, max = 25)),
        numericInput("nb_enfants_sup", "Enfants 18+ dans le supérieur", value = 0, min = 0, max = 6)
      ),

      # --- Revenus ---
      accordion_panel(
        "Revenus",
        icon = bsicons::bs_icon("cash-stack"),
        selectInput("statut1", "Statut adulte 1",
                    choices = c("Salarié" = "salarié", "Chômeur" = "chomeur")),
        numericInput("salaire1", "Salaire net mensuel adulte 1 (€)", value = 600, min = 0, step = 50),
        conditionalPanel(
          "input.statut1 == 'chomeur'",
          numericInput("are1", "ARE mensuelle adulte 1 (€)", value = 0, min = 0, step = 50)
        ),
        conditionalPanel(
          "input.situation == 'couple'",
          selectInput("statut2", "Statut adulte 2",
                      choices = c("Salarié" = "salarié", "Chômeur" = "chomeur")),
          numericInput("salaire2", "Salaire net mensuel adulte 2 (€)", value = 0, min = 0, step = 50),
          conditionalPanel(
            "input.statut2 == 'chomeur'",
            numericInput("are2", "ARE mensuelle adulte 2 (€)", value = 0, min = 0, step = 50)
          )
        ),
        numericInput("pension_recue", "Pension alimentaire reçue (€/mois)", value = 0, min = 0),
        numericInput("pension_payee", "Pension alimentaire payée (€/mois)", value = 0, min = 0)
      ),

      # --- Logement ---
      accordion_panel(
        "Logement",
        icon = bsicons::bs_icon("house-fill"),
        selectInput("logement", "Type de logement",
                    choices = c("Locataire" = "locataire", "Propriétaire" = "proprietaire")),
        conditionalPanel(
          "input.logement == 'locataire'",
          selectInput("zone", "Zone géographique",
                      choices = c("Zone 1 (Paris et agglo)" = "1",
                                  "Zone 2 (Grandes villes)" = "2",
                                  "Zone 3 (Reste)" = "3"),
                      selected = "2"),
          checkboxInput("loyer_plaf", "Loyer au plafond", value = TRUE),
          conditionalPanel(
            "!input.loyer_plaf",
            numericInput("loyer", "Loyer mensuel (€)", value = 500, min = 0, step = 10)
          )
        )
      ),

      # --- Garde d'enfants ---
      accordion_panel(
        "Garde d'enfants",
        icon = bsicons::bs_icon("emoji-smile"),
        numericInput("heures_am", "Heures/mois assistante maternelle", value = 0, min = 0),
        numericInput("heures_dom", "Heures/mois garde à domicile", value = 0, min = 0),
        numericInput("heures_creche", "Heures/mois crèche", value = 0, min = 0),
        selectInput("quotite_prepare", "PREPARE",
                    choices = c("Non" = "0", "Temps plein" = "1",
                                "Mi-temps" = "0.5", "80%" = "0.8"))
      ),

      # --- Droits spécifiques ---
      accordion_panel(
        "Droits spécifiques",
        icon = bsicons::bs_icon("shield-check"),
        checkboxInput("droit_asf", "Droit à l'ASF", value = FALSE),
        checkboxInput("majoration_rsa", "RSA majoré (parent isolé)", value = FALSE),
        checkboxInput("droit_aah1", "AAH adulte 1", value = FALSE),
        conditionalPanel(
          "input.situation == 'couple'",
          checkboxInput("droit_aah2", "AAH adulte 2", value = FALSE)
        )
      )
    ),

    hr(),
    actionButton("simuler", "Simuler", class = "btn-primary btn-lg w-100",
                 icon = icon("calculator"))
  ),

  # --- Contenu principal ---
  layout_columns(
    col_widths = 12,

    # Bandeau Réforme
    card(
      card_header(
        class = "bg-warning text-dark d-flex align-items-center justify-content-between",
        span(bsicons::bs_icon("gear-fill"), " Mode Réforme"),
        checkboxInput("activer_reforme", "Activer la simulation de réforme",
                      value = FALSE, width = "auto")
      ),
      conditionalPanel(
        "input.activer_reforme",
        layout_columns(
          col_widths = c(4, 8),
          div(
            selectInput("reforme_categorie", "Catégorie",
                        choices = names(reforme_categories)),
            uiOutput("reforme_param_ui"),
            div(class = "d-flex gap-2",
                actionButton("ajouter_reforme", "Ajouter", class = "btn-warning btn-sm",
                             icon = icon("plus")),
                actionButton("reset_reforme", "Tout réinitialiser", class = "btn-outline-danger btn-sm",
                             icon = icon("trash"))
            )
          ),
          div(
            tags$strong("Paramètres modifiés :"),
            DTOutput("table_reformes", height = "200px"),
            tags$small(class = "text-muted",
                       "Les graphiques et tableaux compareront automatiquement ",
                       tags$span(class = "text-primary fw-bold", "Actuel"),
                       " vs ",
                       tags$span(class = "text-danger fw-bold", "Réforme"))
          )
        )
      )
    ),

    # Onglets de résultats
    navset_card_tab(
      # === ONGLET RÉSULTATS ===
      nav_panel(
        "Résultats",
        icon = bsicons::bs_icon("bar-chart-fill"),
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title = "Revenu disponible",
            value = uiOutput("revenu_dispo_vb"),
            showcase = bsicons::bs_icon("wallet2"),
            theme = "primary"
          ),
          value_box(
            title = "Niveau de vie",
            value = uiOutput("niveau_vie_vb"),
            showcase = bsicons::bs_icon("graph-up"),
            theme = "success"
          ),
          value_box(
            title = "Impact de la réforme",
            value = uiOutput("impact_vb"),
            showcase = bsicons::bs_icon("arrow-left-right"),
            theme = "warning"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Décomposition du revenu disponible"),
            plotOutput("plot_decomposition", height = "450px")
          ),
          card(
            card_header("Détail des prestations et prélèvements"),
            DTOutput("table_detail")
          )
        )
      ),

      # === ONGLET VARIATION ===
      nav_panel(
        "Variation selon le salaire",
        icon = bsicons::bs_icon("graph-up-arrow"),
        card(
          card_header("Revenu disponible en fonction du salaire (adulte 1)"),
          layout_sidebar(
            sidebar = sidebar(
              position = "right", width = 200,
              numericInput("sal_min", "Salaire min (€)", value = 0, min = 0),
              numericInput("sal_max", "Salaire max (€)", value = 3000, min = 100),
              numericInput("sal_pas", "Pas (€)", value = 50, min = 10)
            ),
            plotOutput("plot_variation", height = "500px")
          )
        )
      ),

      # === ONGLET DÉCOMPOSITION EMPILÉE ===
      nav_panel(
        "Décomposition empilée",
        icon = bsicons::bs_icon("stack"),
        card(
          card_header("Décomposition par composante selon le salaire"),
          plotOutput("plot_empile", height = "500px")
        )
      ),

      # === ONGLET GAINS/PERTES ===
      nav_panel(
        "Gains / Pertes",
        icon = bsicons::bs_icon("arrow-left-right"),
        conditionalPanel(
          "input.activer_reforme",
          card(
            card_header("Impact de la réforme selon le salaire"),
            plotOutput("plot_impact", height = "500px")
          ),
          card(
            card_header("Détail des gains/pertes par prestation"),
            plotOutput("plot_impact_detail", height = "500px")
          )
        ),
        conditionalPanel(
          "!input.activer_reforme",
          card(
            card_body(
              class = "text-center text-muted py-5",
              h4("Activez le mode Réforme pour voir les gains et pertes"),
              p("Cochez la case en haut de la page et modifiez des paramètres législatifs.")
            )
          )
        )
      ),

      # === ONGLET TABLEAU COMPLET ===
      nav_panel(
        "Tableau complet",
        icon = bsicons::bs_icon("table"),
        card(
          card_header("Résultats détaillés de la simulation"),
          DTOutput("table_complet")
        )
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # Stocker les modifications de réforme
  reformes <- reactiveVal(data.frame(
    parametre = character(0),
    nom = character(0),
    valeur_actuelle = numeric(0),
    nouvelle_valeur = numeric(0),
    stringsAsFactors = FALSE
  ))

  # UI dynamique pour le paramètre sélectionné
  output$reforme_param_ui <- renderUI({
    cat <- input$reforme_categorie
    params <- reforme_categories[[cat]]
    param_names <- names(params)
    param_keys <- unlist(params)

    selected_key <- param_keys[1]
    val_actuelle <- p_ref[[selected_key]]

    tagList(
      selectInput("reforme_param", "Paramètre à modifier",
                  choices = setNames(param_keys, param_names)),
      numericInput("reforme_nouvelle_valeur", "Nouvelle valeur",
                   value = round(val_actuelle, 4), step = 0.01)
    )
  })

  # Mettre à jour la valeur par défaut quand on change de paramètre
  observeEvent(input$reforme_param, {
    req(input$reforme_param)
    val <- p_ref[[input$reforme_param]]
    if (!is.null(val) && length(val) == 1) {
      updateNumericInput(session, "reforme_nouvelle_valeur",
                         value = round(val, 4))
    }
  })

  # Ajouter une modification
  observeEvent(input$ajouter_reforme, {
    req(input$reforme_param, input$reforme_nouvelle_valeur)
    param_key <- input$reforme_param
    val_actuelle <- p_ref[[param_key]]
    nouvelle_val <- input$reforme_nouvelle_valeur

    # Trouver le nom lisible
    nom_lisible <- param_key
    for (cat in reforme_categories) {
      idx <- match(param_key, unlist(cat))
      if (!is.na(idx)) {
        nom_lisible <- names(cat)[idx]
        break
      }
    }

    df <- reformes()
    # Remplacer si le paramètre existe déjà
    df <- df[df$parametre != param_key, ]
    df <- rbind(df, data.frame(
      parametre = param_key,
      nom = nom_lisible,
      valeur_actuelle = val_actuelle,
      nouvelle_valeur = nouvelle_val,
      stringsAsFactors = FALSE
    ))
    reformes(df)
  })

  # Réinitialiser
  observeEvent(input$reset_reforme, {
    reformes(data.frame(
      parametre = character(0), nom = character(0),
      valeur_actuelle = numeric(0), nouvelle_valeur = numeric(0),
      stringsAsFactors = FALSE
    ))
  })

  # Tableau des réformes
  output$table_reformes <- renderDT({
    df <- reformes()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "Aucune modification"),
                       options = list(dom = 't'), rownames = FALSE))
    }
    df$variation <- paste0(ifelse(df$nouvelle_valeur >= df$valeur_actuelle, "+", ""),
                           round(df$nouvelle_valeur - df$valeur_actuelle, 2))
    df_show <- df[, c("nom", "valeur_actuelle", "nouvelle_valeur", "variation")]
    names(df_show) <- c("Paramètre", "Actuel", "Réforme", "Δ")
    datatable(df_show, options = list(dom = 't', pageLength = 20, ordering = FALSE),
              rownames = FALSE, selection = "none") |>
      formatRound(columns = c("Actuel", "Réforme"), digits = 2)
  })

  # Construire les paramètres réformés
  get_params_reforme <- function() {
    p <- p_ref
    df <- reformes()
    if (nrow(df) > 0) {
      for (i in 1:nrow(df)) {
        p[[df$parametre[i]]] <- df$nouvelle_valeur[i]
        # Recalculer cf_majore si cf ou cf_majo changent
        if (df$parametre[i] %in% c("cf", "cf_majo")) {
          p$cf_majore <- p$cf + p$cf_majo
        }
      }
    }
    p
  }

  # Construction du vecteur d'âges
  get_ages <- function() {
    ages <- rep(-1, 6)
    nb <- input$nb_enfants
    if (nb >= 1) ages[1] <- input$age1
    if (nb >= 2) ages[2] <- input$age2
    if (nb >= 3) ages[3] <- input$age3
    if (nb >= 4) ages[4] <- input$age4
    if (nb >= 5) ages[5] <- input$age5
    if (nb >= 6) ages[6] <- input$age6
    ages
  }

  # Fonction de simulation avec un jeu de paramètres donné
  run_simulation <- function(p) {
    simuler_cas(
      situation = input$situation,
      ages_enfants = get_ages(),
      type_parent = input$type_parent,
      salaire1 = input$salaire1,
      salaire2 = ifelse(input$situation == "couple", input$salaire2, 0),
      statut1 = input$statut1,
      statut2 = ifelse(input$situation == "couple", input$statut2, "salarié"),
      are1 = ifelse(input$statut1 == "chomeur", input$are1, 0),
      are2 = ifelse(input$situation == "couple" && input$statut2 == "chomeur", input$are2, 0),
      droit_asf = as.numeric(input$droit_asf),
      majoration_rsa = as.numeric(input$majoration_rsa),
      est_locataire = (input$logement == "locataire"),
      zone = as.numeric(input$zone),
      loyer = ifelse(input$loyer_plaf, "plaf", input$loyer),
      droit_aah1 = as.numeric(input$droit_aah1),
      droit_aah2 = ifelse(input$situation == "couple", as.numeric(input$droit_aah2), 0),
      heures_am = input$heures_am,
      heures_dom = input$heures_dom,
      heures_creche = input$heures_creche,
      quotite_prepare = as.numeric(input$quotite_prepare),
      pension_recue = input$pension_recue,
      pension_payee = input$pension_payee,
      nb_enfants_sup = input$nb_enfants_sup,
      p = p
    )
  }

  # Simulations réactives
  resultat_actuel <- eventReactive(input$simuler, { run_simulation(p_ref) })
  resultat_reforme <- eventReactive(input$simuler, {
    if (input$activer_reforme && nrow(reformes()) > 0) {
      run_simulation(get_params_reforme())
    } else {
      NULL
    }
  })

  # ===================== VALUE BOXES =====================
  output$revenu_dispo_vb <- renderUI({
    r <- resultat_actuel()
    rr <- resultat_reforme()
    if (!is.null(rr)) {
      tagList(
        div(paste0(format(round(r$revenu_disponible), big.mark = " "), " €"),
            style = "font-size: 0.8em; color: #666;"),
        div(paste0("→ ", format(round(rr$revenu_disponible), big.mark = " "), " €"),
            style = "font-weight: bold;")
      )
    } else {
      div(paste0(format(round(r$revenu_disponible), big.mark = " "), " €/mois"))
    }
  })

  output$niveau_vie_vb <- renderUI({
    r <- resultat_actuel()
    rr <- resultat_reforme()
    if (!is.null(rr)) {
      tagList(
        div(paste0(format(round(r$niveau_de_vie), big.mark = " "), " €"),
            style = "font-size: 0.8em; color: #666;"),
        div(paste0("→ ", format(round(rr$niveau_de_vie), big.mark = " "), " €"),
            style = "font-weight: bold;")
      )
    } else {
      div(paste0(format(round(r$niveau_de_vie), big.mark = " "), " €/mois"))
    }
  })

  output$impact_vb <- renderUI({
    rr <- resultat_reforme()
    r <- resultat_actuel()
    if (!is.null(rr)) {
      diff <- rr$revenu_disponible - r$revenu_disponible
      couleur <- if (diff > 0) "color: #27ae60;" else if (diff < 0) "color: #e74c3c;" else "color: #666;"
      signe <- if (diff > 0) "+" else ""
      div(
        paste0(signe, format(round(diff), big.mark = " "), " €/mois"),
        style = paste0("font-weight: bold; ", couleur)
      )
    } else {
      div("—", style = "color: #999;")
    }
  })

  # ===================== GRAPHIQUE DECOMPOSITION =====================
  output$plot_decomposition <- renderPlot({
    r <- resultat_actuel()
    rr <- resultat_reforme()

    make_df <- function(res, scenario) {
      data.frame(
        Composante = c("Salaires", "Chômage", "AF", "CF", "ASF", "ARS",
                       "AB PAJE", "PREPARE", "AL", "Bourses", "AAH",
                       "RSA", "Prime activité", "Pension reçue",
                       "Impôt (-)", "Pension payée (-)"),
        Montant = c(res$salaires, res$chomage, res$af, res$cf, res$asf, res$ars,
                    res$ab, res$prepare, res$al, res$total_bourses, res$aah,
                    res$rsa, res$prime_activite, res$pension_recue,
                    -res$irpp_mensuel, -res$pension_payee),
        Scenario = scenario,
        stringsAsFactors = FALSE
      )
    }

    df <- make_df(r, "Actuel")
    if (!is.null(rr)) {
      df <- rbind(df, make_df(rr, "Réforme"))
    }

    df <- df[df$Montant != 0 | (!is.null(rr) & df$Composante %in%
               df$Composante[df$Montant != 0]), ]
    # Garder les composantes qui ont au moins un montant non nul
    composantes_actives <- unique(df$Composante[df$Montant != 0])
    df <- df[df$Composante %in% composantes_actives, ]

    df$Composante <- factor(df$Composante, levels = rev(unique(df$Composante)))

    if (!is.null(rr)) {
      ggplot(df, aes(x = Composante, y = Montant, fill = Scenario)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        coord_flip() +
        scale_fill_manual(values = c("Actuel" = "#3498db", "Réforme" = "#e74c3c")) +
        scale_y_continuous(labels = label_number(suffix = " €")) +
        labs(x = NULL, y = "€/mois", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    } else {
      df$Type <- ifelse(df$Montant >= 0, "Positif", "Négatif")
      ggplot(df, aes(x = Composante, y = Montant, fill = Type)) +
        geom_col(width = 0.6) +
        coord_flip() +
        scale_fill_manual(values = c("Positif" = "#27ae60", "Négatif" = "#e74c3c")) +
        scale_y_continuous(labels = label_number(suffix = " €")) +
        labs(x = NULL, y = "€/mois", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none")
    }
  })

  # ===================== TABLEAU DÉTAILLÉ =====================
  output$table_detail <- renderDT({
    r <- resultat_actuel()
    rr <- resultat_reforme()

    postes <- c("Salaires", "Allocations chômage", "Revenus primaires",
                "---", "Allocations familiales", "Complément familial",
                "ASF", "ARS", "AB PAJE", "PREPARE", "CMG prestation",
                "Total PF", "---",
                "Aides au logement", "Bourses", "AAH", "RSA",
                "Prime d'activité", "Total prestations",
                "---", "Impôt sur le revenu",
                "---", "REVENU DISPONIBLE", "Niveau de vie")

    vals_actuel <- c(
      r$salaires, r$chomage, r$revenus_primaires,
      NA, r$af, r$cf, r$asf, r$ars, r$ab, r$prepare, r$cmg_prestation,
      r$total_pf, NA,
      r$al, r$total_bourses, r$aah, r$rsa,
      r$prime_activite, r$total_prestations,
      NA, r$irpp_mensuel,
      NA, r$revenu_disponible, r$niveau_de_vie
    )

    if (!is.null(rr)) {
      vals_reforme <- c(
        rr$salaires, rr$chomage, rr$revenus_primaires,
        NA, rr$af, rr$cf, rr$asf, rr$ars, rr$ab, rr$prepare, rr$cmg_prestation,
        rr$total_pf, NA,
        rr$al, rr$total_bourses, rr$aah, rr$rsa,
        rr$prime_activite, rr$total_prestations,
        NA, rr$irpp_mensuel,
        NA, rr$revenu_disponible, rr$niveau_de_vie
      )
      diff <- ifelse(is.na(vals_actuel), NA, vals_reforme - vals_actuel)

      df <- data.frame(
        Poste = postes,
        Actuel = ifelse(is.na(vals_actuel), "", format(round(vals_actuel, 2), big.mark = " ", nsmall = 2)),
        Réforme = ifelse(is.na(vals_reforme), "", format(round(vals_reforme, 2), big.mark = " ", nsmall = 2)),
        Δ = ifelse(is.na(diff) | diff == 0, "",
                   paste0(ifelse(diff > 0, "+", ""), format(round(diff, 2), big.mark = " ", nsmall = 2))),
        check.names = FALSE
      )
    } else {
      df <- data.frame(
        Poste = postes,
        `Mensuel (€)` = ifelse(is.na(vals_actuel), "",
                               format(round(vals_actuel, 2), big.mark = " ", nsmall = 2)),
        check.names = FALSE
      )
    }

    datatable(df, options = list(dom = 't', pageLength = 30, ordering = FALSE),
              rownames = FALSE) |>
      formatStyle("Poste", fontWeight = styleEqual(
        c("Revenus primaires", "Total PF", "Total prestations",
          "REVENU DISPONIBLE", "Niveau de vie"),
        rep("bold", 5)
      ))
  })

  # ===================== VARIATION SELON LE SALAIRE =====================
  run_variation <- function(p) {
    salaires <- seq(input$sal_min, input$sal_max, by = input$sal_pas)
    resultats <- lapply(salaires, function(s) {
      res <- simuler_cas(
        situation = input$situation, ages_enfants = get_ages(),
        type_parent = input$type_parent, salaire1 = s,
        salaire2 = ifelse(input$situation == "couple", input$salaire2, 0),
        statut1 = input$statut1,
        statut2 = ifelse(input$situation == "couple", input$statut2, "salarié"),
        droit_asf = as.numeric(input$droit_asf),
        majoration_rsa = as.numeric(input$majoration_rsa),
        est_locataire = (input$logement == "locataire"),
        zone = as.numeric(input$zone),
        loyer = ifelse(input$loyer_plaf, "plaf", input$loyer),
        droit_aah1 = as.numeric(input$droit_aah1),
        droit_aah2 = ifelse(input$situation == "couple", as.numeric(input$droit_aah2), 0),
        heures_am = input$heures_am, heures_dom = input$heures_dom,
        quotite_prepare = as.numeric(input$quotite_prepare),
        pension_recue = input$pension_recue, pension_payee = input$pension_payee,
        nb_enfants_sup = input$nb_enfants_sup, p = p
      )
      data.frame(salaire = s, revenu_disponible = res$revenu_disponible,
                 niveau_de_vie = res$niveau_de_vie,
                 af = res$af, cf = res$cf, asf = res$asf, ars = res$ars, ab = res$ab,
                 al = res$al, rsa = res$rsa, pa = res$prime_activite,
                 aah = res$aah, bourses = res$total_bourses, irpp = res$irpp_mensuel)
    })
    do.call(rbind, resultats)
  }

  donnees_var_actuel <- reactive({ req(input$simuler); run_variation(p_ref) })
  donnees_var_reforme <- reactive({
    req(input$simuler)
    if (input$activer_reforme && nrow(reformes()) > 0) run_variation(get_params_reforme())
    else NULL
  })

  output$plot_variation <- renderPlot({
    df <- donnees_var_actuel()
    dfr <- donnees_var_reforme()

    g <- ggplot(df, aes(x = salaire)) +
      geom_line(aes(y = revenu_disponible, color = "Actuel"), linewidth = 1.2) +
      geom_line(aes(y = salaire, color = "Salaire (45°)"), linetype = "dashed", linewidth = 0.8) +
      geom_hline(yintercept = p_ref$seuil_pauvrete, linetype = "dotted",
                 color = "red", linewidth = 0.8) +
      annotate("text", x = max(df$salaire) * 0.8, y = p_ref$seuil_pauvrete + 30,
               label = paste0("Seuil de pauvreté (", round(p_ref$seuil_pauvrete), " €)"),
               color = "red", size = 3.5)

    if (!is.null(dfr)) {
      g <- g + geom_line(data = dfr, aes(y = revenu_disponible, color = "Réforme"),
                         linewidth = 1.2)
      g <- g + scale_color_manual(values = c("Actuel" = "#3498db", "Réforme" = "#e74c3c",
                                              "Salaire (45°)" = "#bdc3c7"))
    } else {
      g <- g + scale_color_manual(values = c("Actuel" = "#2c3e50", "Salaire (45°)" = "#bdc3c7"))
    }

    g + scale_x_continuous(labels = label_number(suffix = " €")) +
      scale_y_continuous(labels = label_number(suffix = " €")) +
      labs(x = "Salaire mensuel net (adulte 1)", y = "€/mois", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # ===================== DÉCOMPOSITION EMPILÉE =====================
  output$plot_empile <- renderPlot({
    df <- donnees_var_actuel()

    df_long <- data.frame(
      salaire = rep(df$salaire, 10),
      composante = rep(c("AF", "CF", "ASF+ARS", "AB PAJE", "AL",
                         "RSA", "Prime activité", "AAH", "Bourses", "Impôt (-)"),
                       each = nrow(df)),
      montant = c(df$af, df$cf, df$asf + df$ars, df$ab,
                  df$al, df$rsa, df$pa, df$aah, df$bourses, -df$irpp)
    )

    df_long$composante <- factor(df_long$composante,
                                  levels = c("RSA", "Prime activité", "AF", "CF",
                                             "ASF+ARS", "AB PAJE", "AL", "AAH",
                                             "Bourses", "Impôt (-)"))

    couleurs <- c("RSA" = "#e74c3c", "Prime activité" = "#e67e22",
                  "AF" = "#2ecc71", "CF" = "#27ae60", "ASF+ARS" = "#1abc9c",
                  "AB PAJE" = "#3498db", "AL" = "#9b59b6",
                  "AAH" = "#f1c40f", "Bourses" = "#95a5a6", "Impôt (-)" = "#2c3e50")

    ggplot(df_long, aes(x = salaire, y = montant, fill = composante)) +
      geom_area(alpha = 0.8) +
      scale_fill_manual(values = couleurs) +
      scale_x_continuous(labels = label_number(suffix = " €")) +
      scale_y_continuous(labels = label_number(suffix = " €")) +
      labs(x = "Salaire mensuel net (adulte 1)", y = "€/mois", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # ===================== GAINS / PERTES =====================
  output$plot_impact <- renderPlot({
    req(donnees_var_reforme())
    df <- donnees_var_actuel()
    dfr <- donnees_var_reforme()

    diff <- data.frame(
      salaire = df$salaire,
      gain = dfr$revenu_disponible - df$revenu_disponible
    )

    ggplot(diff, aes(x = salaire, y = gain)) +
      geom_area(aes(fill = gain > 0), alpha = 0.6) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
                        labels = c("TRUE" = "Gain", "FALSE" = "Perte"), name = NULL) +
      scale_x_continuous(labels = label_number(suffix = " €")) +
      scale_y_continuous(labels = label_number(suffix = " €")) +
      labs(x = "Salaire mensuel net (adulte 1)",
           y = "Gain (+) / Perte (-) en €/mois",
           title = "Impact de la réforme sur le revenu disponible") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  output$plot_impact_detail <- renderPlot({
    req(donnees_var_reforme())
    df <- donnees_var_actuel()
    dfr <- donnees_var_reforme()

    composantes <- c("AF", "CF", "ASF+ARS", "AB", "AL", "RSA", "Prime act.", "AAH", "IRPP (-)")
    diff_mat <- data.frame(
      salaire = rep(df$salaire, length(composantes)),
      composante = rep(composantes, each = nrow(df)),
      diff = c(
        dfr$af - df$af,
        dfr$cf - df$cf,
        (dfr$asf + dfr$ars) - (df$asf + df$ars),
        dfr$ab - df$ab,
        dfr$al - df$al,
        dfr$rsa - df$rsa,
        dfr$pa - df$pa,
        dfr$aah - df$aah,
        -(dfr$irpp - df$irpp)
      )
    )

    # Ne garder que les composantes avec au moins un écart
    composantes_actives <- unique(diff_mat$composante[abs(diff_mat$diff) > 0.01])
    diff_mat <- diff_mat[diff_mat$composante %in% composantes_actives, ]

    if (nrow(diff_mat) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucun impact détecté") +
               theme_void())
    }

    ggplot(diff_mat, aes(x = salaire, y = diff, color = composante)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      scale_x_continuous(labels = label_number(suffix = " €")) +
      scale_y_continuous(labels = label_number(suffix = " €")) +
      labs(x = "Salaire mensuel net (adulte 1)",
           y = "Variation en €/mois (réforme - actuel)",
           color = NULL,
           title = "Détail de l'impact par prestation") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # ===================== TABLEAU COMPLET =====================
  output$table_complet <- renderDT({
    df <- donnees_var_actuel()
    dfr <- donnees_var_reforme()

    if (!is.null(dfr)) {
      df$rev_dispo_reforme <- dfr$revenu_disponible
      df$gain <- dfr$revenu_disponible - df$revenu_disponible
      names(df) <- c("Salaire", "Rev. dispo. actuel", "Niveau de vie",
                     "AF", "CF", "ASF", "ARS", "AB", "AL", "RSA",
                     "Prime act.", "AAH", "Bourses", "IRPP",
                     "Rev. dispo. réforme", "Gain/Perte")
    } else {
      names(df) <- c("Salaire", "Rev. disponible", "Niveau de vie",
                     "AF", "CF", "ASF", "ARS", "AB", "AL", "RSA",
                     "Prime act.", "AAH", "Bourses", "IRPP")
    }

    datatable(df, options = list(pageLength = 25, scrollX = TRUE)) |>
      formatRound(columns = 2:ncol(df), digits = 0, mark = " ")
  })
}

# =============================================================================
# Lancement
# =============================================================================
shinyApp(ui = ui, server = server)
