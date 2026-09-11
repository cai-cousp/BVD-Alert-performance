# =============================================================================
# BVD Alerts Dashboard — Export module: report download, data export
# =============================================================================

export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "mb-4",
      div(
        class = "d-flex align-items-center mb-3",
        tags$span(
          class = "badge bg-secondary me-2 px-3 py-2 fs-6",
          "Exportation"
        ),
        h3(class = "mb-0", "Options d'exportation des données et rapports")
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-4",
        downloadButton(
          ns("download_report"),
          label = tags$span(
            bs_icon("file-earmark-arrow-down", class = "me-1"),
            "Télécharger le rapport (HTML)"
          ),
          class = "btn btn-primary"
        ),
        tags$a(
          id = ns("view_report"),
          href = "Alert_performance_report.html",
          target = "_blank",
          class = "btn btn-outline-primary",
          tags$span(
            class = "btn-content",
            bs_icon("box-arrow-up-right", class = "me-1"),
            "Consulter le rapport"
          )
        ),
        actionButton(
          ns("open_browser_report"),
          label = tags$span(
            bs_icon("window-stack", class = "me-1"),
            "Ouvrir dans le navigateur"
          ),
          class = "btn btn-outline-secondary",
          title = "Ouvre le rapport directement dans votre navigateur par défaut (Google Chrome / Safari)"
        ),
        tags$script(HTML("
          (function() {
            function getStaticReportUrl() {
              var isIframeApp = window.location.pathname.indexOf('/app_') !== -1;
              return isIframeApp ? '../Alert_performance_report.html' : 'Alert_performance_report.html';
            }

            function syncReportElements() {
              var reportUrl = getStaticReportUrl();
              var viewBtn = document.getElementById('export-view_report');
              if (viewBtn) {
                viewBtn.setAttribute('href', reportUrl);
              }
              var altLink = document.getElementById('export-alt_report_link');
              if (altLink) {
                altLink.setAttribute('href', reportUrl);
                altLink.setAttribute('download', 'Alert_performance_report.html');
              }
            }

            function triggerBlobDownload(blob, filename) {
              var blobUrl = URL.createObjectURL(blob);
              var a = document.createElement('a');
              a.href = blobUrl;
              a.download = filename || 'Alert_performance_report.html';
              a.style.display = 'none';
              document.body.appendChild(a);
              a.click();
              setTimeout(function() {
                if (a.parentNode) a.parentNode.removeChild(a);
                URL.revokeObjectURL(blobUrl);
              }, 60000);
            }

            function downloadReportWithFeedback(btn) {
              var origHTML = btn.innerHTML;
              btn.innerHTML = '<span class=\"spinner-border spinner-border-sm me-2\" role=\"status\" aria-hidden=\"true\"></span><span>T\\u00e9l\\u00e9chargement en cours...</span>';
              btn.style.pointerEvents = 'none';

              // Notify Shiny server to perform desktop save if running locally
              if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
                window.Shiny.setInputValue('export-report_download_requested', new Date().getTime());
              }

              var sessionHref = btn.getAttribute('href');
              var staticUrl = getStaticReportUrl();
              var candidates = [
                staticUrl,
                'Alert_performance_report.html',
                'reports/Alert_performance_report.html'
              ];
              if (sessionHref && sessionHref !== '#' && sessionHref.indexOf('download') !== -1) {
                candidates.push(sessionHref);
              }

              function tryNext(index) {
                if (index >= candidates.length) {
                  btn.innerHTML = origHTML;
                  btn.style.pointerEvents = '';
                  window.location.assign(staticUrl);
                  return;
                }

                var url = candidates[index];
                fetch(url, { cache: 'no-cache' })
                  .then(function(resp) {
                    if (!resp.ok) throw new Error('HTTP ' + resp.status);
                    return resp.blob();
                  })
                  .then(function(blob) {
                    if (blob.size < 1000) throw new Error('File payload too small');
                    triggerBlobDownload(blob, 'Alert_performance_report.html');
                    btn.innerHTML = '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" fill=\"currentColor\" class=\"bi bi-check2-circle me-1\" viewBox=\"0 0 16 16\"><path d=\"M2.5 8a5.5 5.5 0 0 1 8.25-4.764.5.5 0 0 0 .5-.866A6.5 6.5 0 1 0 14.5 8a.5.5 0 0 0-1 0 5.5 5.5 0 1 1-11 0z\"/><path d=\"M15.354 3.354a.5.5 0 0 0-.708-.708L8 9.293 5.354 6.646a.5.5 0 1 0-.708.708l3 3a.5.5 0 0 0 .708 0l7-7z\"/></svg><span>T\\u00e9l\\u00e9charg\\u00e9 !</span>';
                    setTimeout(function() {
                      btn.innerHTML = origHTML;
                      btn.style.pointerEvents = '';
                    }, 2500);
                  })
                  .catch(function(err) {
                    tryNext(index + 1);
                  });
              }

              tryNext(0);
            }

            document.addEventListener('click', function(e) {
              var btn = e.target && e.target.closest ? e.target.closest('#export-download_report') : null;
              if (!btn) return;
              e.preventDefault();
              e.stopPropagation();
              downloadReportWithFeedback(btn);
            }, true);

            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', syncReportElements);
            } else {
              syncReportElements();
            }
            setTimeout(syncReportElements, 500);
            setTimeout(syncReportElements, 2000);
          })();
        ")),
        downloadButton(
          ns("download_data"),
          label = tags$span(
            bs_icon("file-earmark-excel", class = "me-1"),
            "Exporter les données (Excel)"
          ),
          class = "btn btn-outline-primary"
        ),
        downloadButton(
          ns("download_csv"),
          label = tags$span(
            bs_icon("file-earmark-text", class = "me-1"),
            "Exporter les données (CSV)"
          ),
          class = "btn btn-outline-secondary"
        )
      ),
      card(
        card_header(
          tags$span(
            bs_icon("info-circle", class = "me-2 text-secondary"),
            "Détails des formats d'exportation"
          )
        ),
        card_body(
          tags$dl(
            tags$dt("Rapport HTML"),
            tags$dd(
              "Télécharge le rapport Quarto complet et autonome (Alert_performance_report.html, 5.4 Mo) incluant les graphiques interactifs, ",
              "les tableaux de seuils et l'évaluation de performance globale et par zone de santé. ",
              tags$span(
                class = "text-muted",
                "(Disponible également en ",
                tags$a(
                  id = ns("alt_report_link"),
                  href = "Alert_performance_report.html",
                  download = "Alert_performance_report.html",
                  target = "_blank",
                  class = "text-decoration-underline text-primary",
                  "téléchargement direct alternatif"
                ),
                ")"
              )
            ),
            tags$dt("Export Excel"),
            tags$dd(
              "Classeur Excel multi-feuilles contenant les séries chronologiques de tendances, ",
              "les indicateurs synthétiques et les paramètres méthodologiques."
            ),
            tags$dt("Export CSV"),
            tags$dd(
              "Fichier tabulaire (CSV) compatible avec l'ensemble des logiciels statistiques et tableurs."
            )
          )
        )
      )
    ),

    # Methodology documentation sits beneath report generation on the main page.
    tags$section(
      id = ns("documentation"),
      class = "app-documentation mb-5",
      div(
        class = "d-flex align-items-center mb-3",
        tags$span(
          class = "badge bg-primary me-2 px-3 py-2 fs-6",
          "Documentation"
        ),
        h3(class = "mb-0", "Méthodologie")
      ),
      card(
        card_header(
          tags$span(
            bs_icon("diagram-3", class = "me-2 text-secondary"),
            "Workflow des seuils d'alerte"
          )
        ),
        card_body(
          class = "app-documentation-body",
          withMathJax(
            tags$p(
              "Le workflow des seuils d'alerte estime le nombre hebdomadaire attendu d'alertes validées pour ",
              "les cas et les décès dans chaque zone de santé affectée. Il combine trois sources complémentaires ",
              "d'attente : la mortalité de base, la performance historique de la surveillance et la dynamique ",
              "actuelle de l'épidémie."
            ),
            h4("Approche A : mortalité de base"),
            tags$p(
              "L'approche A estime les décès communautaires attendus à partir de la population de la zone et ",
              "d'un taux brut de mortalité de 8,3 décès pour 1 000 personnes-années. Elle fournit un plancher ",
              "pour les alertes de décès, mais ne produit pas de seuil pour les alertes de cas."
            ),
            tags$p(
              class = "app-math",
              "\\[
                E[D_w] = P \\times \\frac{8{,}3}{1000} \\times \\frac{7}{365}
              \\]"
            ),
            tags$p(
              class = "app-math",
              "\\[
                T_A^- = 0{,}9\\,E[D_w],
                \\qquad
                T_A^+ = 1{,}1\\,E[D_w]
              \\]"
            ),
            h4("Approche B : référence historique de Beni"),
            tags$p(
              "L'approche B utilise les alertes validées de la base historique Beni/EVD10. Les semaines ",
              "zone-santé dont la positivité en laboratoire est inférieure à 10 % sont considérées comme des ",
              "périodes de surveillance optimales. Dans ces périodes, le workflow calcule les taux hebdomadaires ",
              "d'alertes de cas et de décès pour 100 000 habitants, retient les zones ayant un volume suffisant ",
              "et une forte performance, puis utilise Beni comme zone de référence."
            ),
            tags$p(
              class = "app-math",
              "\\[
                \\text{positivité}_{z,w} = \\frac{n_{z,w}^{+}}{n_{z,w}^{testés}},
                \\qquad
                \\lambda_z = \\frac{A_z / w_z}{P_z} \\times 100000
              \\]"
            ),
            tags$p(
              class = "app-math",
              "\\[
                T_{z}^{\\pm} = q^{\\pm}(\\lambda_z) \\times \\frac{P_z}{100000}
              \\]"
            ),
            tags$p(
              "Ici, \\(q^{-}\\) et \\(q^{+}\\) sont les quantiles de Poisson aux niveaux 25 % et 95 %, ",
              "appliqués au taux de référence de Beni avant l'ajustement sur la population de la zone."
            ),
            h4("Approche C : attentes dérivées des cas actuels"),
            tags$p(
              "L'approche C adapte les seuils à l'épidémie actuelle. Les cas sont organisés en fenêtres ",
              "hebdomadaires non chevauchantes, ancrées à la date d'apparition des symptômes la plus récente ",
              "parmi les cas confirmés. Les séries récentes de cas et de décès sont tronquées à droite, car des ",
              "apparitions récentes peuvent encore attendre une notification ou une confirmation en laboratoire. ",
              "Le workflow corrige donc la queue de délai de notification avant d'estimer les indicateurs ",
              "épidémiologiques."
            ),
            h5("Nowcasting bayésien"),
            tags$p(
              "Le nowcasting bayésien est appliqué séparément aux séries récentes de cas confirmés et de décès ",
              "confirmés, par zone de santé. Il estime le délai entre l'apparition des symptômes et le rapport ",
              "à partir des enregistrements entièrement observés, ajuste un modèle lognormal de troncature et ",
              "produit des médianes de nowcasting avec des intervalles de crédibilité à 90 %. Le modèle bayésien ",
              "complet est utilisé lorsque la zone dispose d'assez de cas récents, d'assez de jours de série et ",
              "d'assez de délais observés ; sinon, une correction empirique des délais ou une valeur manquante ",
              "est utilisée."
            ),
            tags$p(
              class = "app-math",
              "\\[
                Y_t^{obs} = Y_t\\,F_D(R-t),
                \\qquad
                \\widehat{Y}_t = \\frac{Y_t^{obs}}{F_D(R-t)}
              \\]"
            ),
            tags$p(
              "\\(Y_t\\) est l'incidence vraie à la date d'apparition \\(t\\), \\(Y_t^{obs}\\) le nombre déjà ",
              "observé, \\(R\\) la date de référence et \\(F_D\\) la fonction de répartition du délai de rapport. ",
              "Le modèle bayésien résume la loi a posteriori des incidences de la queue tronquée. Cela évite ",
              "d'interpréter les derniers jours incomplets comme une véritable baisse d'incidence."
            ),
            h5("Taux de croissance r"),
            tags$p(
              "Pour chaque fenêtre de zone de santé, les comptes quotidiens de cas confirmés par date ",
              "d'apparition sont complétés avec des zéros, puis ajustés par un modèle de croissance ",
              "exponentielle. La pente estimée est le taux de croissance quotidien \\(r\\). Une valeur positive ",
              "indique une croissance, une valeur négative un déclin. Une estimation groupée sur l'ensemble des ",
              "zones est utilisée lorsque l'estimation propre à la zone n'est pas disponible."
            ),
            tags$p(
              class = "app-math",
              "\\[
                I(t) = I_0 e^{rt},
                \\qquad
                T_{doublement} = \\frac{\\ln(2)}{r} \\quad (r > 0)
              \\]"
            ),
            h5("Nombre de reproduction effectif Rt"),
            tags$p(
              "Le nombre de reproduction effectif \\(R_t\\) est estimé à partir de l'incidence quotidienne des ",
              "cas confirmés par date d'apparition, à l'aide d'un intervalle intergénérationnel d'EVD de moyenne ",
              "12 jours et d'écart-type 5 jours. Les estimations utilisent les données disponibles jusqu'à la ",
              "fin de chaque fenêtre de seuil et remplacent la queue de délai par les valeurs de nowcasting ",
              "lorsqu'elles existent. Une estimation groupée est utilisée en cas d'indisponibilité de ",
              "l'estimation spécifique à la zone."
            ),
            tags$p(
              class = "app-math",
              "\\[
                SAR = \\frac{R_t}{k}
              \\]"
            ),
            tags$p(
              "\\(k\\) est le nombre de contacts suivis par cas confirmé. Ce taux d'attaque secondaire approximatif ",
              "sert ensuite à estimer les cas secondaires attendus."
            ),
            h5("Multiplicateurs d'alertes beta-c et beta-d"),
            tags$p(
              "Le multiplicateur \\(\\beta_c\\) représente le nombre attendu d'alertes validées de personnes ",
              "vivantes par cas vrai estimé. Le multiplicateur \\(\\beta_d\\) représente le nombre attendu ",
              "d'alertes validées de décès par décès attendu. Pour chaque fenêtre de seuil, ces multiplicateurs ",
              "sont estimés à partir des comptes hebdomadaires récents d'alertes avec un modèle log-linéaire ",
              "de Poisson incluant le logarithme du fardeau attendu comme décalage. L'exponentielle de ",
              "l'ordonnée à l'origine est le multiplicateur."
            ),
            tags$p(
              class = "app-math",
              "\\[
                A_w^v \\sim \\mathrm{Poisson}(\\beta_v E_w^v),
                \\qquad
                \\log E[A_w^v] = \\log E_w^v + \\log \\beta_v
              \\]"
            ),
            tags$p(
              class = "app-math",
              "\\[
                \\widehat{\\beta}_v = \\exp(\\widehat{\\alpha}_0),
                \\qquad
                v \\in \\{cas, décès\\}
              \\]"
            ),
            tags$p(
              "Lorsque la surdispersion est importante, le modèle Poisson est réajusté en modèle binomial ",
              "négatif lorsque cela est possible. Les séries trop courtes ou insuffisantes utilisent le rapport ",
              "direct entre le nombre total d'alertes validées et l'exposition totale, avec des intervalles de ",
              "Poisson exacts."
            ),
            tags$p(
              "Un autre paramètre bêta, fixe, décrit le délai Gamma entre l'apparition des symptômes et le ",
              "décès utilisé dans l'ajustement de détection par rétro-calcul. Il est distinct des multiplicateurs ",
              "\\(\\beta_c\\) et \\(\\beta_d\\)."
            ),
            tags$p(
              class = "app-math",
              "\\[
                \\alpha = \\left(\\frac{\\mu}{\\sigma}\\right)^2 \\approx 4{,}42,
                \\qquad
                \\beta_{délai} = \\frac{\\mu}{\\sigma^2} \\approx 0{,}389
              \\]"
            ),
            tags$p(
              "Ces valeurs sont obtenues avec \\(\\mu = 11{,}37\\) jours et \\(\\sigma = 5{,}41\\) jours."
            ),
            h5("Seuils dérivés des cas"),
            tags$p(
              "Les cas vrais récents sont estimés en ajustant les cas détectés pour la sous-détection. Les ",
              "décès attendus sont obtenus en multipliant les cas vrais estimés par le taux de létalité. Les ",
              "seuils de l'approche C sont ensuite les produits du fardeau estimé et des multiplicateurs ",
              "d'alertes correspondants."
            ),
            tags$p(
              class = "app-math",
              "\\[
                \\widehat{C} = \\frac{C_{détectés}}{\\rho},
                \\qquad
                \\widehat{D} = \\widehat{C} \\times L
              \\]"
            ),
            tags$p(
              class = "app-math",
              "\\[
                T_C^{cas} = \\beta_c\\,\\widehat{C},
                \\qquad
                T_C^{décès} = \\beta_d\\,\\widehat{D}
              \\]"
            ),
            tags$p(
              "\\(\\rho\\) est le taux de détection combiné et \\(L\\) le taux de létalité utilisé. Les bornes ",
              "inférieure et supérieure utilisent les bornes d'intervalle des multiplicateurs."
            ),
            h4("Synthèse des seuils"),
            tags$p(
              "Les seuils finaux de cas sont la moyenne des approches B et C. Les seuils finaux de décès sont ",
              "la moyenne des approches A, B et C. Les seuils centraux sont les points milieux entre les bornes ",
              "finale inférieure et supérieure."
            ),
            tags$p(
              class = "app-math",
              "\\[
                T_{cas}^{\\pm} =
                \\frac{T_B^{\\pm} + T_C^{\\pm}}{2},
                \\qquad
                T_{décès}^{\\pm} =
                \\frac{T_A^{\\pm} + T_B^{\\pm} + T_C^{\\pm}}{3}
              \\]"
            ),
            tags$p(
              class = "app-math",
              "\\[
                T = \\frac{T^- + T^+}{2}
              \\]"
            ),
            tags$p(
              "L'ensemble de la zone affectée inclut chaque zone seulement à partir de sa première notification ",
              "confirmée. Pour les décès, il combine les approches A et C ; pour les cas, il utilise l'approche C. ",
              "La référence historique propre à chaque zone n'est pas additionnée au niveau agrégé."
            ),
            h4("Sorties"),
            tags$p(
              "Le workflow produit des tableaux de seuils par zone de santé et pour l'ensemble affecté, des ",
              "paramètres épidémiologiques intermédiaires et des résumés dans des classeurs. Les artefacts de ",
              "seuils enregistrent la date d'état des données et un identifiant commun d'exécution, permettant ",
              "aux analyses de tendances de vérifier que les résultats par zone et pour l'ensemble proviennent ",
              "bien de la même exécution."
            )
          )
        )
      )
    )
  )
}

export_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {

    filtered_trends_data <- filtered_trends(filters)
    filtered_synth_data <- filtered_synthesis(filters)

    # --- HTML report download -------------------------------------------------
    output$download_report <- downloadHandler(
      filename = function() {
        "Alert_performance_report.html"
      },
      contentType = "text/html; charset=utf-8",
      content = function(file) {
        target_dir <- if (exists("app_dir", inherits = TRUE)) app_dir else NULL
        report_html <- find_alert_report_html(target_dir)

        if (!is.null(report_html) && file.exists(report_html)) {
          file.copy(report_html, file, overwrite = TRUE)
        } else {
          # Fallback: generate simple HTML summary if pre-rendered report is not found
          write_simple_html_report(file, filtered_trends_data(), filtered_synth_data(), filters)
        }
      }
    )

    # --- Excel data export ----------------------------------------------------
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("BVD_Alert_Data_", Sys.Date(), ".xlsx")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        trends_dat <- filtered_trends_data()
        synth_dat  <- filtered_synth_data()

        # Metadata sheet
        metadata <- tibble::tibble(
          Parameter = c(
            "Date range start",
            "Date range end",
            "Number of health zones",
            "Alert level filter",
            "Adequacy filter",
            "Export date",
            "Data source directory"
          ),
          Value = c(
            as.character(filters$date_range()[1]),
            as.character(filters$date_range()[2]),
            as.character(length(filters$selected_hzs())),
            filters$alert_level(),
            paste(filters$adequacy_filter(), collapse = ", "),
            Sys.Date(),
            data$data_dir
          )
        )

        writexl::write_xlsx(
          list(
            Trends       = trends_dat,
            Synthesis    = synth_dat,
            Metadata     = metadata
          ),
          path = file
        )
      }
    )

    # --- CSV data export ------------------------------------------------------
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("BVD_Alert_Trends_", Sys.Date(), ".csv")
      },
      contentType = "text/csv; charset=utf-8",
      content = function(file) {
        readr::write_csv(filtered_trends_data(), file)
      }
    )

    # --- Desktop Local Save & External Browser Observers ---------------------
    observeEvent(input$report_download_requested, {
      target_dir <- if (exists("app_dir", inherits = TRUE)) app_dir else NULL
      report_html <- find_alert_report_html(target_dir)
      home_downloads <- file.path(Sys.getenv("HOME"), "Downloads")

      if (!is.null(report_html) && file.exists(report_html) && dir.exists(home_downloads)) {
        dest_path <- file.path(home_downloads, "Alert_performance_report.html")
        file.copy(report_html, dest_path, overwrite = TRUE)
        showNotification(
          ui = tags$div(
            tags$strong("Rapport sauvegardé avec succès !"),
            tags$p("Une copie a été enregistrée dans vos Téléchargements :", tags$br(), tags$code(dest_path))
          ),
          type = "message",
          duration = 6
        )
      }
    }, ignoreInit = TRUE)

    observeEvent(input$open_browser_report, {
      target_dir <- if (exists("app_dir", inherits = TRUE)) app_dir else NULL
      report_html <- find_alert_report_html(target_dir)

      if (!is.null(report_html) && file.exists(report_html)) {
        utils::browseURL(normalizePath(report_html))
        showNotification(
          "Rapport ouvert dans votre navigateur par défaut.",
          type = "message",
          duration = 4
        )
      } else {
        showNotification(
          "Rapport non trouvé sur le disque.",
          type = "warning",
          duration = 4
        )
      }
    })
  })
}

# --- Helper: locate pre-rendered Alert_performance_report.html ----------------
find_alert_report_html <- function(app_directory = NULL) {
  explicit_app_dir <- !is.null(app_directory)
  if (!explicit_app_dir) {
    if (exists("app_dir", envir = parent.frame())) {
      app_directory <- get("app_dir", envir = parent.frame())
    } else if (exists("app_dir", envir = .GlobalEnv)) {
      app_directory <- get("app_dir", envir = .GlobalEnv)
    }
  }

  detected_root <- NULL
  if (!explicit_app_dir) {
    if (exists("root_dir", envir = parent.frame())) {
      detected_root <- get("root_dir", envir = parent.frame())
    } else if (exists("root_dir", envir = .GlobalEnv)) {
      detected_root <- get("root_dir", envir = .GlobalEnv)
    }
  }
  if (is.null(detected_root) && !is.null(app_directory)) {
    detected_root <- dirname(app_directory)
  }

  # Primary candidates: Alert_performance_report.html
  primary_candidates <- unique(c(
    if (!is.null(detected_root)) file.path(detected_root, "docs", "reports", "Alert_performance_report.html"),
    if (!is.null(app_directory)) file.path(dirname(app_directory), "docs", "reports", "Alert_performance_report.html"),
    file.path(getwd(), "docs", "reports", "Alert_performance_report.html"),
    file.path(getwd(), "..", "docs", "reports", "Alert_performance_report.html"),
    file.path("..", "docs", "reports", "Alert_performance_report.html"),
    file.path("docs", "reports", "Alert_performance_report.html"),
    if (!is.null(app_directory)) file.path(app_directory, "Alert_performance_report.html"),
    if (!is.null(app_directory)) file.path(app_directory, "www", "Alert_performance_report.html"),
    file.path(getwd(), "ShinyApp", "Alert_performance_report.html"),
    file.path(getwd(), "Alert_performance_report.html"),
    file.path(getwd(), "www", "Alert_performance_report.html")
  ))

  existing_primary <- primary_candidates[!is.na(primary_candidates) & file.exists(primary_candidates)]
  if (length(existing_primary) > 0L) {
    valid_primary <- existing_primary[file.info(existing_primary)$size > 1000000L]
    if (length(valid_primary) > 0L) {
      existing_primary <- valid_primary
    }
    if (length(existing_primary) > 1L) {
      mtimes <- file.info(existing_primary)$mtime
      best_idx <- which.max(mtimes)
      return(normalizePath(existing_primary[[best_idx]], mustWork = FALSE))
    }
    return(normalizePath(existing_primary[[1L]], mustWork = FALSE))
  }

  # Fallback candidates: legacy report_template.html
  fallback_candidates <- unique(c(
    if (!is.null(detected_root)) file.path(detected_root, "docs", "reports", "report_template.html"),
    if (!is.null(app_directory)) file.path(app_directory, "report_template.html"),
    if (!is.null(app_directory)) file.path(app_directory, "www", "report_template.html"),
    if (!is.null(app_directory)) file.path(dirname(app_directory), "docs", "reports", "report_template.html"),
    file.path(getwd(), "docs", "reports", "report_template.html"),
    file.path(getwd(), "ShinyApp", "report_template.html"),
    file.path(getwd(), "report_template.html"),
    file.path(getwd(), "www", "report_template.html"),
    file.path("..", "docs", "reports", "report_template.html"),
    file.path("docs", "reports", "report_template.html")
  ))

  existing_fallback <- fallback_candidates[!is.na(fallback_candidates) & file.exists(fallback_candidates)]
  if (length(existing_fallback) > 0L) {
    if (length(existing_fallback) > 1L) {
      mtimes <- file.info(existing_fallback)$mtime
      best_idx <- which.max(mtimes)
      return(normalizePath(existing_fallback[[best_idx]], mustWork = FALSE))
    }
    return(normalizePath(existing_fallback[[1L]], mustWork = FALSE))
  }

  NULL
}

# Alias for backward compatibility
find_report_template_html <- find_alert_report_html

# --- Fallback simple HTML report generator -----------------------------------
write_simple_html_report <- function(file, trends_dat, synth_dat, filters) {
  report_date <- Sys.Date()
  n_hz <- if (!is.null(trends_dat) && "zone_sante_notification" %in% names(trends_dat)) {
    n_distinct(trends_dat$zone_sante_notification)
  } else 0L
  total_alerts <- if (!is.null(trends_dat) && "total_alerts" %in% names(trends_dat)) {
    sum(trends_dat$total_alerts, na.rm = TRUE)
  } else 0L
  n_weeks <- if (!is.null(trends_dat) && "week_start" %in% names(trends_dat)) {
    n_distinct(trends_dat$week_start)
  } else 0L
  date_start <- as.character(filters$date_range()[1])
  date_end   <- as.character(filters$date_range()[2])

  adeq_df <- if (exists("recent_adequacy", envir = .GlobalEnv)) {
    get("recent_adequacy", envir = .GlobalEnv)
  } else if (!is.null(synth_dat) && "adequacy_category" %in% names(synth_dat)) {
    synth_dat
  } else {
    data.frame(adequacy_category = character(0))
  }

  data_src <- if (exists("data_folder", envir = .GlobalEnv)) {
    get("data_folder", envir = .GlobalEnv)
  } else if (exists("output_base", envir = .GlobalEnv)) {
    get("output_base", envir = .GlobalEnv)
  } else {
    "Operational pipeline"
  }

  adequacy_rows <- paste0(
    sapply(c("Under-alerting", "Adequate", "Over-alerting"), function(cat) {
      n <- if ("adequacy_category" %in% names(adeq_df)) sum(adeq_df$adequacy_category == cat, na.rm = TRUE) else 0L
      paste0("<tr><td>", cat, "</td><td>", n, "</td></tr>")
    }),
    collapse = "\n"
  )

  html_content <- paste0(
    "<!DOCTYPE html>
<html lang='en'>
<head>
<meta charset='UTF-8'>
<title>BVD Alert Report</title>
<style>
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
       max-width: 800px; margin: 40px auto; padding: 0 20px; color: #333; }
h1 { color: #0D6EFD; border-bottom: 2px solid #0D6EFD; padding-bottom: 10px; }
h2 { color: #495057; margin-top: 30px; }
table { border-collapse: collapse; width: 100%; margin: 15px 0; }
th, td { border: 1px solid #dee2e6; padding: 8px 12px; text-align: left; }
th { background-color: #f8f9fa; }
.summary-box { background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 15px 0; }
.footer { margin-top: 40px; font-size: 0.85em; color: #6c757d; }
</style>
</head>
<body>
<h1>BVD Alert Dashboard Report</h1>
<p>Generated: ", report_date, "</p>

<div class='summary-box'>
<h2>Filter Summary</h2>
<ul>
<li><strong>Date range:</strong> ", date_start, " to ", date_end, "</li>
<li><strong>Health zones:</strong> ", n_hz, " selected</li>
<li><strong>Alert level:</strong> ", filters$alert_level(), "</li>
<li><strong>Adequacy filter:</strong> ", paste(filters$adequacy_filter(), collapse = ", "), "</li>
</ul>
</div>

<h2>Key Statistics</h2>
<ul>
<li>Total validated alerts (all HZs, all weeks): <strong>",
formatC(total_alerts, big.mark = ","), "</strong></li>
<li>Number of weeks in selection: <strong>",
n_weeks, "</strong></li>
<li>Number of health zones: <strong>", n_hz, "</strong></li>
</ul>

<h2>Recent Adequacy Summary</h2>
<table>
<tr><th>Adequacy Category</th><th>Number of HZs</th></tr>
", adequacy_rows, "
</table>

<div class='footer'>
<p>This report was generated by the BVD Alerts Dashboard Shiny application.</p>
<p>Data source: ", data_src, "</p>
<p>Methodology: Alert thresholds are computed using three approaches: (A) CMR-based expected deaths, ",
"(B) Beni historical benchmark from EVD10, and (C) case-derived expectations via detection rates, ",
"Rt, and SAR. The consensus threshold averages the relevant approaches.</p>
</div>
</body>
</html>"
  )

  writeLines(html_content, file)
}
