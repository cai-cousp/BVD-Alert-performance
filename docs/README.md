# Documentation

This folder consolidates the public documentation that ships with the
repository: glossaries of the analysis output columns, the Quarto methods
notes, and the historical implementation plans.

> **Methodology design notes are intentionally excluded** from the public
> repository. They live locally under `docs/methodology/` (gitignored) and
> are not redistributed.

## Glossaries

Column-by-column definitions for the analysis output tables.

- [Thresholds glossary (EN)](glossaries/01_thresholds_glossary_en.md) /
  [FR](glossaries/01_thresholds_glossary_fr.md) — for `01_thresholds_synthesis.rds`
- [Trends glossary (EN)](glossaries/02_trends_glossary_en.md) /
  [FR](glossaries/02_trends_glossary_fr.md) — for `02_trends_smooth_adeq.rds`

## Methods

Quarto methods notes describing the analysis workflow, inputs, assumptions,
and limitations. Render locally with `quarto render <file>.qmd`.

- Threshold methods note — [EN](methods/methods_note_alert_thresholds.qmd) /
  [FR](methods/methods_note_alert_thresholds_fr.qmd)
- Trends methods note — [EN](methods/methods_note_alert_trends.qmd) /
  [FR](methods/methods_note_alert_trends_fr.qmd)

## Reports

Epidemiological surveillance reports and templates for decision-makers and field teams.

- [Alert Performance Report](reports/Alert_performance_report.qmd) (`Alert_performance_report.qmd`) —
  Self-contained publication report synthesizing epidemic indicators, longitudinal alert trends,
  curve fitting, adequacy ratios, and spatial notification performance. Render locally with:
  `quarto render docs/reports/Alert_performance_report.qmd`

## Plans

Historical implementation and design plans.

- [Implementation plan](plans/implementation_plan.md) — main implementation
  plan (EVD17 alert thresholds and trends)
- [Implementation plan: time windows](plans/implementation_plan_timewindows.md) —
  7-day windows, no reporting-lag logic
