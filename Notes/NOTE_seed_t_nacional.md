# Technical note — construction of `seed_t_nacional` (maize + sorghum Seed, 1991-2023)

## Context
Intra-sector input (retained seed) for the TFP specification with expanded
intermediate inputs. Replaces the previous attempt at `seed_t_nacional`,
which had three problems: (1) no methodological splice between FAOSTAT
sources, (2) maize/sorghum price mixed grain and forage varieties together,
(3) price not volume-weighted.

## Sources
- `FAO_NUL.csv` — FAOSTAT Food Balances, new methodology (domain code
  `FBS`), coverage 2010-2023.
- `FAO_UL.csv` — FAOSTAT Food Balances, old methodology (domain code
  `FBSH`), coverage through 2013.
- Element filtered: `Semillas` (Seed) only — `Pienso` (Feed) is excluded
  because `Y_t` (SIAP) measures crops only, never livestock, so Feed does
  not create double-counting in this model; only Seed (retained seed,
  crop→same type of production) does.
- Products: `Maíz y productos`, `Sorgo y productos`. Wheat excluded (see
  Bug 1 below — its Seed-specific splice ratio was not verified here; in
  the earlier Feed+Seed splice it came out unstable, 3-5x with no pattern,
  so it's kept out as a precaution).

## Method — ratio splice over the overlap period
The two sources cover different methodologies; the ratio
`Valor_nuevo / Valor_viejo` was computed over the 2010-2013 overlap (the
only range where both series are available) and the average of that ratio
was used to rescale the old series (1991-2009) to the new series' metric
before concatenating them.

**Ratios obtained (Seed only — do not reuse the Feed+Seed ratios, which are
different):**

| Product | Mean ratio | Std. dev. |
|---|---|---|
| Maíz y productos | 0.4983 | 0.0135 |
| Sorgo y productos | 0.9158 | 0.0423 |

Both ratios are stable over the overlap (see full table in the data
commit) — sufficient support to trust the splice.

## Bugs fixed relative to the previous version
1. **Missing splice**: the previous version concatenated `FAO_NUL`/`FAO_UL`
   with a hard cut at 2010 and no adjustment ratio, risking an artificial
   level jump at the cutoff.
2. **Contaminated price**: the maize/sorghum price averaged grain and forage
   varieties together via an unqualified regex (`"Ma[ií]z"` also matched
   "Maíz forrajero en verde"). Still pending a fix at valuation time (see
   limitation below).
3. **Unweighted price**: a simple `mean()` across states was used instead
   of weighting by production volume, inconsistent with the rest of the
   pipeline (`indice_climatico`, `Y_nacional`).

## Pending limitation — series delivered in tonnes ONLY
This commit delivers `Seed_t_toneladas` (1991-2023, 33 observations),
not valued in constant 2015 pesos. Valuation (`Seed_valor = Seed_t *
precio_prom`) requires `prod_ag_est` and `precios_base`, not available in
this build environment — pending a run in the environment with those
objects loaded.

## Verification pending before use in the model
- Confirm that `range(modelo_df_v3$Año)` doesn't lose extra observations
  from the Seed_t `lag()` relative to the original `modelo_df` (see the
  sample-comparison note from the previous session).
- Align the `H` of the `A_hat_v3` state-space model with that of
  `ss_model_Ahat` (both should be `matrix(0)`), avoiding variance
  differences not attributable to Seed.
