# Post-Harvest Labour Organisation: replication code and aggregate benchmarks

This repository provides public computational replication materials for a descriptive study of post-harvest labour organisation in Nigerian farm households using GHS-Panel Waves 4 and 5. It contains code, release-safe documentation, an aggregate Figure 1 asset, and aggregate benchmark outputs. It does **not** contain a manuscript, manuscript draft, title page, supplementary manuscript file, microdata, respondent-level derivatives, or private internal audit materials.

## Research scope

The code distinguishes three non-interchangeable descriptive objects:

1. **Worker-crop arrangements** — the analytic observations; these are not counts of unique people.
2. **Task-expanded records** — multiple-response representations of arrangements when more than one task is reported.
3. **Recorded arrangement-level person-hours** — valid only after returning from task-expanded records to the worker-crop arrangement.

The analysis is descriptive. It does not estimate causal effects, individual behavioural transitions, worker productivity, wages, bargaining power, household welfare, or task-specific time.

## Data and access

The eight required Nigeria GHS-Panel Wave 4 and Wave 5 source files are restricted and are deliberately excluded. Obtain them lawfully from the authorised Nigeria National Bureau of Statistics / World Bank Microdata Library route identified in [docs/SOURCE_MANIFEST.csv](docs/SOURCE_MANIFEST.csv). Keep the files outside this repository.

Before execution, compare acquired-file SHA-256 checksums against the source manifest. Do not silently replace an input with another release. The local Wave 5 files used for validation match the published schema and case-count metadata for `NGA_2023_GHSP-W5_v02_M`; the historical download-date manifest was not retained.

## Expected construction and checks

Successful reproduction must obtain the following exact counts:

| Check | Wave 4 | Wave 5 | Total |
|---|---:|---:|---:|
| Raw task-worker records | — | — | 754,524 |
| Eligible worker-crop arrangements | 17,053 | 19,147 | 36,200 |
| Task-expanded records | 30,096 | 41,866 | 71,962 |

The controlled validation passed 196 grouped source comparisons, 153 descriptive benchmark comparisons, and all 24 approved Figure 1 cells. Those aggregate comparison records are retained in [reference_outputs](reference_outputs).

Task categories overlap. Their incidences do not sum to 100% and do not measure task-specific hours. Repeated hours are never summed across task flags: doing so would inflate hours by 86.6% in Wave 4 and 103.7% in Wave 5. External arrangements can aggregate multiple workers, so their recorded hours are group contributions—not individual worker intensity or productivity.

## Contents

* `config/` — release-safe input configuration example.
* `environment/` — R dependency requirements.
* `src/` — construction, estimation, source-validation, and figure code.
* `tests/` — source, numerical, and public-package checks.
* `reference_outputs/` — aggregate validation benchmarks only.
* `figures/` — aggregate Figure 1 output.
* `docs/` — source manifest, data dictionary, estimands, survey design, harmonisation, and limitations.

## Local reproduction

1. Install R 4.6.1 and the packages listed in `environment/requirements.R`.
2. Obtain authorised access to the eight GHS files, outside this repository.
3. Check source files:

   ```sh
   Rscript tests/source_checks/01_verify_source_manifest.R /path/to/restricted/GHS
   ```

4. Copy `config/example_config.R` to `config/local_config.R`; set `GHS_RAW_DIR` and a restricted `PAPER3_RUNTIME_DIR`, both outside the repository.
5. Run from the repository root:

   ```sh
   GHS_RAW_DIR=/path/to/restricted/GHS PAPER3_RUNTIME_DIR=/path/to/restricted/runtime Rscript RUN_REPRODUCTION.R
   ```

6. Confirm that the benchmark summary reports 196 grouped checks, 153 descriptive outputs, and 24 Figure 1 cells as passing.

The GitHub Actions workflow intentionally runs only a safe package-integrity check. It cannot run the microdata-dependent workflow because restricted inputs are neither committed nor supplied to CI.

## Survey design and interpretation restrictions

Wave 4 uses `wt_wave4`; Wave 5 uses `wt_cross_wave5`. Estimates use released clusters and strata, household-level numerator/denominator aggregation, linearised variance estimation, and `survey.lonely.psu = "adjust"`. The 358 Wave 4 external records with missing screeners/no follow-up and the 16 affirmative unpaid/exchange screeners without worker counts remain unresolved; they are not treated as nonparticipation. Wave 4 accessible-area coverage and task-menu differences qualify cross-wave descriptions. Details are in [docs/SURVEY_DESIGN.md](docs/SURVEY_DESIGN.md), [docs/HARMONISATION.md](docs/HARMONISATION.md), and [docs/LIMITATIONS.md](docs/LIMITATIONS.md).

## Reuse and citation

Use the metadata in `CITATION.cff` when citing this code package. No software licence is attached until the authors have confirmed ownership and third-party permissions. GHS source data and other third-party materials are not licensed by this repository.
