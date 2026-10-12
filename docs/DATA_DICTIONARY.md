# Data dictionary

The repository does not redistribute GHS microdata. This is a construction dictionary for the released code.

| Concept | Source role | Released construction |
|---|---|---|
| Household identifier | Cover, roster, PHL1, PHL2 | Join key only; never released in outputs |
| Worker-crop arrangement | PHL1 or PHL2 plus crop code | Analytic unit after eligibility and de-duplication |
| Task flags | PHL1/PHL2 post-harvest task questions | Multiple-response task indicators; never treated as exclusive shares |
| `person_hours` | Reported worker count, days, hours | Household-member: days × usual hours; external group: worker count × days × typical hours |
| Labour arrangement | PHL1/PHL2 structure | Household member, hired paid, or outside-household unpaid/exchange |
| Weights | Cover file | `wt_wave4` (Wave 4), `wt_cross_wave5` (Wave 5) |
| Design variables | Cover file | Released cluster and strata fields |

The exact source filenames, module roles, expected bytes, and SHA-256 values are in [SOURCE_MANIFEST.csv](SOURCE_MANIFEST.csv).
