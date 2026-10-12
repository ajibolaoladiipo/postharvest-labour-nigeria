# Survey design

Wave 4 uses `wt_wave4`; Wave 5 uses `wt_cross_wave5`. The scripts form stratified, clustered survey designs with the released cover-file cluster and strata fields, `nest = TRUE`, and `survey.lonely.psu = "adjust"`. Eligible arrangements are linked to a valid-weight household frame so households with zero qualifying arrangements remain available to the ratio-estimation frame where required.

The reconstruction validated 2,761 contributing Wave 4 households and 2,612 contributing Wave 5 households, with 437 and 530 PSUs, respectively, and six strata in each wave. The analysis does not claim individual longitudinal transitions.
