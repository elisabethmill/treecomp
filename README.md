# A comps-based approach for interpreting tree-based predictions with an application to the NFL draft

Elisabeth Millington and Scott Powers

Random forests are a powerful yet opaque machine learning predictive model. This poses challenges for interpretability, particularly in environments where decisions can be very high-stakes, like professional sports. In this paper, we present a method for interpreting random forest predictions in a sports setting through the lens of player comps. Player comps are a common practice in player scouting, where prospects are evaluated based on their perceived similarity to past players. We can define interpretable similarity scores that quantify how much each training observation contributes to a given prediction by leveraging the connection between random forests and adaptive nearest neighbors. A player’s similarity scores can be viewed as quantifiable player comps, and the random forest's prediction can be recovered by taking a weighted average of the outcome variable among those comps, with weights determined by the similarity scores. We apply this methodology to evaluate quarterback prospects in the 2025 National Football League Draft using ESPN’s Total Quarterback Rating (QBR) per season. We show that our approach captures meaningful structure in the data while providing interpretability, allowing us to identify comps for top prospects such as Cam Ward, Jaxson Dart, Shedeur Sanders, and Dillon Gabriel.

## Installation

```R
devtools::install_github("elisabethmill/treecomp")
```
