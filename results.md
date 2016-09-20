
# 2016 Results

## Week 1 (9/11/16)
- Entered 150 Lineups for ($3 a piece) into DraftKings' `NFL $5M FANTASY FOOTBALL MILLIONAIRE [$1M TO 1ST]` 
  - Our top lineup placed 3975 (199.42 points) out of 1436510 entries. (99.7 percentile)
    - Lineup Consisted of `Drew Brees`, `Lamar Miller`, `Spencer Ware`, `Brandin Cooks`, `Michael Crabtree`, `Stefon Diggs`, `Delanie Walker`, `T.J. Yeldon`, `Seahawks Defense`
    - Used formulation = `lineup_Type_3`
      - Feasibility Constraints
      - Defense Constraint
      - QB-WR Stack
      - QB-OpposingWR Stack
- Won `$638.43` **Net $188.43** (41.78% ROI) 
- *Takeaways: Most importantly there's an issue with the lineup generation and the distribution of assets. `Julio Jones` appeared in over 90% of our lineups. Although he had the highest Projected Points, this is surely too much exposure.* 

## Week 2 (9/18/16)
- Entered 150 Lineups for ($3 a piece) into DraftKings' `NFL $1M Play-Action [$50K TO 1ST]` 
  - Our top lineup placed 1022 (197.18 points) out of 392156 entries. (99.74 percentile)
    - Lineup Consisted of `Dak Prescott`, `C.J. Anderson`, `DeAngelo Williams`, `Jamison Crowder`, `Stefon Diggs`, `Julio Jones`, `Greg Olsen`, `Cole Beasley`, `Broncos Defense`
    - Used formulation = `lineup_Type_3`
      - Feasibility Constraints
      - Defense Constraint
      - QB-WR Stack
      - QB-OpposingWR Stack
- Won `$385.00` **Net -$65** (-14.44% ROI) 
- *Takeaways: This week we made no changes to the construction of our lineups. The two interesting things we noted were that:* 
  - 1. We believe the distribution our of results are similar between `week 1` and `week 2`. The difference in ROI could be caused by the payout structure being different between the two contests. Week 1 payed out to the top 40% while week 2 only payed out to the top 25% of lineups.
  - 2. It is interesting to note that our top lineup for both weeks scored approximately 200 fps. My speculation is that this is because an ideal lineup has a couple of low projected players who allow the lineups to gain a huge edge and our greedy algorithm will have a hard time outputting these kinds of lineups. 
