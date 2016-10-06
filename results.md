
# 2016 Results
Lifetime ROI: *(188.43-65+207+83) / 450* = **91.9%**
## Week 1 (9/11/16)
- Entered 150 Lineups for ($3 a piece) into DraftKings' `NFL $5M FANTASY FOOTBALL MILLIONAIRE [$1M TO 1ST]` 
  - Our top lineup placed 3975 (199.42 points) out of 1436510 entries. (99.7 percentile)
    - Lineup Consisted of `Drew Brees`, `Lamar Miller`, `Spencer Ware`, `Brandin Cooks`, `Michael Crabtree`, `Stefon Diggs`, `Delanie Walker`, `T.J. Yeldon`, `Seahawks Defense`
    - Used formulation = `lineup_Type_3`
      - Feasibility Constraints
      - Defense Constraint
      - QB-WR Stack
      - QB-OpposingWR Stack
    - Overlap `4`
    - Projections: Rotogrinders
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
    - Overlap `4`
    - Projections: Rotogrinders
- Won `$385.00` **Net -$65** (-14.44% ROI) 
- *Takeaways: This week we made no changes to the construction of our lineups. The two interesting things we noted were that:* 
  - 1. We believe the distribution our of results are similar between `week 1` and `week 2`. The difference in ROI could be caused by the payout structure being different between the two contests. Week 1 payed out to the top 40% while week 2 only payed out to the top 25% of lineups.
  - 2. It is interesting to note that our top lineup for both weeks scored approximately 200 fps. Our speculation is that this is because an ideal lineup has a couple of low projected players who allow the lineups to gain a huge edge and our greedy algorithm will have a hard time outputting these kinds of lineups. 
  
## Week 3 (9/25/16)
- Entered 150 Lineups for ($3 a piece) into DraftKings' `NFL $1.25M Play-Action [$1,250,000 Guaranteed]` 
  - Our top lineup placed 1762 (204.04 points) out of 490196 entries. (99.64 percentile)
    - Lineup Consisted of `Andrew Luck`, `Christine Michael`, `Shane Vereen`, `Antonio Brown`, `T.Y. Hilton`, `Jarvis Landry`, `Jimmy Graham`, `Travis Benjamin`, `Ravens Defense`
    - Used formulation = `lineup_Type_3`
      - Feasibility Constraints
      - Defense Constraint
      - QB-WR Stack
      - QB-OpposingWR Stack
    - Overlap `4`
    - Projections: Rotogrinders
- Won `$657.00` **Net $207** (46% ROI) 
- *Takeaways: During this week we (1) updated our Projections from Rotogrinders to DailyFantasyNerd as they seem to be more accurate and (2) completed the TestingSuite that allows us to test/tune all of our parameters.*

## Week 4 (10/02/16)
- Entered 150 Lineups for ($3 a piece) into DraftKings' `NFL $1.25M Play-Action [$1,250,000 Guaranteed]` 
  - Our top lineup placed 908 (203.5 points) out of 490196 entries. (99.84 percentile)
    - Lineup Consisted of `Ben Roethlisberger`, `Ezekiel Elliott`, `Mark Ingram`, `Antonio Brown`, `John Brown`, `Robert Woods`, `Jordan Reed`, `Terrance Williams`, `Texans Defense`
    - Used formulation = `lineup_Type_2`
      - Feasibility Constraints
      - Defense Constraint
      - QB-WR Stack
    - Overlap `2`
    - Projections: DailyFantasyNerd
- Won `$533.00` **Net $83** (18.4% ROI) 
- *Notes: Due to a bug that was discovered while submitting entries (Players with `Sr.` or `Jr.` in their name has projections of `0`). Our lineups were optimized on incomplete data. Had we fixed this bug in time, our Lineup would've won $1142, 153.8% ROI* 
- *Takeaways: TBD* 
