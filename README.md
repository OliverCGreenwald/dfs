# DFS

This is adaptation of the techniques mentioned in the paper [Picking Winners Using Integer Programming](http://arxiv.org/pdf/1604.01455v2.pdf) by [David Hunter](http://orc.scripts.mit.edu/people/student.php?name=dshunter), [Juan Pablo Vielma](http://www.mit.edu/~jvielma/), and [Tauhid Zaman](http://zlisto.scripts.mit.edu/home/). 
The original repo can be found here: https://github.com/dscotthunter/Fantasy-Hockey-IP-Code

 

## How to install the required software to run Julia code. 
- [Julia](http://julialang.org/)
  - Download from the site
- [GLPK](https://www.gnu.org/software/glpk/)
  - `brew install homebrew/science/glpk`
- [JuMP](https://github.com/JuliaOpt/JuMP.jl)
  -  `julia> Pkg.add("JuMP")`
- [DataFrames.jl](https://github.com/JuliaStats/DataFrames.jl)
  - `julia> Pkg.add("DataFrames")`
- [GLPKMathProgInterface.jl](https://github.com/JuliaOpt/GLPKMathProgInterface.jl)
  - `julia> Pkg.add("GLPKMathProgInterface")`


## Running code
Enter the `/optimizationCode/` directory 
Run `exec '/Applications/Julia-0.4.6.app/Contents/Resources/julia/bin/julia'` in terminal to start Julia
Run
```julia
julia> include("lineupGeneration.jl")
```

## Organization of Reposity: (as of 9/19/16)
1. `exampleCodeFromPaper`
  - Contents of https://github.com/dscotthunter/Fantasy-Hockey-IP-Code
  - Was used as a reference 
2. `correlationAnalysis`
  - All work before we started working on the actual lineup generation.
  - The files in this folder all look to see which *stacks* are viable in Daily Fantasy Football 
3. `optimizationCode` 
  - Holds the `Julia` code that writes our lineups. (`lineupGeneration.jl`)
  - Allows us to clean the `Rotogrinders Weekly Projections` and `DraftKings Player Salaries+ID` csv's. (`cleanPlayerData.R`)
  - Sanity Check of our proposed lineup exposures (`calculateExposure.R`)
4. `testingLineups`
  - Before entering *Week 1 2016* our basic testing of any data we could find was done here. 
5. `resultsAnalysis`
  - Any analysis of our weekly results in the GPP Draftkings contests are found here


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
