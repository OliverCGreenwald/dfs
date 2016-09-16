# DFS
======================

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

# 2016 Results

## Week 1 (9/10/16)
- Entered 150 Lineups for ($3 a piece) into DraftKings' `NFL $5M FANTASY FOOTBALL MILLIONAIRE [$1M TO 1ST]` 
  - Our top lineup placed 3975 (199.42 points) out of 1436510 entries. (99.7 percentile)
    - Lineup Consisted of `Drew Brees`, `Lamar Miller`, `Spencer Ware`, `Brandin Cooks`, `Michael Crabtree`, `Stefon Diggs`, `Delanie Walker`, `T.J. Yeldon`, `Seahawks Defense`
    - Used formulation = `lineup_Type_3`
      - Feasibility Constraints
      - Defense Constraint
      - QB-WR Stack
      - QB-OpposingWR Stack
- Won `$638.43` (41.78% ROI) 
