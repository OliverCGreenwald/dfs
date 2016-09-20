# DFS

## Authors:
[**Michael Chiang**](linkedin.com/in/mlchiang): mlchiang@princeton.edu

[**Alan Du**](https://www.linkedin.com/in/alan-du-6525b28a): aydu@princeton.edu


## Note:

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
6. `results.md`
  - Details of our results in 2016 NFL Draftkings Contests.
