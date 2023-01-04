# Vidbid plutus scripts

Repo that contains all the scripts required to build the plutus files containing the onchain code. 

## How to
- get nix shell
```
cd ../../cardano/plutus-apps/
nix-shell
cd ../../vidbid/vidbid-plutus-scripts/
```
- Build plutus file
```
# update dependencies required in this repo
cabal update
#build an executable
cabal build
#Compile into plutus file
cabal run
```