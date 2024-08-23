#!/bin/bash

# script to run a number of model runs under different fuel price scenarios
# Note: must use git bash (unix) to run this in the terminal

# Use option -g or -c or -o assign which fuel price to shock
MOVE_GAS="FALSE"
MOVE_OIL="FALSE"
MOVE_COAL="FALSE"

while getopts 'goc' opt
do
    case $opt in 
        g) MOVE_GAS="TRUE" ;;
        o) MOVE_OIL="TRUE";;
        c) MOVE_COAL="TRUE"
    esac
done

# clear out the necessary directories in preparation
# dump marginal prices to csv'
if [ $MOVE_GAS = "TRUE" ]
then
    rm ../Model/Results/sim_generation/PE_output_generation_gas*
    rm ../Model/Results/sim_wholesale/PE_output_wholesaleprice_gas*
fi

if [ $MOVE_OIL = "TRUE" ]
then
    rm ../Model/Results/sim_generation/PE_output_generation_oil*
    rm ../Model/Results/sim_wholesale/PE_output_wholesaleprice_oil*
fi

if [ $MOVE_COAL = "TRUE" ]
then
    rm ../Model/Results/sim_generation/PE_output_generation_col*
    rm ../Model/Results/sim_wholesale/PE_output_wholesaleprice_col*
fi

#rm ../Model/Results/sim_generation/PE_output_generation*
#rm ../Model/Results/sim_wholesale/PE_output_wholesaleprice*

# run the file conversion script beforehand to generate the default data
./csv_to_gdx_batch.sh -o

# and the optimization...
./run_pe.sh

# and rename the output accordingly
mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_baseline.csv
mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_baseline.csv

# then start the simulations
for i in {1..100}
do
  # convert the ith fuel_price data file from csv to gdx format  
  # csv2gdx ../Data/updated_data/fuel_shock/PE_pf_SHOCK_${i}.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf

  if [ $MOVE_GAS = "TRUE" ]
  then
      csv2gdx ../Data/updated_data/fuel_shock/PE_pf_SHOCK_gas_${i}.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf
      # run the optimization model with that fuel price file
      ./run_pe.sh
      # take the output and rename/move it to a folder for storage
      mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_gas_${i}.csv
      mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_gas_${i}.csv
  fi

  if [ $MOVE_OIL = "TRUE" ]
  then
      csv2gdx ../Data/updated_data/fuel_shock/PE_pf_SHOCK_oil_${i}.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf
      # run the optimization model with that fuel price file
      ./run_pe.sh
      # take the output and rename/move it to a folder for storage
      mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_oil_${i}.csv
      mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_oil_${i}.csv
  fi

  if [ $MOVE_COAL = "TRUE" ]
  then
      csv2gdx ../Data/updated_data/fuel_shock/PE_pf_SHOCK_col_${i}.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf
      # run the optimization model with that fuel price file
      ./run_pe.sh
      # take the output and rename/move it to a folder for storage
      mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_col_${i}.csv
      mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_col_${i}.csv
  fi

  # take the output and rename/move it to a folder for storage
  #mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_${i}.csv
  #mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_${i}.csv

done

