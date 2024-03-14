#!/bin/bash

# script to run a number of model runs under different fuel price scenarios
# Note: must use git bash (unix) to run this in the terminal

# clear out the necessary directories in preparation
rm ../Model/Results/sim_generation/PE_output_generation*
rm ../Model/Results/sim_wholesale/PE_output_wholesaleprice*

# run the file conversion script beforehand to generate the default data
./csv_to_gdx_batch -o

# and the optimization...
./run_pe.sh

# and rename the output accordingly
mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_baseline.csv
mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_baseline.csv

# then start the simulations
for i in {1..100}
do
  # convert the ith fuel_price data file from csv to gdx format  
  csv2gdx ../Data/updated_data/fuel_shock/PE_pf_SHOCK_${i}.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf

  # run the optimization model with that fuel price file
  ./run_pe.sh

  # take the output and rename/move it to a folder for storage
  mv ../Model/Results/PE_output_generation.csv ../Model/Results/sim_generation/PE_output_generation_${i}.csv
  mv ../Model/Results/PE_output_wholesaleprice.csv ../Model/Results/sim_wholesale/PE_output_wholesaleprice_${i}.csv

done

