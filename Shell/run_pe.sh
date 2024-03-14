#!/bin/bash

# a script to run the partial equilibrium dispatch model
# use the flag -o to run the original, unedited model (multiperiod, more complex)
ORIGINAL="FALSE"

# use the flag -s to run the PE model at the state level instead of the regional level
STATE="FALSE"

while getopts 'os' opt
do
    case $opt in 
        o) ORIGINAL="TRUE" ;;
        s) STATE="TRUE"
    esac
done

if [ $ORIGINAL = "TRUE" ]
then
    echo "Running Original PE model..."
    gams ../Model/model_OG.gms
else
    echo "Running Updated PE model..."
    if [ $STATE = "TRUE" ]
    then 
        echo "Running PE at state level..."
        gams ../Model/model_STATE.gms
    else
        echo "Running PE at regional level..."
        gams ../Model/model.gms
    fi
fi

# dump marginal prices to csv'
if [ $STATE = "TRUE" ]
then
    gdxdump ../Model/Output/pivot_STATE.gdx output=../Model/Results/PE_output_wholesaleprice_STATE.csv format=csv symb=chk_price header="load_segment,region,year,value"
else
    gdxdump ../Model/Output/pivot.gdx output=../Model/Results/PE_output_wholesaleprice.csv format=csv symb=chk_price header="load_segment,region,year,value"
fi