#!/bin/bash

# script to convert rolled-up MEEDE data files (from R) to gdx data files
# Note: must use git bash (unix) to run this in the terminal

# Use option -o to look for data for the 1-period PE run
ONLY2020="FALSE"

# Use the option -s to run the PE model with shocked fuel prices
FUEL_SHOCK="FALSE"

while getopts 'os' opt
do
    case $opt in 
        o) ONLY2020="TRUE" ;;
        s) FUEL_SHOCK="TRUE"
    esac
done

## ---New data from MEEDE -----------------------------------------

if [ $ONLY2020 = "TRUE" ]
then
    echo "Gathering Data for 1-period PE run..."
    ## ---Updated Data to remove all years but 2020-----------------------------
    ## --- Region Level --- 
    # biosupply
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_biosupply_2020.csv output=../Data/output/biosupply_2020.gdx index=1..3 values=lastCol useHeader=Y id=biosupply

    # capcost
    #../26.1/csv2gdx.exe ../Data/updated_data/PE_capcost_2020.csv output=../Data/output/capcost_2020.gdx index=1..3 values=lastCol useHeader=Y id=capcost

    # dadj
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_dadj_2020.csv output=../Data/output/dadj_2020.gdx index=1..2 values=lastCol useHeader=Y id=dadj

    # pbio
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pbio_2020.csv output=../Data/output/pbio_2020.gdx index=1..3 values=lastCol useHeader=Y id=pbio

    # peak
    #../26.1/csv2gdx.exe ../Data/updated_data/PE_peak_2020.csv output=../Data/output/peak_2020.gdx index=1..2 values=lastCol useHeader=Y id=peak

    # pele
    #../26.1/csv2gdx.exe ../Data/updated_data/PE_pele_2020.csv output=../Data/output/pele_2020.gdx index=1..2 values=lastCol useHeader=Y id=pele

    ## --- State Level ---
    # biosupply
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_biosupply_2020_STATE.csv output=../Data/output/biosupply_2020_STATE.gdx index=1..3 values=lastCol useHeader=Y id=biosupply

    # capcost
    #../26.1/csv2gdx.exe ../Data/updated_data/PE_capcost_2020.csv output=../Data/output/capcost_2020.gdx index=1..3 values=lastCol useHeader=Y id=capcost

    # dadj
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_dadj_2020_STATE.csv output=../Data/output/dadj_2020_STATE.gdx index=1..2 values=lastCol useHeader=Y id=dadj

    # pbio
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pbio_2020_STATE.csv output=../Data/output/pbio_2020_STATE.gdx index=1..3 values=lastCol useHeader=Y id=pbio

    # peak
    #../26.1/csv2gdx.exe ../Data/updated_data/PE_peak_2020.csv output=../Data/output/peak_2020.gdx index=1..2 values=lastCol useHeader=Y id=peak

    # pele
    #../26.1/csv2gdx.exe ../Data/updated_data/PE_pele_2020.csv output=../Data/output/pele_2020.gdx index=1..2 values=lastCol useHeader=Y id=pele
else
    echo "Gathering Data for multi-period PE run..."
    ## ---Updated Data to remove 2010,2015 years-----------------------------
    # biosupply
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_biosupply.csv output=../Data/output/biosupply_2020.gdx index=1..3 values=lastCol useHeader=Y id=biosupply

    # capcost
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_capcost.csv output=../Data/output/capcost_2020.gdx index=1..3 values=lastCol useHeader=Y id=capcost

    # dadj
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_dadj.csv output=../Data/output/dadj_2020.gdx index=1..2 values=lastCol useHeader=Y id=dadj

    # pbio
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pbio.csv output=../Data/output/pbio_2020.gdx index=1..3 values=lastCol useHeader=Y id=pbio

    # peak
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_peak.csv output=../Data/output/peak_2020.gdx index=1..2 values=lastCol useHeader=Y id=peak

    # peak
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pele.csv output=../Data/output/pele_2020.gdx index=1..2 values=lastCol useHeader=Y id=pele
fi

## These files have the same names regardless of the model run
## --- Region Level ---
# capacity
../26.1/csv2gdx.exe ../Data/updated_data/PE_capacity.csv output=../Data/output/MEEDE_capacity.gdx index=1..3 values=lastCol useHeader=Y id=capacity  

# count
#../26.1/csv2gdx.exe ../Data/updated_data/PE_count.csv output=../Data/output/MEEDE_count.gdx index=1..2 values=lastCol useHeader=Y id=count

# heatrate
../26.1/csv2gdx.exe ../Data/updated_data/PE_heatrate.csv output=../Data/output/MEEDE_heatrate.gdx index=1..3 values=lastCol useHeader=Y id=heatrate

# vomcost
../26.1/csv2gdx.exe ../Data/updated_data/PE_vomcost.csv output=../Data/output/MEEDE_vomcost.gdx index=1..2 values=lastCol useHeader=Y id=vomcost

# fomcost
../26.1/csv2gdx.exe ../Data/updated_data/PE_fomcost.csv output=../Data/output/MEEDE_fomcost.gdx index=1..2 values=lastCol useHeader=Y id=fomcost

# size
#../26.1/csv2gdx.exe ../Data/updated_data/PE_size.csv output=../Data/output/MEEDE_size.gdx index=1..3 values=lastCol useHeader=Y id=size

# dele
../26.1/csv2gdx.exe ../Data/updated_data/PE_dele.csv output=../Data/output/MEEDE_dele.gdx index=1..2 values=lastCol useHeader=Y id=dele

## --- State Level ---
# capacity
../26.1/csv2gdx.exe ../Data/updated_data/PE_capacity_STATE.csv output=../Data/output/MEEDE_capacity_STATE.gdx index=1..3 values=lastCol useHeader=Y id=capacity  

# count
#../26.1/csv2gdx.exe ../Data/updated_data/PE_count_STATE.csv output=../Data/output/MEEDE_count.gdx index=1..2 values=lastCol useHeader=Y id=count

# heatrate
../26.1/csv2gdx.exe ../Data/updated_data/PE_heatrate_STATE.csv output=../Data/output/MEEDE_heatrate_STATE.gdx index=1..3 values=lastCol useHeader=Y id=heatrate

# vomcost
../26.1/csv2gdx.exe ../Data/updated_data/PE_vomcost_STATE.csv output=../Data/output/MEEDE_vomcost_STATE.gdx index=1..2 values=lastCol useHeader=Y id=vomcost

# fomcost
../26.1/csv2gdx.exe ../Data/updated_data/PE_fomcost_STATE.csv output=../Data/output/MEEDE_fomcost_STATE.gdx index=1..2 values=lastCol useHeader=Y id=fomcost

# size
#../26.1/csv2gdx.exe ../Data/updated_data/PE_size.csv output=../Data/output/MEEDE_size.gdx index=1..3 values=lastCol useHeader=Y id=size

# dele
../26.1/csv2gdx.exe ../Data/updated_data/PE_dele_STATE.csv output=../Data/output/MEEDE_dele_STATE.gdx index=1..2 values=lastCol useHeader=Y id=dele

# maxCF
../26.1/csv2gdx.exe ../Data/updated_data/PE_maxCF_STATE.csv output=../Data/output/maxCF_STATE.gdx index=1..3 values=lastCol useHeader=Y id=maxCF

# hours
../26.1/csv2gdx.exe ../Data/updated_data/PE_hours_STATE.csv output=../Data/output/hours_STATE.gdx index=1..2 values=lastCol useHeader=Y id=hours

# transmit_limit
../26.1/csv2gdx.exe ../Data/updated_data/PE_transmit_limit_STATE.csv output=../Data/output/transmit_limit_STATE.gdx index=1..2 values=lastCol useHeader=Y id=transmit_limit

# loadpct
../26.1/csv2gdx.exe ../Data/updated_data/PE_loadpct_STATE.csv output=../Data/output/loadpct_STATE.gdx index=1..2 values=lastCol useHeader=Y id=loadpct

# fueltype
../26.1/csv2gdx.exe ../Data/updated_data/PE_fueltype.csv output=../Data/output/fueltype.gdx index=1..2 values=lastCol useHeader=Y id=fueltype

# emis_factor
../26.1/csv2gdx.exe ../Data/updated_data/PE_emis_factor_STATE.csv output=../Data/output/emis_factor_STATE.gdx index=1..3 values=lastCol useHeader=Y id=emis_factor

if [ $FUEL_SHOCK = "TRUE" ]
then
    echo "Using shocked fuel prices..."
    # pf--fuel price
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pf_SHOCK.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf
else
    echo "Using default fuel prices..."
    # pf--fuel price
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pf.csv output=../Data/output/MEEDE_pf.gdx index=1..3 values=lastCol useHeader=Y id=pf

    # pf--fuel price -- state level
    ../26.1/csv2gdx.exe ../Data/updated_data/PE_pf_STATE.csv output=../Data/output/MEEDE_pf_STATE.gdx index=1..3 values=lastCol useHeader=Y id=pf
fi