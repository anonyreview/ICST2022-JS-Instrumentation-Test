#!/bin/bash

echo 'create example dir for saving results'
echo
mkdir example

echo 'start EM driver for ncs-js on port 40100'
echo

pushd EMB-js/rest/ncs
EM_PORT=40100 npm run em > log_em_ncs_js_40100.txt 2>&1 &

CONTROLLER_PID=$!

popd

sleep 5

echo 'start evomaster for JS-MIO with 100000 HTTP calls'
echo
java  -Xms2G -Xmx4G  -jar jar/evomaster.jar  --testSuiteSplitType=NONE --outputFormat=JS_JEST --testSuiteFileName=EM_JS_MIO_Test --blackBox=False --algorithm=MIO --stoppingCriterion=FITNESS_EVALUATIONS --maxActionEvaluations=100000 --statisticsColumnId=ncs-js --seed=1 --sutControllerPort=40100 --outputFolder=example --statisticsFile=example/statistics_ncs_js.csv --snapshotInterval=5 --snapshotStatisticsFile=example/snapshot_ncs_js.csv --appendToStatisticsFile=true --writeStatistics=true

echo
echo
echo 'start evomaster for JS-Random with 100000 HTTP calls'
echo
java  -Xms2G -Xmx4G  -jar jar/evomaster.jar  --testSuiteSplitType=NONE --outputFormat=JS_JEST --testSuiteFileName=EM_JS_Random_Test --blackBox=False --algorithm=RANDOM --stoppingCriterion=FITNESS_EVALUATIONS --maxActionEvaluations=100000 --statisticsColumnId=ncs-js --seed=1 --sutControllerPort=40100 --outputFolder=example --statisticsFile=example/statistics_ncs_js.csv --snapshotInterval=5 --snapshotStatisticsFile=example/snapshot_ncs_js.csv --appendToStatisticsFile=true --writeStatistics=true

kill $CONTROLLER_PID