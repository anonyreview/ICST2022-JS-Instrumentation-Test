#!/bin/bash

echo 'create example dir for saving results'
echo
mkdir example

echo 'start ncs-js with c8 on port 8080'
echo

pushd EMB-js/rest/ncs
TB=4.5 c8 node src/serverWithCov.js > log_em_ncs_js_8080.txt 2>&1 &

CONTROLLER_PID=$!

popd

sleep 5

echo 'start evomaster for BB with 3.97 minutes'
echo
java  -Xms2G -Xmx4G  -jar jar/evomaster.jar  --problemType=REST --testSuiteSplitType=NONE --outputFormat=JS_JEST --testSuiteFileName=EM_BB_ft_Test --blackBox=True --bbSwaggerUrl=http://localhost:8080/swagger.json --maxTime=238s --statisticsColumnId=ncs-js --seed=1 --outputFolder=example --statisticsFile=example/statistics_ncs_js.csv --snapshotInterval=5 --snapshotStatisticsFile=example/snapshot_ncs_js.csv --appendToStatisticsFile=true --writeStatistics=true

echo "please wait a bit in order to stop sut properly for collecting coverage info"
wait $CONTROLLER_PID

pushd EMB-js/rest/ncs
c8 --reporter=json report
c8 --reporter=json-summary report
popd

echo "DONE"
