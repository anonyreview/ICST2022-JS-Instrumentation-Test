#!/usr/bin/env python3

## this script uses some code form https://github.com/EMResearch/EMB/blob/master/scripts/dist.py

EVOMASTER_VERSION = "1.2.2-SNAPSHOT"

import os
import shutil
import platform
from subprocess import run
from os.path import expanduser


### Environment variables ###

HOME = expanduser("~")
SCRIPT_LOCATION = os.path.dirname(os.path.realpath(__file__))
PROJ_LOCATION = os.path.abspath(SCRIPT_LOCATION)

SHELL = platform.system() == 'Windows'

def buildEvoMaster() :

    mvnres = run(["mvn", "clean", "install", "-DskipTests"], shell=SHELL, cwd=os.path.join(PROJ_LOCATION, "EvoMaster"))
    mvnres = mvnres.returncode

    if mvnres != 0:
        print("\nERROR: Maven command failed for building EvoMaster")
        exit(1)

# build evomaster
buildEvoMaster()

def buildJS(path, name):
    print("Building '"+name+"' from " + path)
    res = run(["npm", "install"], shell=SHELL, cwd=path).returncode
    if res != 0:
        print("\nERROR installing packages with NPM in " + path)
        exit(1)
    res = run(["npm", "run", "build"], shell=SHELL, cwd=path).returncode
    if res != 0:
        print("\nERROR when building " + path)
        exit(1)

# build js driver
buildJS(os.path.abspath(os.path.join(PROJ_LOCATION, "EvoMaster","client-js","evomaster-client-js")), "evomaster-client-js")

# build js case studies
buildJS(os.path.abspath(os.path.join(PROJ_LOCATION, "EMB-js","rest","ncs")), "ncs")
buildJS(os.path.abspath(os.path.join(PROJ_LOCATION, "EMB-js","rest","scs")), "scs")
buildJS(os.path.abspath(os.path.join(PROJ_LOCATION, "EMB-js","rest","cyclotron")), "cyclotron")
buildJS(os.path.abspath(os.path.join(PROJ_LOCATION, "EMB-js","rest","disease-sh-api")), "disease-sh-api")
buildJS(os.path.abspath(os.path.join(PROJ_LOCATION, "EMB-js","rest","nestjs-realworld-example-app")), "nestjs-realworld-example-app")


print("\n\nSUCCESS\n\n")
