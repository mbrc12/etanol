#!/bin/sh


mkdir TestSuite
mkdir ~/.etanol

echo "Downloading stuff.."
wget http://central.maven.org/maven2/com/google/guava/guava/23.0/guava-23.0.jar -O TestSuite/guava.jar
wget https://cdnverify.eta-lang.org/eta-binaries/etanol/rt_java7.jar -O TestSuite/rt7.jar
wget https://cdnverify.eta-lang.org/eta-binaries/etanol/rt_java8.jar -O TestSuite/rt8.jar
wget https://raw.githubusercontent.com/mbrc12/etanol/api/resources/config -O ~/.etanol/config

mkdir TestSuite/guava
mkdir TestSuite/rt7
mkdir TestSuite/rt8

echo "Unzipping stuff.."
unzip TestSuite/guava.jar -d TestSuite/guava/
unzip TestSuite/rt7.jar -d TestSuite/rt7/
unzip TestSuite/rt8.jar -d TestSuite/rt8/

mkdir Output

echo "Building"

stack build

echo "Testing Java 7 : java/lang"
stack exec -- etanolx -a TestSuite/rt7/java/lang/ -s "" -o Output/rt7javalang.db

echo "Testing Java 8 : java/lang"
stack exec -- etanolx -a TestSuite/rt8/java/ -s "" -o Output/rt8java.db

echo "Copying to tests/Tests"
cp Output/rt8java.db test/Tests/java.db  

echo "Performing stack test"
stack test
 
echo "Testing Java 8 : javax"
stack exec -- etanolx -a TestSuite/rt8/javax/ -s "Output/rt8java.db" -o Output/rt8javax.db

echo "Testing Google Guava 25"
stack exec -- etanolx -a TestSuite/guava/ -s "Output/rt8java.db  Output/rt8javax.db" -o Output/guava.db

echo "Cleaning.."
rm -rf TestSuite
rm -rf Output
