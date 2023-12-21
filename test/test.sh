#!/bin/bash

shopt -s nullglob

compilo=$1

compile () {
  #$compilo $1 $2 > /dev/null 2>&1;
  $compilo $1 $2;
}

score=0
max=0

echo -e "\033[32m[----------] BAD\033[0m"
for f in test/bad/*.ulm; do
    max=`expr $max + 1`;
    compile $f;
    case $? in
      "0")
      echo -e "\033[31m[  FAILED  ]\033[0m "$f" (should fail)";;
      "1")
      score=`expr $score + 1`
      echo -e "\033[32m[  PASSED  ]\033[0m "$f;;
      *)
      echo -e "\033[35m[  FAILED  ]\033[0m "$f" (failed for an incorrect reason)";;
    esac
done
echo

echo -e "\033[32m[----------] GOOD\033[0m"
for f in test/good/*.ulm test/exec/*.ulm; do
    max=`expr $max + 1`;
    compile $f;
    case $? in
      "1")
      echo -e "\033[31m[  FAILED  ]\033[0m "$f" (should succeed)";;
      "0")
      score=`expr $score + 1`
      echo -e "\033[32m[  PASSED  ]\033[0m "$f;;
      *)
      echo -e "\033[35m[  FAILED  ]\033[0m "$f" (failed for an incorrect reason)";;
    esac
done
echo

echo -e "\033[32m[----------] WARNINGS BAD\033[0m"
for f in test/warnings/bad/*.ulm; do
    max=`expr $max + 1`;
    compile --fatal-warnings $f;
    case $? in
      "0")
      echo -e "\033[31m[  FAILED  ]\033[0m "$f" (should fail)";;
      "1")
      score=`expr $score + 1`
      echo -e "\033[32m[  PASSED  ]\033[0m "$f;;
      *)
      echo -e "\033[35m[  FAILED  ]\033[0m "$f" (failed for an incorrect reason)";;
    esac
done
echo

echo -e "\033[32m[----------] WARNINGS GOOD\033[0m"
for f in test/warnings/good/*.ulm; do
    max=`expr $max + 1`;
    compile --fatal-warnings $f;
    case $? in
      "1")
      echo -e "\033[31m[  FAILED  ]\033[0m "$f" (should succeed)";;
      "0")
      score=`expr $score + 1`
      echo -e "\033[32m[  PASSED  ]\033[0m "$f;;
      *)
      echo -e "\033[35m[  FAILED  ]\033[0m "$f" (failed for an incorrect reason)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;
echo -n "Result: $score/$max : $percent%";

echo

if [[ "$score" != "$max" ]]; then
  exit 1
fi
