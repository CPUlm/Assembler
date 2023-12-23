#!/bin/bash

shopt -s nullglob

compilo=$1

compile () {
  #$compilo $1 $2 > /dev/null 2>&1;
  $compilo $1 $2;
}

score=0
max=0

echo -e "\033[32m[----------] lexer\033[0m"
for f in test/lexer/*.ulm; do
    max=`expr $max + 1`;
    $compilo --lex-only $f | diff - "${f%.ulm}.out";
    case $? in
      "1")
      echo -e "\033[31m[  FAILED  ]\033[0m "$f" (tokens mismatch)";;
      "0")
      score=`expr $score + 1`
      echo -e "\033[32m[  PASSED  ]\033[0m "$f;;
      *)
      echo -e "\033[35m[  FAILED  ]\033[0m "$f" (failed for an incorrect reason)";;
    esac
done
echo

echo -e "\033[32m[----------] parser/bad\033[0m"
for f in test/parser/bad/*.ulm; do
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

echo -e "\033[32m[----------] parser/good\033[0m"
for f in test/parser/good/*.ulm; do
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

echo -e "\033[32m[----------] warnings/bad\033[0m"
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

echo -e "\033[32m[----------] warnings/good\033[0m"
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
