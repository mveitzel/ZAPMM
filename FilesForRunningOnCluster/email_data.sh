#!/bin/bash
rm run.zip
zip run.zip *.dat COWS.*
echo "Here is the latest cow data" | mutt -a "run.zip" -s "cow data" -c mveitzel@gmail.com
