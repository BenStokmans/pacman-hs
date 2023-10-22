for i in $(find src -type f -name "*.hs")
do
   touch "hindent_$($i)"
   cat $i | hindent >> "hindent_$($i)"
   rm $i
   cp "hindent_$($i)" $i
   rm "hindent_$($i)"
done