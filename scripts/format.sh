stylish-haskell . -ri

for i in $(find src -type f -name "*.hs")
do
   cat $i | hindent >> "hindent_$($i)"
   rm $i
   mv "hindent_$($i)" $i
done