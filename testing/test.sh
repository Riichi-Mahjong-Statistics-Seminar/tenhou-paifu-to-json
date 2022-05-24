ghc --make -O2 Main.hs
sleep 1s
time ./Main <./sample/test >./sample/test.json
sleep 1s
time ./Main <./sample/test1 >./sample/test1.json
sleep 1s
time ./Main <./sample/pao >./sample/pao.json