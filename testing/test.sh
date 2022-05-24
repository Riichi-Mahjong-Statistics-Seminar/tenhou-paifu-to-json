ghc --make -O2 Main.hs
time ./Main <./sample/test >./sample/test.json
time ./Main <./sample/test1 >./sample/test1.json
time ./Main <./sample/pao >./sample/pao.json
time ./Main <./sample/fukugou >./sample/fukugou.json