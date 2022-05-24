ghc --make -O2 Main.hs
time ./main <./sample/test >./sample/test.json
time ./main <./sample/test1 >./sample/test1.json
time ./main <./sample/pao >./sample/pao.json
time ./main <./sample/fukugou >./sample/fukugou.json