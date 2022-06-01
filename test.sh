cd src
ghc --make -O2 ./Main.hs
cd ..
time ./src/Main <./samples/test     >./samples/test.json
time ./src/Main <./samples/yakumann >./samples/yakumann.json
time ./src/Main <./samples/pao      >./samples/pao.json
time ./src/Main <./samples/fukugou  >./samples/fukugou.json