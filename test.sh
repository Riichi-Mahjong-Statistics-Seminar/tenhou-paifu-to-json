cd src
ghc --make -O2 ./Main.hs
cd ..
time ./src/Main <./samples/test.xml     >./samples/test.json
time ./src/Main <./samples/yakumann.xml >./samples/yakumann.json
time ./src/Main <./samples/pao.xml      >./samples/pao.json
time ./src/Main <./samples/fukugou.xml  >./samples/fukugou.json
time ./src/Main <./samples/kakan.xml    >./samples/kakan.json