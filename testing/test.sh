ghc --make -O2 Main.hs
time runhaskell Main.hs <./sample/test >./sample/test.json
time runhaskell Main.hs <./sample/test1 >./sample/test1.json
time runhaskell Main.hs <./sample/pao >./sample/pao.json