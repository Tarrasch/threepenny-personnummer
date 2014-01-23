## Install & Run

    cabal install --reinstall threepenny-gui -fbuildExamples
    ghc --make PersonNummer.hs
    ./PersonNummer &
    firefox http://0.0.0.0:10000/

## Other info

    ghci PersonNummer.hs
    *Main> [ (gender, head $ filter (flip validate gender) ["123456" ++ show x | x <- [0..]]) | gender <- [Male, Female]]
    [(Male,"1234561015"),(Female,"1234561007")]
