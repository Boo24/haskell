module Main where
    import Queue
    import SimpleQueue
    import FastQ
    import BestQueue
    import System.CPUTime
    
    test t n =do
        s <- getCPUTime
        let t1 = foldl (\q x -> push x q) t [1..n]
        print $ extr t1
        e <- getCPUTime
        print $ fromIntegral (e - s) / 10^12
        
    extr q = if emp q then 0 else (1 + (extr (pop q)))
        
    main = do
        let t1 :: SimpleQueue Int; t1 = create 1
        putStrLn "first test:"
        test t1 1000
        let t2 :: FastQ Int; t2 = create 1
        putStrLn "second test:"
        test t2 100000
        let t3 :: BestQueue Int; t3 = create 1
        putStrLn "third test:"
        test t3 100000