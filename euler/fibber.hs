import Criterion.Main (Benchmarkable, defaultMain, bench, bgroup)
import Control.Exception (evaluate)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Append this to the end of "Fibber.hs".

instance Benchmarkable (Int -> a) where
    run f u = evaluate (f u) >> return ()

main = defaultMain [
        bgroup "fib" [ bench "fib 10" $ \n -> fib (10+n-n)
                     , bench "fib 35" $ \n -> fib (35+n-n)
                     , bench "fib 37" $ \n -> fib (37+n-n)
                     ]
                    ]

