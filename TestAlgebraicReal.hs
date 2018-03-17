--TestAlgebraicReal.hs
--Copyright Laurence Emms 2018
--Test framework for algebraic reals

import qualified AlgebraicReal

main = --putStrLn ("AddDigits: " ++ [d0, ',', d1]) >>
       putStrLn (show (AlgebraicReal.splitDigits (show ((read "9") + (read "9"))))) >>
       AlgebraicReal.printARealClean (AlgebraicReal.addAReal 
                                         (AlgebraicReal.AReal "1111111111111" "1111111111111")
                                         (AlgebraicReal.AReal "1111111111111" "1111111111111") iPrecision fPrecision) iPrecision fPrecision >>
       putStr "\n" >>
       AlgebraicReal.printARealClean (AlgebraicReal.addAReal 
                                         (AlgebraicReal.AReal "7777777777777" "7777777777777")
                                         (AlgebraicReal.AReal "1111111111111" "5555555555555") iPrecision fPrecision) iPrecision fPrecision >>
       putStr "\n" >>
       AlgebraicReal.printARealClean (AlgebraicReal.addAReal 
                                         (AlgebraicReal.AReal "7777" "7777")
                                         (AlgebraicReal.AReal "1111" "5555") iPrecision fPrecision) iPrecision fPrecision >>
       putStr "\n" >>
       putStr "Infinite + finite: " >>
       AlgebraicReal.printARealClean (AlgebraicReal.addAReal 
                                         (AlgebraicReal.AReal (repeat '2') (repeat '7'))
                                         (AlgebraicReal.AReal "1111" "5555") iPrecision fPrecision) iPrecision fPrecision >>
       putStr "\n" >>
       putStr "Infinite + Infinite: " >>
       AlgebraicReal.printARealClean (AlgebraicReal.addAReal 
                                         (AlgebraicReal.AReal (repeat '2') (repeat '7'))
                                         (AlgebraicReal.AReal (repeat '3') (repeat '1')) iPrecision fPrecision) iPrecision fPrecision >>
       putStr "\n" >>
       putStr "Infinite + Infinite 2: " >>
       AlgebraicReal.printARealClean (AlgebraicReal.addAReal 
                                         (AlgebraicReal.AReal (repeat '2') (repeat '7'))
                                         (AlgebraicReal.AReal (repeat '6') (repeat '3')) iPrecision fPrecision) iPrecision fPrecision >>
       putStr "\n" >>
       putStrLn "Done"
           where iPrecision = 1000
                 fPrecision = 1000
                 integral = toInteger 13408246082356082356 
                 fractional = toInteger 3214697023569087203560
                 aReal = AlgebraicReal.makeAReal integral fractional
                 (d0, d1) = AlgebraicReal.addDigits '9' '3'
