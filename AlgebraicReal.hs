--AlgebraicReal.hs
--Copyright Laurence Emms 2018
--Algebraic data type for real numbers

module AlgebraicReal (AReal(..),
                      splitDigits,
                      addDigits,
                      addAReal,
                      zero,
                      makeAReal,
                      printAReal,
                      printARealClean) where

--Algebraic real numbers are (potentially infinite) lists of digits where the head
--of the list is adjacent to the decimal point
data AReal = AReal [Char] [Char] deriving (Show, Eq, Ord)

splitDigits :: [Char] -> (Char, Char)
splitDigits [ones] = ('0', ones)
splitDigits [tens, ones] = (tens, ones)

addDigits :: Char -> Char -> (Char, Char)
addDigits d0 d1 = splitDigits (show ((read [d0]) + (read [d1])))

--Tuple is (digit-list, carry)
addDigitList :: [Char] -> [Char] -> ([Char], Char) -> ([Char], Char)
addDigitList [] [] (is, c) = (is, c)
addDigitList (i0 : i0s) [] (is, c) = addDigitList i0s [] (d1 : is, d0)
    where (d0, d1) = splitDigits (show ((read [i0]) + (read [c])))
addDigitList [] (i1 : i1s) (is, c) = addDigitList [] i1s (d1 : is, d0)
    where (d0, d1) = splitDigits (show ((read [i1]) + (read [c])))
addDigitList (i0 : i0s) (i1 : i1s) (is, c) = addDigitList i0s i1s (d1 : is, d0)
    where (d0, d1) = splitDigits (show ((read [i0]) + (read [i1]) + (read [c])))

addAReal :: AReal -> AReal -> Int -> Int -> AReal
addAReal (AReal i0 f0) (AReal i1 f1) iPrecision fPrecision
    | ic == '0' = AReal (reverse is) fs
    | otherwise = AReal ((reverse is) ++ [ic]) fs
        where (fs, fc) = addDigitList (reverse (take fPrecision (f0 ++ (repeat '0')))) (reverse (take fPrecision (f1 ++ (repeat '0')))) ([], '0')
              (is, ic) = addDigitList (take iPrecision (i0 ++ (repeat '0'))) (take iPrecision (i1 ++ (repeat '0'))) ([], fc)

zero :: AReal
zero = AReal (repeat '0') (repeat '0')

makeAReal :: Integer -> Integer -> AReal
makeAReal integral fractional
    = AReal ((reverse (show integral)) ++ (repeat '0')) ((show fractional) ++ (repeat '0'))

printAReal :: AReal -> Int -> Int -> IO ()
printAReal (AReal integral fractional) integralPrecision fractionalPrecision
    = putStr (reverse (take integralPrecision integral)) >>
      putStr "." >>
      putStr (take fractionalPrecision fractional)

printARealClean :: AReal -> Int -> Int -> IO ()
printARealClean (AReal integral fractional) integralPrecision fractionalPrecision
    = putStr (dropWhile (\x -> x == '0') (reverse (take integralPrecision integral))) >>
      putStr "." >>
      putStr (take fractionalPrecision fractional)
