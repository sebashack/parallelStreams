--
-- Derived from a program believed to be originally written by John
-- Launchbury, and incorporating the RSA algorithm which is in the
-- public domain.
--

module RSA where

import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Parallel.Strategies (parBuffer, withStrategy, rdeepseq)
import Data.ByteString.Lazy.Char8 (ByteString)
import ByteStringCompat

encrypt, decrypt :: Integer -> Integer -> ByteString -> ByteString

-- <<encrypt
encrypt n e = B.unlines
            . withStrategy (parBuffer 100 rdeepseq)                             
            . map (B.pack . show . power e n . code)   
            . chunk (size n)                           
-- >>

decrypt n d = B.concat
            . map (B.pack . decode . power d n)
            . integers
            . B.lines

integers :: [ByteString] -> [Integer]
integers bs = [ i | Just (i,_) <- map B.readInteger bs ]

-------- Converting between Strings and Integers -----------

code :: ByteString -> Integer
code = B.foldl' accum 0
  where accum x y = (128 * x) + fromIntegral (fromEnum y)

decode :: Integer -> String
decode n = reverse (expand n)
   where expand 0 = []
         expand x = toEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)

chunk :: Int -> ByteString -> [ByteString]
chunk n xs | B.null xs = []
chunk n xs = as : chunk n bs
  where (as,bs) = B.splitAt (fromIntegral n) xs

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100	-- log_128 10 = 0.4745

------- Constructing keys -------------------------

makeKeys :: Integer -> Integer -> (Integer, Integer, Integer)
makeKeys r s = (p*q, d, invert ((p-1)*(q-1)) d)
   where   p = nextPrime r
           q = nextPrime s
	   d = nextPrime (p+q+1)

nextPrime :: Integer -> Integer
nextPrime a = head (filter prime [odd,odd+2..])
  where  odd | even a = a+1
             | True   = a
         prime p = and [power (p-1) p x == 1 | x <- [3,5,7]]

invert :: Integer -> Integer -> Integer
invert n a = if e<0 then e+n else e
  where  e=iter n 0 a 1

iter :: Integer -> Integer -> Integer -> Integer -> Integer
iter g v 0 w = v
iter g v h w = iter h w (g `mod` h) (v - (g `div` h)*w)

------- Fast exponentiation, mod m -----------------

power :: Integer -> Integer -> Integer -> Integer
power 0 m x          = 1
power n m x | even n = sqr (power (n `div` 2) m x) `mod` m
	    | True   = (x * power (n-1) m x) `mod` m

sqr :: Integer -> Integer
sqr x = x * x

