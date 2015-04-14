{-# OPTIONS_GHC -Wall #-}
{-
https://blog.cloudflare.com/a-relatively-easy-to-understand-primer-on-elliptic-curve-cryptography/

http://www.reddit.com/r/programming/comments/1p7f1g/the_first_explanation_of_elliptic_curve/cczsr8a?context=2
-}

module Foo where

{-
  example:
  pmax = 91 = 7 * 13
  pk   = 5
  sk   = 29

  ax + by = gcd(a, b)
  a = pk
  x = sk
  b = totient(n) = phi(n)
  y = multiples as pk * sk = 1 mod totient(n) = k
  pk * sk + totient(n) * y = 1
  5x + (6 * 12)y = 1

  n = 7 * 13 = 91
  a ^ Prime `mod` Prime = 1
  phi(7 * 13) = (7 - 1) * (13 - 1)
  a ^ phi(n) = 1 mod n if gcd(a, n) = 1 (relative prime)
  m ^ x ^ y `mod` n = m
  m ^ x ^ y = m ^ (x * y) = m * (m ^ (x * y - 1))
  m ^ (x * y - 1) = m ^ (phi(n) ^ k) = 1
  5x + 72 y = 1
  (x * y - 1) `mod` phi(n) = 0
-}
sk :: Int -> Int -> Int -> Int
sk pk f1 f2 = let totient = (f1 - 1) * (f2 - 1)
               in euc pk totient

gcdList :: Int -> Int -> [Int]
gcdList a b | b == 0 = [a]
            | otherwise     =  a : (gcdList b (a `mod` b))

{-
http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
-}
euc :: Int -> Int -> Int
euc a b = euc' [] [b, a] [0, 1] [1, 0]
  where
    euc' :: [Int] -> [Int] -> [Int] -> [Int] -> Int
    euc' q r@(r1:r0:_) s@(s1:s0:_) t@(t1:t0:_)
         | r1 == 0 = s0
         | otherwise = let q2 = r0 `div` r1
                           r2 = r0 - q2 * r1
                           s2 = s0 - q2 * s1
                           t2 = t0 - q2 * t1
                        in euc' (q2:q) (r2:r) (s2:s) (t2:t)
    euc' _ _ _ _ = undefined

enc :: Integer -> Integer -> Integer -> Integer
enc = modMul

dec :: Integer -> Integer -> Integer -> Integer
dec = enc

{-
modMul :: Int -> Int -> Int -> Int -> Int
modMul _ pmax 0 acc = acc
modMul n pmax k acc = modMul n pmax (k - 1) ((acc * n) `mod` pmax)
-}
modMul :: Integer -> Integer -> Integer -> Integer
modMul pmax k n = n ^ k `mod` pmax

testEnc :: Integer -> Integer
testEnc n = enc 91 5 n

testDec :: Integer -> Integer
testDec n = dec 91 29 n
