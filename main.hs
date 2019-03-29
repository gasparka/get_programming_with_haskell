-- myGCD a b = if remainder == 0
--     then b
--     else myGCD b remainder
--     where remainder = a `mod` b

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b )

-- myTail (_:xs) = xs
-- myTail [] = []

myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs


ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))


collatz 1 = [1]
collatz n = if even n
    then n:collatz (n `div` 2)
    else n:collatz (n*3+1)

myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs