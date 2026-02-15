
-- tri :: Int -> Int
-- tri 0 = 0
-- tri n = n + tri(n - 1)

-- triAcc :: Int -> Int -> Int
-- triAcc 0 acc = acc
-- triAcc n acc = triAcc (n - 1) (acc + n)

duplicateAll :: [a] -> [a]
-- duplicateAll [] = []
-- duplicateAll (x:xs) = x : x : duplicateAll xs

duplicateAllAcc :: [a] -> [a] -> [a]
duplicateAllAcc [] acc = acc
duplicateAllAcc (x:xs) acc = duplicateAllAcc xs (acc ++ [x, x])


duplicateAll xs = duplicateAllAcc xs []