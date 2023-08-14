bubbleSort :: (Ord a)=>[a]->[a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs)
    | x > y = y:bubbleSort (x:xs)
    | otherwise = x:bubbleSort(y:xs)

isSorted :: (Ord a) => [a] -> Bool
isSorted [x] = True
isSorted (x:y:xs) = if x>y then False else isSorted (y:xs)

bubbleSortMain :: (Ord a)=>[a]->[a]
bubbleSortMain xs = let list = bubbleSort xs in if isSorted list then list else bubbleSortMain list

main = print(bubbleSortMain [2,1,3,10,6,1,100,99,101,2002,10])