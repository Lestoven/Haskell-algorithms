insert_sorted :: (Ord a)=> [a]->a->[a]

insert_sorted [] y = [y]
insert_sorted (x:xs) y
    | y>x = x:insert_sorted xs y
    | otherwise = y:x:xs

insertionSort :: (Ord a)=>[a]->[a]
insertionSort [] = []
insertionSort (x:xs) = insert_sorted (insertionSort xs) x
     

main = print(insertionSort [2,1,3,10,6,1,10,99,1,100,2110,1,2,3])