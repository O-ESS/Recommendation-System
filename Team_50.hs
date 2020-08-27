module DataFile where

import System.Random
import System.IO.Unsafe

users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory =  [
                        ("user1", [["item1", "item2", "item3"], ["item1", "item2", "item4"]]),
                        ("user2", [["item2", "item5"], ["item4", "item5"]]),
                        ("user3", [["item3", "item2"]]),
                        ("user4", [])
                    ]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]) :createEmptyFreqList xs 
----------------------------------------------------------------------------------------------------------
getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]

getAllUsersStats [] = []
getAllUsersStats ((a,b):xs) = (a,listTheItems items b):getAllUsersStats xs
--ListTheItems
listTheItems [] b = []
listTheItems (x:xs) b = (x,anotherItems x b items):listTheItems xs b
--AnotherItems
anotherItems x m [] = []
anotherItems item b (x:xs) = if (item == x || countOcc item x b == 0)then anotherItems item b xs 
							 else  (x,countOcc item x b):anotherItems item b xs 
--countOcc
countOcc item m []=0
countOcc item m (x:xs) = if(elem item x && elem m x)then 1+ countOcc item m xs
						 else countOcc item m xs 
----------------------------------------------------------------------------------------------------------
purchasesIntersection :: Eq a => [(a,[(a,Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a,Int)])]]
purchasesIntersection a b =  getinters  b a 

getUserStats user []=[]
getUserStats user ((a,b):xs) = if a==user then b else getUserStats user xs
--
getinters  [] i = []
getinters  ((a,b):xs)  i = itemsInters i b :getinters xs i  
--itemsInters
itemsInters [] _ = [] 
itemsInters ((a,[]):xs) l = itemsInters xs l
itemsInters ((a,b):xs) l = if containsitem a l then (a,helper_1 b m):itemsInters xs l else itemsInters xs l where m = getlist a l
--getlist
getlist _ [] = []
getlist item ((a,b):xs) = if a==item then b else getlist item xs 
--containsitem
containsitem _ []= True
containsitem item ((a,b):xs) = if a == item && b ==[] then False else containsitem item xs 
--helper_1
helper_1 [] x = x
helper_1 ((a,c):xs) l = (a,getcount a c l ):helper_1 xs j  where j = removeItem a l
-- getcount
getcount a c []= c
getcount a c ((m,b):xs)= if a == m then c+b else getcount a c xs  
--removeItem 
removeItem a [] =[]
removeItem a ((m,b):xs)=if a==m then xs else (m,b):removeItem a xs
-------------------------------------------------------------------------------------------------------------   
freqListItems:: String -> [(String, Int)]
freqListItems a = freqListItems1 a purchasesHistory

freqListItems1 a [] = [] 
freqListItems1 a ((b,c):xs) = if a==b
                              then getFrequency c items
							  else freqListItems1 a xs

getFrequency c [] = []
getFrequency c (x:xs) = if countOcc1 c x == 0
                        then   getFrequency c xs 
					    else   (x,countOcc1 c x):getFrequency c xs
							 
countOcc1 [] x = 0
countOcc1 (xe:xs) x = if elem x xe
                      then length xe -1 + countOcc1 xs x 
					  else countOcc1 xs x
------------------------------------------------------------------------------------------------------------
freqListCart:: String ->[String] -> [(String, Int)]
freqListCart a b =  compress1 (freqListCart1 (getCart a purchasesHistory) b) items

getCart a [] = [] 
getCart a ((b,c):xs) = if a==b
                              then c
							  else getCart a xs

freqListCart1 c b = (concat (longList c b))

longList c [] = []
longList c (x:xs) = concat (getList c x):longList c xs

getList [] x = []
getList (xe:xs) x = if elem x xe
                    then (removeElem xe x):getList xs x
					else getList xs x
--changed it from putElem to removeElem because im removing it
removeElem [] x = []
removeElem (xe:xs) x = if x==xe
                    then removeElem xs x
					else xe:removeElem xs x
					
compress1 a [] = [] 
compress1 a (x:xs) = if getOcc a x ==0
                     then compress1 a xs
					 else (x,getOcc a x):compress1 a xs
					 
getOcc [] x = 0
getOcc (xe:xs) x = if x == xe
                   then 1+getOcc xs x
				   else getOcc xs x
-------------------------------------------------------------------------------------------------------------
freqListCartAndItems:: String -> [String] -> [(String, Int)] 
freqListCartAndItems a c = sum2 (freqListCart a c) (freqListItems a) items

sum2:: [(String, Int)]  -> [(String, Int)]  -> [String] -> [(String, Int)]  
sum2 a b [] = []
sum2 a b (x:xs) = if (getA a x) + (getB b x) ==0
                  then sum2 a b xs
                  else (x,getA a x + getB b x):(sum2 a b xs)

getA [] x = 0
getA ((a,b):xs) x = if x == a
                    then b
                    else getA xs x

getB [] x = 0
getB ((a,b):xs) x = if x == a
                    then b
                    else getB xs x			
--------------------------------------------------------------------------------------------------------------
freqListUsers:: String -> [(String, Int)] 

getCurrentUserList a = getCart a (getAllUsersStats purchasesHistory) --by returning only currentUser from getAllUsersStats
getOtherUsersList a = removeCart a (getAllUsersStats purchasesHistory) --by removing currentUser from getAllUsersStats

removeCart a [] = [] 
removeCart a ((b,c):xs) = if a==b
                          then removeCart a xs
						  else (b,c):removeCart a xs
						 
						 
--Iterate over CONCATINATED list returned by purchasesIntersection && items && count the total frequencies 
freqListUsers a = iterate1 (removeListTitle a) items

removeListTitle a = concat(remove1 (purchasesIntersection (getCurrentUserList a) (getOtherUsersList a)))
remove1 [] = []
remove1 (x:xs) = concat(remove2 x): remove1 xs
remove2 [] = []
remove2 (y:ys) = remove3 y:remove2 ys
remove3 (a,b)= b

-- condition /=0 because without it would return elements with frequency 0, not required 
iterate1 _ [] = []
iterate1 a (y:ys) = if sumFrequencies (iterate2 a y) /=0 then (y,sumFrequencies (iterate2 a y)) : iterate1 a ys else iterate1 a ys
iterate2 [] _ = []
iterate2 ((a,b):xs) y = if a == y 
						then b:iterate2 xs y
						else iterate2 xs y

sumFrequencies [] = 0
sumFrequencies (x:xs) = x + (sumFrequencies xs)
--------------------------------------------------------------------------------------------------------------
--generates List with the items from freq list
genList []=[]
genList ((a,b):xs) = itemList a b ++ genList xs
-- make copies of one item 
itemList a 0 = []
itemList a b = a:itemList a (b-1)

recommendEmptyCart :: String -> String
recommendEmptyCart user = if (length list) ==0 then [] 
						  else list!!i where (list,i) =(genList (freqListItems user),randomZeroToX ((length list)-1))
---------------------------------------------------------------------------------------------------------------
recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user c = if (length list) ==0 then [] 
									 else list!!i where (list,i) =(genList (freqListCartAndItems user c),randomZeroToX ((length list)-1))
---------------------------------------------------------------------------------------------------------------
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user = if (length list) ==0 then [] 
						     else list!!i where (list,i) =(genList (freqListUsers user),randomZeroToX ((length list)-1))
---------------------------------------------------------------------------------------------------------------
recommend :: String -> [String] -> String
--I check if user doesn't have a history then I choose a random item from items list else I call the helper method
recommend user cart  = if (getCart user purchasesHistory) ==[] then items !!(randomZeroToX ((length items)-1)) else recommendHelper user cart

--If user has a history & cart is empty then I call recommendHelper2 else recommendHelper3 
recommendHelper user cart = if cart==[] then recommendHelper2 user 
							else recommendHelper3 user cart
							
--Cart is empty, randomly choose between recommendEmptyCart & recommendBasedOnUsers
recommendHelper2 user = if randomZeroToX(1) == 0 then  recommendEmptyCart user 
							else recommendBasedOnUsers user
							
--Cart is not empty, randomly choose between recommendBasedOnItemsInCart & recommendBasedOnUsers
recommendHelper3 user cart = if randomZeroToX(1) == 0 then  recommendBasedOnItemsInCart user cart
							else recommendBasedOnUsers user

