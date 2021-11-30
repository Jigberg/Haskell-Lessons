Prelude lathund - does not compile

-- map does a "function" on all the things in a list. Use ( ) around the argument. 
map (+2) [1..10]
>> [3,4,5,6,7,8,9,10,11,12]

-- String to char list using map. Not always necesarry. 
map (:[]) "Hello"
>> ["H","e","l","l","o"]

-- filter removes all of something from a list.
filter even [1..10]
>> [2,4,6,8,10]

-- Returns bool if the list contains argument.
elem 1 [1..10]
>> True

-- Creates a list with an item bool/int/char int many times.
replicate 5 True  or   replicate 5 5
>> [True,True,True,True,True]   [5,5,5,5,5]

-- Takes specific item from list depending on spot.
[1,2,3,4,5]!!2
>> 3

-- Given a list and a bool statement check all values in the list. The bool statement can be a function. 
all (<5) [1,2,3,4]
>> True

-- Returns the "argument" first items of the list. WORKS ON infintive lists!
take 7 [1..10]
>> [1,2,3,4,5,6,7]

-- Works/Fills up the return list until the argument on the return list is met. 
takeWhile (<5) [1..10]
>> [1,2,3,4]

-- Removes the first element of the list
tail [1..10]
>> [2,3,4,5,6,7,8,9,10]

-- Returns the first item
head [1..10]
>> 1

-- Returns the last item
last [1..10]
>> 10

-- Zips two lists together in pairs. Leaves out numbers that does not get a partner. 
zip [1..5] [6..10]
>> [(1,6),(2,7),(3,8),(4,9),(5,10)]

-- Applays the same function over and over again on the starting value. Its infinity.
iterate (*2) 1
>> [1,2,4,8,16,32...]

-- Repeats an arguemnt forever. 
repeat 1
>> [1,1,1,1,1,1..]

-- Repeats the cycle forever. 
cycle [1,2,3]
>> [1,2,3,1,2,3,1,2,3,1,2,3..]

listToMaybe :: [a] -> Maybe a
listToMaybe [1..10]
>> Just 1

listToMaybe []
>> Nothing

-- Takes an item from a list and generates 1 value. a Gen.
elements [1..10]
>> generates of the given values.

-- Chooses 1 of the generator functions in the list based on the weighted value in front. Then calls it. 
frequency [ (9,funcGenerator1) , (1,funcGenerator2) ]
funcGenerator1 ...
funcGenerator2 ...

-- Takes an int and a generator function and creates a list of that int size. 
vectorOf 3 funcGenerator1
>> [funcGenerator1,funcGenerator1,funcGenerator1]