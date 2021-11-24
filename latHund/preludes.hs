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