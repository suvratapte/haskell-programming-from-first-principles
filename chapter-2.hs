-- Chapter 2 exercises

-- Exercises: Comprehension check

-- 1 - No change.

-- 2
areaOfCircle radius = 3.14 * radius * radius

-- 3
areaOfCircleBetterVersion radius = pi * radius * radius


-- Exercises: Paranthesis and association

-- 1 - No
-- 2 - Yes
-- 3 - No

-- Exercises: Heal the Sick

-- 1
-- let area x = 3.14 * (x * x)

-- 2
-- Answer: let double x = x * 3

-- 3
-- x = 7
-- y = 10
-- f = x + y

-- Exercises: A Head Code

-- 1 - let x = 5 in x - 5
-- 2 - let x = 5 in x * x - 25
-- 3 - let x = 5; y = 6 in x * y - 30
-- 4 - let x = 3; y = 1000 in x + 3 - 6

-- 1
example1 = x * 3 + y where x = 3; y = 1000
-- 2
example2 = x * 5 where x = 10 * 5; y = 10
-- 3
example3 = z / x + y where x = 7; y = negate x; z = y * 10


-- 2.11 Chapter exercises

-- Paranthesization

-- example in the book (what I think it should be)
-- (2 + (2 * 3)) - 3

-- 1
-- (2 + (2 * 3)) - 1

-- 2
-- (^) 10 $ (1 + 1)

-- 3
-- ((2 ^ 2) * (4 ^ 5)) + 1

-- Equivalent expressions

-- 1 - Yes
-- 2 - Yes
-- 3 - No
-- 4 - No
-- 5 - No

-- More fun with functions

z = 7

y = z + 8

x = y ^ 2

waxOn = x * 5

waxOnWithWhere = x * 5 where x = y ^ 2

triple x = x * 3

waxOff x = triple x
