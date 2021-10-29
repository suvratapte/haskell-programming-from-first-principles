-- Exercise

{-
In the URL shortener, an important step was omitted. We’re not checking if we’re
overwriting an existing short code, which is entirely possible despite them
being randomly generated. We can calculate the odds of this by examining the
cardinality of the values.

-- alphaNum = ['A'..'Z'] ++ ['0'..'9']
-- shortyGen =
--  replicateM 7 (randomElement alphaNum)
length alphaNum ^ 7 == 78364164096

So, the problem is, what if we accidentally clobber a previously generated short
URI? There are a few ways of solving this. One is to check to see if the short
URI already exists in the database before saving it and throwing an error if it
does. This is going to be vanishingly unlikely to happen unless you’ve suddenly
become a very popular URI shortening service, but it’d prevent the loss of any
data. Your exercise is to devise some means of making this less likely. The
easiest way would be to simply make the short codes long enough that you’d need
to run a computer until the heat death of the universe to get a collision, but
you should try throwing an error in the first handler we showed you first.
-}

-- Solution: the `handleSaveSafe` function in `shawty/app/Main.hs`.
