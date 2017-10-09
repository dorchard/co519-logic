import Sat

{-
   Run the examples like the following:
     $ ghci Examples.hs
     > pretty $ sat example

   You can check the assignment in the following way:
     > check example $ sat example
-}

-- Examples
contradict = [[Positive "x"], [Negative "x"]]

taut = [[Positive "x", Negative "x"]]

example =
    [[Positive "x", Negative "y"],
     [Positive "y", Positive "z"],
     [Negative "z", Negative "x"]]

example0 =
    [[Positive "x", Negative "y"],
     [Negative "x", Positive "y"]]

example1 =
    [[Positive "a", Positive "b"]
    ,[Negative "a", Positive "b"]]

example2 =
    [[Positive "a", Positive "b", Positive "c"]
    ,[Negative "a", Positive "b", Positive "c"]
    ,[Negative "a", Negative "b", Positive "c"]
    ,[Negative "a", Negative "b", Negative "c"]]

example3 =
    [[Positive "a", Positive "b", Positive "c"]
    ,[Negative "a", Positive "b", Positive "c"]
    ,[Negative "a", Negative "b", Positive "c"]]

trafficLight =
    [[Negative "r", Positive "g'"]
    ,[Negative "r", Negative "r'"]
    ,[Negative "g", Negative "g'"]
    ,[Negative "g", Positive "r'"]
    ,[Positive "r", Positive "g"]
    ,[Negative "r", Negative "g"]
    ,[Negative "r'", Positive "r'"]
    ,[Negative "r'", Positive "g'"]
    ,[Negative "g'", Positive "r'"]
    ,[Negative "g'", Positive "g'"]]

example4 =
    [[Positive "x", Negative "y"]
    ,[Negative "z", Negative "y"]
    ,[Negative "x", Positive "z", Negative "w"]
    ,[Positive "y", Positive "w"]]

example5 =
    [[Positive "a", Positive "b", Positive "c"]
    ,[Negative "a", Positive "b"]
    ,[Negative "a", Negative "b", Negative "c"]]

-- Run tests
testSuite = all check [contradict, taut, example, example0, example1, example2, example3, trafficLight]