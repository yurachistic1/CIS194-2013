module LogAnalysis where

import Log
import Text.Read (readMaybe)

-- Part One --

readMInt :: String -> Maybe Int
readMInt = readMaybe


parseIW :: MessageType -> Maybe TimeStamp -> [String] -> LogMessage
parseIW msgT (Just t) msg = LogMessage msgT t (unwords msg)
parseIW _ _ msg           = Unknown (unwords msg)


parseE :: Maybe Int -> Maybe TimeStamp -> [String] -> LogMessage
parseE (Just code) (Just t) msg = LogMessage (Error code) t (unwords msg)
parseE _ _ msg                  = Unknown (unwords msg)


parseString :: String -> LogMessage
parseString logLine
    | ("I" : t : msg)       <- lst = parseIW Info (readMInt t) msg
    | ("W" : t : msg)       <- lst = parseIW Warning (readMInt t) msg
    | ("E" : e : t : msg)   <- lst = parseE (readMInt e) (readMInt t) msg
    | otherwise                    = Unknown logLine
    where 
        lst = words logLine


parse :: String -> [LogMessage]
parse log = map parseString (lines log)

-- Part Two --

insert :: LogMessage -> MessageTree -> MessageTree
insert valid@(LogMessage _ t1 _ ) (Node n1 m@(LogMessage _ t2 _) n2)
    | t1 < t2                               = Node (insert valid n1) m n2
    | otherwise                             = Node n1 m (insert valid n2)
insert valid@(LogMessage _ _ _ ) (Leaf)     = Node Leaf valid Leaf 
insert msg tree                             = tree


build :: [LogMessage] -> MessageTree
build list = foldr insert Leaf list


inOrder :: MessageTree -> [LogMessage]
inOrder (Node r logM l) = inOrder r ++ [logM] ++ inOrder l
inOrder Leaf            = [] 


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = map extractLogMessage (filter checkError orderedList)
    where orderedList = (inOrder . build) list


checkError :: LogMessage -> Bool
checkError (LogMessage (Error e) _ _)
    | e > 50    = True
    | otherwise = False
checkError _  = False

extractLogMessage :: LogMessage -> String
extractLogMessage (LogMessage _ _ msg)  = msg
extractLogMessage log                   = "Invalid Log"