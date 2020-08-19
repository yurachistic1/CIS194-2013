module LogAnalysis where

import Log
import Text.Read (readMaybe)

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
    | ("I" : t : msg) <- lst            = parseIW Info (readMInt t) msg
    | ("W" : t : msg) <- lst            = parseIW Warning (readMInt t) msg
    | ("E" : e : t : msg) <- lst        = parseE (readMInt e) (readMInt t) msg
    | otherwise                         = Unknown logLine
    where 
        lst = words logLine


parse :: String -> [LogMessage]
parse log = map parseString (lines log)

