{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
      ("E":sev:t:msg) -> LogMessage (Error (read sev)) (read t) (unwords msg)
      ("W":t:msg) -> LogMessage Warning (read t) (unwords msg)
      ("I":t:msg) -> LogMessage Info (read t) (unwords msg)
      _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) msgTree = msgTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node left m'@(LogMessage _ ets _) right)
  | ts <= ets = Node (insert m left) m' right
  | otherwise = Node left m' (insert m right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map msg $ filter important sorted
  where sorted = inOrder $ build ms
        important (LogMessage (Error sev) _ _) = sev >= 50
        important _ = False
        msg (LogMessage _ _ m) = m
        msg (Unknown _) = ""
