-- 2. Using id as a control function, traverse id performs a preorder traversal of a tree: 
-- it returns a parent directory before its children. 
-- Write a control function that makes traverse perform a postorder traversal, in which it returns children before their parent. 

-- The call to traverse should be something like:
traverse (\list -> tail list ++ [head list])

-- To see the result we can use this helper function:
displayPaths :: IO [Info] -> IO ()
displayPaths iol = do
  list <- iol
  mapM_ putStrLn (map infoPath list)