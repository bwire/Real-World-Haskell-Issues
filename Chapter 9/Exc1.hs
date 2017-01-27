-- 1. What should you pass to traverse to traverse a directory tree in reverse alphabetic order?
orderF :: [Info] -> [Info]
orderF = reverse

-- or rather
orderF :: [Info] -> [Info]
orderF = reverse . sort