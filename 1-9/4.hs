isPalindrome n = reverse digits == digits where digits = show n

maxPalindromeWhen x =
  let res = dropWhile (not . isPalindrome) [x * y| y <- [x,x-1..100]]
  in case res of
    [] -> []
    hd:_ -> [hd]

getAnswer = maximum . concatMap maxPalindromeWhen $ [999,998..100]

