main = interact respondPalindromes

respondPalindromes contents = unlines (map (\xs ->
                    if isPalindrome xs then "palindrome!" else "not palindrome!") (lines contents))
                    where isPalindrome xs = xs == reverse xs
