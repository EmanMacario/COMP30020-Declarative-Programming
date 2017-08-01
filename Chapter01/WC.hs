-- Program that calculates the number of lines in its input

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"