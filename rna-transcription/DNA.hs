module DNA where 

transcribe :: Char -> Char
transcribe c = case c of
		'G' -> 'C'
		'C' -> 'G'
		'T' -> 'A'
		'A' -> 'U'
		_   -> error $ "Unknown nucleotide " ++ show c

toRNA :: String -> String
toRNA = map transcribe
