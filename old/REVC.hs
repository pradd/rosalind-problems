{-
A DNA string is formed from the alphabet containing A, C, G, and T. The complement of A is T, and the complement of C is G.

The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, 
then taking the complement of each symbol (e.g., the reverse complement of GTCA is TGAC).

Given: A DNA string s of length at most 1000 bp.

Return: The reverse complement of s.

Sample Dataset
AAAACCCGGT
Sample Output
ACCGGGTTTT
-}

input = "AAAACCCGGT"

base_pair 'G' = 'C'
base_pair 'C' = 'G'
base_pair 'T' = 'A'
base_pair 'A' = 'T'

complement = map base_pair

reverse_complement = reverse . complement

main = putStrLn $ reverse_complement input
