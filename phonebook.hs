-- phonebook.hs

-- generate 5000 people
-- each person has firstname, lastname, 0-3 phonenumbers
-- provide lookups on firstname, lastname, and phonenumber

import System.Random
import Char

numOfPeople = 50000

main = do
	pickAnOption

pickAnOption = do
	putStrLn $ "Pick an option: \n\t1. Generate new phonebook \n\t2. Look up a person"
	num <- getLine 
	if (read num) == 1 
		then do putStrLn "Generating phonebook..."
		else if (read num) == 2 
			then do putStrLn "some new menu:"
			else do putStrLn "invalid choice, try again"
				pickAnOption


generatePhoneBook = do
	gen <- newStdGen
	writeFile "phonebook.txt" (unlines (map show ( listOfPeople numOfPeople gen)))
	putStr $ "Wrote " ++ (show numOfPeople) ++ " numbers to phonebook.txt\n" 

listOfPeople 0 gen = [(randomPerson gen)]
listOfPeople num gen = randomPerson (fst $ split gen) : (listOfPeople (num - 1) (snd $ split gen))

randomPerson gen =  Person (randomName (fst $ split gen)) (randomName (snd $ split gen)) (randomPhoneNumbers gen)

randomName :: (RandomGen g) => g -> [Char]
randomName gen = capitalize $ take (randomNumBetween 3 11 gen) (randomRs ('a', 'z') gen)

randomVowels gen = randomRs

randomPhoneNumbers :: (RandomGen g) => g -> [String]
randomPhoneNumbers gen = take (randomNumBetween 0 3 gen) (infinitePhoneNumbers gen)

infinitePhoneNumbers gen = randomPhoneNumber (fst $ split gen) : infinitePhoneNumbers (snd $ split gen)

randomPhoneNumber gen = randomDigits (fst $ split gen) 3 ++ "-" ++ randomDigits (snd $ split gen) 3 ++ "-" ++ randomDigits (snd $ split $ fst $ split gen) 4

randomDigits gen num = take num (randomRs ('0', '9') gen)

randomNumBetween lower upper gen = fst $ randomR(lower, upper) gen

capitalize (firstLetter:restOfName) = (toUpper firstLetter) : restOfName

data Person = Person	{ firstName :: String
						, lastName :: String
						, phoneNumbers :: [String]
						} deriving Show 

