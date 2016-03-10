-- 368574
-- MATHFUN Coursework

import Text.Printf
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (sortBy)

-- Types
type Title = String
type Director = String
type Year = Int
type UserRating = (String, Int)
type Ratings = [UserRating]

-- Film Type definition
data Film = Film Title Director Year Ratings
    deriving (Eq, Ord, Show, Read)

validNumerics :: String
validNumerics = "0123456789"

-- Test Database
testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 [("Amy",5), ("Bill",8), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",7), ("Megan",4)],
                Film "The Fly" "David Cronenberg" 1986 [("Megan",4), ("Fred",7), ("Chris",5), ("Ian",0), ("Amy",6)],
                Film "Psycho" "Alfred Hitchcock" 1960 [("Bill",4), ("Jo",4), ("Garry",8), ("Kevin",7), ("Olga",8), ("Liz",10), ("Ian",9)],
                Film "Body Of Lies" "Ridley Scott" 2008 [("Sam",3), ("Neal",7), ("Kevin",2), ("Chris",5), ("Olga",6)],
                Film "Avatar" "James Cameron" 2009 [("Olga",1), ("Wally",8), ("Megan",9), ("Tim",5), ("Zoe",8), ("Emma",3)],
                Film "Titanic" "James Cameron" 1997 [("Zoe",7), ("Amy",1), ("Emma",5), ("Heidi",3), ("Jo",8), ("Megan",5), ("Olga",7), ("Tim",10)],
                Film "The Departed" "Martin Scorsese" 2006 [("Heidi",3), ("Jo",8), ("Megan",5), ("Tim",3), ("Fred",5)],
                Film "Aliens" "Ridley Scott" 1986 [("Fred",9), ("Dave",6), ("Amy",10), ("Bill",7), ("Wally",1), ("Zoe",5)],
                Film "Kingdom Of Heaven" "Ridley Scott" 2005 [("Garry",3), ("Chris",7), ("Emma",5), ("Bill",1), ("Dave",3)],
                Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 [("Ian",9), ("Amy",1), ("Emma",7), ("Sam",8), ("Wally",5), ("Zoe",6)],
                Film "Bridge of Spies" "Steven Spielberg" 2015 [("Fred",3), ("Garry",4), ("Amy",10), ("Bill",7), ("Wally",6)],
                Film "Vertigo" "Alfred Hitchcock" 1958 [("Bill",8), ("Emma",5), ("Garry",1), ("Kevin",6), ("Olga",6), ("Tim",10)],
                Film "The Birds" "Alfred Hitchcock" 1963 [("Garry",7), ("Kevin",8), ("Olga",4), ("Tim",8), ("Wally",3)],
                Film "Jaws" "Steven Spielberg" 1975 [("Fred",3), ("Garry",0), ("Jo",3), ("Neal",9), ("Emma",7)],
                Film "The Martian" "Ridley Scott" 2015 [("Emma",7), ("Sam",8), ("Wally",5), ("Dave",10)],
                Film "The Shawshank Redemption" "Frank Darabont" 1994 [("Jo",8), ("Sam",10), ("Zoe",4), ("Dave",7), ("Emma",3), ("Garry",10), ("Kevin",7)],
                Film "Gladiator" "Ridley Scott" 2000 [("Garry",7), ("Ian",4), ("Neal",5), ("Wally",3), ("Emma",4)],
                Film "The Green Mile" "Frank Darabont" 1999 [("Sam",3), ("Zoe",4), ("Dave",7), ("Wally",5), ("Jo",5)],
                Film "True Lies" "James Cameron" 1994 [("Dave",3), ("Kevin",10), ("Jo",0)],
                Film "Super 8" "J J Abrams" 2011 [("Dave",7), ("Wally",3), ("Garry",5), ("Megan",4)],
                Film "Minority Report" "Steven Spielberg" 2002 [("Dave",6), ("Garry",6), ("Megan",2), ("Sam",7), ("Wally",8)],
                Film "War Horse" "Steven Spielberg" 2011 [("Dave",6), ("Garry",6), ("Megan",3), ("Sam",7), ("Wally",8), ("Zoe",8)],
                Film "The Terminal" "Steven Spielberg" 2004 [("Olga",8), ("Heidi",8), ("Bill",2), ("Sam",6), ("Garry",8)],
                Film "Star Wars: The Force Awakens" "J J Abrams" 2015 [("Olga",6), ("Zoe",6), ("Bill",9), ("Sam",7), ("Wally",8), ("Emma",8)],
                Film "Hugo" "Martin Scorsese" 2011 [("Sam",9), ("Wally",3), ("Zoe",5), ("Liz",7)]
               ]

-- Functional code
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm name director year db = db ++ [Film name director year []]

-- Formats a list of films as a string
filmsAsString :: [Film] -> String
filmsAsString db = foldr (++) [] (map formatFilmAsString db)

formatFilmAsString :: Film -> String
formatFilmAsString (Film title dir year rating ) = title ++ ", " ++ dir ++ ", " ++ show year ++ ", " ++ printf "%3.2f" (calcRating rating) ++"\n"

calcFilmRating :: Film -> Float
calcFilmRating (Film _ _ _ rating) = calcRating rating

calcRating :: [UserRating] -> Float
calcRating rating = fromIntegral (sum (map snd rating)) / max 1 (fromIntegral (length rating))

listFilmsByDirector :: String ->[Film] -> [Film]
listFilmsByDirector director db = filter (\(Film _ fd _ _) -> fd == director) db

filmsByDirectorAsString :: String -> [Film] -> String
filmsByDirectorAsString director db = filmsAsString (listFilmsByDirector director db)

listFilmsByRating :: Float -> [Film] -> [Film]
listFilmsByRating r db = filter (\(Film _ _ _ rating) -> calcRating rating >= r) db

filmsByRatingAsString :: Float -> [Film] -> String
filmsByRatingAsString r db = filmsAsString (listFilmsByRating r db)

ratingOfFilmsByDirector :: String -> [Film] -> Float
ratingOfFilmsByDirector  director db = averageRatingofList (listFilmsByDirector director db)

averageRatingofList :: [Film] -> Float
averageRatingofList db = (sum (map calcFilmRating db)) / fromIntegral (length db)

userRatingsAsString :: String -> [Film] -> String
userRatingsAsString user db = foldr (++) [] (map (\film -> userRatingOfFilmAsString user film) db)

userRatingOfFilmAsString :: String -> Film -> String
userRatingOfFilmAsString user (Film title _ _ ratings)
    | userRatingExists user ratings = title ++ ", " ++ show (snd (head [x | x <- ratings, fst x == user])) ++ "\n"
    | otherwise = ""

userRatingExists :: String -> Ratings -> Bool
userRatingExists user ratings
    | (filter (\(a,_) -> a == user) ratings) == [] = False
    | otherwise = True

addUserRating :: String -> String -> Int -> [Film] -> [Film]
addUserRating title user review db
    | not (filmExists title db) = db
    | otherwise = (filter (\(Film ftitle _ _ _) -> ftitle /= title) db) ++ [newRating (filmByTitle title db) user review]

newRating :: Film -> String -> Int -> Film
newRating (Film ftitle fdir fyr ratings) user review = (Film ftitle fdir fyr ((filter (\(a,b) -> a /= user) ratings) ++ [(user, review)]))

filmByTitle :: String -> [Film] -> Film
filmByTitle title db = head (filter (\(Film ftitle _ _ _) -> ftitle == title) db)

filmExists :: String -> [Film] -> Bool
filmExists title db
    | (filter (\(Film ftitle _ _ _) -> ftitle == title) db) == [] = False
    | otherwise = True

directorExists :: String -> [Film] -> Bool
directorExists director db
    | (filter (\(Film _ fdirector _ _) -> fdirector == director) db) == [] = False
    | otherwise = True

sortedYearListAsString :: Int -> Int -> [Film] -> String
sortedYearListAsString yrB yrE db = filmsAsString (sortFilmsByRating (listFilmsByYears yrB yrE db))

sortFilmsByRating :: [Film] ->[Film]
sortFilmsByRating db = map fst (sortBy (compare `on` snd) (map getRating db))

-- used to pass back a tuple used to sort films by rating
getRating :: Film -> (Film, Float)
getRating film = (film, calcFilmRating film)

listFilmsByYears :: Int -> Int -> [Film] -> [Film]
listFilmsByYears yrB yrE db = filter (\(Film _ _ yr _) -> yr >= yrB && yr <= yrE) db


-- Demo function to test basic functionality without persistence
-- testDatabase doesn't change and nothing is saved/loaded to/from file
demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2016 film "The BFG" by "Steven Spielberg" to testDatabase
demo 1 = putStrLn (filmsAsString (addFilm "The BFG" "Steven Spielberg" 2016 testDatabase))
--demo 2  = putStrLn (fnToTurnAListOfFilmsIntoAMultiLineString testDatabase)
demo 2 = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films by "Ridley Scott"
demo 3 = putStrLn (filmsByDirectorAsString "Ridley Scott" testDatabase)
--demo 4  = putStrLn all films with website rating >= 7
demo 4 = putStrLn (filmsByRatingAsString 7.0 testDatabase)
--demo 5  = putStrLn average website rating for "Ridley Scott"
demo 5 = putStrLn (printf "%3.2f" (ratingOfFilmsByDirector "Ridley Scott" testDatabase))
--demo 6  = putStrLn film titles and user ratings for "Emma"
demo 6 = putStrLn (userRatingsAsString "Emma" testDatabase)
--demo 7  = putStrLn all films after Emma rates "Hugo" 10
demo 7 = putStrLn (filmsAsString(addUserRating "Hugo" "Emma" 10 testDatabase))
--demo 77 = putStrLn all films after Emma rates "Avatar" 10
demo 77 = putStrLn (filmsAsString(addUserRating "Avatar" "Emma" 10 testDatabase))
--demo 8  = putStrLn "films between 2010 and 2014 sorted by website rating"
demo 8 = putStrLn (sortedYearListAsString 2010 2014 testDatabase)


-- User interface code
main :: IO()
main = do
    putStrLn "Welcome"
    putStr "Please enter your name: "
    name <- getLine
    putStrLn ("\nHello " ++ name)
    printHelp
    dbcontents <- readFile "filmdb.txt"
    menu name (read (dbcontents) :: [Film])

menu :: String -> [Film] -> IO()
menu name db = do
    putStr ("\nEnter command: ")
    option <- getLine
    optionHandler option name db


optionHandler :: String -> String -> [Film] -> IO()
-- Show contents of database
optionHandler "1" name db = do
    putStrLn "\nList of all films"
    putStr (filmsAsString db)
    menu name db
optionHandler "11" name db = do
    putStrLn "\nSorted list of all films"
    putStr (filmsAsString (sortFilmsByRating db))
    menu name db
-- Add film to database
optionHandler "2" name db = do
    putStr "Enter details of film\nTitle: "
    title <- getLine
    putStr "Director: "
    director <- getLine
    putStr "Release Year: "
    year <- getInt
    if (year >= 0)
        then do
            menu name (addFilm title director year db)
        else do
            putStrLn "Invalid year"
            menu name db
-- Display all films by director
optionHandler "3" name db = do
    putStr "Enter director name: "
    director <- getLine
    if (directorExists director db)
        then do
            putStrLn ("\nAll films by " ++ director)
            putStr (filmsByDirectorAsString director db)
            menu name db
        else do
            putStrLn ("Director not found")
            menu name db
-- Display films above a certain rating
optionHandler "4" name db = do
    putStr "Enter rating to filter by: "
    rating <- getInt
    if rating <= 10 && rating >= 0
        then do
            putStrLn ("\nAll films rated higher than " ++ show(rating))
            putStr (filmsByRatingAsString (fromIntegral rating) db)
        else do
            putStrLn "The rating was not between 0 and 10 or contained invalid characters"
    menu name db
-- Display average rating of films by a director
optionHandler "5" name db = do
    putStr "Enter director name: "
    director <- getLine
    if (directorExists director db)
        then do
            putStr ("Average rating of films by " ++ director ++ " is ")
            putStrLn (printf "%3.2f" (ratingOfFilmsByDirector director db))
            menu name db
        else do
            putStrLn ("Director not found")
            menu name db
-- Display films a user has rated
optionHandler "6" name db = do
    putStr "Enter username: "
    username <- getLine
    if (userRatingsAsString username db) == ""
        then do
            putStrLn "User not found"
            menu name db
        else do
            putStrLn ("\nAll films rated by " ++ username)
            putStr (userRatingsAsString username db)
            menu name db
-- Add or edit a rating
optionHandler "7" name db = do
    putStr "Enter film to rate: "
    film <- getLine
    if (filmExists film db)
        then do
            putStr "Enter rating out of 10: "
            rating <-getInt
            if rating <= 10 && rating >= 0
                then do
                    putStrLn ("You gave " ++ film ++ " a rating of " ++ show(rating))
                    menu name (addUserRating film name rating db)
                else do
                    putStrLn "The rating was not between 0 and 10 or contained invalid characters"
                    menu name db
        else do
            putStrLn "Film not found"
            menu name db
-- Search by years
optionHandler "8" name db = do
    putStr "Enter oldest year to filter by: "
    yrB <- getInt
    putStr "Enter latest year to filter by: "
    yrE <- getInt
    if yrB >= 0 && yrE >= 0
        then do
            if yrE >= yrB
                then do
                    if (sortedYearListAsString yrB yrE db) /= ""
                        then do
                            putStr "\nList of films released between "
                            putStrLn (show(yrB) ++ " and "++ show(yrE) ++ "\n")
                            putStr (sortedYearListAsString yrB yrE db)
                            menu name db
                        else do
                            putStrLn "No films found during this year range"
                            menu name db
                else do
                    putStrLn "Latest year must be equal or greater than oldest year"
                    menu name db
        else do
            putStrLn "Years were invalid"
            menu name db
-- Exit program
optionHandler "exit" _ db = do
    putStrLn "Saving database and closing"
    saveDB db
-- Displays command list
optionHandler "help" name db = do
    printHelp
    menu name db
-- Wildcard catches invalid commands
optionHandler _ name db = do
    putStrLn "Command not found, type help for list of valid commands"
    menu name db


printHelp :: IO()
printHelp = do
    putStrLn "Here is a list of commands:"
    putStrLn "   1 : Display all films"
    putStrLn "  11 : Display all films sorted by rating"
    putStrLn "   2 : Add a new film"
    putStrLn "   3 : Filter by director"
    putStrLn "   4 : Filter by rating"
    putStrLn "   5 : Display average rating of all films by a director"
    putStrLn "   6 : Display films a user has rated"
    putStrLn "   7 : Rate a film"
    putStrLn "   8 : Filter by years, and sort by rating"
    putStrLn "help : displays this list"
    putStrLn "exit : saves the database and closes the program"

getInt :: IO Int
getInt = do
    str <- getLine
    if (filter(\ch -> isDigit ch ) str) == ""
        then do
            return (-1)
        else do
            return ((read str) :: Int)

saveDB :: [Film] -> IO()
saveDB db = writeFile "filmdb.txt" (show db)

resetDB :: IO()
resetDB = saveDB testDatabase
