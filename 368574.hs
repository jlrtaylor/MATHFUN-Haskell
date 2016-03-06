-- MATHFUN Coursework
-- 368574

import Text.Printf

-- Types
type Title = String
type Director = String
type Year = Int
type UserRating = (String, Int)
type Ratings = [UserRating]

-- Film Type definition
data Film = Film Title Director Year Ratings
    deriving (Eq, Ord, Show, Read)

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

--  Your functional code goes here

addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm name director year db = db ++ [Film name director year []]

filmsAsString :: [Film] -> String
filmsAsString db = foldr (++) [] (listFilmsAsString db)

listFilmsAsString :: [Film] -> [String]
listFilmsAsString db = map filmAsString db

filmAsString :: Film -> String
filmAsString (Film title dir year rating ) = title ++ ", " ++ dir ++ ", " ++ show year ++ ", " ++ printf "%3.2f" (calcRating rating) ++"\n"

calcFilmRating :: Film -> Float
calcFilmRating (Film _ _ _ rating) = calcRating rating

calcRating :: [UserRating] -> Float
calcRating rating = fromIntegral (sum (map snd rating)) / fromIntegral (length rating)

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

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).
-- demo :: Int -> IO ()

--demo 1  = putStrLn all films after adding 2016 film "The BFG" by "Steven Spielberg" to testDatabase
--demo 2  = putStrLn (fnToTurnAListOfFilmsIntoAMultiLineString testDatabase)
--demo 3  = putStrLn all films by "Ridley Scott"
--demo 4  = putStrLn all films with website rating >= 7
--demo 5  = putStrLn average website rating for "Ridley Scott"
--demo 6  = putStrLn film titles and user ratings for "Emma"
--demo 7  = putStrLn all films after Emma rates "Hugo" 10
--demo 77 = putStrLn all films after Emma rates "Avatar" 10
--demo 8  = putStrLn "films between 2010 and 2014 sorted by website rating"


-- Your user interface code goes here