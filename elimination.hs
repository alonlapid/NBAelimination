import MaxFlow
import Data.Dates
import Data.Time
import Data.List.Split
import Data.Maybe

data Game = Game {
                    round ::Int
                  , time :: Maybe ZonedTime
                  , location :: String 
                  , hometeam :: String 
                  , awayteam :: String 
                  , result :: Maybe (Int, Int) 
                  } deriving (Show)

type Games = [Game]



--- https://stackoverflow.com/questions/60793414/how-to-download-internet-file-locally-in-haskell
loadGames::String -> IO Games    
loadGames fileName =   do
                        content <- readFile fileName                         
                        let thelines = tail (lines content)
                        return (map linetoGame thelines)
                        
linetoGame:: String -> Game
linetoGame line  =  let tokens = (splitOn "," line)::[String]
                        round = read (tokens!!0) :: Int   
                        time = (parseTime  defaultTimeLocale  "%d/%m/%Y %H:%M" (formatDatestr(tokens!!1)) )::Maybe ZonedTime
                        location = tokens!!2                      
                        hometeam = tokens!!3
                        awayteam = tokens!!4
                        result = Just( gameresult (tokens!!5 ))
                    in (Game round  time location  hometeam awayteam result )



gameresult:: String -> (Int,Int) 
gameresult str  = let  tokens = (splitOn "-" str)::[String]
                   in (read (tokens!!0) :: Int   , read (tokens!!1) :: Int  )


formatDatestr str =   let tokens = (splitOn "/" str)::[String]                
                          day  = if length (tokens!!0)  < 2 then "0" ++(tokens!!0) else tokens!!0
                          month  = if length (tokens!!1)  < 2 then "0" ++ (tokens!!1) else tokens!!1
                      in   day ++ "/" ++   month ++ "/" ++ tokens!!2