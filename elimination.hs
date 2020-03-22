import MaxFlow
import Data.Dates
import Data.Time
import Data.List.Split
import Data.Maybe
import Data.Sort
---import Network.Wreq                  -- package wreq
---import Control.Lens                  -- package lens
---import qualified Data.ByteString.Lazy as BL


data Game = Game {
                    round ::Int
                  , time :: Maybe LocalTime
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
                        time = strtoTime (tokens!!1)
                        location = tokens!!2                      
                        hometeam = tokens!!3
                        awayteam = tokens!!4
                        result = Just( gameresult (tokens!!5 ))
                    in (Game round  time location  hometeam awayteam result )



gameresult:: String -> (Int,Int) 
gameresult str  = let  tokens = (splitOn "-" str)::[String]
                   in (read (tokens!!0) :: Int   , read (tokens!!1) :: Int  )


formatDatestr :: [Char] -> [Char]
formatDatestr str =   let tokens = (splitOn "/" str)::[String]                
                          day  = if length (tokens!!0)  < 2 then "0" ++(tokens!!0) else tokens!!0
                          month  = if length (tokens!!1)  < 2 then "0" ++ (tokens!!1) else tokens!!1
                      in   day ++ "/" ++   month ++ "/" ++ tokens!!2
                      
cutofdate:: IO Games -> String -> IO Games
cutofdate games s  = do
                        games' <- games 
                        case strtoTime  s of
                            Nothing -> error "Bad date- date should be provided the format of dd/mm/yyyy hh:mm "
                            ( Just ltime) -> return (  map (\(Game round t location hometeam awayteam result ) -> if t >=  ( Just ltime) then  (Game round t location hometeam awayteam result ) else (Game round t location hometeam awayteam Nothing ) ) games'     )
                      
                        

strtoTime :: [Char] -> Maybe LocalTime
strtoTime str = case  ((parseTime  defaultTimeLocale  "%d/%m/%Y %H:%M" (formatDatestr(str)) )::Maybe ZonedTime )of                   
                    Nothing -> Nothing
                    Just (ZonedTime ltime zone) -> (Just ltime)  


teams:: IO Games -> IO [String]
teams games =  do 
                    games' <- games
                    let teams = map (\(Game  _ _ _  hometeam _ _) -> hometeam)  games'
                    return (foldr  (\x acc -> if elem x acc then acc else x:acc ) [] teams)

g = loadGames "nba.csv"                    