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

--- NBA teams is assigned a conference (east or west)
data Team = Team {
                    name::String
                    ,conference::String
                  } deriving (Show) 

type Teams = [Team]

--- loads the teams information 
loadTeams::String -> IO Teams    
loadTeams fileName =   do
                        content <- readFile fileName                         
                        let thelines = tail (lines content)
                        return (map linetoTeam thelines)   


--- Load the teams information 
linetoTeam :: String -> Team 
linetoTeam line =  let tokens = (splitOn "," line)::[String]
                       name  = (tokens!!0) :: String 
                       conference  = (tokens!!1) :: String   
                   in (Team name conference ) 

--- Load the NBA game results 
loadGames::String -> IO Teams -> IO Games    
loadGames fileName teams =   do
                        content <- readFile fileName                         
                        teams' <- teams 
                        let thelines = tail (lines content)
                            games = (map linetoGame thelines)
                            games' = if checkGames games teams' then games else error "invaid game data " 
                        return games'

-- check if the games data is well formatted  
checkGames::Games -> Teams -> Bool 
checkGames [] teams = True
checkGames games []  = False   
checkGames ((Game round t location hometeam awayteam result ):xs)  teams = let t1 = isNothing (getTeam hometeam teams)
                                                                               t2 = isNothing (getTeam awayteam teams)
                                                                           in ( if t1 || t2 then error $ "bad data" ++ hometeam ++ " " ++  awayteam  else checkGames xs teams   )
                                                                               
                                                                                                                                                           

--- return team information 
getTeam::String -> Teams -> (Maybe Team)     
getTeam teamname []  = Nothing 
getTeam teamname ((Team name conference):xs)  = if name == teamname then (Just  (Team name conference))  else  getTeam teamname xs    

--- Convert a single line of input into a game object 
linetoGame:: String -> Game
linetoGame line  =  let tokens = (splitOn "," line)::[String]
                        round = read (tokens!!0) :: Int   
                        time = strtoTime (tokens!!1)
                        location = tokens!!2                      
                        hometeam = tokens!!3
                        awayteam = tokens!!4
                        result = Just( gameresult (tokens!!5 ))
                    in (Game round  time location  hometeam awayteam result )


-- Extract the result of a game from a string. For example, "100-96" become (100,96)
gameresult:: String -> (Int,Int) 
gameresult str  = let  tokens = (splitOn "-" str)::[String]
                   in (read (tokens!!0) :: Int   , read (tokens!!1) :: Int  )


--make sure that the date string has 2 digits for the month and the day in the month. 
formatDatestr :: [Char] -> [Char]
formatDatestr str =   let tokens = (splitOn "/" str)::[String]                
                          day  = if length (tokens!!0)  < 2 then "0" ++(tokens!!0) else tokens!!0
                          month  = if length (tokens!!1)  < 2 then "0" ++ (tokens!!1) else tokens!!1
                      in   day ++ "/" ++   month ++ "/" ++ tokens!!2

-- Erease the results for games after specific date so wehave some data to work with                      
cutofdate:: IO Games -> String -> IO Games
cutofdate games s  = do
                        games' <- games 
                        return (cutofdate'  games' s) 
                      

cutofdate':: Games -> String -> Games
cutofdate' games s =  case strtoTime  s of
                            Nothing -> error "Bad date- date should be provided the format of dd/mm/yyyy hh:mm "
                            ( Just ltime) ->   map (\(Game round t location hometeam awayteam result ) -> if t >=  ( Just ltime) then  (Game round t location hometeam awayteam result ) else (Game round t location hometeam awayteam Nothing ) ) games    
                        
--- Convert a date string into a date
strtoTime :: [Char] -> Maybe LocalTime
strtoTime str = case  ((parseTime  defaultTimeLocale  "%d/%m/%Y %H:%M" (formatDatestr(str)) )::Maybe ZonedTime )of                   
                    Nothing -> Nothing
                    Just (ZonedTime ltime zone) -> (Just ltime)  


--- Return a distinc list of teams 
teams:: IO Games -> IO [String]
teams games =  do 
                    games' <- games
                    return (teams' games')


teams':: Games -> [String]
teams' games =  let teams = map (\(Game  _ _ _  hometeam _ _) -> hometeam)  games
                    in  (foldr  (\x acc -> if elem x acc then acc else x:acc ) [] teams)

--- The games remaining to play 
gamesToPlay:: IO Games -> IO Games
gamesToPlay games =   do 
                        games' <- games
                        return (gamesToPlay' games')

gamesToPlay':: Games -> Games
gamesToPlay' games = (filter (\(Game round t location hometeam awayteam result ) -> result == Nothing ) games)

g_teams = loadTeams "teams.csv"     
g_games = loadGames "nba.csv"  g_teams                  


