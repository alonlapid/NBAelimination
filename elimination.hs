import MaxFlow
import Data.Dates
import Data.Time
import Data.List.Split
import Data.Maybe
import Data.Sort
import Data.Set (Set)
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

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


-- the points for each team 
data TeamScore = TeamScore{
                    teamname::String
                    ,conf::String
                    ,points::Int 
                  } deriving (Show) 

type Standings = [TeamScore]

instance Eq TeamScore where
  (TeamScore name1 conf1 points1) == (TeamScore name2 conf2 points2) = points1 == points2

instance Ord TeamScore where
  compare  (TeamScore name1 conf1 points1) (TeamScore name2 conf2 points2)  | points1 == points2 = EQ
                                                                            | points1 < points2 = LT
                                                                            | otherwise = GT
                


--- loads the teams information 
loadTeams::String -> IO Teams    
loadTeams fileName =   do
                        content <- readFile fileName                         
                        let thelines = tail (lines content)
                        return (map linetoTeam thelines)   


--- Load the teams information 
linetoTeam :: String -> Team 
linetoTeam line =  let tokens = (splitOn "," line)::[String]
                       name  =  (tokens!!0) :: String 
                       conference  =  filter (\c -> isAlpha c) $ (tokens!!1) :: String   
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
                            ( Just ltime) ->   map (\(Game round t location hometeam awayteam result ) -> if t <  ( Just ltime) then  (Game round t location hometeam awayteam result ) else (Game round t location hometeam awayteam Nothing ) ) games    
                        
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
standing:: IO Teams-> IO Games ->  String -> IO Standings
standing teams games conf = do 
                        games' <- games
                        teams' <- teams
                        return (standing' teams' games' conf)


--- Get the standing of the teams
standing':: Teams-> Games -> String -> Standings 
standing' teams games conf = let emptymap = (Map.fromList [])::Map.Map String  TeamScore
                                 updatedmap =  foldl (\acc game  ->   prcoessGame teams game acc) emptymap games     
                                 l = sort $  map(\x -> snd x)  $ Map.toList updatedmap                               
                                 in  ( filter (\(TeamScore _ conf' points) -> conf == conf') l )



--- process a single game and add the points to the winning team
prcoessGame:: Teams-> Game ->  Map.Map String TeamScore -> Map.Map String  TeamScore
prcoessGame teams (Game round t location hometeam awayteam Nothing ) scoremap = scoremap
prcoessGame teams (Game round t location hometeam awayteam (Just(home,away)) ) scoremap = let (wteam,lteam)  =  if home >away then (hometeam,  awayteam) else (awayteam,hometeam)                                                                                              
                                                                                              wconf = getConf teams wteam
                                                                                              lconf = getConf teams lteam                                                                                               
                                                                                              updated = case Map.lookup wteam scoremap of 
                                                                                                         Nothing -> Map.insert  wteam (TeamScore wteam wconf 1) scoremap
                                                                                                         (Just (TeamScore _ conf points)) ->  Map.insert  wteam (TeamScore wteam conf (points + 1) ) scoremap
                                                                                              updated2 = case Map.lookup lteam scoremap of 
                                                                                                         Nothing -> Map.insert  lteam (TeamScore lteam lconf 0) updated
                                                                                                         (Just (TeamScore _ conf points)) -> updated      
                                                                                          in  updated2 
--- Get the conference for the team
getConf :: [Team] -> [Char] -> String                                                                                   
getConf teams teamName  = let (Team name conference) = head $ filter ( \(Team name conference) -> name == teamName) teams
                          in conference  


--- The games remaining to play 
gamesToPlay:: IO Games -> IO Games
gamesToPlay games =   do 
                        games' <- games
                        return (gamesToPlay' games')

gamesToPlay':: Games -> Games
gamesToPlay' games = (filter (\(Game round t location hometeam awayteam result ) -> result == Nothing ) games)

--- tranform a pair into a canonical form.
canonicalform:: Ord b => (b, b) -> (b, b)
canonicalform (x,y) = if x < y then (x,y) else (y,x) 

-- for a given list of games return a summary of the remaining  games to play. 
gamesToPlaySummary:: IO Games -> IO  [(String,String,Int)]
gamesToPlaySummary games = do 
                               games' <- games 
                               return (gamesToPlaySummary' games'  )                             


gamesToPlaySummary':: Games -> [(String,String,Int)]
gamesToPlaySummary' games =    let emptymap = (Map.fromList [])::Map.Map (String, String) Int
                                   updatedmap = foldl (\acc game  ->   addgameToPlaySummary game acc) emptymap $ gamesToPlay' games
                                   l =  (Map.toList updatedmap )   
                                   in map(\((team1,team2),val) -> (team1,team2,val) ) l


addgameToPlaySummary:: Num a => Game -> Map.Map (String, String) a -> Map.Map (String, String) a
addgameToPlaySummary (Game round t location hometeam awayteam result) m =  let key = canonicalform (hometeam,awayteam)
                                                                               updated  = case Map.lookup key m of 
                                                                                            Nothing -> Map.insert  key 1 m 
                                                                                            ((Just val) )->  Map.insert  key (1+val) m 
                                                                           in updated                                                                                      


                                                                                        
g_teams = loadTeams "teams.csv"     
g_games_all = loadGames "nba.csv"  g_teams   
g_games = cutofdate g_games_all  "1/4/2019 20:00"               



