-- build with -threaded or forkIO does nothing
-- TODO: Optionally allow custom remote username for a server
-- TODO: Allow different key algorithms and/or custom key locations
-- BUG: Probably upstream - segfault when cwd not included in relative path UPDATE: Yeah this is libssh2 being difficult
-- Timeouts now work even for the initial connect(), but you need to use a modified version of simplessh - patch attached
-- TODO: allow passphrase to be entered before opening key NOTE: When I ask interactively the handle will have been closed already (if the server list has been passed via stdin)
-- Not sure what to do about above, I don't want to supply credentials in the cl arguments, maybe I can ask for passphrase before touching the handle and then split out init as the list.
import Network.SSH.Client.SimpleSSH
import Control.Monad.Parallel as ParM
import System.Environment
import System.Posix.User
import Text.Read
import System.Timeout
import qualified Data.Text as T
import System.IO (isEOF, hSetEcho,stdin, openFile, IOMode (ReadWriteMode))
import Control.Monad (unless)
data SSHEnv = SSHEnv String String String String
data ServerAddress = ServerAddress String Integer

buildEnv :: [Char] -> IO SSHEnv
buildEnv key_algorithm = do
  uentry <- (getEffectiveUserID >>= getUserEntryForID)
  -- assume no passphrase
  let ssh_dir = (homeDirectory uentry) ++ "/.ssh"
      known_hosts = ssh_dir ++ "/known_hosts"
      pubkey = ssh_dir ++ "/id_" ++ key_algorithm ++ ".pub"
      privkey = ssh_dir ++ "/id_" ++ key_algorithm
  -- putStrLn $ "Please enter passphrase for key" ++ privkey ++ " or press return for empty:"
  -- hSetEcho stdin False
  -- getLine >>= \p -> do
  -- let passphrase = filter (/= '\n') p
  -- hSetEcho stdin True
  return $ SSHEnv known_hosts pubkey privkey ""


runCommandOnHost
  :: String
     -> Integer -> String -> String -> Integer -> SSHEnv -> IO [Char]
runCommandOnHost host port username command timeout_secs env = do
  -- assume rsa
  let (SSHEnv known_hosts pubkey privkey passphrase) = env
      action = buildCommandAction command
  r <- timeout (fromInteger $ timeout_secs * 1000000) $ runSimpleSSH $ withSessionKey host port known_hosts username pubkey privkey passphrase timeout_secs action
  case r of
    Nothing -> return $ "Timeout on " ++ host ++ "\n"
    Just(someres) -> case someres of
      Left err -> do
        return $ foldl1 (++) [show err," error raised on ",host,"\n"]
      Right (Result stdOut stdErr exitCode) -> do
        let prettify = read . show
        return $ foldl1 (++) ["Host: ",host,"\n","Out: ",prettify stdOut,"Err: ",prettify stdErr,"Exit: ",show exitCode,"\n\n"]

buildCommandAction :: String -> Session -> SimpleSSH Result
buildCommandAction command session = execCommand session command


dispatchThreads
  :: String
     -> String -> String -> SSHEnv -> Integer -> IO [Maybe (IO [Char])]
dispatchThreads username command serverlist env timeout_secs = do
    hosts <- parseServerList serverlist
    let makeThread (Just (ServerAddress server port)) = do putStrLn $ "Working on " ++ server
                                                           return $ Just $ runCommandOnHost server port username command timeout_secs env
        makeThread Nothing = return $ Nothing
      in
      ParM.mapM makeThread hosts

demangleServer :: [Char] -> IO (Maybe ServerAddress)
demangleServer serverstring =
  let (address:port) = map T.unpack $ T.splitOn (T.pack ":") (T.pack serverstring)
  in case port of
        [] -> return $ Just (ServerAddress (address) 22)
        (p:[]) -> if (((readMaybe p) :: Maybe Integer) == Nothing) then do
          putStrLn $ "Port not an integer: " ++ p ++ " in " ++ address
          return Nothing
                           else return $ Just (ServerAddress (address) $ read p)
        (_:_) -> do
          putStrLn $ "Ignoring bogus line: " ++ serverstring
          return Nothing



parseServerList :: String -> IO [Maybe ServerAddress]
parseServerList thelist =
 ParM.mapM demangleServer $ lines thelist

main :: IO ()
main = do
  args <- getArgs
  myname <- getProgName
  case args of
    (username:command:timeout_secs:serverlist:[]) -> do
      thelist <- readFile serverlist
      getResults username command timeout_secs thelist
    (username:command:timeout_secs:[]) -> do
      getContents >>= getResults username command timeout_secs
    _ -> fail $ "Usage: " ++ myname ++ " remote_username command timeout_secs [serverlist_filename]"
    

getResults :: [Char] -> [Char] -> String -> String -> IO ()
getResults username command timeout_secs serverlist = do
  putStrLn $ "Running " ++ command ++ " as " ++ username
  env <- buildEnv "rsa"
  results <- dispatchThreads username command serverlist env $ read timeout_secs
  Prelude.mapM_ (\x -> case x of
                    Nothing -> return ()
                    Just output -> output >>= putStr) results
