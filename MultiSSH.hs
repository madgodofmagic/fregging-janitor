-- build with -threaded or forkIO does nothing
-- TODO: Allow server list to be entered/piped in via stdin
-- TODO: Optionally allow custom remote username for a server
-- TODO: Allow different key algorithms and/or custom key locations
-- BUG: Probably upstream - segfault when cwd not included in relative path UPDATE: Yeah this is libssh2 being difficult
-- CAVEAT: There doesn't seem to be a good way to force a timeout or even cancel a connection if it's waiting for the handshake to complete, the timeout here only cancels long command execution, the ssh timeout will always be based on system-wide defaults (TODO: Find a less minimalistic ssh lib)
-- Until then a. interrupt the process b. set a tcp timeout system-wide or c. don't put dead servers (i.e. the destinations which won't respond with an RST) in the config
-- TODO: allow passphrase to be entered before opening key
import Network.SSH.Client.SimpleSSH
import Control.Monad.Parallel as ParM
import System.Environment
import System.Posix.User
import Text.Read
import System.Timeout
import qualified Data.Text as T
data SSHEnv = SSHEnv String String String String
data ServerAddress = ServerAddress String Integer

buildenv :: [Char] -> IO SSHEnv
buildenv key_algorithm = do
  uentry <- (getLoginName >>= getUserEntryForName)
  -- assume no passphrase
  let passphrase = ""
      ssh_dir = (homeDirectory uentry) ++ "/.ssh"
      known_hosts = ssh_dir ++ "/known_hosts"
      pubkey = ssh_dir ++ "/id_" ++ key_algorithm ++ ".pub"
      privkey = ssh_dir ++ "/id_" ++ key_algorithm
  return $ SSHEnv known_hosts pubkey privkey passphrase





runCommandOnHost
  :: [Char] -> Integer -> String -> String -> Int -> IO ()
runCommandOnHost host port username command timeout_secs = do
  -- assume rsa
  (SSHEnv known_hosts pubkey privkey passphrase) <- buildenv "rsa"
  
  let client = withSessionKey host port known_hosts username pubkey privkey passphrase 
  r <- timeout (timeout_secs * 1000000) $ runSimpleSSH $ client $ buildCommandAction command
  let res = case r of
        Nothing -> Left(Connect)
        Just(goodres) -> goodres
  case res of
    Left err -> do let message = show err ++ " error raised on " ++ host ++ "\n"
                   putStr message

    Right (Result stdOut stdErr exitCode) -> do
      putStrLn $ "Working on " ++ host
      let prettify = read . show
          message = foldl (++) "Host: " [host,"\n","Out: ",prettify stdOut,"Err: ",prettify stdErr,"Exit: ",show exitCode,"\n\n"]
      putStrLn message


buildCommandAction :: String -> Session -> SimpleSSH Result
buildCommandAction command session = execCommand session command








dispatch_threads :: String -> String -> FilePath -> Int -> IO ()
dispatch_threads username command serverlist timeout_secs = do
    hosts <- parse_serverlist serverlist
    let makeThread (Just (ServerAddress server port)) = runCommandOnHost server port username command timeout_secs >> return ()
                                                           
        makeThread Nothing = return ()
      in
      ParM.mapM_ makeThread hosts

demangle_server :: [Char] -> IO (Maybe ServerAddress)
demangle_server serverstring =
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

parse_serverlist :: FilePath -> IO [Maybe ServerAddress]
parse_serverlist listpath = do
 thelist <- readFile listpath
 ParM.mapM demangle_server $ lines thelist

main :: IO ()
main = do
  args <- getArgs
  myname <- getProgName
  case args of
    (username:command:serverlist:timeout_secs:[]) -> do
      putStrLn $ "Running " ++ command ++ " as " ++ username
      dispatch_threads username command serverlist $ read timeout_secs
      putStrLn "Done."
    _ -> fail $ "Usage: " ++ myname ++ " remote_username command serverlist_filename timeout_secs"
    
