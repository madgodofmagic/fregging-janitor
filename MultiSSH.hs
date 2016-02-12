-- build with -threaded or forkIO does nothing
-- TODO: Allow server list to be entered/piped in via stdin
-- TODO: Optionally allow custom remote username for a server
-- TODO: Wait on all threads instead of blocking in a 'getch()'
-- TODO: Allow different key algorithms and/or custom key locations
-- BUG: Probably upstream - segfault when cwd not included in relative path
-- BUG: Could be some implicit timeout not being hit, I couldn't see any errors reported for stuff with incorrect ssh port specified
-- TODO: allow passphrase to be entered before opening key
import Network.SSH.Client.SimpleSSH
import Control.Concurrent
import System.Environment
import System.Posix.User
import Text.Read
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

runCommandOnHost :: String -> Integer -> String -> String -> IO ()
runCommandOnHost host port username command = do
  -- assume rsa
  (SSHEnv known_hosts pubkey privkey passphrase) <- buildenv "rsa"
  let client = withSessionKey host port known_hosts username pubkey privkey passphrase
  res <- runSimpleSSH $ client $ buildCommandAction command
  case res of
    Left err -> putStrLn $ show err ++ " error raised on " ++ host
    Right goodres -> print goodres

buildCommandAction :: String -> Session -> SimpleSSH Result
buildCommandAction command session = execCommand session command

dispatch_threads :: String -> String -> FilePath -> IO ()
dispatch_threads username command serverlist = do
    hosts <- parse_serverlist serverlist
    let makeThread (Just (ServerAddress server port)) = do _ <- ($) forkIO $ runCommandOnHost server port username command
                                                           return ()
        makeThread Nothing = do return ()
      in
      mapM_ makeThread hosts

block :: IO ()
block = interact $ take 1

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
 mapM demangle_server $ lines thelist

main :: IO ()
main = do
  args <- getArgs
  myname <- getProgName
  case args of
    (username:command:serverlist:[]) -> do
      putStrLn $ "Running " ++ command ++ " as " ++ username
      dispatch_threads username command serverlist
      block
    _ -> fail $ "Usage: " ++ myname ++ " remote_username command serverlist_filename"
