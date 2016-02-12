 import Network.SSH.Client.SimpleSSH
 import Control.Concurrent
 import System.Environment
 import System.Posix.User

 runCommandOnHost :: String -> String -> String -> IO ()
 runCommandOnHost host username command = do
   local_user <- getLoginName
   uentry <- getUserEntryForName local_user
   -- assume no passphrase
   let passphrase = ""
       homedir = homeDirectory uentry
       luser_prefix = homedir ++ "/.ssh"
       known_hosts = luser_prefix ++ "/known_hosts"
   -- assume rsa
       pubkey = luser_prefix ++ "/id_rsa.pub"
       privkey = luser_prefix ++ "/id_rsa"
       client = withSessionKey host 22 known_hosts username pubkey privkey passphrase
   res <- runSimpleSSH $ client $ buildCommandAction command
   case res of
     Left err -> putStrLn $ show err ++ " on " ++ host
     Right goodres -> print goodres


 buildCommandAction :: String -> Session -> SimpleSSH Result
 buildCommandAction command session = execCommand session command


 dispatch_threads :: String -> String -> FilePath -> IO ()
 dispatch_threads username command serverlist = do
     hostlist <- readFile serverlist
     let hosts = lines hostlist
         makeThread server = forkIO $ runCommandOnHost server username command
       in do
       mapM_ makeThread hosts  
 block :: IO ()
 block = interact $ take 1

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
