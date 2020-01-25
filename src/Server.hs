module Server
  ( entry
  )
where

import qualified Network.Socket                as S
import           Control.Monad
import           Text.Read
import           Data.List                      ( elemIndex )

import           Logic

entry :: IO ()
entry = do
  server <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.bind server (S.SockAddrInet 4203 S.iNADDR_ANY)
  putStrLn "Listening on 4203"
  S.listen server 2
  players <- acceptNClients 2 server
  sendN players (map ((++ "\n") . ("Hello, Player " ++) . show) [1 ..])
  gameLoop players


gameLoop :: [S.Socket] -> IO ()
gameLoop players = do
  (msgs, end) <- recvNClose players

  unless end $ do
    -- Validate input
    case maybeChoices msgs of
      Just choices -> sendN players (processChoices choices)
      Nothing      -> return ()

    gameLoop players
  where maybeChoices = mapM (readMaybe :: String -> Maybe Choice)


-- Logic of the game
processChoices :: [Choice] -> [String]
processChoices msgs =
  map ((++ "\n") . ordToWin . uncurry compare) $ zip msgs (reverse msgs)


-- Wait for n clients to connect
acceptNClients :: Int -> S.Socket -> IO [S.Socket]
acceptNClients n server = replicateM n (fmap fst (S.accept server))


-- Send messages to all clients
sendN :: [S.Socket] -> [String] -> IO ()
sendN clients msgs = mapM_ (uncurry S.send) (zip clients msgs)


-- Receive messages from all clients
recvN :: [S.Socket] -> IO [String]
recvN = mapM (S.recv `flip` 4096)


-- Receive messages from all clients
-- if some client closed a connection then
-- close connection for all clients
recvNClose :: [S.Socket] -> IO ([String], Bool)
recvNClose clients = do
  msgs <- recvN clients

  let someClosed = atLeastOneClosed msgs

  when someClosed $ mapM_ S.close clients
  return (msgs, someClosed)
  where atLeastOneClosed = elem ""
