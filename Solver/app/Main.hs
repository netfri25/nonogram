{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Solver (solve)
import Board (BoardGroups(..), Grid, parseBoardGroups)

import System.Socket
import System.Socket.Family.Inet (InetPort, Inet, inetAny, SocketAddress(..))
import System.Socket.Type.Stream (Stream)
import System.Socket.Protocol.TCP (TCP)
import Control.Exception (bracket, catch, throwIO)
import Control.Monad (forever, void, when)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Char8 as BS

type ServerSocket = Socket Inet Stream TCP

bufCap :: Int
bufCap = 4096

serverPort :: InetPort
serverPort = 6969

localhost :: SocketAddress Inet
localhost = SocketAddressInet inetAny serverPort

connections :: Int
connections = 10

main :: IO ()
main = do
  bracket (socket :: IO ServerSocket) close $ \s -> do
    setSocketOption s (ReuseAddress True)
    bind s localhost
    listen s connections
    putStrLn "INFO: server is now running"
    forever $ acceptAndHandle s `catch` handleError

handleError :: SocketException -> IO ()
handleError e
  | e == eConnectionReset = putStrLn "connection closed"
  | otherwise = putStrLn $ unwords ["ERROR:", show e]

acceptAndHandle :: ServerSocket -> IO ()
acceptAndHandle s = bracket (accept s) disconnect $ \(p, addr) -> do
  putStrLn $ unwords ["INFO:", "connected to", show (inetAddress addr) ++ ':' : show (inetPort addr)]
  forever $ do
    recv <- unpack <$> receive p bufCap mempty
    putStrLn $ unwords ["CLIENT:", recv]
    when (null recv) (throwIO eConnectionReset)
    let to_send = maybe (pack "0") (packedBoard . solve) (parseBoardGroups recv)
    putStrLn $ unwords ["SOLUTION:", unpack to_send]
    void $ send p to_send mempty
  where
    disconnect (p, addr) = do
      close p
      putStrLn $ unwords ["INFO:", show (inetAddress addr) ++ ':' : show (inetPort addr), "disconnected"]

packedBoard :: Grid -> BS.ByteString
packedBoard = pack . concatMap (concatMap $ maybe "." show)

testBoard :: BoardGroups
testBoard = MkBoardGroups
  [[5], [1, 4], [2, 1], [1, 2], [1, 1, 2], [1, 4], [1, 1, 2]]
  [[6], [1], [1, 1], [2, 2], [2, 2], [7], [2, 1, 2]]

testBoard2 :: BoardGroups
testBoard2 = MkBoardGroups
  [[6,6,1],[1,3,1,2,1,2],[3,1,2,1,3,1],[1,1,3,1,2,2],[1,1,1,1,6,1],[1,1,1,1,1,3,2],[1,1,1,2,3,2,2],[1,1,2,2,2,1],[1,3,1,1,1,1],[1,1,1,1,1,3],[2,6,2,3],[1,4,5,1,1],[5,1,1,2,1],[1,1,1,3],[1,2,2,1],[2,1,3,4,2],[1,1,1,1,1],[3,1,1,1,1],[1,1,2,1],[1,1,7,1,1,2]]
  [[2,4,2,1,1],[2,1,2,2,1,1],[1,1,3,3,1],[1,2,2,3,1],[1,2,1,3,1,1],[1,2,3,2,1,1],[2,3,1,2,1],[4,2,2,1,1,1],[2,1,2,2,2,2],[1,3,1,3],[4,2,2,1,1],[1,4,1,2,1],[2,1,1,3,1,2],[2,2,2,2,1],[1,4,1,1,1,1],[7,1,2,3],[1,1,5,1,1],[1,1,3,1],[1,4,1,1,1,1],[3,4,3,2]]

testBoard3 :: BoardGroups
testBoard3 = MkBoardGroups
  [[6],[6,1],[1,3],[1,2,1,2],[3,1,2],[1,3,1],[1,1,3],[1,2,2],[1,1,1,1],[6,1]]
  [[1,5],[2,1,2],[2,2,1],[2,1,1,2],[2,1,1,1],[2,5,1],[1,1,2,2],[1,1,1,2],[3,3],[1,1,1]]

testBoard4 :: BoardGroups
testBoard4 = MkBoardGroups
  [[1,9,1,2],[2,1,1,3,2,1],[1,2,1,1,2],[1,3,1,2,1,1,1],[1,2,1,1],[1,2,1,2,3],[3,1,1,3,1],[3,2,2,2,1,2,1],[2,1,1,2,1],[1,7,4],[1,1,1,4,2],[4,4,1,2],[1,1,1,1,2,1],[3,3,1,1,2],[2,1,1,1],[2,8,5],[1,1,1,5,2,1],[1,1,3,2,3,2],[2,1,2,4,2],[1,1,1,1,2]]
  [[2,1,3,1,5],[1,2,1,5,2],[1,1,2,2,1,3],[1,1,1,1,1],[2,3,1,1,1,1,2],[1,1,3,1,1],[3,3,11],[1,3,1,1,1,1,1,1],[1,2,1,3,2,1],[2,3,3],[3,1,2,1,1,4],[1,1,2,2,1,2,1],[1,1,1,1,1,2],[1,5,1,1],[3,1,2,2,1],[1,2,1,1,1],[2,3,1,4],[2,1,1,1,2],[1,2,1,1,1,3],[1,3,2,3,4]]

-- seed: 1051823018
testBoard5 :: BoardGroups
testBoard5 = MkBoardGroups
  [[2,2,6,1],[1,1,1,3,1,3],[1,2,4,4],[1,4,2,2],[2,2,1,2,1,3],[1,6,1,2,2],[1,4,1,1,3],[3,1,1,3,1],[3,2,5,3],[1,2,1,1,8],[1,1,1,1,4,1],[1,2,2,4,3],[12,6],[2,2,4,1,1,1],[4,6,4,1],[1,1,1,2,1],[1,1,1,2,1,4],[5,1,1,2],[1,7,1,2],[2,2,1,2,2,2,1]]
  [[1,2,1,1,1,1],[2,2,6,2],[1,1,3,1],[2,3,1,1,2,4,1],[1,4,3,2,3],[1,1,2,1,3,2],[1,2,2,4,3],[1,4,3,1,1],[3,2,1,3,1,1],[3,1,2,4,2],[2,2,6,4],[1,2,2,1,3,2],[3,6],[1,2,3,1,2],[1,6,5,1],[2,3,1,2,1],[5,4,1,1,1],[13,2,1],[1,1,1,2,2,5],[1,1,3,2,1,2]]

-- https://upload.wikimedia.org/wikipedia/commons/9/9f/Nonogram_wiki.svg
testBoard6 :: BoardGroups
testBoard6 = MkBoardGroups
  [[8,7,5,7],[5,4,3,3],[3,3,2,3],[4,3,2,2],[3,3,2,2],[3,4,2,2],[4,5,2],[3,5,1],[4,3,2],[3,4,2],[4,4,2],[3,6,2],[3,2,3,1],[4,3,4,2],[3,2,3,2],[6,5],[4,5],[3,3],[3,3],[1,1]]
  [[1],[1],[2],[4],[7],[9],[2,8],[1,8],[8],[1,9],[2,7],[3,4],[6,4],[8,5],[1,11],[1,7],[8],[1,4,8],[6,8],[4,7],[2,4],[1,4],[5],[1,4],[1,5],[7],[5],[3],[1],[1]]
