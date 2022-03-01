{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DDnetFriends (ddnet_update, DDDiff, make_dd) where

import GHC.Generics

import Data.Aeson (FromJSON, parseJSON, Value (Object), (.:))
import Network.HTTP.Simple (httpJSON, getResponseBody)

data Player = Player {
    player_name :: String,
    clan :: String
}
instance FromJSON Player where
    parseJSON (Object v) = Player <$>
            v .: "name" <*>
            v .: "clan"

data ServerInfo = ServerInfo {
    game_type :: String,
    server_name :: String,
    current_map :: String,
    clients :: [Player]
}
instance FromJSON ServerInfo where
    parseJSON (Object v) = do
        info <- v .: "info"
        ServerInfo <$>
            info .: "game_type" <*>
            info .: "name" <*>
            (info .: "map" >>= (.: "name")) <*>
            info .: "clients"

    parseJSON _ = mempty

data ApiResponse = ApiResponse {
    servers :: [ServerInfo]
} deriving (Generic)
instance FromJSON ApiResponse

data FriendInfo = FriendInfo {
    f_name :: String,
    f_server :: String,
    f_map_name :: String
} deriving (Eq)

data DDDiff = DDJoined String String String | DDLeft String deriving (Show)

server_friends :: [String] -> ServerInfo -> [FriendInfo]
server_friends friends srv =
    let s_name = server_name srv
        m_name = current_map srv
        s_frens = filter (`elem` friends) $ map player_name $ clients srv
    in map (\f -> FriendInfo f s_name m_name) s_frens

data DDState = DDState [FriendInfo]

make_dd :: IO DDState -- in case we need IO later
make_dd = return $ DDState []

ddnet_update :: [String] -> DDState -> IO ([DDDiff], DDState)
ddnet_update friends (DDState last_online) = do
    ans <- getResponseBody <$> httpJSON "http://master2.ddnet.tw/ddnet/15/servers.json"
    let current_online = concatMap (server_friends friends) $ servers ans

    let joined = filter (\p -> p `notElem` last_online) current_online
    let left   = filter (\p -> p `notElem` current_online) last_online

    let joined_diff = map (\(FriendInfo nam serv map) -> DDJoined nam serv map) joined
    let left_diff = map (\(FriendInfo nam _ _) -> DDLeft nam) left
    --print $ joined_diff ++ left_diff

    return (joined_diff ++ left_diff, DDState current_online)
