{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Exception (ErrorCall(..), bracket, evaluate, try)
import Database.MSSQLServer.Connection
    ( close,
      connectWithoutEncryption,
      defaultConnectInfo,
      ConnectInfo(connectPassword, connectHost, connectPort,
                  connectDatabase, connectUser),
      Connection
    )
import Database.MSSQLServer.Query ( sql, Only (..) )
import Data.Time ( UTCTime )
import Data.Time.Format.ISO8601 (iso8601Show)

connectionInfo :: ConnectInfo
connectionInfo =
    defaultConnectInfo
    { connectHost = "localhost"
    , connectPort = "1433"
    , connectDatabase = "master"
    , connectUser = "sa"
    , connectPassword = "***********"
    }

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connectionInfo = bracket (connectWithoutEncryption connectionInfo) close

main :: IO ()
main = do
    withConnection connectionInfo testParseUTCTime

testParseUTCTime :: Connection -> IO ()
testParseUTCTime conn = do
    [Only (iso8601Show -> "2025-08-11T12:34:56Z")] <- sql conn "SELECT CAST('2025-08-11 12:34:56' AS datetime)" :: IO [Only UTCTime]
    [Only (Just (iso8601Show -> "2025-08-11T12:34:56Z"))] <- sql conn "SELECT CAST('2025-08-11 12:34:56' AS datetime)" :: IO [Only (Maybe UTCTime)]
    [Only (iso8601Show -> "2025-08-11T12:34:56Z")] <- sql conn "SELECT CAST('2025-08-11 12:34:56' AS datetime2)" :: IO [Only UTCTime]
    [Only (Just (iso8601Show -> "2025-08-11T12:34:56Z"))] <- sql conn "SELECT CAST('2025-08-11 12:34:56' AS datetime2)" :: IO [Only (Maybe UTCTime)]

    -- The UTCTime implementation correctly reads NULL values.
    --
    -- NOTE: Error values are lazy, so unless the caller forces the individual value,
    -- invalid data won't be validated.
    [Only e] <- sql conn "SELECT CAST(NULL AS datetime)" :: IO [Only UTCTime]
    Left (ErrorCall msg) <- try $ evaluate e
    [Only e] <- sql conn "SELECT CAST(NULL AS datetime2)" :: IO [Only UTCTime]
    Left (ErrorCall msg) <- try $ evaluate e
    [Only Nothing] <- sql conn "SELECT CAST(NULL AS datetime)" :: IO [Only (Maybe UTCTime)]
    [Only Nothing] <- sql conn "SELECT CAST(NULL AS datetime2)" :: IO [Only (Maybe UTCTime)]

    -- The data type of the result column is validated even if the value is null when it can be.
    [Only e] <- sql conn "SELECT CAST(NULL AS int)" :: IO [Only (Maybe UTCTime)]
    Left (ErrorCall msg) <- try $ evaluate e

    pure ()
