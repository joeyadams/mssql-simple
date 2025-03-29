module Database.MSSQLServer.Query.SqlRow
    ( SqlValue(..)
    , SqlRow(..)
    ) where

import Database.MSSQLServer.Query.Row (Row(..))
import Database.Tds.Primitives.SqlValue (SqlValue(..))
import Database.Tds.Message (Data(..), mcdTypeInfo)

-- | An instance of 'Row' that can handle a dynamic number of columns.
-- SQL Server returns the column information once per result set, so every row
-- will have the same number of columns.
newtype SqlRow = SqlRow [SqlValue]
    deriving Show

instance Row SqlRow where
    fromListOfRawBytes columns =
        -- "Compile" the field converters ahead of time so partial application
        -- only needs to map types to conversion functions once.
        let converters = map (fromRawBytes . mcdTypeInfo) columns
            columnCount = length converters
        in \values ->
            if length values == columnCount
                then SqlRow $ zipWith ($) converters values
                else error "Mismatch between column count and row's value count"
