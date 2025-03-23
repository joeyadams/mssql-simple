{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


module Database.MSSQLServer.Query.ResultSet (
                                            -- * Type classes
                                              ResultSet (..)
                                            , Result (..)

                                            -- * Custom parsers
                                            , parseNoResult
                                            , parseRowCount
                                            , parseReturnStatus
                                            , parseListOfRows
                                            , parseListOfRowsWith
                                            ) where


import Control.Applicative(Alternative((<|>)),many,(<$>))
import Database.Tds.Message
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.TokenStreamParser
import Database.MSSQLServer.Query.Template

import Control.Monad(forM)
import Language.Haskell.TH (runIO,pprint)

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad.Trans.Class (lift)
#endif
#else
import Control.Monad.Error
#endif


errorDone :: Parser TokenStream
errorDone = do
  _  <- many $ satisfy $ not . isTSError
  ts <- satisfy isTSError
  _  <- many $ satisfy $ not . isTSDoneOrDoneProc -- [MEMO] skip Info
  _  <- satisfy isFinalTSDoneOrDoneProc
  return ts
  where
    isTSError :: TokenStream -> Bool
    isTSError (TSError{}) = True
    isTSError _ = False


trySatisfy :: (TokenStream -> Bool) -> Parser' TokenStream
trySatisfy f = do
  ts <- lift $ (satisfyNotError f) <|> errorDone
  case ts of
    TSError ei -> throwError ei
    _ -> return ts

trySatisfyMany :: (TokenStream -> Bool) -> Parser' [TokenStream]
trySatisfyMany f = do
  tss <- lift $ (many $ satisfyNotError f) <|> ((\x->[x]) <$> errorDone)
  case tss of
    (TSError ei):_ -> throwError ei
    _ -> return tss



noResultDone :: Parser' ()
noResultDone = do
  _ <- trySatisfyMany $ not . isTSDoneOrDoneProc
  _ <- trySatisfy isTSDoneOrDoneProc
  return ()

noResultFinalDone :: Parser' ()
noResultFinalDone = do
  _ <- trySatisfyMany $ not . isFinalTSDoneOrDoneProc
  _ <- trySatisfy isFinalTSDoneOrDoneProc
  return ()

noResultFinalDone' :: Parser' ()
noResultFinalDone' = do
  _ <- trySatisfyMany $ not . isTSDoneOrDoneProc
  _ <- trySatisfy isFinalTSDoneOrDoneProc
  return ()



returnStatus :: Parser' ReturnStatus
returnStatus = do
  _ <- trySatisfyMany $ not . isTSReturnStatus
  TSReturnStatus rets <- trySatisfy isTSReturnStatus
  return $ ReturnStatus $ fromIntegral rets
  where
    isTSReturnStatus :: TokenStream -> Bool
    isTSReturnStatus (TSReturnStatus{}) = True
    isTSReturnStatus _ = False

returnStatusDone :: Parser' ReturnStatus
returnStatusDone = do
  rets <- returnStatus
  _ <- trySatisfyMany $ not . isTSDoneProc -- [MEMO] skip ReturnValue
  _ <- trySatisfy isTSDoneProc
  return rets

returnStatusFinalDone :: Parser' ReturnStatus
returnStatusFinalDone = do
  rets <- returnStatus
  _ <- trySatisfyMany $ not . isFinalTSDoneProc -- [MEMO] skip ReturnValue
  _ <- trySatisfy isFinalTSDoneProc
  return rets

returnStatusFinalDone' :: Parser' ReturnStatus
returnStatusFinalDone' = do
  rets <- returnStatus
  _ <- trySatisfyMany $ not . isTSDoneProc -- [MEMO] skip ReturnValue
  _ <- trySatisfy isFinalTSDoneProc
  return rets



rowCountDone :: Parser' RowCount
rowCountDone = do
  _ <- trySatisfyMany $ not . isTSDone
  TSDone (Done _ _ rc) <- trySatisfy isTSDone
  return $ RowCount $ fromIntegral rc

rowCountFinalDone :: Parser' RowCount
rowCountFinalDone = do
  _ <- trySatisfyMany $ not . isFinalTSDone
  TSDone (Done _ _ rc) <- trySatisfy isFinalTSDone
  return $ RowCount $ fromIntegral rc

rowCountFinalDone' :: Parser' RowCount
rowCountFinalDone' = do
  _ <- trySatisfyMany $ not . isTSDone
  TSDone (Done _ _ rc) <- trySatisfy isFinalTSDone
  return $ RowCount $ fromIntegral rc

listOfRow :: RowParser a -> Parser' [a]
listOfRow rowParser = do
  tsCmd <- trySatisfy isTSColMetaData
  _ <- trySatisfyMany $ not . isTSRow -- [MEMO] skip Order
  tsRows <- trySatisfyMany isTSRow
  return $
    let
      (TSColMetaData (maybeCmd)) = tsCmd
      mcds = case (\(ColMetaData x) -> x) <$> maybeCmd of
               Nothing -> error "listOfRow: ColMetaData is necessary"
               Just mcds' -> mcds'
      rows = (\(TSRow row) -> getRawBytes <$> row) <$> tsRows
    in rowParser mcds <$> rows
  where

    isTSColMetaData :: TokenStream -> Bool
    isTSColMetaData (TSColMetaData{}) = True
    isTSColMetaData _ = False

    isTSRow :: TokenStream -> Bool
    isTSRow (TSRow{}) = True
    isTSRow _ = False

    getRawBytes :: RowColumnData -> RawBytes
    getRawBytes (RCDOrdinal dt) = dt
    getRawBytes (RCDLarge _ _ dt) = dt

-- | Read a result set, expecting more result sets from this batch afterward.
listOfRowDone :: RowParser a -> Parser' ([a])
listOfRowDone rowParser = do
  rs <- listOfRow rowParser
  _ <- trySatisfyMany $ not . isTSDone -- [MEMO] necesarry ?
  _ <- trySatisfy $ isTSDone
  return rs

-- | Read a result set from a batch that only returns one result set.
listOfRowFinalDone :: RowParser a -> Parser' ([a])
listOfRowFinalDone rowParser = do
  rs <- listOfRow rowParser
  _ <- trySatisfyMany $ not . isFinalTSDone -- [MEMO] necesarry ?
  _ <- trySatisfy $ isFinalTSDone
  return rs

-- | Read the last result set from a batch that returns multiple result sets.
listOfRowFinalDone' :: RowParser a -> Parser' ([a])
listOfRowFinalDone' rowParser = do
  rs <- listOfRow rowParser
  _ <- trySatisfyMany $ not . isTSDone -- [MEMO] necesarry ?
  _ <- trySatisfy $ isFinalTSDone
  return rs


parseNoResult :: Parser' ()
parseNoResult = noResultFinalDone

parseRowCount :: Parser' RowCount
parseRowCount = rowCountFinalDone

parseReturnStatus :: Parser' ReturnStatus
parseReturnStatus = returnStatusFinalDone

-- TODO: Try using listOfRowDone here to support multiple queries in a batch via monadic composition.
parseListOfRows :: (Row a) => Parser' [a]
parseListOfRows = listOfRowFinalDone fromListOfRawBytes

-- TODO: Try using listOfRowDone here to support multiple queries in a batch via monadic composition.
parseListOfRowsWith :: RowParser a -> Parser' [a]
parseListOfRowsWith = listOfRowFinalDone


class ResultSet a where
  resultSetParser :: Parser' a


instance ResultSet () where
  resultSetParser = noResultFinalDone

instance ResultSet RowCount where
  resultSetParser = rowCountFinalDone

instance ResultSet ReturnStatus where
  resultSetParser = returnStatusFinalDone

-- TODO: parameterized version
instance (Row a) => ResultSet [a] where
  resultSetParser = parseListOfRows



-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- resultSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (Result a1, Result a2) => ResultSet (a1, a2) where
--  resultSetParser = do
--    !r1 <- resultParser False :: (Result a1) => Parser' a1
--    !r2 <- resultParser True :: (Result a2) => Parser' a2
--    return  (r1,r2)
--


class Result a where
  resultParser :: Bool -> Parser' a -- [MEMO] 1st param: isFinal

instance Result () where
  resultParser True = noResultFinalDone'
  resultParser _ = noResultDone

instance Result RowCount where
  resultParser True = rowCountFinalDone'
  resultParser _ = rowCountDone

instance Result ReturnStatus where
  resultParser True = returnStatusFinalDone'
  resultParser _ = returnStatusDone

instance Row a => Result [a] where
  resultParser True = listOfRowFinalDone' fromListOfRawBytes
  resultParser _ = listOfRowDone fromListOfRawBytes


