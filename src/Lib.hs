{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( prune
    ) where

import           Control.Monad.State
import qualified Data.ByteString     as BS
import           Data.Foldable       (toList)
import qualified Data.Set            as Set (Set, difference, empty, insert)
import           Xeno.SAX            (process)

data FieldDefs = FieldDefs
  { used    :: Set.Set BS.ByteString
  , defined :: Set.Set BS.ByteString
  } deriving Show

type Parent = [BS.ByteString]

prune :: BS.ByteString -> [BS.ByteString]
prune input = toList $ Set.difference (defined result) (used result)
  where
    (_, result) =
      execState
        (process
          openTag
          attribute
          ignore   -- endOpenTag
          ignore   -- text
          closeTag
          ignore   -- cdata
          input)
        ([], mkFieldDefs)

mkFieldDefs :: FieldDefs
mkFieldDefs = FieldDefs Set.empty Set.empty

openTag :: BS.ByteString -> State (Parent, FieldDefs) ()
openTag a = do
  modify (\(p, f) -> (a:p, f))
  return ()

closeTag :: BS.ByteString -> State (Parent, FieldDefs) ()
closeTag _ = do
  modify (\(p, f) -> (tail p, f))
  return ()

attribute :: BS.ByteString -> BS.ByteString -> State (Parent, FieldDefs) ()
attribute name value = do
  modify (\(p, f) -> (p, update p name value f))
  return ()

ignore ::  a -> State (Parent, FieldDefs) ()
ignore _ = return ()

update :: [BS.ByteString]
       -> BS.ByteString
       -> BS.ByteString
       -> FieldDefs
       -> FieldDefs
update [] _ _ f                                           = f
update [_] _ _ f                                          = f
update ("field":"header":_) "name" value (FieldDefs u d)  = FieldDefs (Set.insert value u) d
update ("field":"trailer":_) "name" value (FieldDefs u d) = FieldDefs (Set.insert value u) d
update ("field":"message":_) "name" value (FieldDefs u d) = FieldDefs (Set.insert value u) d
update ("field":"fields":_) "name" value (FieldDefs u d)  = FieldDefs u (Set.insert value d)
update _ _ _ f                                            = f
