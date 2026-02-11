{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDirectoryEnumerator@.
module ObjC.Foundation.NSDirectoryEnumerator
  ( NSDirectoryEnumerator
  , IsNSDirectoryEnumerator(..)
  , skipDescendents
  , skipDescendants
  , fileAttributes
  , directoryAttributes
  , isEnumeratingDirectoryPostOrder
  , level
  , skipDescendentsSelector
  , skipDescendantsSelector
  , fileAttributesSelector
  , directoryAttributesSelector
  , isEnumeratingDirectoryPostOrderSelector
  , levelSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- skipDescendents@
skipDescendents :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO ()
skipDescendents nsDirectoryEnumerator  =
  sendMsg nsDirectoryEnumerator (mkSelector "skipDescendents") retVoid []

-- | @- skipDescendants@
skipDescendants :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO ()
skipDescendants nsDirectoryEnumerator  =
  sendMsg nsDirectoryEnumerator (mkSelector "skipDescendants") retVoid []

-- | @- fileAttributes@
fileAttributes :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO (Id NSDictionary)
fileAttributes nsDirectoryEnumerator  =
  sendMsg nsDirectoryEnumerator (mkSelector "fileAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- directoryAttributes@
directoryAttributes :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO (Id NSDictionary)
directoryAttributes nsDirectoryEnumerator  =
  sendMsg nsDirectoryEnumerator (mkSelector "directoryAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isEnumeratingDirectoryPostOrder@
isEnumeratingDirectoryPostOrder :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO Bool
isEnumeratingDirectoryPostOrder nsDirectoryEnumerator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDirectoryEnumerator (mkSelector "isEnumeratingDirectoryPostOrder") retCULong []

-- | @- level@
level :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO CULong
level nsDirectoryEnumerator  =
  sendMsg nsDirectoryEnumerator (mkSelector "level") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skipDescendents@
skipDescendentsSelector :: Selector
skipDescendentsSelector = mkSelector "skipDescendents"

-- | @Selector@ for @skipDescendants@
skipDescendantsSelector :: Selector
skipDescendantsSelector = mkSelector "skipDescendants"

-- | @Selector@ for @fileAttributes@
fileAttributesSelector :: Selector
fileAttributesSelector = mkSelector "fileAttributes"

-- | @Selector@ for @directoryAttributes@
directoryAttributesSelector :: Selector
directoryAttributesSelector = mkSelector "directoryAttributes"

-- | @Selector@ for @isEnumeratingDirectoryPostOrder@
isEnumeratingDirectoryPostOrderSelector :: Selector
isEnumeratingDirectoryPostOrderSelector = mkSelector "isEnumeratingDirectoryPostOrder"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

