{-# LANGUAGE DataKinds #-}
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
  , directoryAttributesSelector
  , fileAttributesSelector
  , isEnumeratingDirectoryPostOrderSelector
  , levelSelector
  , skipDescendantsSelector
  , skipDescendentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- skipDescendents@
skipDescendents :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO ()
skipDescendents nsDirectoryEnumerator =
  sendMessage nsDirectoryEnumerator skipDescendentsSelector

-- | @- skipDescendants@
skipDescendants :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO ()
skipDescendants nsDirectoryEnumerator =
  sendMessage nsDirectoryEnumerator skipDescendantsSelector

-- | @- fileAttributes@
fileAttributes :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO (Id NSDictionary)
fileAttributes nsDirectoryEnumerator =
  sendMessage nsDirectoryEnumerator fileAttributesSelector

-- | @- directoryAttributes@
directoryAttributes :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO (Id NSDictionary)
directoryAttributes nsDirectoryEnumerator =
  sendMessage nsDirectoryEnumerator directoryAttributesSelector

-- | @- isEnumeratingDirectoryPostOrder@
isEnumeratingDirectoryPostOrder :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO Bool
isEnumeratingDirectoryPostOrder nsDirectoryEnumerator =
  sendMessage nsDirectoryEnumerator isEnumeratingDirectoryPostOrderSelector

-- | @- level@
level :: IsNSDirectoryEnumerator nsDirectoryEnumerator => nsDirectoryEnumerator -> IO CULong
level nsDirectoryEnumerator =
  sendMessage nsDirectoryEnumerator levelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skipDescendents@
skipDescendentsSelector :: Selector '[] ()
skipDescendentsSelector = mkSelector "skipDescendents"

-- | @Selector@ for @skipDescendants@
skipDescendantsSelector :: Selector '[] ()
skipDescendantsSelector = mkSelector "skipDescendants"

-- | @Selector@ for @fileAttributes@
fileAttributesSelector :: Selector '[] (Id NSDictionary)
fileAttributesSelector = mkSelector "fileAttributes"

-- | @Selector@ for @directoryAttributes@
directoryAttributesSelector :: Selector '[] (Id NSDictionary)
directoryAttributesSelector = mkSelector "directoryAttributes"

-- | @Selector@ for @isEnumeratingDirectoryPostOrder@
isEnumeratingDirectoryPostOrderSelector :: Selector '[] Bool
isEnumeratingDirectoryPostOrderSelector = mkSelector "isEnumeratingDirectoryPostOrder"

-- | @Selector@ for @level@
levelSelector :: Selector '[] CULong
levelSelector = mkSelector "level"

