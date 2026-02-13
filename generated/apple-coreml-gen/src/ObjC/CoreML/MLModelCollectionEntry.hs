{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLModelCollectionEntry Information about a model in a model collection.
--
-- Generated bindings for @MLModelCollectionEntry@.
module ObjC.CoreML.MLModelCollectionEntry
  ( MLModelCollectionEntry
  , IsMLModelCollectionEntry(..)
  , isEqualToModelCollectionEntry
  , init_
  , new
  , modelIdentifier
  , modelURL
  , initSelector
  , isEqualToModelCollectionEntrySelector
  , modelIdentifierSelector
  , modelURLSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isEqualToModelCollectionEntry:@
isEqualToModelCollectionEntry :: (IsMLModelCollectionEntry mlModelCollectionEntry, IsMLModelCollectionEntry entry) => mlModelCollectionEntry -> entry -> IO Bool
isEqualToModelCollectionEntry mlModelCollectionEntry entry =
  sendMessage mlModelCollectionEntry isEqualToModelCollectionEntrySelector (toMLModelCollectionEntry entry)

-- | @- init@
init_ :: IsMLModelCollectionEntry mlModelCollectionEntry => mlModelCollectionEntry -> IO (Id MLModelCollectionEntry)
init_ mlModelCollectionEntry =
  sendOwnedMessage mlModelCollectionEntry initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLModelCollectionEntry"
    sendOwnedClassMessage cls' newSelector

-- | @- modelIdentifier@
modelIdentifier :: IsMLModelCollectionEntry mlModelCollectionEntry => mlModelCollectionEntry -> IO RawId
modelIdentifier mlModelCollectionEntry =
  sendMessage mlModelCollectionEntry modelIdentifierSelector

-- | @- modelURL@
modelURL :: IsMLModelCollectionEntry mlModelCollectionEntry => mlModelCollectionEntry -> IO RawId
modelURL mlModelCollectionEntry =
  sendMessage mlModelCollectionEntry modelURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToModelCollectionEntry:@
isEqualToModelCollectionEntrySelector :: Selector '[Id MLModelCollectionEntry] Bool
isEqualToModelCollectionEntrySelector = mkSelector "isEqualToModelCollectionEntry:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelCollectionEntry)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @modelIdentifier@
modelIdentifierSelector :: Selector '[] RawId
modelIdentifierSelector = mkSelector "modelIdentifier"

-- | @Selector@ for @modelURL@
modelURLSelector :: Selector '[] RawId
modelURLSelector = mkSelector "modelURL"

