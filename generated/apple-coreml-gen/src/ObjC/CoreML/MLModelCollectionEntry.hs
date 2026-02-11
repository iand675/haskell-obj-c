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
  , isEqualToModelCollectionEntrySelector
  , initSelector
  , newSelector
  , modelIdentifierSelector
  , modelURLSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isEqualToModelCollectionEntry:@
isEqualToModelCollectionEntry :: (IsMLModelCollectionEntry mlModelCollectionEntry, IsMLModelCollectionEntry entry) => mlModelCollectionEntry -> entry -> IO Bool
isEqualToModelCollectionEntry mlModelCollectionEntry  entry =
  withObjCPtr entry $ \raw_entry ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlModelCollectionEntry (mkSelector "isEqualToModelCollectionEntry:") retCULong [argPtr (castPtr raw_entry :: Ptr ())]

-- | @- init@
init_ :: IsMLModelCollectionEntry mlModelCollectionEntry => mlModelCollectionEntry -> IO (Id MLModelCollectionEntry)
init_ mlModelCollectionEntry  =
    sendMsg mlModelCollectionEntry (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLModelCollectionEntry"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- | @- modelIdentifier@
modelIdentifier :: IsMLModelCollectionEntry mlModelCollectionEntry => mlModelCollectionEntry -> IO RawId
modelIdentifier mlModelCollectionEntry  =
    fmap (RawId . castPtr) $ sendMsg mlModelCollectionEntry (mkSelector "modelIdentifier") (retPtr retVoid) []

-- | @- modelURL@
modelURL :: IsMLModelCollectionEntry mlModelCollectionEntry => mlModelCollectionEntry -> IO RawId
modelURL mlModelCollectionEntry  =
    fmap (RawId . castPtr) $ sendMsg mlModelCollectionEntry (mkSelector "modelURL") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToModelCollectionEntry:@
isEqualToModelCollectionEntrySelector :: Selector
isEqualToModelCollectionEntrySelector = mkSelector "isEqualToModelCollectionEntry:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @modelIdentifier@
modelIdentifierSelector :: Selector
modelIdentifierSelector = mkSelector "modelIdentifier"

-- | @Selector@ for @modelURL@
modelURLSelector :: Selector
modelURLSelector = mkSelector "modelURL"

