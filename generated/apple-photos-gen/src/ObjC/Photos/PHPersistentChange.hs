{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPersistentChange@.
module ObjC.Photos.PHPersistentChange
  ( PHPersistentChange
  , IsPHPersistentChange(..)
  , new
  , init_
  , changeDetailsForObjectType_error
  , changeToken
  , changeDetailsForObjectType_errorSelector
  , changeTokenSelector
  , initSelector
  , newSelector

  -- * Enum types
  , PHObjectType(PHObjectType)
  , pattern PHObjectTypeAsset
  , pattern PHObjectTypeAssetCollection
  , pattern PHObjectTypeCollectionList

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPersistentChange)
new  =
  do
    cls' <- getRequiredClass "PHPersistentChange"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPHPersistentChange phPersistentChange => phPersistentChange -> IO (Id PHPersistentChange)
init_ phPersistentChange =
  sendOwnedMessage phPersistentChange initSelector

-- | @- changeDetailsForObjectType:error:@
changeDetailsForObjectType_error :: (IsPHPersistentChange phPersistentChange, IsNSError error_) => phPersistentChange -> PHObjectType -> error_ -> IO (Id PHPersistentObjectChangeDetails)
changeDetailsForObjectType_error phPersistentChange objectType error_ =
  sendMessage phPersistentChange changeDetailsForObjectType_errorSelector objectType (toNSError error_)

-- | @- changeToken@
changeToken :: IsPHPersistentChange phPersistentChange => phPersistentChange -> IO (Id PHPersistentChangeToken)
changeToken phPersistentChange =
  sendMessage phPersistentChange changeTokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHPersistentChange)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPersistentChange)
initSelector = mkSelector "init"

-- | @Selector@ for @changeDetailsForObjectType:error:@
changeDetailsForObjectType_errorSelector :: Selector '[PHObjectType, Id NSError] (Id PHPersistentObjectChangeDetails)
changeDetailsForObjectType_errorSelector = mkSelector "changeDetailsForObjectType:error:"

-- | @Selector@ for @changeToken@
changeTokenSelector :: Selector '[] (Id PHPersistentChangeToken)
changeTokenSelector = mkSelector "changeToken"

