{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPersistentObjectChangeDetails@.
module ObjC.Photos.PHPersistentObjectChangeDetails
  ( PHPersistentObjectChangeDetails
  , IsPHPersistentObjectChangeDetails(..)
  , new
  , init_
  , objectType
  , insertedLocalIdentifiers
  , updatedLocalIdentifiers
  , deletedLocalIdentifiers
  , deletedLocalIdentifiersSelector
  , initSelector
  , insertedLocalIdentifiersSelector
  , newSelector
  , objectTypeSelector
  , updatedLocalIdentifiersSelector

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
new :: IO (Id PHPersistentObjectChangeDetails)
new  =
  do
    cls' <- getRequiredClass "PHPersistentObjectChangeDetails"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id PHPersistentObjectChangeDetails)
init_ phPersistentObjectChangeDetails =
  sendOwnedMessage phPersistentObjectChangeDetails initSelector

-- | @- objectType@
objectType :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO PHObjectType
objectType phPersistentObjectChangeDetails =
  sendMessage phPersistentObjectChangeDetails objectTypeSelector

-- | @- insertedLocalIdentifiers@
insertedLocalIdentifiers :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id NSSet)
insertedLocalIdentifiers phPersistentObjectChangeDetails =
  sendMessage phPersistentObjectChangeDetails insertedLocalIdentifiersSelector

-- | @- updatedLocalIdentifiers@
updatedLocalIdentifiers :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id NSSet)
updatedLocalIdentifiers phPersistentObjectChangeDetails =
  sendMessage phPersistentObjectChangeDetails updatedLocalIdentifiersSelector

-- | @- deletedLocalIdentifiers@
deletedLocalIdentifiers :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id NSSet)
deletedLocalIdentifiers phPersistentObjectChangeDetails =
  sendMessage phPersistentObjectChangeDetails deletedLocalIdentifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHPersistentObjectChangeDetails)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPersistentObjectChangeDetails)
initSelector = mkSelector "init"

-- | @Selector@ for @objectType@
objectTypeSelector :: Selector '[] PHObjectType
objectTypeSelector = mkSelector "objectType"

-- | @Selector@ for @insertedLocalIdentifiers@
insertedLocalIdentifiersSelector :: Selector '[] (Id NSSet)
insertedLocalIdentifiersSelector = mkSelector "insertedLocalIdentifiers"

-- | @Selector@ for @updatedLocalIdentifiers@
updatedLocalIdentifiersSelector :: Selector '[] (Id NSSet)
updatedLocalIdentifiersSelector = mkSelector "updatedLocalIdentifiers"

-- | @Selector@ for @deletedLocalIdentifiers@
deletedLocalIdentifiersSelector :: Selector '[] (Id NSSet)
deletedLocalIdentifiersSelector = mkSelector "deletedLocalIdentifiers"

