{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , objectTypeSelector
  , insertedLocalIdentifiersSelector
  , updatedLocalIdentifiersSelector
  , deletedLocalIdentifiersSelector

  -- * Enum types
  , PHObjectType(PHObjectType)
  , pattern PHObjectTypeAsset
  , pattern PHObjectTypeAssetCollection
  , pattern PHObjectTypeCollectionList

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

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPersistentObjectChangeDetails)
new  =
  do
    cls' <- getRequiredClass "PHPersistentObjectChangeDetails"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id PHPersistentObjectChangeDetails)
init_ phPersistentObjectChangeDetails  =
  sendMsg phPersistentObjectChangeDetails (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- objectType@
objectType :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO PHObjectType
objectType phPersistentObjectChangeDetails  =
  fmap (coerce :: CLong -> PHObjectType) $ sendMsg phPersistentObjectChangeDetails (mkSelector "objectType") retCLong []

-- | @- insertedLocalIdentifiers@
insertedLocalIdentifiers :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id NSSet)
insertedLocalIdentifiers phPersistentObjectChangeDetails  =
  sendMsg phPersistentObjectChangeDetails (mkSelector "insertedLocalIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updatedLocalIdentifiers@
updatedLocalIdentifiers :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id NSSet)
updatedLocalIdentifiers phPersistentObjectChangeDetails  =
  sendMsg phPersistentObjectChangeDetails (mkSelector "updatedLocalIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deletedLocalIdentifiers@
deletedLocalIdentifiers :: IsPHPersistentObjectChangeDetails phPersistentObjectChangeDetails => phPersistentObjectChangeDetails -> IO (Id NSSet)
deletedLocalIdentifiers phPersistentObjectChangeDetails  =
  sendMsg phPersistentObjectChangeDetails (mkSelector "deletedLocalIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @objectType@
objectTypeSelector :: Selector
objectTypeSelector = mkSelector "objectType"

-- | @Selector@ for @insertedLocalIdentifiers@
insertedLocalIdentifiersSelector :: Selector
insertedLocalIdentifiersSelector = mkSelector "insertedLocalIdentifiers"

-- | @Selector@ for @updatedLocalIdentifiers@
updatedLocalIdentifiersSelector :: Selector
updatedLocalIdentifiersSelector = mkSelector "updatedLocalIdentifiers"

-- | @Selector@ for @deletedLocalIdentifiers@
deletedLocalIdentifiersSelector :: Selector
deletedLocalIdentifiersSelector = mkSelector "deletedLocalIdentifiers"

