{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainerEventResult@.
module ObjC.CoreData.NSPersistentCloudKitContainerEventResult
  ( NSPersistentCloudKitContainerEventResult
  , IsNSPersistentCloudKitContainerEventResult(..)
  , init_
  , new
  , result
  , resultType
  , initSelector
  , newSelector
  , resultSelector
  , resultTypeSelector

  -- * Enum types
  , NSPersistentCloudKitContainerEventResultType(NSPersistentCloudKitContainerEventResultType)
  , pattern NSPersistentCloudKitContainerEventResultTypeEvents
  , pattern NSPersistentCloudKitContainerEventResultTypeCountEvents

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSPersistentCloudKitContainerEventResult nsPersistentCloudKitContainerEventResult => nsPersistentCloudKitContainerEventResult -> IO (Id NSPersistentCloudKitContainerEventResult)
init_ nsPersistentCloudKitContainerEventResult  =
  sendMsg nsPersistentCloudKitContainerEventResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSPersistentCloudKitContainerEventResult)
new  =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- result@
result :: IsNSPersistentCloudKitContainerEventResult nsPersistentCloudKitContainerEventResult => nsPersistentCloudKitContainerEventResult -> IO RawId
result nsPersistentCloudKitContainerEventResult  =
  fmap (RawId . castPtr) $ sendMsg nsPersistentCloudKitContainerEventResult (mkSelector "result") (retPtr retVoid) []

-- | @- resultType@
resultType :: IsNSPersistentCloudKitContainerEventResult nsPersistentCloudKitContainerEventResult => nsPersistentCloudKitContainerEventResult -> IO NSPersistentCloudKitContainerEventResultType
resultType nsPersistentCloudKitContainerEventResult  =
  fmap (coerce :: CLong -> NSPersistentCloudKitContainerEventResultType) $ sendMsg nsPersistentCloudKitContainerEventResult (mkSelector "resultType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @result@
resultSelector :: Selector
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

