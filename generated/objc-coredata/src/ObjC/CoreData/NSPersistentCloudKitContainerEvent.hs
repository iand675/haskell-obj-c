{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainerEvent@.
module ObjC.CoreData.NSPersistentCloudKitContainerEvent
  ( NSPersistentCloudKitContainerEvent
  , IsNSPersistentCloudKitContainerEvent(..)
  , new
  , init_
  , identifier
  , storeIdentifier
  , type_
  , startDate
  , endDate
  , succeeded
  , error_
  , newSelector
  , initSelector
  , identifierSelector
  , storeIdentifierSelector
  , typeSelector
  , startDateSelector
  , endDateSelector
  , succeededSelector
  , errorSelector

  -- * Enum types
  , NSPersistentCloudKitContainerEventType(NSPersistentCloudKitContainerEventType)
  , pattern NSPersistentCloudKitContainerEventTypeSetup
  , pattern NSPersistentCloudKitContainerEventTypeImport
  , pattern NSPersistentCloudKitContainerEventTypeExport

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

-- | @+ new@
new :: IO (Id NSPersistentCloudKitContainerEvent)
new  =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSPersistentCloudKitContainerEvent)
init_ nsPersistentCloudKitContainerEvent  =
  sendMsg nsPersistentCloudKitContainerEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSUUID)
identifier nsPersistentCloudKitContainerEvent  =
  sendMsg nsPersistentCloudKitContainerEvent (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- storeIdentifier@
storeIdentifier :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSString)
storeIdentifier nsPersistentCloudKitContainerEvent  =
  sendMsg nsPersistentCloudKitContainerEvent (mkSelector "storeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO NSPersistentCloudKitContainerEventType
type_ nsPersistentCloudKitContainerEvent  =
  fmap (coerce :: CLong -> NSPersistentCloudKitContainerEventType) $ sendMsg nsPersistentCloudKitContainerEvent (mkSelector "type") retCLong []

-- | @- startDate@
startDate :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSDate)
startDate nsPersistentCloudKitContainerEvent  =
  sendMsg nsPersistentCloudKitContainerEvent (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSDate)
endDate nsPersistentCloudKitContainerEvent  =
  sendMsg nsPersistentCloudKitContainerEvent (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- succeeded@
succeeded :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO Bool
succeeded nsPersistentCloudKitContainerEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentCloudKitContainerEvent (mkSelector "succeeded") retCULong []

-- | @- error@
error_ :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSError)
error_ nsPersistentCloudKitContainerEvent  =
  sendMsg nsPersistentCloudKitContainerEvent (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @storeIdentifier@
storeIdentifierSelector :: Selector
storeIdentifierSelector = mkSelector "storeIdentifier"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @succeeded@
succeededSelector :: Selector
succeededSelector = mkSelector "succeeded"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

