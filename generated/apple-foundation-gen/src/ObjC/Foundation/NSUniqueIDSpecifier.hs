{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUniqueIDSpecifier@.
module ObjC.Foundation.NSUniqueIDSpecifier
  ( NSUniqueIDSpecifier
  , IsNSUniqueIDSpecifier(..)
  , initWithCoder
  , initWithContainerClassDescription_containerSpecifier_key_uniqueID
  , uniqueID
  , setUniqueID
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_uniqueIDSelector
  , setUniqueIDSelector
  , uniqueIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsNSUniqueIDSpecifier nsUniqueIDSpecifier, IsNSCoder inCoder) => nsUniqueIDSpecifier -> inCoder -> IO (Id NSUniqueIDSpecifier)
initWithCoder nsUniqueIDSpecifier inCoder =
  sendOwnedMessage nsUniqueIDSpecifier initWithCoderSelector (toNSCoder inCoder)

-- | @- initWithContainerClassDescription:containerSpecifier:key:uniqueID:@
initWithContainerClassDescription_containerSpecifier_key_uniqueID :: (IsNSUniqueIDSpecifier nsUniqueIDSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property) => nsUniqueIDSpecifier -> classDesc -> container -> property -> RawId -> IO (Id NSUniqueIDSpecifier)
initWithContainerClassDescription_containerSpecifier_key_uniqueID nsUniqueIDSpecifier classDesc container property uniqueID =
  sendOwnedMessage nsUniqueIDSpecifier initWithContainerClassDescription_containerSpecifier_key_uniqueIDSelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property) uniqueID

-- | @- uniqueID@
uniqueID :: IsNSUniqueIDSpecifier nsUniqueIDSpecifier => nsUniqueIDSpecifier -> IO RawId
uniqueID nsUniqueIDSpecifier =
  sendMessage nsUniqueIDSpecifier uniqueIDSelector

-- | @- setUniqueID:@
setUniqueID :: IsNSUniqueIDSpecifier nsUniqueIDSpecifier => nsUniqueIDSpecifier -> RawId -> IO ()
setUniqueID nsUniqueIDSpecifier value =
  sendMessage nsUniqueIDSpecifier setUniqueIDSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSUniqueIDSpecifier)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:uniqueID:@
initWithContainerClassDescription_containerSpecifier_key_uniqueIDSelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString, RawId] (Id NSUniqueIDSpecifier)
initWithContainerClassDescription_containerSpecifier_key_uniqueIDSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:uniqueID:"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] RawId
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector '[RawId] ()
setUniqueIDSelector = mkSelector "setUniqueID:"

