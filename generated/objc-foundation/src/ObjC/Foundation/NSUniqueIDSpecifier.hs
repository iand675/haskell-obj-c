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
  , uniqueIDSelector
  , setUniqueIDSelector


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

-- | @- initWithCoder:@
initWithCoder :: (IsNSUniqueIDSpecifier nsUniqueIDSpecifier, IsNSCoder inCoder) => nsUniqueIDSpecifier -> inCoder -> IO (Id NSUniqueIDSpecifier)
initWithCoder nsUniqueIDSpecifier  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsUniqueIDSpecifier (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerClassDescription:containerSpecifier:key:uniqueID:@
initWithContainerClassDescription_containerSpecifier_key_uniqueID :: (IsNSUniqueIDSpecifier nsUniqueIDSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property) => nsUniqueIDSpecifier -> classDesc -> container -> property -> RawId -> IO (Id NSUniqueIDSpecifier)
initWithContainerClassDescription_containerSpecifier_key_uniqueID nsUniqueIDSpecifier  classDesc container property uniqueID =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
        sendMsg nsUniqueIDSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:uniqueID:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr (unRawId uniqueID) :: Ptr ())] >>= ownedObject . castPtr

-- | @- uniqueID@
uniqueID :: IsNSUniqueIDSpecifier nsUniqueIDSpecifier => nsUniqueIDSpecifier -> IO RawId
uniqueID nsUniqueIDSpecifier  =
  fmap (RawId . castPtr) $ sendMsg nsUniqueIDSpecifier (mkSelector "uniqueID") (retPtr retVoid) []

-- | @- setUniqueID:@
setUniqueID :: IsNSUniqueIDSpecifier nsUniqueIDSpecifier => nsUniqueIDSpecifier -> RawId -> IO ()
setUniqueID nsUniqueIDSpecifier  value =
  sendMsg nsUniqueIDSpecifier (mkSelector "setUniqueID:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:uniqueID:@
initWithContainerClassDescription_containerSpecifier_key_uniqueIDSelector :: Selector
initWithContainerClassDescription_containerSpecifier_key_uniqueIDSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:uniqueID:"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector
setUniqueIDSelector = mkSelector "setUniqueID:"

