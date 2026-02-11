{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIndexSpecifier@.
module ObjC.Foundation.NSIndexSpecifier
  ( NSIndexSpecifier
  , IsNSIndexSpecifier(..)
  , initWithContainerClassDescription_containerSpecifier_key_index
  , index
  , setIndex
  , initWithContainerClassDescription_containerSpecifier_key_indexSelector
  , indexSelector
  , setIndexSelector


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

-- | @- initWithContainerClassDescription:containerSpecifier:key:index:@
initWithContainerClassDescription_containerSpecifier_key_index :: (IsNSIndexSpecifier nsIndexSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property) => nsIndexSpecifier -> classDesc -> container -> property -> CLong -> IO (Id NSIndexSpecifier)
initWithContainerClassDescription_containerSpecifier_key_index nsIndexSpecifier  classDesc container property index =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
        sendMsg nsIndexSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:index:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argCLong (fromIntegral index)] >>= ownedObject . castPtr

-- | @- index@
index :: IsNSIndexSpecifier nsIndexSpecifier => nsIndexSpecifier -> IO CLong
index nsIndexSpecifier  =
  sendMsg nsIndexSpecifier (mkSelector "index") retCLong []

-- | @- setIndex:@
setIndex :: IsNSIndexSpecifier nsIndexSpecifier => nsIndexSpecifier -> CLong -> IO ()
setIndex nsIndexSpecifier  value =
  sendMsg nsIndexSpecifier (mkSelector "setIndex:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:index:@
initWithContainerClassDescription_containerSpecifier_key_indexSelector :: Selector
initWithContainerClassDescription_containerSpecifier_key_indexSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:index:"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector
setIndexSelector = mkSelector "setIndex:"

