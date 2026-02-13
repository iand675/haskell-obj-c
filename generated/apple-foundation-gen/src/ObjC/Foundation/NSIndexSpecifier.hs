{-# LANGUAGE DataKinds #-}
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
  , indexSelector
  , initWithContainerClassDescription_containerSpecifier_key_indexSelector
  , setIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithContainerClassDescription:containerSpecifier:key:index:@
initWithContainerClassDescription_containerSpecifier_key_index :: (IsNSIndexSpecifier nsIndexSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property) => nsIndexSpecifier -> classDesc -> container -> property -> CLong -> IO (Id NSIndexSpecifier)
initWithContainerClassDescription_containerSpecifier_key_index nsIndexSpecifier classDesc container property index =
  sendOwnedMessage nsIndexSpecifier initWithContainerClassDescription_containerSpecifier_key_indexSelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property) index

-- | @- index@
index :: IsNSIndexSpecifier nsIndexSpecifier => nsIndexSpecifier -> IO CLong
index nsIndexSpecifier =
  sendMessage nsIndexSpecifier indexSelector

-- | @- setIndex:@
setIndex :: IsNSIndexSpecifier nsIndexSpecifier => nsIndexSpecifier -> CLong -> IO ()
setIndex nsIndexSpecifier value =
  sendMessage nsIndexSpecifier setIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:index:@
initWithContainerClassDescription_containerSpecifier_key_indexSelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString, CLong] (Id NSIndexSpecifier)
initWithContainerClassDescription_containerSpecifier_key_indexSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:index:"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CLong
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector '[CLong] ()
setIndexSelector = mkSelector "setIndex:"

