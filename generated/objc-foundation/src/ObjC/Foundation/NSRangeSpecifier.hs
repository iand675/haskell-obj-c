{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRangeSpecifier@.
module ObjC.Foundation.NSRangeSpecifier
  ( NSRangeSpecifier
  , IsNSRangeSpecifier(..)
  , initWithCoder
  , initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifier
  , startSpecifier
  , setStartSpecifier
  , endSpecifier
  , setEndSpecifier
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector
  , startSpecifierSelector
  , setStartSpecifierSelector
  , endSpecifierSelector
  , setEndSpecifierSelector


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
initWithCoder :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSCoder inCoder) => nsRangeSpecifier -> inCoder -> IO (Id NSRangeSpecifier)
initWithCoder nsRangeSpecifier  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsRangeSpecifier (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifier :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSScriptObjectSpecifier startSpec, IsNSScriptObjectSpecifier endSpec) => nsRangeSpecifier -> classDesc -> container -> property -> startSpec -> endSpec -> IO (Id NSRangeSpecifier)
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifier nsRangeSpecifier  classDesc container property startSpec endSpec =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
      withObjCPtr startSpec $ \raw_startSpec ->
        withObjCPtr endSpec $ \raw_endSpec ->
            sendMsg nsRangeSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_startSpec :: Ptr ()), argPtr (castPtr raw_endSpec :: Ptr ())] >>= ownedObject . castPtr

-- | @- startSpecifier@
startSpecifier :: IsNSRangeSpecifier nsRangeSpecifier => nsRangeSpecifier -> IO (Id NSScriptObjectSpecifier)
startSpecifier nsRangeSpecifier  =
  sendMsg nsRangeSpecifier (mkSelector "startSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartSpecifier:@
setStartSpecifier :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSScriptObjectSpecifier value) => nsRangeSpecifier -> value -> IO ()
setStartSpecifier nsRangeSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRangeSpecifier (mkSelector "setStartSpecifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endSpecifier@
endSpecifier :: IsNSRangeSpecifier nsRangeSpecifier => nsRangeSpecifier -> IO (Id NSScriptObjectSpecifier)
endSpecifier nsRangeSpecifier  =
  sendMsg nsRangeSpecifier (mkSelector "endSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndSpecifier:@
setEndSpecifier :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSScriptObjectSpecifier value) => nsRangeSpecifier -> value -> IO ()
setEndSpecifier nsRangeSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRangeSpecifier (mkSelector "setEndSpecifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector :: Selector
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:"

-- | @Selector@ for @startSpecifier@
startSpecifierSelector :: Selector
startSpecifierSelector = mkSelector "startSpecifier"

-- | @Selector@ for @setStartSpecifier:@
setStartSpecifierSelector :: Selector
setStartSpecifierSelector = mkSelector "setStartSpecifier:"

-- | @Selector@ for @endSpecifier@
endSpecifierSelector :: Selector
endSpecifierSelector = mkSelector "endSpecifier"

-- | @Selector@ for @setEndSpecifier:@
setEndSpecifierSelector :: Selector
setEndSpecifierSelector = mkSelector "setEndSpecifier:"

