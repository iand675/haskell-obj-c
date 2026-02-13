{-# LANGUAGE DataKinds #-}
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
  , endSpecifierSelector
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector
  , setEndSpecifierSelector
  , setStartSpecifierSelector
  , startSpecifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSCoder inCoder) => nsRangeSpecifier -> inCoder -> IO (Id NSRangeSpecifier)
initWithCoder nsRangeSpecifier inCoder =
  sendOwnedMessage nsRangeSpecifier initWithCoderSelector (toNSCoder inCoder)

-- | @- initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifier :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSScriptObjectSpecifier startSpec, IsNSScriptObjectSpecifier endSpec) => nsRangeSpecifier -> classDesc -> container -> property -> startSpec -> endSpec -> IO (Id NSRangeSpecifier)
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifier nsRangeSpecifier classDesc container property startSpec endSpec =
  sendOwnedMessage nsRangeSpecifier initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property) (toNSScriptObjectSpecifier startSpec) (toNSScriptObjectSpecifier endSpec)

-- | @- startSpecifier@
startSpecifier :: IsNSRangeSpecifier nsRangeSpecifier => nsRangeSpecifier -> IO (Id NSScriptObjectSpecifier)
startSpecifier nsRangeSpecifier =
  sendMessage nsRangeSpecifier startSpecifierSelector

-- | @- setStartSpecifier:@
setStartSpecifier :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSScriptObjectSpecifier value) => nsRangeSpecifier -> value -> IO ()
setStartSpecifier nsRangeSpecifier value =
  sendMessage nsRangeSpecifier setStartSpecifierSelector (toNSScriptObjectSpecifier value)

-- | @- endSpecifier@
endSpecifier :: IsNSRangeSpecifier nsRangeSpecifier => nsRangeSpecifier -> IO (Id NSScriptObjectSpecifier)
endSpecifier nsRangeSpecifier =
  sendMessage nsRangeSpecifier endSpecifierSelector

-- | @- setEndSpecifier:@
setEndSpecifier :: (IsNSRangeSpecifier nsRangeSpecifier, IsNSScriptObjectSpecifier value) => nsRangeSpecifier -> value -> IO ()
setEndSpecifier nsRangeSpecifier value =
  sendMessage nsRangeSpecifier setEndSpecifierSelector (toNSScriptObjectSpecifier value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSRangeSpecifier)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString, Id NSScriptObjectSpecifier, Id NSScriptObjectSpecifier] (Id NSRangeSpecifier)
initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifierSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:"

-- | @Selector@ for @startSpecifier@
startSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
startSpecifierSelector = mkSelector "startSpecifier"

-- | @Selector@ for @setStartSpecifier:@
setStartSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setStartSpecifierSelector = mkSelector "setStartSpecifier:"

-- | @Selector@ for @endSpecifier@
endSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
endSpecifierSelector = mkSelector "endSpecifier"

-- | @Selector@ for @setEndSpecifier:@
setEndSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setEndSpecifierSelector = mkSelector "setEndSpecifier:"

