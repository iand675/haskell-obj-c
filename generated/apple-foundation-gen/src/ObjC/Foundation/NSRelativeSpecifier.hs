{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRelativeSpecifier@.
module ObjC.Foundation.NSRelativeSpecifier
  ( NSRelativeSpecifier
  , IsNSRelativeSpecifier(..)
  , initWithCoder
  , initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifier
  , relativePosition
  , setRelativePosition
  , baseSpecifier
  , setBaseSpecifier
  , baseSpecifierSelector
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector
  , relativePositionSelector
  , setBaseSpecifierSelector
  , setRelativePositionSelector

  -- * Enum types
  , NSRelativePosition(NSRelativePosition)
  , pattern NSRelativeAfter
  , pattern NSRelativeBefore

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithCoder:@
initWithCoder :: (IsNSRelativeSpecifier nsRelativeSpecifier, IsNSCoder inCoder) => nsRelativeSpecifier -> inCoder -> IO (Id NSRelativeSpecifier)
initWithCoder nsRelativeSpecifier inCoder =
  sendOwnedMessage nsRelativeSpecifier initWithCoderSelector (toNSCoder inCoder)

-- | @- initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifier :: (IsNSRelativeSpecifier nsRelativeSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSScriptObjectSpecifier baseSpecifier) => nsRelativeSpecifier -> classDesc -> container -> property -> NSRelativePosition -> baseSpecifier -> IO (Id NSRelativeSpecifier)
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifier nsRelativeSpecifier classDesc container property relPos baseSpecifier =
  sendOwnedMessage nsRelativeSpecifier initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property) relPos (toNSScriptObjectSpecifier baseSpecifier)

-- | @- relativePosition@
relativePosition :: IsNSRelativeSpecifier nsRelativeSpecifier => nsRelativeSpecifier -> IO NSRelativePosition
relativePosition nsRelativeSpecifier =
  sendMessage nsRelativeSpecifier relativePositionSelector

-- | @- setRelativePosition:@
setRelativePosition :: IsNSRelativeSpecifier nsRelativeSpecifier => nsRelativeSpecifier -> NSRelativePosition -> IO ()
setRelativePosition nsRelativeSpecifier value =
  sendMessage nsRelativeSpecifier setRelativePositionSelector value

-- | @- baseSpecifier@
baseSpecifier :: IsNSRelativeSpecifier nsRelativeSpecifier => nsRelativeSpecifier -> IO (Id NSScriptObjectSpecifier)
baseSpecifier nsRelativeSpecifier =
  sendMessage nsRelativeSpecifier baseSpecifierSelector

-- | @- setBaseSpecifier:@
setBaseSpecifier :: (IsNSRelativeSpecifier nsRelativeSpecifier, IsNSScriptObjectSpecifier value) => nsRelativeSpecifier -> value -> IO ()
setBaseSpecifier nsRelativeSpecifier value =
  sendMessage nsRelativeSpecifier setBaseSpecifierSelector (toNSScriptObjectSpecifier value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSRelativeSpecifier)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString, NSRelativePosition, Id NSScriptObjectSpecifier] (Id NSRelativeSpecifier)
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:"

-- | @Selector@ for @relativePosition@
relativePositionSelector :: Selector '[] NSRelativePosition
relativePositionSelector = mkSelector "relativePosition"

-- | @Selector@ for @setRelativePosition:@
setRelativePositionSelector :: Selector '[NSRelativePosition] ()
setRelativePositionSelector = mkSelector "setRelativePosition:"

-- | @Selector@ for @baseSpecifier@
baseSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
baseSpecifierSelector = mkSelector "baseSpecifier"

-- | @Selector@ for @setBaseSpecifier:@
setBaseSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setBaseSpecifierSelector = mkSelector "setBaseSpecifier:"

