{-# LANGUAGE PatternSynonyms #-}
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
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector
  , relativePositionSelector
  , setRelativePositionSelector
  , baseSpecifierSelector
  , setBaseSpecifierSelector

  -- * Enum types
  , NSRelativePosition(NSRelativePosition)
  , pattern NSRelativeAfter
  , pattern NSRelativeBefore

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
import ObjC.CoreFoundation.Internal.Enums

-- | @- initWithCoder:@
initWithCoder :: (IsNSRelativeSpecifier nsRelativeSpecifier, IsNSCoder inCoder) => nsRelativeSpecifier -> inCoder -> IO (Id NSRelativeSpecifier)
initWithCoder nsRelativeSpecifier  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsRelativeSpecifier (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifier :: (IsNSRelativeSpecifier nsRelativeSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSScriptObjectSpecifier baseSpecifier) => nsRelativeSpecifier -> classDesc -> container -> property -> NSRelativePosition -> baseSpecifier -> IO (Id NSRelativeSpecifier)
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifier nsRelativeSpecifier  classDesc container property relPos baseSpecifier =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
      withObjCPtr baseSpecifier $ \raw_baseSpecifier ->
          sendMsg nsRelativeSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argCULong (coerce relPos), argPtr (castPtr raw_baseSpecifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- relativePosition@
relativePosition :: IsNSRelativeSpecifier nsRelativeSpecifier => nsRelativeSpecifier -> IO NSRelativePosition
relativePosition nsRelativeSpecifier  =
  fmap (coerce :: CULong -> NSRelativePosition) $ sendMsg nsRelativeSpecifier (mkSelector "relativePosition") retCULong []

-- | @- setRelativePosition:@
setRelativePosition :: IsNSRelativeSpecifier nsRelativeSpecifier => nsRelativeSpecifier -> NSRelativePosition -> IO ()
setRelativePosition nsRelativeSpecifier  value =
  sendMsg nsRelativeSpecifier (mkSelector "setRelativePosition:") retVoid [argCULong (coerce value)]

-- | @- baseSpecifier@
baseSpecifier :: IsNSRelativeSpecifier nsRelativeSpecifier => nsRelativeSpecifier -> IO (Id NSScriptObjectSpecifier)
baseSpecifier nsRelativeSpecifier  =
  sendMsg nsRelativeSpecifier (mkSelector "baseSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBaseSpecifier:@
setBaseSpecifier :: (IsNSRelativeSpecifier nsRelativeSpecifier, IsNSScriptObjectSpecifier value) => nsRelativeSpecifier -> value -> IO ()
setBaseSpecifier nsRelativeSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRelativeSpecifier (mkSelector "setBaseSpecifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:@
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector :: Selector
initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifierSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:"

-- | @Selector@ for @relativePosition@
relativePositionSelector :: Selector
relativePositionSelector = mkSelector "relativePosition"

-- | @Selector@ for @setRelativePosition:@
setRelativePositionSelector :: Selector
setRelativePositionSelector = mkSelector "setRelativePosition:"

-- | @Selector@ for @baseSpecifier@
baseSpecifierSelector :: Selector
baseSpecifierSelector = mkSelector "baseSpecifier"

-- | @Selector@ for @setBaseSpecifier:@
setBaseSpecifierSelector :: Selector
setBaseSpecifierSelector = mkSelector "setBaseSpecifier:"

