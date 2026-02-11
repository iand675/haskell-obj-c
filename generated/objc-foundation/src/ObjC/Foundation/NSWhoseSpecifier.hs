{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWhoseSpecifier@.
module ObjC.Foundation.NSWhoseSpecifier
  ( NSWhoseSpecifier
  , IsNSWhoseSpecifier(..)
  , initWithCoder
  , initWithContainerClassDescription_containerSpecifier_key_test
  , test
  , setTest
  , startSubelementIdentifier
  , setStartSubelementIdentifier
  , startSubelementIndex
  , setStartSubelementIndex
  , endSubelementIdentifier
  , setEndSubelementIdentifier
  , endSubelementIndex
  , setEndSubelementIndex
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_testSelector
  , testSelector
  , setTestSelector
  , startSubelementIdentifierSelector
  , setStartSubelementIdentifierSelector
  , startSubelementIndexSelector
  , setStartSubelementIndexSelector
  , endSubelementIdentifierSelector
  , setEndSubelementIdentifierSelector
  , endSubelementIndexSelector
  , setEndSubelementIndexSelector

  -- * Enum types
  , NSWhoseSubelementIdentifier(NSWhoseSubelementIdentifier)
  , pattern NSIndexSubelement
  , pattern NSEverySubelement
  , pattern NSMiddleSubelement
  , pattern NSRandomSubelement
  , pattern NSNoSubelement

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
initWithCoder :: (IsNSWhoseSpecifier nsWhoseSpecifier, IsNSCoder inCoder) => nsWhoseSpecifier -> inCoder -> IO (Id NSWhoseSpecifier)
initWithCoder nsWhoseSpecifier  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsWhoseSpecifier (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerClassDescription:containerSpecifier:key:test:@
initWithContainerClassDescription_containerSpecifier_key_test :: (IsNSWhoseSpecifier nsWhoseSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSScriptWhoseTest test) => nsWhoseSpecifier -> classDesc -> container -> property -> test -> IO (Id NSWhoseSpecifier)
initWithContainerClassDescription_containerSpecifier_key_test nsWhoseSpecifier  classDesc container property test =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
      withObjCPtr test $ \raw_test ->
          sendMsg nsWhoseSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:test:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_test :: Ptr ())] >>= ownedObject . castPtr

-- | @- test@
test :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO (Id NSScriptWhoseTest)
test nsWhoseSpecifier  =
  sendMsg nsWhoseSpecifier (mkSelector "test") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTest:@
setTest :: (IsNSWhoseSpecifier nsWhoseSpecifier, IsNSScriptWhoseTest value) => nsWhoseSpecifier -> value -> IO ()
setTest nsWhoseSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWhoseSpecifier (mkSelector "setTest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startSubelementIdentifier@
startSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO NSWhoseSubelementIdentifier
startSubelementIdentifier nsWhoseSpecifier  =
  fmap (coerce :: CULong -> NSWhoseSubelementIdentifier) $ sendMsg nsWhoseSpecifier (mkSelector "startSubelementIdentifier") retCULong []

-- | @- setStartSubelementIdentifier:@
setStartSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> NSWhoseSubelementIdentifier -> IO ()
setStartSubelementIdentifier nsWhoseSpecifier  value =
  sendMsg nsWhoseSpecifier (mkSelector "setStartSubelementIdentifier:") retVoid [argCULong (coerce value)]

-- | @- startSubelementIndex@
startSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO CLong
startSubelementIndex nsWhoseSpecifier  =
  sendMsg nsWhoseSpecifier (mkSelector "startSubelementIndex") retCLong []

-- | @- setStartSubelementIndex:@
setStartSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> CLong -> IO ()
setStartSubelementIndex nsWhoseSpecifier  value =
  sendMsg nsWhoseSpecifier (mkSelector "setStartSubelementIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- endSubelementIdentifier@
endSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO NSWhoseSubelementIdentifier
endSubelementIdentifier nsWhoseSpecifier  =
  fmap (coerce :: CULong -> NSWhoseSubelementIdentifier) $ sendMsg nsWhoseSpecifier (mkSelector "endSubelementIdentifier") retCULong []

-- | @- setEndSubelementIdentifier:@
setEndSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> NSWhoseSubelementIdentifier -> IO ()
setEndSubelementIdentifier nsWhoseSpecifier  value =
  sendMsg nsWhoseSpecifier (mkSelector "setEndSubelementIdentifier:") retVoid [argCULong (coerce value)]

-- | @- endSubelementIndex@
endSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO CLong
endSubelementIndex nsWhoseSpecifier  =
  sendMsg nsWhoseSpecifier (mkSelector "endSubelementIndex") retCLong []

-- | @- setEndSubelementIndex:@
setEndSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> CLong -> IO ()
setEndSubelementIndex nsWhoseSpecifier  value =
  sendMsg nsWhoseSpecifier (mkSelector "setEndSubelementIndex:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:test:@
initWithContainerClassDescription_containerSpecifier_key_testSelector :: Selector
initWithContainerClassDescription_containerSpecifier_key_testSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:test:"

-- | @Selector@ for @test@
testSelector :: Selector
testSelector = mkSelector "test"

-- | @Selector@ for @setTest:@
setTestSelector :: Selector
setTestSelector = mkSelector "setTest:"

-- | @Selector@ for @startSubelementIdentifier@
startSubelementIdentifierSelector :: Selector
startSubelementIdentifierSelector = mkSelector "startSubelementIdentifier"

-- | @Selector@ for @setStartSubelementIdentifier:@
setStartSubelementIdentifierSelector :: Selector
setStartSubelementIdentifierSelector = mkSelector "setStartSubelementIdentifier:"

-- | @Selector@ for @startSubelementIndex@
startSubelementIndexSelector :: Selector
startSubelementIndexSelector = mkSelector "startSubelementIndex"

-- | @Selector@ for @setStartSubelementIndex:@
setStartSubelementIndexSelector :: Selector
setStartSubelementIndexSelector = mkSelector "setStartSubelementIndex:"

-- | @Selector@ for @endSubelementIdentifier@
endSubelementIdentifierSelector :: Selector
endSubelementIdentifierSelector = mkSelector "endSubelementIdentifier"

-- | @Selector@ for @setEndSubelementIdentifier:@
setEndSubelementIdentifierSelector :: Selector
setEndSubelementIdentifierSelector = mkSelector "setEndSubelementIdentifier:"

-- | @Selector@ for @endSubelementIndex@
endSubelementIndexSelector :: Selector
endSubelementIndexSelector = mkSelector "endSubelementIndex"

-- | @Selector@ for @setEndSubelementIndex:@
setEndSubelementIndexSelector :: Selector
setEndSubelementIndexSelector = mkSelector "setEndSubelementIndex:"

