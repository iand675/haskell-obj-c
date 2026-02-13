{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , endSubelementIdentifierSelector
  , endSubelementIndexSelector
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_testSelector
  , setEndSubelementIdentifierSelector
  , setEndSubelementIndexSelector
  , setStartSubelementIdentifierSelector
  , setStartSubelementIndexSelector
  , setTestSelector
  , startSubelementIdentifierSelector
  , startSubelementIndexSelector
  , testSelector

  -- * Enum types
  , NSWhoseSubelementIdentifier(NSWhoseSubelementIdentifier)
  , pattern NSIndexSubelement
  , pattern NSEverySubelement
  , pattern NSMiddleSubelement
  , pattern NSRandomSubelement
  , pattern NSNoSubelement

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
initWithCoder :: (IsNSWhoseSpecifier nsWhoseSpecifier, IsNSCoder inCoder) => nsWhoseSpecifier -> inCoder -> IO (Id NSWhoseSpecifier)
initWithCoder nsWhoseSpecifier inCoder =
  sendOwnedMessage nsWhoseSpecifier initWithCoderSelector (toNSCoder inCoder)

-- | @- initWithContainerClassDescription:containerSpecifier:key:test:@
initWithContainerClassDescription_containerSpecifier_key_test :: (IsNSWhoseSpecifier nsWhoseSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSScriptWhoseTest test) => nsWhoseSpecifier -> classDesc -> container -> property -> test -> IO (Id NSWhoseSpecifier)
initWithContainerClassDescription_containerSpecifier_key_test nsWhoseSpecifier classDesc container property test =
  sendOwnedMessage nsWhoseSpecifier initWithContainerClassDescription_containerSpecifier_key_testSelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property) (toNSScriptWhoseTest test)

-- | @- test@
test :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO (Id NSScriptWhoseTest)
test nsWhoseSpecifier =
  sendMessage nsWhoseSpecifier testSelector

-- | @- setTest:@
setTest :: (IsNSWhoseSpecifier nsWhoseSpecifier, IsNSScriptWhoseTest value) => nsWhoseSpecifier -> value -> IO ()
setTest nsWhoseSpecifier value =
  sendMessage nsWhoseSpecifier setTestSelector (toNSScriptWhoseTest value)

-- | @- startSubelementIdentifier@
startSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO NSWhoseSubelementIdentifier
startSubelementIdentifier nsWhoseSpecifier =
  sendMessage nsWhoseSpecifier startSubelementIdentifierSelector

-- | @- setStartSubelementIdentifier:@
setStartSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> NSWhoseSubelementIdentifier -> IO ()
setStartSubelementIdentifier nsWhoseSpecifier value =
  sendMessage nsWhoseSpecifier setStartSubelementIdentifierSelector value

-- | @- startSubelementIndex@
startSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO CLong
startSubelementIndex nsWhoseSpecifier =
  sendMessage nsWhoseSpecifier startSubelementIndexSelector

-- | @- setStartSubelementIndex:@
setStartSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> CLong -> IO ()
setStartSubelementIndex nsWhoseSpecifier value =
  sendMessage nsWhoseSpecifier setStartSubelementIndexSelector value

-- | @- endSubelementIdentifier@
endSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO NSWhoseSubelementIdentifier
endSubelementIdentifier nsWhoseSpecifier =
  sendMessage nsWhoseSpecifier endSubelementIdentifierSelector

-- | @- setEndSubelementIdentifier:@
setEndSubelementIdentifier :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> NSWhoseSubelementIdentifier -> IO ()
setEndSubelementIdentifier nsWhoseSpecifier value =
  sendMessage nsWhoseSpecifier setEndSubelementIdentifierSelector value

-- | @- endSubelementIndex@
endSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> IO CLong
endSubelementIndex nsWhoseSpecifier =
  sendMessage nsWhoseSpecifier endSubelementIndexSelector

-- | @- setEndSubelementIndex:@
setEndSubelementIndex :: IsNSWhoseSpecifier nsWhoseSpecifier => nsWhoseSpecifier -> CLong -> IO ()
setEndSubelementIndex nsWhoseSpecifier value =
  sendMessage nsWhoseSpecifier setEndSubelementIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSWhoseSpecifier)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:test:@
initWithContainerClassDescription_containerSpecifier_key_testSelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString, Id NSScriptWhoseTest] (Id NSWhoseSpecifier)
initWithContainerClassDescription_containerSpecifier_key_testSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:test:"

-- | @Selector@ for @test@
testSelector :: Selector '[] (Id NSScriptWhoseTest)
testSelector = mkSelector "test"

-- | @Selector@ for @setTest:@
setTestSelector :: Selector '[Id NSScriptWhoseTest] ()
setTestSelector = mkSelector "setTest:"

-- | @Selector@ for @startSubelementIdentifier@
startSubelementIdentifierSelector :: Selector '[] NSWhoseSubelementIdentifier
startSubelementIdentifierSelector = mkSelector "startSubelementIdentifier"

-- | @Selector@ for @setStartSubelementIdentifier:@
setStartSubelementIdentifierSelector :: Selector '[NSWhoseSubelementIdentifier] ()
setStartSubelementIdentifierSelector = mkSelector "setStartSubelementIdentifier:"

-- | @Selector@ for @startSubelementIndex@
startSubelementIndexSelector :: Selector '[] CLong
startSubelementIndexSelector = mkSelector "startSubelementIndex"

-- | @Selector@ for @setStartSubelementIndex:@
setStartSubelementIndexSelector :: Selector '[CLong] ()
setStartSubelementIndexSelector = mkSelector "setStartSubelementIndex:"

-- | @Selector@ for @endSubelementIdentifier@
endSubelementIdentifierSelector :: Selector '[] NSWhoseSubelementIdentifier
endSubelementIdentifierSelector = mkSelector "endSubelementIdentifier"

-- | @Selector@ for @setEndSubelementIdentifier:@
setEndSubelementIdentifierSelector :: Selector '[NSWhoseSubelementIdentifier] ()
setEndSubelementIdentifierSelector = mkSelector "setEndSubelementIdentifier:"

-- | @Selector@ for @endSubelementIndex@
endSubelementIndexSelector :: Selector '[] CLong
endSubelementIndexSelector = mkSelector "endSubelementIndex"

-- | @Selector@ for @setEndSubelementIndex:@
setEndSubelementIndexSelector :: Selector '[CLong] ()
setEndSubelementIndexSelector = mkSelector "setEndSubelementIndex:"

