{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTokenField@.
module ObjC.AppKit.NSTokenField
  ( NSTokenField
  , IsNSTokenField(..)
  , delegate
  , setDelegate
  , tokenStyle
  , setTokenStyle
  , completionDelay
  , setCompletionDelay
  , defaultCompletionDelay
  , tokenizingCharacterSet
  , setTokenizingCharacterSet
  , defaultTokenizingCharacterSet
  , completionDelaySelector
  , defaultCompletionDelaySelector
  , defaultTokenizingCharacterSetSelector
  , delegateSelector
  , setCompletionDelaySelector
  , setDelegateSelector
  , setTokenStyleSelector
  , setTokenizingCharacterSetSelector
  , tokenStyleSelector
  , tokenizingCharacterSetSelector

  -- * Enum types
  , NSTokenStyle(NSTokenStyle)
  , pattern NSTokenStyleDefault
  , pattern NSTokenStyleNone
  , pattern NSTokenStyleRounded
  , pattern NSTokenStyleSquared
  , pattern NSTokenStylePlainSquared

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- delegate@
delegate :: IsNSTokenField nsTokenField => nsTokenField -> IO RawId
delegate nsTokenField =
  sendMessage nsTokenField delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTokenField nsTokenField => nsTokenField -> RawId -> IO ()
setDelegate nsTokenField value =
  sendMessage nsTokenField setDelegateSelector value

-- | @- tokenStyle@
tokenStyle :: IsNSTokenField nsTokenField => nsTokenField -> IO NSTokenStyle
tokenStyle nsTokenField =
  sendMessage nsTokenField tokenStyleSelector

-- | @- setTokenStyle:@
setTokenStyle :: IsNSTokenField nsTokenField => nsTokenField -> NSTokenStyle -> IO ()
setTokenStyle nsTokenField value =
  sendMessage nsTokenField setTokenStyleSelector value

-- | @- completionDelay@
completionDelay :: IsNSTokenField nsTokenField => nsTokenField -> IO CDouble
completionDelay nsTokenField =
  sendMessage nsTokenField completionDelaySelector

-- | @- setCompletionDelay:@
setCompletionDelay :: IsNSTokenField nsTokenField => nsTokenField -> CDouble -> IO ()
setCompletionDelay nsTokenField value =
  sendMessage nsTokenField setCompletionDelaySelector value

-- | @+ defaultCompletionDelay@
defaultCompletionDelay :: IO CDouble
defaultCompletionDelay  =
  do
    cls' <- getRequiredClass "NSTokenField"
    sendClassMessage cls' defaultCompletionDelaySelector

-- | @- tokenizingCharacterSet@
tokenizingCharacterSet :: IsNSTokenField nsTokenField => nsTokenField -> IO (Id NSCharacterSet)
tokenizingCharacterSet nsTokenField =
  sendMessage nsTokenField tokenizingCharacterSetSelector

-- | @- setTokenizingCharacterSet:@
setTokenizingCharacterSet :: (IsNSTokenField nsTokenField, IsNSCharacterSet value) => nsTokenField -> value -> IO ()
setTokenizingCharacterSet nsTokenField value =
  sendMessage nsTokenField setTokenizingCharacterSetSelector (toNSCharacterSet value)

-- | @+ defaultTokenizingCharacterSet@
defaultTokenizingCharacterSet :: IO (Id NSCharacterSet)
defaultTokenizingCharacterSet  =
  do
    cls' <- getRequiredClass "NSTokenField"
    sendClassMessage cls' defaultTokenizingCharacterSetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @tokenStyle@
tokenStyleSelector :: Selector '[] NSTokenStyle
tokenStyleSelector = mkSelector "tokenStyle"

-- | @Selector@ for @setTokenStyle:@
setTokenStyleSelector :: Selector '[NSTokenStyle] ()
setTokenStyleSelector = mkSelector "setTokenStyle:"

-- | @Selector@ for @completionDelay@
completionDelaySelector :: Selector '[] CDouble
completionDelaySelector = mkSelector "completionDelay"

-- | @Selector@ for @setCompletionDelay:@
setCompletionDelaySelector :: Selector '[CDouble] ()
setCompletionDelaySelector = mkSelector "setCompletionDelay:"

-- | @Selector@ for @defaultCompletionDelay@
defaultCompletionDelaySelector :: Selector '[] CDouble
defaultCompletionDelaySelector = mkSelector "defaultCompletionDelay"

-- | @Selector@ for @tokenizingCharacterSet@
tokenizingCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
tokenizingCharacterSetSelector = mkSelector "tokenizingCharacterSet"

-- | @Selector@ for @setTokenizingCharacterSet:@
setTokenizingCharacterSetSelector :: Selector '[Id NSCharacterSet] ()
setTokenizingCharacterSetSelector = mkSelector "setTokenizingCharacterSet:"

-- | @Selector@ for @defaultTokenizingCharacterSet@
defaultTokenizingCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
defaultTokenizingCharacterSetSelector = mkSelector "defaultTokenizingCharacterSet"

