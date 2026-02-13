{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTokenFieldCell@.
module ObjC.AppKit.NSTokenFieldCell
  ( NSTokenFieldCell
  , IsNSTokenFieldCell(..)
  , tokenStyle
  , setTokenStyle
  , completionDelay
  , setCompletionDelay
  , defaultCompletionDelay
  , tokenizingCharacterSet
  , setTokenizingCharacterSet
  , defaultTokenizingCharacterSet
  , delegate
  , setDelegate
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

-- | @- tokenStyle@
tokenStyle :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO NSTokenStyle
tokenStyle nsTokenFieldCell =
  sendMessage nsTokenFieldCell tokenStyleSelector

-- | @- setTokenStyle:@
setTokenStyle :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> NSTokenStyle -> IO ()
setTokenStyle nsTokenFieldCell value =
  sendMessage nsTokenFieldCell setTokenStyleSelector value

-- | @- completionDelay@
completionDelay :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO CDouble
completionDelay nsTokenFieldCell =
  sendMessage nsTokenFieldCell completionDelaySelector

-- | @- setCompletionDelay:@
setCompletionDelay :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> CDouble -> IO ()
setCompletionDelay nsTokenFieldCell value =
  sendMessage nsTokenFieldCell setCompletionDelaySelector value

-- | @+ defaultCompletionDelay@
defaultCompletionDelay :: IO CDouble
defaultCompletionDelay  =
  do
    cls' <- getRequiredClass "NSTokenFieldCell"
    sendClassMessage cls' defaultCompletionDelaySelector

-- | @- tokenizingCharacterSet@
tokenizingCharacterSet :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO (Id NSCharacterSet)
tokenizingCharacterSet nsTokenFieldCell =
  sendMessage nsTokenFieldCell tokenizingCharacterSetSelector

-- | @- setTokenizingCharacterSet:@
setTokenizingCharacterSet :: (IsNSTokenFieldCell nsTokenFieldCell, IsNSCharacterSet value) => nsTokenFieldCell -> value -> IO ()
setTokenizingCharacterSet nsTokenFieldCell value =
  sendMessage nsTokenFieldCell setTokenizingCharacterSetSelector (toNSCharacterSet value)

-- | @+ defaultTokenizingCharacterSet@
defaultTokenizingCharacterSet :: IO (Id NSCharacterSet)
defaultTokenizingCharacterSet  =
  do
    cls' <- getRequiredClass "NSTokenFieldCell"
    sendClassMessage cls' defaultTokenizingCharacterSetSelector

-- | @- delegate@
delegate :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO RawId
delegate nsTokenFieldCell =
  sendMessage nsTokenFieldCell delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> RawId -> IO ()
setDelegate nsTokenFieldCell value =
  sendMessage nsTokenFieldCell setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

