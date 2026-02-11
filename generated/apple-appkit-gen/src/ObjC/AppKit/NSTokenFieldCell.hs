{-# LANGUAGE PatternSynonyms #-}
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
  , tokenStyleSelector
  , setTokenStyleSelector
  , completionDelaySelector
  , setCompletionDelaySelector
  , defaultCompletionDelaySelector
  , tokenizingCharacterSetSelector
  , setTokenizingCharacterSetSelector
  , defaultTokenizingCharacterSetSelector
  , delegateSelector
  , setDelegateSelector

  -- * Enum types
  , NSTokenStyle(NSTokenStyle)
  , pattern NSTokenStyleDefault
  , pattern NSTokenStyleNone
  , pattern NSTokenStyleRounded
  , pattern NSTokenStyleSquared
  , pattern NSTokenStylePlainSquared

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- tokenStyle@
tokenStyle :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO NSTokenStyle
tokenStyle nsTokenFieldCell  =
    fmap (coerce :: CULong -> NSTokenStyle) $ sendMsg nsTokenFieldCell (mkSelector "tokenStyle") retCULong []

-- | @- setTokenStyle:@
setTokenStyle :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> NSTokenStyle -> IO ()
setTokenStyle nsTokenFieldCell  value =
    sendMsg nsTokenFieldCell (mkSelector "setTokenStyle:") retVoid [argCULong (coerce value)]

-- | @- completionDelay@
completionDelay :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO CDouble
completionDelay nsTokenFieldCell  =
    sendMsg nsTokenFieldCell (mkSelector "completionDelay") retCDouble []

-- | @- setCompletionDelay:@
setCompletionDelay :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> CDouble -> IO ()
setCompletionDelay nsTokenFieldCell  value =
    sendMsg nsTokenFieldCell (mkSelector "setCompletionDelay:") retVoid [argCDouble value]

-- | @+ defaultCompletionDelay@
defaultCompletionDelay :: IO CDouble
defaultCompletionDelay  =
  do
    cls' <- getRequiredClass "NSTokenFieldCell"
    sendClassMsg cls' (mkSelector "defaultCompletionDelay") retCDouble []

-- | @- tokenizingCharacterSet@
tokenizingCharacterSet :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO (Id NSCharacterSet)
tokenizingCharacterSet nsTokenFieldCell  =
    sendMsg nsTokenFieldCell (mkSelector "tokenizingCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTokenizingCharacterSet:@
setTokenizingCharacterSet :: (IsNSTokenFieldCell nsTokenFieldCell, IsNSCharacterSet value) => nsTokenFieldCell -> value -> IO ()
setTokenizingCharacterSet nsTokenFieldCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTokenFieldCell (mkSelector "setTokenizingCharacterSet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ defaultTokenizingCharacterSet@
defaultTokenizingCharacterSet :: IO (Id NSCharacterSet)
defaultTokenizingCharacterSet  =
  do
    cls' <- getRequiredClass "NSTokenFieldCell"
    sendClassMsg cls' (mkSelector "defaultTokenizingCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> IO RawId
delegate nsTokenFieldCell  =
    fmap (RawId . castPtr) $ sendMsg nsTokenFieldCell (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSTokenFieldCell nsTokenFieldCell => nsTokenFieldCell -> RawId -> IO ()
setDelegate nsTokenFieldCell  value =
    sendMsg nsTokenFieldCell (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tokenStyle@
tokenStyleSelector :: Selector
tokenStyleSelector = mkSelector "tokenStyle"

-- | @Selector@ for @setTokenStyle:@
setTokenStyleSelector :: Selector
setTokenStyleSelector = mkSelector "setTokenStyle:"

-- | @Selector@ for @completionDelay@
completionDelaySelector :: Selector
completionDelaySelector = mkSelector "completionDelay"

-- | @Selector@ for @setCompletionDelay:@
setCompletionDelaySelector :: Selector
setCompletionDelaySelector = mkSelector "setCompletionDelay:"

-- | @Selector@ for @defaultCompletionDelay@
defaultCompletionDelaySelector :: Selector
defaultCompletionDelaySelector = mkSelector "defaultCompletionDelay"

-- | @Selector@ for @tokenizingCharacterSet@
tokenizingCharacterSetSelector :: Selector
tokenizingCharacterSetSelector = mkSelector "tokenizingCharacterSet"

-- | @Selector@ for @setTokenizingCharacterSet:@
setTokenizingCharacterSetSelector :: Selector
setTokenizingCharacterSetSelector = mkSelector "setTokenizingCharacterSet:"

-- | @Selector@ for @defaultTokenizingCharacterSet@
defaultTokenizingCharacterSetSelector :: Selector
defaultTokenizingCharacterSetSelector = mkSelector "defaultTokenizingCharacterSet"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

