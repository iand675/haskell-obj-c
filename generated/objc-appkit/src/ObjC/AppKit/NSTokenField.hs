{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTokenField@.
module ObjC.AppKit.NSTokenField
  ( NSTokenField
  , IsNSTokenField(..)
  , tokenStyle
  , setTokenStyle
  , completionDelay
  , setCompletionDelay
  , defaultCompletionDelay
  , tokenizingCharacterSet
  , setTokenizingCharacterSet
  , defaultTokenizingCharacterSet
  , tokenStyleSelector
  , setTokenStyleSelector
  , completionDelaySelector
  , setCompletionDelaySelector
  , defaultCompletionDelaySelector
  , tokenizingCharacterSetSelector
  , setTokenizingCharacterSetSelector
  , defaultTokenizingCharacterSetSelector

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
tokenStyle :: IsNSTokenField nsTokenField => nsTokenField -> IO NSTokenStyle
tokenStyle nsTokenField  =
  fmap (coerce :: CULong -> NSTokenStyle) $ sendMsg nsTokenField (mkSelector "tokenStyle") retCULong []

-- | @- setTokenStyle:@
setTokenStyle :: IsNSTokenField nsTokenField => nsTokenField -> NSTokenStyle -> IO ()
setTokenStyle nsTokenField  value =
  sendMsg nsTokenField (mkSelector "setTokenStyle:") retVoid [argCULong (coerce value)]

-- | @- completionDelay@
completionDelay :: IsNSTokenField nsTokenField => nsTokenField -> IO CDouble
completionDelay nsTokenField  =
  sendMsg nsTokenField (mkSelector "completionDelay") retCDouble []

-- | @- setCompletionDelay:@
setCompletionDelay :: IsNSTokenField nsTokenField => nsTokenField -> CDouble -> IO ()
setCompletionDelay nsTokenField  value =
  sendMsg nsTokenField (mkSelector "setCompletionDelay:") retVoid [argCDouble (fromIntegral value)]

-- | @+ defaultCompletionDelay@
defaultCompletionDelay :: IO CDouble
defaultCompletionDelay  =
  do
    cls' <- getRequiredClass "NSTokenField"
    sendClassMsg cls' (mkSelector "defaultCompletionDelay") retCDouble []

-- | @- tokenizingCharacterSet@
tokenizingCharacterSet :: IsNSTokenField nsTokenField => nsTokenField -> IO (Id NSCharacterSet)
tokenizingCharacterSet nsTokenField  =
  sendMsg nsTokenField (mkSelector "tokenizingCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTokenizingCharacterSet:@
setTokenizingCharacterSet :: (IsNSTokenField nsTokenField, IsNSCharacterSet value) => nsTokenField -> value -> IO ()
setTokenizingCharacterSet nsTokenField  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTokenField (mkSelector "setTokenizingCharacterSet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ defaultTokenizingCharacterSet@
defaultTokenizingCharacterSet :: IO (Id NSCharacterSet)
defaultTokenizingCharacterSet  =
  do
    cls' <- getRequiredClass "NSTokenField"
    sendClassMsg cls' (mkSelector "defaultTokenizingCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

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

