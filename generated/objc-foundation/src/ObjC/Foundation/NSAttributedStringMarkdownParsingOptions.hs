{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedStringMarkdownParsingOptions@.
module ObjC.Foundation.NSAttributedStringMarkdownParsingOptions
  ( NSAttributedStringMarkdownParsingOptions
  , IsNSAttributedStringMarkdownParsingOptions(..)
  , init_
  , allowsExtendedAttributes
  , setAllowsExtendedAttributes
  , interpretedSyntax
  , setInterpretedSyntax
  , failurePolicy
  , setFailurePolicy
  , languageCode
  , setLanguageCode
  , appliesSourcePositionAttributes
  , setAppliesSourcePositionAttributes
  , initSelector
  , allowsExtendedAttributesSelector
  , setAllowsExtendedAttributesSelector
  , interpretedSyntaxSelector
  , setInterpretedSyntaxSelector
  , failurePolicySelector
  , setFailurePolicySelector
  , languageCodeSelector
  , setLanguageCodeSelector
  , appliesSourcePositionAttributesSelector
  , setAppliesSourcePositionAttributesSelector

  -- * Enum types
  , NSAttributedStringMarkdownInterpretedSyntax(NSAttributedStringMarkdownInterpretedSyntax)
  , pattern NSAttributedStringMarkdownInterpretedSyntaxFull
  , pattern NSAttributedStringMarkdownInterpretedSyntaxInlineOnly
  , pattern NSAttributedStringMarkdownInterpretedSyntaxInlineOnlyPreservingWhitespace
  , NSAttributedStringMarkdownParsingFailurePolicy(NSAttributedStringMarkdownParsingFailurePolicy)
  , pattern NSAttributedStringMarkdownParsingFailureReturnError
  , pattern NSAttributedStringMarkdownParsingFailureReturnPartiallyParsedIfPossible

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
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO (Id NSAttributedStringMarkdownParsingOptions)
init_ nsAttributedStringMarkdownParsingOptions  =
  sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- allowsExtendedAttributes@
allowsExtendedAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO Bool
allowsExtendedAttributes nsAttributedStringMarkdownParsingOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "allowsExtendedAttributes") retCULong []

-- | @- setAllowsExtendedAttributes:@
setAllowsExtendedAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> Bool -> IO ()
setAllowsExtendedAttributes nsAttributedStringMarkdownParsingOptions  value =
  sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "setAllowsExtendedAttributes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- interpretedSyntax@
interpretedSyntax :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO NSAttributedStringMarkdownInterpretedSyntax
interpretedSyntax nsAttributedStringMarkdownParsingOptions  =
  fmap (coerce :: CLong -> NSAttributedStringMarkdownInterpretedSyntax) $ sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "interpretedSyntax") retCLong []

-- | @- setInterpretedSyntax:@
setInterpretedSyntax :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> NSAttributedStringMarkdownInterpretedSyntax -> IO ()
setInterpretedSyntax nsAttributedStringMarkdownParsingOptions  value =
  sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "setInterpretedSyntax:") retVoid [argCLong (coerce value)]

-- | @- failurePolicy@
failurePolicy :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO NSAttributedStringMarkdownParsingFailurePolicy
failurePolicy nsAttributedStringMarkdownParsingOptions  =
  fmap (coerce :: CLong -> NSAttributedStringMarkdownParsingFailurePolicy) $ sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "failurePolicy") retCLong []

-- | @- setFailurePolicy:@
setFailurePolicy :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> NSAttributedStringMarkdownParsingFailurePolicy -> IO ()
setFailurePolicy nsAttributedStringMarkdownParsingOptions  value =
  sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "setFailurePolicy:") retVoid [argCLong (coerce value)]

-- | @- languageCode@
languageCode :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO (Id NSString)
languageCode nsAttributedStringMarkdownParsingOptions  =
  sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "languageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguageCode:@
setLanguageCode :: (IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions, IsNSString value) => nsAttributedStringMarkdownParsingOptions -> value -> IO ()
setLanguageCode nsAttributedStringMarkdownParsingOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "setLanguageCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- appliesSourcePositionAttributes@
appliesSourcePositionAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO Bool
appliesSourcePositionAttributes nsAttributedStringMarkdownParsingOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "appliesSourcePositionAttributes") retCULong []

-- | @- setAppliesSourcePositionAttributes:@
setAppliesSourcePositionAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> Bool -> IO ()
setAppliesSourcePositionAttributes nsAttributedStringMarkdownParsingOptions  value =
  sendMsg nsAttributedStringMarkdownParsingOptions (mkSelector "setAppliesSourcePositionAttributes:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @allowsExtendedAttributes@
allowsExtendedAttributesSelector :: Selector
allowsExtendedAttributesSelector = mkSelector "allowsExtendedAttributes"

-- | @Selector@ for @setAllowsExtendedAttributes:@
setAllowsExtendedAttributesSelector :: Selector
setAllowsExtendedAttributesSelector = mkSelector "setAllowsExtendedAttributes:"

-- | @Selector@ for @interpretedSyntax@
interpretedSyntaxSelector :: Selector
interpretedSyntaxSelector = mkSelector "interpretedSyntax"

-- | @Selector@ for @setInterpretedSyntax:@
setInterpretedSyntaxSelector :: Selector
setInterpretedSyntaxSelector = mkSelector "setInterpretedSyntax:"

-- | @Selector@ for @failurePolicy@
failurePolicySelector :: Selector
failurePolicySelector = mkSelector "failurePolicy"

-- | @Selector@ for @setFailurePolicy:@
setFailurePolicySelector :: Selector
setFailurePolicySelector = mkSelector "setFailurePolicy:"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @appliesSourcePositionAttributes@
appliesSourcePositionAttributesSelector :: Selector
appliesSourcePositionAttributesSelector = mkSelector "appliesSourcePositionAttributes"

-- | @Selector@ for @setAppliesSourcePositionAttributes:@
setAppliesSourcePositionAttributesSelector :: Selector
setAppliesSourcePositionAttributesSelector = mkSelector "setAppliesSourcePositionAttributes:"

