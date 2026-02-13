{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsExtendedAttributesSelector
  , appliesSourcePositionAttributesSelector
  , failurePolicySelector
  , initSelector
  , interpretedSyntaxSelector
  , languageCodeSelector
  , setAllowsExtendedAttributesSelector
  , setAppliesSourcePositionAttributesSelector
  , setFailurePolicySelector
  , setInterpretedSyntaxSelector
  , setLanguageCodeSelector

  -- * Enum types
  , NSAttributedStringMarkdownInterpretedSyntax(NSAttributedStringMarkdownInterpretedSyntax)
  , pattern NSAttributedStringMarkdownInterpretedSyntaxFull
  , pattern NSAttributedStringMarkdownInterpretedSyntaxInlineOnly
  , pattern NSAttributedStringMarkdownInterpretedSyntaxInlineOnlyPreservingWhitespace
  , NSAttributedStringMarkdownParsingFailurePolicy(NSAttributedStringMarkdownParsingFailurePolicy)
  , pattern NSAttributedStringMarkdownParsingFailureReturnError
  , pattern NSAttributedStringMarkdownParsingFailureReturnPartiallyParsedIfPossible

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO (Id NSAttributedStringMarkdownParsingOptions)
init_ nsAttributedStringMarkdownParsingOptions =
  sendOwnedMessage nsAttributedStringMarkdownParsingOptions initSelector

-- | @- allowsExtendedAttributes@
allowsExtendedAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO Bool
allowsExtendedAttributes nsAttributedStringMarkdownParsingOptions =
  sendMessage nsAttributedStringMarkdownParsingOptions allowsExtendedAttributesSelector

-- | @- setAllowsExtendedAttributes:@
setAllowsExtendedAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> Bool -> IO ()
setAllowsExtendedAttributes nsAttributedStringMarkdownParsingOptions value =
  sendMessage nsAttributedStringMarkdownParsingOptions setAllowsExtendedAttributesSelector value

-- | @- interpretedSyntax@
interpretedSyntax :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO NSAttributedStringMarkdownInterpretedSyntax
interpretedSyntax nsAttributedStringMarkdownParsingOptions =
  sendMessage nsAttributedStringMarkdownParsingOptions interpretedSyntaxSelector

-- | @- setInterpretedSyntax:@
setInterpretedSyntax :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> NSAttributedStringMarkdownInterpretedSyntax -> IO ()
setInterpretedSyntax nsAttributedStringMarkdownParsingOptions value =
  sendMessage nsAttributedStringMarkdownParsingOptions setInterpretedSyntaxSelector value

-- | @- failurePolicy@
failurePolicy :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO NSAttributedStringMarkdownParsingFailurePolicy
failurePolicy nsAttributedStringMarkdownParsingOptions =
  sendMessage nsAttributedStringMarkdownParsingOptions failurePolicySelector

-- | @- setFailurePolicy:@
setFailurePolicy :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> NSAttributedStringMarkdownParsingFailurePolicy -> IO ()
setFailurePolicy nsAttributedStringMarkdownParsingOptions value =
  sendMessage nsAttributedStringMarkdownParsingOptions setFailurePolicySelector value

-- | @- languageCode@
languageCode :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO (Id NSString)
languageCode nsAttributedStringMarkdownParsingOptions =
  sendMessage nsAttributedStringMarkdownParsingOptions languageCodeSelector

-- | @- setLanguageCode:@
setLanguageCode :: (IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions, IsNSString value) => nsAttributedStringMarkdownParsingOptions -> value -> IO ()
setLanguageCode nsAttributedStringMarkdownParsingOptions value =
  sendMessage nsAttributedStringMarkdownParsingOptions setLanguageCodeSelector (toNSString value)

-- | @- appliesSourcePositionAttributes@
appliesSourcePositionAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> IO Bool
appliesSourcePositionAttributes nsAttributedStringMarkdownParsingOptions =
  sendMessage nsAttributedStringMarkdownParsingOptions appliesSourcePositionAttributesSelector

-- | @- setAppliesSourcePositionAttributes:@
setAppliesSourcePositionAttributes :: IsNSAttributedStringMarkdownParsingOptions nsAttributedStringMarkdownParsingOptions => nsAttributedStringMarkdownParsingOptions -> Bool -> IO ()
setAppliesSourcePositionAttributes nsAttributedStringMarkdownParsingOptions value =
  sendMessage nsAttributedStringMarkdownParsingOptions setAppliesSourcePositionAttributesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSAttributedStringMarkdownParsingOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @allowsExtendedAttributes@
allowsExtendedAttributesSelector :: Selector '[] Bool
allowsExtendedAttributesSelector = mkSelector "allowsExtendedAttributes"

-- | @Selector@ for @setAllowsExtendedAttributes:@
setAllowsExtendedAttributesSelector :: Selector '[Bool] ()
setAllowsExtendedAttributesSelector = mkSelector "setAllowsExtendedAttributes:"

-- | @Selector@ for @interpretedSyntax@
interpretedSyntaxSelector :: Selector '[] NSAttributedStringMarkdownInterpretedSyntax
interpretedSyntaxSelector = mkSelector "interpretedSyntax"

-- | @Selector@ for @setInterpretedSyntax:@
setInterpretedSyntaxSelector :: Selector '[NSAttributedStringMarkdownInterpretedSyntax] ()
setInterpretedSyntaxSelector = mkSelector "setInterpretedSyntax:"

-- | @Selector@ for @failurePolicy@
failurePolicySelector :: Selector '[] NSAttributedStringMarkdownParsingFailurePolicy
failurePolicySelector = mkSelector "failurePolicy"

-- | @Selector@ for @setFailurePolicy:@
setFailurePolicySelector :: Selector '[NSAttributedStringMarkdownParsingFailurePolicy] ()
setFailurePolicySelector = mkSelector "setFailurePolicy:"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector '[] (Id NSString)
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector '[Id NSString] ()
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @appliesSourcePositionAttributes@
appliesSourcePositionAttributesSelector :: Selector '[] Bool
appliesSourcePositionAttributesSelector = mkSelector "appliesSourcePositionAttributes"

-- | @Selector@ for @setAppliesSourcePositionAttributes:@
setAppliesSourcePositionAttributesSelector :: Selector '[Bool] ()
setAppliesSourcePositionAttributesSelector = mkSelector "setAppliesSourcePositionAttributes:"

