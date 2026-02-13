{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLTokenizer@.
module ObjC.NaturalLanguage.NLTokenizer
  ( NLTokenizer
  , IsNLTokenizer(..)
  , initWithUnit
  , setLanguage
  , tokenRangeAtIndex
  , tokenRangeForRange
  , tokensForRange
  , enumerateTokensInRange_usingBlock
  , unit
  , string
  , setString
  , enumerateTokensInRange_usingBlockSelector
  , initWithUnitSelector
  , setLanguageSelector
  , setStringSelector
  , stringSelector
  , tokenRangeAtIndexSelector
  , tokenRangeForRangeSelector
  , tokensForRangeSelector
  , unitSelector

  -- * Enum types
  , NLTokenUnit(NLTokenUnit)
  , pattern NLTokenUnitWord
  , pattern NLTokenUnitSentence
  , pattern NLTokenUnitParagraph
  , pattern NLTokenUnitDocument

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithUnit:@
initWithUnit :: IsNLTokenizer nlTokenizer => nlTokenizer -> NLTokenUnit -> IO (Id NLTokenizer)
initWithUnit nlTokenizer unit =
  sendOwnedMessage nlTokenizer initWithUnitSelector unit

-- | @- setLanguage:@
setLanguage :: (IsNLTokenizer nlTokenizer, IsNSString language) => nlTokenizer -> language -> IO ()
setLanguage nlTokenizer language =
  sendMessage nlTokenizer setLanguageSelector (toNSString language)

-- | @- tokenRangeAtIndex:@
tokenRangeAtIndex :: IsNLTokenizer nlTokenizer => nlTokenizer -> CULong -> IO NSRange
tokenRangeAtIndex nlTokenizer characterIndex =
  sendMessage nlTokenizer tokenRangeAtIndexSelector characterIndex

-- | @- tokenRangeForRange:@
tokenRangeForRange :: IsNLTokenizer nlTokenizer => nlTokenizer -> NSRange -> IO NSRange
tokenRangeForRange nlTokenizer range =
  sendMessage nlTokenizer tokenRangeForRangeSelector range

-- | @- tokensForRange:@
tokensForRange :: IsNLTokenizer nlTokenizer => nlTokenizer -> NSRange -> IO (Id NSArray)
tokensForRange nlTokenizer range =
  sendMessage nlTokenizer tokensForRangeSelector range

-- | @- enumerateTokensInRange:usingBlock:@
enumerateTokensInRange_usingBlock :: IsNLTokenizer nlTokenizer => nlTokenizer -> NSRange -> Ptr () -> IO ()
enumerateTokensInRange_usingBlock nlTokenizer range block =
  sendMessage nlTokenizer enumerateTokensInRange_usingBlockSelector range block

-- | @- unit@
unit :: IsNLTokenizer nlTokenizer => nlTokenizer -> IO NLTokenUnit
unit nlTokenizer =
  sendMessage nlTokenizer unitSelector

-- | @- string@
string :: IsNLTokenizer nlTokenizer => nlTokenizer -> IO (Id NSString)
string nlTokenizer =
  sendMessage nlTokenizer stringSelector

-- | @- setString:@
setString :: (IsNLTokenizer nlTokenizer, IsNSString value) => nlTokenizer -> value -> IO ()
setString nlTokenizer value =
  sendMessage nlTokenizer setStringSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUnit:@
initWithUnitSelector :: Selector '[NLTokenUnit] (Id NLTokenizer)
initWithUnitSelector = mkSelector "initWithUnit:"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector '[Id NSString] ()
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @tokenRangeAtIndex:@
tokenRangeAtIndexSelector :: Selector '[CULong] NSRange
tokenRangeAtIndexSelector = mkSelector "tokenRangeAtIndex:"

-- | @Selector@ for @tokenRangeForRange:@
tokenRangeForRangeSelector :: Selector '[NSRange] NSRange
tokenRangeForRangeSelector = mkSelector "tokenRangeForRange:"

-- | @Selector@ for @tokensForRange:@
tokensForRangeSelector :: Selector '[NSRange] (Id NSArray)
tokensForRangeSelector = mkSelector "tokensForRange:"

-- | @Selector@ for @enumerateTokensInRange:usingBlock:@
enumerateTokensInRange_usingBlockSelector :: Selector '[NSRange, Ptr ()] ()
enumerateTokensInRange_usingBlockSelector = mkSelector "enumerateTokensInRange:usingBlock:"

-- | @Selector@ for @unit@
unitSelector :: Selector '[] NLTokenUnit
unitSelector = mkSelector "unit"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[Id NSString] ()
setStringSelector = mkSelector "setString:"

