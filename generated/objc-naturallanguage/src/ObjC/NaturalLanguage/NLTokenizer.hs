{-# LANGUAGE PatternSynonyms #-}
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
  , initWithUnitSelector
  , setLanguageSelector
  , tokenRangeAtIndexSelector
  , tokenRangeForRangeSelector
  , tokensForRangeSelector
  , enumerateTokensInRange_usingBlockSelector
  , unitSelector
  , stringSelector
  , setStringSelector

  -- * Enum types
  , NLTokenUnit(NLTokenUnit)
  , pattern NLTokenUnitWord
  , pattern NLTokenUnitSentence
  , pattern NLTokenUnitParagraph
  , pattern NLTokenUnitDocument

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithUnit:@
initWithUnit :: IsNLTokenizer nlTokenizer => nlTokenizer -> NLTokenUnit -> IO (Id NLTokenizer)
initWithUnit nlTokenizer  unit =
  sendMsg nlTokenizer (mkSelector "initWithUnit:") (retPtr retVoid) [argCLong (coerce unit)] >>= ownedObject . castPtr

-- | @- setLanguage:@
setLanguage :: (IsNLTokenizer nlTokenizer, IsNSString language) => nlTokenizer -> language -> IO ()
setLanguage nlTokenizer  language =
withObjCPtr language $ \raw_language ->
    sendMsg nlTokenizer (mkSelector "setLanguage:") retVoid [argPtr (castPtr raw_language :: Ptr ())]

-- | @- tokenRangeAtIndex:@
tokenRangeAtIndex :: IsNLTokenizer nlTokenizer => nlTokenizer -> CULong -> IO NSRange
tokenRangeAtIndex nlTokenizer  characterIndex =
  sendMsgStret nlTokenizer (mkSelector "tokenRangeAtIndex:") retNSRange [argCULong (fromIntegral characterIndex)]

-- | @- tokenRangeForRange:@
tokenRangeForRange :: IsNLTokenizer nlTokenizer => nlTokenizer -> NSRange -> IO NSRange
tokenRangeForRange nlTokenizer  range =
  sendMsgStret nlTokenizer (mkSelector "tokenRangeForRange:") retNSRange [argNSRange range]

-- | @- tokensForRange:@
tokensForRange :: IsNLTokenizer nlTokenizer => nlTokenizer -> NSRange -> IO (Id NSArray)
tokensForRange nlTokenizer  range =
  sendMsg nlTokenizer (mkSelector "tokensForRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- enumerateTokensInRange:usingBlock:@
enumerateTokensInRange_usingBlock :: IsNLTokenizer nlTokenizer => nlTokenizer -> NSRange -> Ptr () -> IO ()
enumerateTokensInRange_usingBlock nlTokenizer  range block =
  sendMsg nlTokenizer (mkSelector "enumerateTokensInRange:usingBlock:") retVoid [argNSRange range, argPtr (castPtr block :: Ptr ())]

-- | @- unit@
unit :: IsNLTokenizer nlTokenizer => nlTokenizer -> IO NLTokenUnit
unit nlTokenizer  =
  fmap (coerce :: CLong -> NLTokenUnit) $ sendMsg nlTokenizer (mkSelector "unit") retCLong []

-- | @- string@
string :: IsNLTokenizer nlTokenizer => nlTokenizer -> IO (Id NSString)
string nlTokenizer  =
  sendMsg nlTokenizer (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setString:@
setString :: (IsNLTokenizer nlTokenizer, IsNSString value) => nlTokenizer -> value -> IO ()
setString nlTokenizer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nlTokenizer (mkSelector "setString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUnit:@
initWithUnitSelector :: Selector
initWithUnitSelector = mkSelector "initWithUnit:"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @tokenRangeAtIndex:@
tokenRangeAtIndexSelector :: Selector
tokenRangeAtIndexSelector = mkSelector "tokenRangeAtIndex:"

-- | @Selector@ for @tokenRangeForRange:@
tokenRangeForRangeSelector :: Selector
tokenRangeForRangeSelector = mkSelector "tokenRangeForRange:"

-- | @Selector@ for @tokensForRange:@
tokensForRangeSelector :: Selector
tokensForRangeSelector = mkSelector "tokensForRange:"

-- | @Selector@ for @enumerateTokensInRange:usingBlock:@
enumerateTokensInRange_usingBlockSelector :: Selector
enumerateTokensInRange_usingBlockSelector = mkSelector "enumerateTokensInRange:usingBlock:"

-- | @Selector@ for @unit@
unitSelector :: Selector
unitSelector = mkSelector "unit"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

