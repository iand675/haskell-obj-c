{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLContextualEmbeddingResult@.
module ObjC.NaturalLanguage.NLContextualEmbeddingResult
  ( NLContextualEmbeddingResult
  , IsNLContextualEmbeddingResult(..)
  , init_
  , tokenVectorAtIndex_tokenRange
  , string
  , language
  , sequenceLength
  , initSelector
  , tokenVectorAtIndex_tokenRangeSelector
  , stringSelector
  , languageSelector
  , sequenceLengthSelector


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

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO (Id NLContextualEmbeddingResult)
init_ nlContextualEmbeddingResult  =
  sendMsg nlContextualEmbeddingResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- tokenVectorAtIndex:tokenRange:@
tokenVectorAtIndex_tokenRange :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> CULong -> Ptr NSRange -> IO (Id NSArray)
tokenVectorAtIndex_tokenRange nlContextualEmbeddingResult  characterIndex tokenRange =
  sendMsg nlContextualEmbeddingResult (mkSelector "tokenVectorAtIndex:tokenRange:") (retPtr retVoid) [argCULong (fromIntegral characterIndex), argPtr tokenRange] >>= retainedObject . castPtr

-- | @- string@
string :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO (Id NSString)
string nlContextualEmbeddingResult  =
  sendMsg nlContextualEmbeddingResult (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- language@
language :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO (Id NSString)
language nlContextualEmbeddingResult  =
  sendMsg nlContextualEmbeddingResult (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sequenceLength@
sequenceLength :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO CULong
sequenceLength nlContextualEmbeddingResult  =
  sendMsg nlContextualEmbeddingResult (mkSelector "sequenceLength") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @tokenVectorAtIndex:tokenRange:@
tokenVectorAtIndex_tokenRangeSelector :: Selector
tokenVectorAtIndex_tokenRangeSelector = mkSelector "tokenVectorAtIndex:tokenRange:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @sequenceLength@
sequenceLengthSelector :: Selector
sequenceLengthSelector = mkSelector "sequenceLength"

