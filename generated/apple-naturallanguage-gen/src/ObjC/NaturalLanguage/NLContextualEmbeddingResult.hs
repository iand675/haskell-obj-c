{-# LANGUAGE DataKinds #-}
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
  , languageSelector
  , sequenceLengthSelector
  , stringSelector
  , tokenVectorAtIndex_tokenRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO (Id NLContextualEmbeddingResult)
init_ nlContextualEmbeddingResult =
  sendOwnedMessage nlContextualEmbeddingResult initSelector

-- | @- tokenVectorAtIndex:tokenRange:@
tokenVectorAtIndex_tokenRange :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> CULong -> Ptr NSRange -> IO (Id NSArray)
tokenVectorAtIndex_tokenRange nlContextualEmbeddingResult characterIndex tokenRange =
  sendMessage nlContextualEmbeddingResult tokenVectorAtIndex_tokenRangeSelector characterIndex tokenRange

-- | @- string@
string :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO (Id NSString)
string nlContextualEmbeddingResult =
  sendMessage nlContextualEmbeddingResult stringSelector

-- | @- language@
language :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO (Id NSString)
language nlContextualEmbeddingResult =
  sendMessage nlContextualEmbeddingResult languageSelector

-- | @- sequenceLength@
sequenceLength :: IsNLContextualEmbeddingResult nlContextualEmbeddingResult => nlContextualEmbeddingResult -> IO CULong
sequenceLength nlContextualEmbeddingResult =
  sendMessage nlContextualEmbeddingResult sequenceLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NLContextualEmbeddingResult)
initSelector = mkSelector "init"

-- | @Selector@ for @tokenVectorAtIndex:tokenRange:@
tokenVectorAtIndex_tokenRangeSelector :: Selector '[CULong, Ptr NSRange] (Id NSArray)
tokenVectorAtIndex_tokenRangeSelector = mkSelector "tokenVectorAtIndex:tokenRange:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @sequenceLength@
sequenceLengthSelector :: Selector '[] CULong
sequenceLengthSelector = mkSelector "sequenceLength"

