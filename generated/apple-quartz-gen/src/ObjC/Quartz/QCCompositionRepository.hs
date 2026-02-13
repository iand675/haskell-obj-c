{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCCompositionRepository@.
module ObjC.Quartz.QCCompositionRepository
  ( QCCompositionRepository
  , IsQCCompositionRepository(..)
  , sharedCompositionRepository
  , compositionWithIdentifier
  , compositionsWithProtocols_andAttributes
  , allCompositions
  , allCompositionsSelector
  , compositionWithIdentifierSelector
  , compositionsWithProtocols_andAttributesSelector
  , sharedCompositionRepositorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedCompositionRepository@
sharedCompositionRepository :: IO (Id QCCompositionRepository)
sharedCompositionRepository  =
  do
    cls' <- getRequiredClass "QCCompositionRepository"
    sendClassMessage cls' sharedCompositionRepositorySelector

-- | @- compositionWithIdentifier:@
compositionWithIdentifier :: (IsQCCompositionRepository qcCompositionRepository, IsNSString identifier) => qcCompositionRepository -> identifier -> IO (Id QCComposition)
compositionWithIdentifier qcCompositionRepository identifier =
  sendMessage qcCompositionRepository compositionWithIdentifierSelector (toNSString identifier)

-- | @- compositionsWithProtocols:andAttributes:@
compositionsWithProtocols_andAttributes :: (IsQCCompositionRepository qcCompositionRepository, IsNSArray protocols, IsNSDictionary attributes) => qcCompositionRepository -> protocols -> attributes -> IO (Id NSArray)
compositionsWithProtocols_andAttributes qcCompositionRepository protocols attributes =
  sendMessage qcCompositionRepository compositionsWithProtocols_andAttributesSelector (toNSArray protocols) (toNSDictionary attributes)

-- | @- allCompositions@
allCompositions :: IsQCCompositionRepository qcCompositionRepository => qcCompositionRepository -> IO (Id NSArray)
allCompositions qcCompositionRepository =
  sendMessage qcCompositionRepository allCompositionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCompositionRepository@
sharedCompositionRepositorySelector :: Selector '[] (Id QCCompositionRepository)
sharedCompositionRepositorySelector = mkSelector "sharedCompositionRepository"

-- | @Selector@ for @compositionWithIdentifier:@
compositionWithIdentifierSelector :: Selector '[Id NSString] (Id QCComposition)
compositionWithIdentifierSelector = mkSelector "compositionWithIdentifier:"

-- | @Selector@ for @compositionsWithProtocols:andAttributes:@
compositionsWithProtocols_andAttributesSelector :: Selector '[Id NSArray, Id NSDictionary] (Id NSArray)
compositionsWithProtocols_andAttributesSelector = mkSelector "compositionsWithProtocols:andAttributes:"

-- | @Selector@ for @allCompositions@
allCompositionsSelector :: Selector '[] (Id NSArray)
allCompositionsSelector = mkSelector "allCompositions"

