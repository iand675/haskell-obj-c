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
  , sharedCompositionRepositorySelector
  , compositionWithIdentifierSelector
  , compositionsWithProtocols_andAttributesSelector
  , allCompositionsSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedCompositionRepository@
sharedCompositionRepository :: IO (Id QCCompositionRepository)
sharedCompositionRepository  =
  do
    cls' <- getRequiredClass "QCCompositionRepository"
    sendClassMsg cls' (mkSelector "sharedCompositionRepository") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- compositionWithIdentifier:@
compositionWithIdentifier :: (IsQCCompositionRepository qcCompositionRepository, IsNSString identifier) => qcCompositionRepository -> identifier -> IO (Id QCComposition)
compositionWithIdentifier qcCompositionRepository  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg qcCompositionRepository (mkSelector "compositionWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- compositionsWithProtocols:andAttributes:@
compositionsWithProtocols_andAttributes :: (IsQCCompositionRepository qcCompositionRepository, IsNSArray protocols, IsNSDictionary attributes) => qcCompositionRepository -> protocols -> attributes -> IO (Id NSArray)
compositionsWithProtocols_andAttributes qcCompositionRepository  protocols attributes =
withObjCPtr protocols $ \raw_protocols ->
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg qcCompositionRepository (mkSelector "compositionsWithProtocols:andAttributes:") (retPtr retVoid) [argPtr (castPtr raw_protocols :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @- allCompositions@
allCompositions :: IsQCCompositionRepository qcCompositionRepository => qcCompositionRepository -> IO (Id NSArray)
allCompositions qcCompositionRepository  =
  sendMsg qcCompositionRepository (mkSelector "allCompositions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCompositionRepository@
sharedCompositionRepositorySelector :: Selector
sharedCompositionRepositorySelector = mkSelector "sharedCompositionRepository"

-- | @Selector@ for @compositionWithIdentifier:@
compositionWithIdentifierSelector :: Selector
compositionWithIdentifierSelector = mkSelector "compositionWithIdentifier:"

-- | @Selector@ for @compositionsWithProtocols:andAttributes:@
compositionsWithProtocols_andAttributesSelector :: Selector
compositionsWithProtocols_andAttributesSelector = mkSelector "compositionsWithProtocols:andAttributes:"

-- | @Selector@ for @allCompositions@
allCompositionsSelector :: Selector
allCompositionsSelector = mkSelector "allCompositions"

