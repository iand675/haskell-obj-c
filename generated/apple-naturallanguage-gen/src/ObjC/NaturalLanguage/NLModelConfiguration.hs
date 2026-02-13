{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLModelConfiguration@.
module ObjC.NaturalLanguage.NLModelConfiguration
  ( NLModelConfiguration
  , IsNLModelConfiguration(..)
  , supportedRevisionsForType
  , currentRevisionForType
  , type_
  , language
  , revision
  , currentRevisionForTypeSelector
  , languageSelector
  , revisionSelector
  , supportedRevisionsForTypeSelector
  , typeSelector

  -- * Enum types
  , NLModelType(NLModelType)
  , pattern NLModelTypeClassifier
  , pattern NLModelTypeSequence

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ supportedRevisionsForType:@
supportedRevisionsForType :: NLModelType -> IO (Id NSIndexSet)
supportedRevisionsForType type_ =
  do
    cls' <- getRequiredClass "NLModelConfiguration"
    sendClassMessage cls' supportedRevisionsForTypeSelector type_

-- | @+ currentRevisionForType:@
currentRevisionForType :: NLModelType -> IO CULong
currentRevisionForType type_ =
  do
    cls' <- getRequiredClass "NLModelConfiguration"
    sendClassMessage cls' currentRevisionForTypeSelector type_

-- | @- type@
type_ :: IsNLModelConfiguration nlModelConfiguration => nlModelConfiguration -> IO NLModelType
type_ nlModelConfiguration =
  sendMessage nlModelConfiguration typeSelector

-- | @- language@
language :: IsNLModelConfiguration nlModelConfiguration => nlModelConfiguration -> IO (Id NSString)
language nlModelConfiguration =
  sendMessage nlModelConfiguration languageSelector

-- | @- revision@
revision :: IsNLModelConfiguration nlModelConfiguration => nlModelConfiguration -> IO CULong
revision nlModelConfiguration =
  sendMessage nlModelConfiguration revisionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedRevisionsForType:@
supportedRevisionsForTypeSelector :: Selector '[NLModelType] (Id NSIndexSet)
supportedRevisionsForTypeSelector = mkSelector "supportedRevisionsForType:"

-- | @Selector@ for @currentRevisionForType:@
currentRevisionForTypeSelector :: Selector '[NLModelType] CULong
currentRevisionForTypeSelector = mkSelector "currentRevisionForType:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NLModelType
typeSelector = mkSelector "type"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] CULong
revisionSelector = mkSelector "revision"

