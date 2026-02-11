{-# LANGUAGE PatternSynonyms #-}
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
  , supportedRevisionsForTypeSelector
  , currentRevisionForTypeSelector
  , typeSelector
  , languageSelector
  , revisionSelector

  -- * Enum types
  , NLModelType(NLModelType)
  , pattern NLModelTypeClassifier
  , pattern NLModelTypeSequence

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
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ supportedRevisionsForType:@
supportedRevisionsForType :: NLModelType -> IO (Id NSIndexSet)
supportedRevisionsForType type_ =
  do
    cls' <- getRequiredClass "NLModelConfiguration"
    sendClassMsg cls' (mkSelector "supportedRevisionsForType:") (retPtr retVoid) [argCLong (coerce type_)] >>= retainedObject . castPtr

-- | @+ currentRevisionForType:@
currentRevisionForType :: NLModelType -> IO CULong
currentRevisionForType type_ =
  do
    cls' <- getRequiredClass "NLModelConfiguration"
    sendClassMsg cls' (mkSelector "currentRevisionForType:") retCULong [argCLong (coerce type_)]

-- | @- type@
type_ :: IsNLModelConfiguration nlModelConfiguration => nlModelConfiguration -> IO NLModelType
type_ nlModelConfiguration  =
  fmap (coerce :: CLong -> NLModelType) $ sendMsg nlModelConfiguration (mkSelector "type") retCLong []

-- | @- language@
language :: IsNLModelConfiguration nlModelConfiguration => nlModelConfiguration -> IO (Id NSString)
language nlModelConfiguration  =
  sendMsg nlModelConfiguration (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- revision@
revision :: IsNLModelConfiguration nlModelConfiguration => nlModelConfiguration -> IO CULong
revision nlModelConfiguration  =
  sendMsg nlModelConfiguration (mkSelector "revision") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedRevisionsForType:@
supportedRevisionsForTypeSelector :: Selector
supportedRevisionsForTypeSelector = mkSelector "supportedRevisionsForType:"

-- | @Selector@ for @currentRevisionForType:@
currentRevisionForTypeSelector :: Selector
currentRevisionForTypeSelector = mkSelector "currentRevisionForType:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

