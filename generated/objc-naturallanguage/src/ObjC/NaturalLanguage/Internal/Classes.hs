{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.NaturalLanguage.Internal.Classes (
    module ObjC.NaturalLanguage.Internal.Classes,
    module ObjC.CoreML.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- NLContextualEmbedding ----------

-- | Phantom type for @NLContextualEmbedding@.
data NLContextualEmbedding

instance IsObjCObject (Id NLContextualEmbedding) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLContextualEmbedding"

class IsNSObject a => IsNLContextualEmbedding a where
  toNLContextualEmbedding :: a -> Id NLContextualEmbedding

instance IsNLContextualEmbedding (Id NLContextualEmbedding) where
  toNLContextualEmbedding = unsafeCastId

instance IsNSObject (Id NLContextualEmbedding) where
  toNSObject = unsafeCastId

-- ---------- NLContextualEmbeddingResult ----------

-- | Phantom type for @NLContextualEmbeddingResult@.
data NLContextualEmbeddingResult

instance IsObjCObject (Id NLContextualEmbeddingResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLContextualEmbeddingResult"

class IsNSObject a => IsNLContextualEmbeddingResult a where
  toNLContextualEmbeddingResult :: a -> Id NLContextualEmbeddingResult

instance IsNLContextualEmbeddingResult (Id NLContextualEmbeddingResult) where
  toNLContextualEmbeddingResult = unsafeCastId

instance IsNSObject (Id NLContextualEmbeddingResult) where
  toNSObject = unsafeCastId

-- ---------- NLEmbedding ----------

-- | Phantom type for @NLEmbedding@.
data NLEmbedding

instance IsObjCObject (Id NLEmbedding) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLEmbedding"

class IsNSObject a => IsNLEmbedding a where
  toNLEmbedding :: a -> Id NLEmbedding

instance IsNLEmbedding (Id NLEmbedding) where
  toNLEmbedding = unsafeCastId

instance IsNSObject (Id NLEmbedding) where
  toNSObject = unsafeCastId

-- ---------- NLGazetteer ----------

-- | Phantom type for @NLGazetteer@.
data NLGazetteer

instance IsObjCObject (Id NLGazetteer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLGazetteer"

class IsNSObject a => IsNLGazetteer a where
  toNLGazetteer :: a -> Id NLGazetteer

instance IsNLGazetteer (Id NLGazetteer) where
  toNLGazetteer = unsafeCastId

instance IsNSObject (Id NLGazetteer) where
  toNSObject = unsafeCastId

-- ---------- NLLanguageRecognizer ----------

-- | Phantom type for @NLLanguageRecognizer@.
data NLLanguageRecognizer

instance IsObjCObject (Id NLLanguageRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLLanguageRecognizer"

class IsNSObject a => IsNLLanguageRecognizer a where
  toNLLanguageRecognizer :: a -> Id NLLanguageRecognizer

instance IsNLLanguageRecognizer (Id NLLanguageRecognizer) where
  toNLLanguageRecognizer = unsafeCastId

instance IsNSObject (Id NLLanguageRecognizer) where
  toNSObject = unsafeCastId

-- ---------- NLModel ----------

-- | Phantom type for @NLModel@.
data NLModel

instance IsObjCObject (Id NLModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLModel"

class IsNSObject a => IsNLModel a where
  toNLModel :: a -> Id NLModel

instance IsNLModel (Id NLModel) where
  toNLModel = unsafeCastId

instance IsNSObject (Id NLModel) where
  toNSObject = unsafeCastId

-- ---------- NLModelConfiguration ----------

-- | Phantom type for @NLModelConfiguration@.
data NLModelConfiguration

instance IsObjCObject (Id NLModelConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLModelConfiguration"

class IsNSObject a => IsNLModelConfiguration a where
  toNLModelConfiguration :: a -> Id NLModelConfiguration

instance IsNLModelConfiguration (Id NLModelConfiguration) where
  toNLModelConfiguration = unsafeCastId

instance IsNSObject (Id NLModelConfiguration) where
  toNSObject = unsafeCastId

-- ---------- NLTagger ----------

-- | Phantom type for @NLTagger@.
data NLTagger

instance IsObjCObject (Id NLTagger) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLTagger"

class IsNSObject a => IsNLTagger a where
  toNLTagger :: a -> Id NLTagger

instance IsNLTagger (Id NLTagger) where
  toNLTagger = unsafeCastId

instance IsNSObject (Id NLTagger) where
  toNSObject = unsafeCastId

-- ---------- NLTokenizer ----------

-- | Phantom type for @NLTokenizer@.
data NLTokenizer

instance IsObjCObject (Id NLTokenizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NLTokenizer"

class IsNSObject a => IsNLTokenizer a where
  toNLTokenizer :: a -> Id NLTokenizer

instance IsNLTokenizer (Id NLTokenizer) where
  toNLTokenizer = unsafeCastId

instance IsNSObject (Id NLTokenizer) where
  toNSObject = unsafeCastId
