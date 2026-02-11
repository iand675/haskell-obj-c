{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValueTransformer@.
module ObjC.Foundation.NSValueTransformer
  ( NSValueTransformer
  , IsNSValueTransformer(..)
  , setValueTransformer_forName
  , valueTransformerForName
  , valueTransformerNames
  , transformedValueClass
  , allowsReverseTransformation
  , transformedValue
  , reverseTransformedValue
  , setValueTransformer_forNameSelector
  , valueTransformerForNameSelector
  , valueTransformerNamesSelector
  , transformedValueClassSelector
  , allowsReverseTransformationSelector
  , transformedValueSelector
  , reverseTransformedValueSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ setValueTransformer:forName:@
setValueTransformer_forName :: (IsNSValueTransformer transformer, IsNSString name) => transformer -> name -> IO ()
setValueTransformer_forName transformer name =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    withObjCPtr transformer $ \raw_transformer ->
      withObjCPtr name $ \raw_name ->
        sendClassMsg cls' (mkSelector "setValueTransformer:forName:") retVoid [argPtr (castPtr raw_transformer :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @+ valueTransformerForName:@
valueTransformerForName :: IsNSString name => name -> IO (Id NSValueTransformer)
valueTransformerForName name =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "valueTransformerForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ valueTransformerNames@
valueTransformerNames :: IO (Id NSArray)
valueTransformerNames  =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    sendClassMsg cls' (mkSelector "valueTransformerNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ transformedValueClass@
transformedValueClass :: IO Class
transformedValueClass  =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "transformedValueClass") (retPtr retVoid) []

-- | @+ allowsReverseTransformation@
allowsReverseTransformation :: IO Bool
allowsReverseTransformation  =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "allowsReverseTransformation") retCULong []

-- | @- transformedValue:@
transformedValue :: IsNSValueTransformer nsValueTransformer => nsValueTransformer -> RawId -> IO RawId
transformedValue nsValueTransformer  value =
  fmap (RawId . castPtr) $ sendMsg nsValueTransformer (mkSelector "transformedValue:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- reverseTransformedValue:@
reverseTransformedValue :: IsNSValueTransformer nsValueTransformer => nsValueTransformer -> RawId -> IO RawId
reverseTransformedValue nsValueTransformer  value =
  fmap (RawId . castPtr) $ sendMsg nsValueTransformer (mkSelector "reverseTransformedValue:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValueTransformer:forName:@
setValueTransformer_forNameSelector :: Selector
setValueTransformer_forNameSelector = mkSelector "setValueTransformer:forName:"

-- | @Selector@ for @valueTransformerForName:@
valueTransformerForNameSelector :: Selector
valueTransformerForNameSelector = mkSelector "valueTransformerForName:"

-- | @Selector@ for @valueTransformerNames@
valueTransformerNamesSelector :: Selector
valueTransformerNamesSelector = mkSelector "valueTransformerNames"

-- | @Selector@ for @transformedValueClass@
transformedValueClassSelector :: Selector
transformedValueClassSelector = mkSelector "transformedValueClass"

-- | @Selector@ for @allowsReverseTransformation@
allowsReverseTransformationSelector :: Selector
allowsReverseTransformationSelector = mkSelector "allowsReverseTransformation"

-- | @Selector@ for @transformedValue:@
transformedValueSelector :: Selector
transformedValueSelector = mkSelector "transformedValue:"

-- | @Selector@ for @reverseTransformedValue:@
reverseTransformedValueSelector :: Selector
reverseTransformedValueSelector = mkSelector "reverseTransformedValue:"

