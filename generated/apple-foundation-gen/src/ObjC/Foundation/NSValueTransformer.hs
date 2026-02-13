{-# LANGUAGE DataKinds #-}
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
  , allowsReverseTransformationSelector
  , reverseTransformedValueSelector
  , setValueTransformer_forNameSelector
  , transformedValueClassSelector
  , transformedValueSelector
  , valueTransformerForNameSelector
  , valueTransformerNamesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ setValueTransformer:forName:@
setValueTransformer_forName :: (IsNSValueTransformer transformer, IsNSString name) => transformer -> name -> IO ()
setValueTransformer_forName transformer name =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    sendClassMessage cls' setValueTransformer_forNameSelector (toNSValueTransformer transformer) (toNSString name)

-- | @+ valueTransformerForName:@
valueTransformerForName :: IsNSString name => name -> IO (Id NSValueTransformer)
valueTransformerForName name =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    sendClassMessage cls' valueTransformerForNameSelector (toNSString name)

-- | @+ valueTransformerNames@
valueTransformerNames :: IO (Id NSArray)
valueTransformerNames  =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    sendClassMessage cls' valueTransformerNamesSelector

-- | @+ transformedValueClass@
transformedValueClass :: IO Class
transformedValueClass  =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    sendClassMessage cls' transformedValueClassSelector

-- | @+ allowsReverseTransformation@
allowsReverseTransformation :: IO Bool
allowsReverseTransformation  =
  do
    cls' <- getRequiredClass "NSValueTransformer"
    sendClassMessage cls' allowsReverseTransformationSelector

-- | @- transformedValue:@
transformedValue :: IsNSValueTransformer nsValueTransformer => nsValueTransformer -> RawId -> IO RawId
transformedValue nsValueTransformer value =
  sendMessage nsValueTransformer transformedValueSelector value

-- | @- reverseTransformedValue:@
reverseTransformedValue :: IsNSValueTransformer nsValueTransformer => nsValueTransformer -> RawId -> IO RawId
reverseTransformedValue nsValueTransformer value =
  sendMessage nsValueTransformer reverseTransformedValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValueTransformer:forName:@
setValueTransformer_forNameSelector :: Selector '[Id NSValueTransformer, Id NSString] ()
setValueTransformer_forNameSelector = mkSelector "setValueTransformer:forName:"

-- | @Selector@ for @valueTransformerForName:@
valueTransformerForNameSelector :: Selector '[Id NSString] (Id NSValueTransformer)
valueTransformerForNameSelector = mkSelector "valueTransformerForName:"

-- | @Selector@ for @valueTransformerNames@
valueTransformerNamesSelector :: Selector '[] (Id NSArray)
valueTransformerNamesSelector = mkSelector "valueTransformerNames"

-- | @Selector@ for @transformedValueClass@
transformedValueClassSelector :: Selector '[] Class
transformedValueClassSelector = mkSelector "transformedValueClass"

-- | @Selector@ for @allowsReverseTransformation@
allowsReverseTransformationSelector :: Selector '[] Bool
allowsReverseTransformationSelector = mkSelector "allowsReverseTransformation"

-- | @Selector@ for @transformedValue:@
transformedValueSelector :: Selector '[RawId] RawId
transformedValueSelector = mkSelector "transformedValue:"

-- | @Selector@ for @reverseTransformedValue:@
reverseTransformedValueSelector :: Selector '[RawId] RawId
reverseTransformedValueSelector = mkSelector "reverseTransformedValue:"

