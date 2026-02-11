{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPredicate@.
module ObjC.Foundation.NSPredicate
  ( NSPredicate
  , IsNSPredicate(..)
  , predicateWithFormat_argumentArray
  , predicateWithFormat
  , predicateWithFormat_arguments
  , predicateFromMetadataQueryString
  , predicateWithValue
  , predicateWithSubstitutionVariables
  , evaluateWithObject
  , evaluateWithObject_substitutionVariables
  , allowEvaluation
  , predicateFormat
  , predicateWithFormat_argumentArraySelector
  , predicateWithFormatSelector
  , predicateWithFormat_argumentsSelector
  , predicateFromMetadataQueryStringSelector
  , predicateWithValueSelector
  , predicateWithSubstitutionVariablesSelector
  , evaluateWithObjectSelector
  , evaluateWithObject_substitutionVariablesSelector
  , allowEvaluationSelector
  , predicateFormatSelector


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

-- | @+ predicateWithFormat:argumentArray:@
predicateWithFormat_argumentArray :: (IsNSString predicateFormat, IsNSArray arguments) => predicateFormat -> arguments -> IO (Id NSPredicate)
predicateWithFormat_argumentArray predicateFormat arguments =
  do
    cls' <- getRequiredClass "NSPredicate"
    withObjCPtr predicateFormat $ \raw_predicateFormat ->
      withObjCPtr arguments $ \raw_arguments ->
        sendClassMsg cls' (mkSelector "predicateWithFormat:argumentArray:") (retPtr retVoid) [argPtr (castPtr raw_predicateFormat :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateWithFormat:@
predicateWithFormat :: IsNSString predicateFormat => predicateFormat -> IO (Id NSPredicate)
predicateWithFormat predicateFormat =
  do
    cls' <- getRequiredClass "NSPredicate"
    withObjCPtr predicateFormat $ \raw_predicateFormat ->
      sendClassMsg cls' (mkSelector "predicateWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_predicateFormat :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateWithFormat:arguments:@
predicateWithFormat_arguments :: IsNSString predicateFormat => predicateFormat -> RawId -> IO (Id NSPredicate)
predicateWithFormat_arguments predicateFormat argList =
  do
    cls' <- getRequiredClass "NSPredicate"
    withObjCPtr predicateFormat $ \raw_predicateFormat ->
      sendClassMsg cls' (mkSelector "predicateWithFormat:arguments:") (retPtr retVoid) [argPtr (castPtr raw_predicateFormat :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateFromMetadataQueryString:@
predicateFromMetadataQueryString :: IsNSString queryString => queryString -> IO (Id NSPredicate)
predicateFromMetadataQueryString queryString =
  do
    cls' <- getRequiredClass "NSPredicate"
    withObjCPtr queryString $ \raw_queryString ->
      sendClassMsg cls' (mkSelector "predicateFromMetadataQueryString:") (retPtr retVoid) [argPtr (castPtr raw_queryString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateWithValue:@
predicateWithValue :: Bool -> IO (Id NSPredicate)
predicateWithValue value =
  do
    cls' <- getRequiredClass "NSPredicate"
    sendClassMsg cls' (mkSelector "predicateWithValue:") (retPtr retVoid) [argCULong (if value then 1 else 0)] >>= retainedObject . castPtr

-- | @- predicateWithSubstitutionVariables:@
predicateWithSubstitutionVariables :: (IsNSPredicate nsPredicate, IsNSDictionary variables) => nsPredicate -> variables -> IO (Id NSPredicate)
predicateWithSubstitutionVariables nsPredicate  variables =
withObjCPtr variables $ \raw_variables ->
    sendMsg nsPredicate (mkSelector "predicateWithSubstitutionVariables:") (retPtr retVoid) [argPtr (castPtr raw_variables :: Ptr ())] >>= retainedObject . castPtr

-- | @- evaluateWithObject:@
evaluateWithObject :: IsNSPredicate nsPredicate => nsPredicate -> RawId -> IO Bool
evaluateWithObject nsPredicate  object =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPredicate (mkSelector "evaluateWithObject:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- evaluateWithObject:substitutionVariables:@
evaluateWithObject_substitutionVariables :: (IsNSPredicate nsPredicate, IsNSDictionary bindings) => nsPredicate -> RawId -> bindings -> IO Bool
evaluateWithObject_substitutionVariables nsPredicate  object bindings =
withObjCPtr bindings $ \raw_bindings ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPredicate (mkSelector "evaluateWithObject:substitutionVariables:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_bindings :: Ptr ())]

-- | @- allowEvaluation@
allowEvaluation :: IsNSPredicate nsPredicate => nsPredicate -> IO ()
allowEvaluation nsPredicate  =
  sendMsg nsPredicate (mkSelector "allowEvaluation") retVoid []

-- | @- predicateFormat@
predicateFormat :: IsNSPredicate nsPredicate => nsPredicate -> IO (Id NSString)
predicateFormat nsPredicate  =
  sendMsg nsPredicate (mkSelector "predicateFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithFormat:argumentArray:@
predicateWithFormat_argumentArraySelector :: Selector
predicateWithFormat_argumentArraySelector = mkSelector "predicateWithFormat:argumentArray:"

-- | @Selector@ for @predicateWithFormat:@
predicateWithFormatSelector :: Selector
predicateWithFormatSelector = mkSelector "predicateWithFormat:"

-- | @Selector@ for @predicateWithFormat:arguments:@
predicateWithFormat_argumentsSelector :: Selector
predicateWithFormat_argumentsSelector = mkSelector "predicateWithFormat:arguments:"

-- | @Selector@ for @predicateFromMetadataQueryString:@
predicateFromMetadataQueryStringSelector :: Selector
predicateFromMetadataQueryStringSelector = mkSelector "predicateFromMetadataQueryString:"

-- | @Selector@ for @predicateWithValue:@
predicateWithValueSelector :: Selector
predicateWithValueSelector = mkSelector "predicateWithValue:"

-- | @Selector@ for @predicateWithSubstitutionVariables:@
predicateWithSubstitutionVariablesSelector :: Selector
predicateWithSubstitutionVariablesSelector = mkSelector "predicateWithSubstitutionVariables:"

-- | @Selector@ for @evaluateWithObject:@
evaluateWithObjectSelector :: Selector
evaluateWithObjectSelector = mkSelector "evaluateWithObject:"

-- | @Selector@ for @evaluateWithObject:substitutionVariables:@
evaluateWithObject_substitutionVariablesSelector :: Selector
evaluateWithObject_substitutionVariablesSelector = mkSelector "evaluateWithObject:substitutionVariables:"

-- | @Selector@ for @allowEvaluation@
allowEvaluationSelector :: Selector
allowEvaluationSelector = mkSelector "allowEvaluation"

-- | @Selector@ for @predicateFormat@
predicateFormatSelector :: Selector
predicateFormatSelector = mkSelector "predicateFormat"

