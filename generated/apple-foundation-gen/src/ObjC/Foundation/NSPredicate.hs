{-# LANGUAGE DataKinds #-}
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
  , allowEvaluationSelector
  , evaluateWithObjectSelector
  , evaluateWithObject_substitutionVariablesSelector
  , predicateFormatSelector
  , predicateFromMetadataQueryStringSelector
  , predicateWithFormatSelector
  , predicateWithFormat_argumentArraySelector
  , predicateWithFormat_argumentsSelector
  , predicateWithSubstitutionVariablesSelector
  , predicateWithValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ predicateWithFormat:argumentArray:@
predicateWithFormat_argumentArray :: (IsNSString predicateFormat, IsNSArray arguments) => predicateFormat -> arguments -> IO (Id NSPredicate)
predicateWithFormat_argumentArray predicateFormat arguments =
  do
    cls' <- getRequiredClass "NSPredicate"
    sendClassMessage cls' predicateWithFormat_argumentArraySelector (toNSString predicateFormat) (toNSArray arguments)

-- | @+ predicateWithFormat:@
predicateWithFormat :: IsNSString predicateFormat => predicateFormat -> IO (Id NSPredicate)
predicateWithFormat predicateFormat =
  do
    cls' <- getRequiredClass "NSPredicate"
    sendClassMessage cls' predicateWithFormatSelector (toNSString predicateFormat)

-- | @+ predicateWithFormat:arguments:@
predicateWithFormat_arguments :: IsNSString predicateFormat => predicateFormat -> RawId -> IO (Id NSPredicate)
predicateWithFormat_arguments predicateFormat argList =
  do
    cls' <- getRequiredClass "NSPredicate"
    sendClassMessage cls' predicateWithFormat_argumentsSelector (toNSString predicateFormat) argList

-- | @+ predicateFromMetadataQueryString:@
predicateFromMetadataQueryString :: IsNSString queryString => queryString -> IO (Id NSPredicate)
predicateFromMetadataQueryString queryString =
  do
    cls' <- getRequiredClass "NSPredicate"
    sendClassMessage cls' predicateFromMetadataQueryStringSelector (toNSString queryString)

-- | @+ predicateWithValue:@
predicateWithValue :: Bool -> IO (Id NSPredicate)
predicateWithValue value =
  do
    cls' <- getRequiredClass "NSPredicate"
    sendClassMessage cls' predicateWithValueSelector value

-- | @- predicateWithSubstitutionVariables:@
predicateWithSubstitutionVariables :: (IsNSPredicate nsPredicate, IsNSDictionary variables) => nsPredicate -> variables -> IO (Id NSPredicate)
predicateWithSubstitutionVariables nsPredicate variables =
  sendMessage nsPredicate predicateWithSubstitutionVariablesSelector (toNSDictionary variables)

-- | @- evaluateWithObject:@
evaluateWithObject :: IsNSPredicate nsPredicate => nsPredicate -> RawId -> IO Bool
evaluateWithObject nsPredicate object =
  sendMessage nsPredicate evaluateWithObjectSelector object

-- | @- evaluateWithObject:substitutionVariables:@
evaluateWithObject_substitutionVariables :: (IsNSPredicate nsPredicate, IsNSDictionary bindings) => nsPredicate -> RawId -> bindings -> IO Bool
evaluateWithObject_substitutionVariables nsPredicate object bindings =
  sendMessage nsPredicate evaluateWithObject_substitutionVariablesSelector object (toNSDictionary bindings)

-- | @- allowEvaluation@
allowEvaluation :: IsNSPredicate nsPredicate => nsPredicate -> IO ()
allowEvaluation nsPredicate =
  sendMessage nsPredicate allowEvaluationSelector

-- | @- predicateFormat@
predicateFormat :: IsNSPredicate nsPredicate => nsPredicate -> IO (Id NSString)
predicateFormat nsPredicate =
  sendMessage nsPredicate predicateFormatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithFormat:argumentArray:@
predicateWithFormat_argumentArraySelector :: Selector '[Id NSString, Id NSArray] (Id NSPredicate)
predicateWithFormat_argumentArraySelector = mkSelector "predicateWithFormat:argumentArray:"

-- | @Selector@ for @predicateWithFormat:@
predicateWithFormatSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateWithFormatSelector = mkSelector "predicateWithFormat:"

-- | @Selector@ for @predicateWithFormat:arguments:@
predicateWithFormat_argumentsSelector :: Selector '[Id NSString, RawId] (Id NSPredicate)
predicateWithFormat_argumentsSelector = mkSelector "predicateWithFormat:arguments:"

-- | @Selector@ for @predicateFromMetadataQueryString:@
predicateFromMetadataQueryStringSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateFromMetadataQueryStringSelector = mkSelector "predicateFromMetadataQueryString:"

-- | @Selector@ for @predicateWithValue:@
predicateWithValueSelector :: Selector '[Bool] (Id NSPredicate)
predicateWithValueSelector = mkSelector "predicateWithValue:"

-- | @Selector@ for @predicateWithSubstitutionVariables:@
predicateWithSubstitutionVariablesSelector :: Selector '[Id NSDictionary] (Id NSPredicate)
predicateWithSubstitutionVariablesSelector = mkSelector "predicateWithSubstitutionVariables:"

-- | @Selector@ for @evaluateWithObject:@
evaluateWithObjectSelector :: Selector '[RawId] Bool
evaluateWithObjectSelector = mkSelector "evaluateWithObject:"

-- | @Selector@ for @evaluateWithObject:substitutionVariables:@
evaluateWithObject_substitutionVariablesSelector :: Selector '[RawId, Id NSDictionary] Bool
evaluateWithObject_substitutionVariablesSelector = mkSelector "evaluateWithObject:substitutionVariables:"

-- | @Selector@ for @allowEvaluation@
allowEvaluationSelector :: Selector '[] ()
allowEvaluationSelector = mkSelector "allowEvaluation"

-- | @Selector@ for @predicateFormat@
predicateFormatSelector :: Selector '[] (Id NSString)
predicateFormatSelector = mkSelector "predicateFormat"

