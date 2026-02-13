{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaPropertyPredicate@.
module ObjC.MediaPlayer.MPMediaPropertyPredicate
  ( MPMediaPropertyPredicate
  , IsMPMediaPropertyPredicate(..)
  , predicateWithValue_forProperty
  , predicateWithValue_forProperty_comparisonType
  , property
  , value
  , comparisonType
  , comparisonTypeSelector
  , predicateWithValue_forPropertySelector
  , predicateWithValue_forProperty_comparisonTypeSelector
  , propertySelector
  , valueSelector

  -- * Enum types
  , MPMediaPredicateComparison(MPMediaPredicateComparison)
  , pattern MPMediaPredicateComparisonEqualTo
  , pattern MPMediaPredicateComparisonContains

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ predicateWithValue:forProperty:@
predicateWithValue_forProperty :: IsNSString property => RawId -> property -> IO (Id MPMediaPropertyPredicate)
predicateWithValue_forProperty value property =
  do
    cls' <- getRequiredClass "MPMediaPropertyPredicate"
    sendClassMessage cls' predicateWithValue_forPropertySelector value (toNSString property)

-- | @+ predicateWithValue:forProperty:comparisonType:@
predicateWithValue_forProperty_comparisonType :: IsNSString property => RawId -> property -> MPMediaPredicateComparison -> IO (Id MPMediaPropertyPredicate)
predicateWithValue_forProperty_comparisonType value property comparisonType =
  do
    cls' <- getRequiredClass "MPMediaPropertyPredicate"
    sendClassMessage cls' predicateWithValue_forProperty_comparisonTypeSelector value (toNSString property) comparisonType

-- | @- property@
property :: IsMPMediaPropertyPredicate mpMediaPropertyPredicate => mpMediaPropertyPredicate -> IO (Id NSString)
property mpMediaPropertyPredicate =
  sendMessage mpMediaPropertyPredicate propertySelector

-- | @- value@
value :: IsMPMediaPropertyPredicate mpMediaPropertyPredicate => mpMediaPropertyPredicate -> IO RawId
value mpMediaPropertyPredicate =
  sendMessage mpMediaPropertyPredicate valueSelector

-- | @- comparisonType@
comparisonType :: IsMPMediaPropertyPredicate mpMediaPropertyPredicate => mpMediaPropertyPredicate -> IO MPMediaPredicateComparison
comparisonType mpMediaPropertyPredicate =
  sendMessage mpMediaPropertyPredicate comparisonTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithValue:forProperty:@
predicateWithValue_forPropertySelector :: Selector '[RawId, Id NSString] (Id MPMediaPropertyPredicate)
predicateWithValue_forPropertySelector = mkSelector "predicateWithValue:forProperty:"

-- | @Selector@ for @predicateWithValue:forProperty:comparisonType:@
predicateWithValue_forProperty_comparisonTypeSelector :: Selector '[RawId, Id NSString, MPMediaPredicateComparison] (Id MPMediaPropertyPredicate)
predicateWithValue_forProperty_comparisonTypeSelector = mkSelector "predicateWithValue:forProperty:comparisonType:"

-- | @Selector@ for @property@
propertySelector :: Selector '[] (Id NSString)
propertySelector = mkSelector "property"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @comparisonType@
comparisonTypeSelector :: Selector '[] MPMediaPredicateComparison
comparisonTypeSelector = mkSelector "comparisonType"

