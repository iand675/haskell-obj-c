{-# LANGUAGE PatternSynonyms #-}
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
  , predicateWithValue_forPropertySelector
  , predicateWithValue_forProperty_comparisonTypeSelector
  , propertySelector
  , valueSelector
  , comparisonTypeSelector

  -- * Enum types
  , MPMediaPredicateComparison(MPMediaPredicateComparison)
  , pattern MPMediaPredicateComparisonEqualTo
  , pattern MPMediaPredicateComparisonContains

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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ predicateWithValue:forProperty:@
predicateWithValue_forProperty :: IsNSString property => RawId -> property -> IO (Id MPMediaPropertyPredicate)
predicateWithValue_forProperty value property =
  do
    cls' <- getRequiredClass "MPMediaPropertyPredicate"
    withObjCPtr property $ \raw_property ->
      sendClassMsg cls' (mkSelector "predicateWithValue:forProperty:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateWithValue:forProperty:comparisonType:@
predicateWithValue_forProperty_comparisonType :: IsNSString property => RawId -> property -> MPMediaPredicateComparison -> IO (Id MPMediaPropertyPredicate)
predicateWithValue_forProperty_comparisonType value property comparisonType =
  do
    cls' <- getRequiredClass "MPMediaPropertyPredicate"
    withObjCPtr property $ \raw_property ->
      sendClassMsg cls' (mkSelector "predicateWithValue:forProperty:comparisonType:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argCLong (coerce comparisonType)] >>= retainedObject . castPtr

-- | @- property@
property :: IsMPMediaPropertyPredicate mpMediaPropertyPredicate => mpMediaPropertyPredicate -> IO (Id NSString)
property mpMediaPropertyPredicate  =
  sendMsg mpMediaPropertyPredicate (mkSelector "property") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsMPMediaPropertyPredicate mpMediaPropertyPredicate => mpMediaPropertyPredicate -> IO RawId
value mpMediaPropertyPredicate  =
  fmap (RawId . castPtr) $ sendMsg mpMediaPropertyPredicate (mkSelector "value") (retPtr retVoid) []

-- | @- comparisonType@
comparisonType :: IsMPMediaPropertyPredicate mpMediaPropertyPredicate => mpMediaPropertyPredicate -> IO MPMediaPredicateComparison
comparisonType mpMediaPropertyPredicate  =
  fmap (coerce :: CLong -> MPMediaPredicateComparison) $ sendMsg mpMediaPropertyPredicate (mkSelector "comparisonType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithValue:forProperty:@
predicateWithValue_forPropertySelector :: Selector
predicateWithValue_forPropertySelector = mkSelector "predicateWithValue:forProperty:"

-- | @Selector@ for @predicateWithValue:forProperty:comparisonType:@
predicateWithValue_forProperty_comparisonTypeSelector :: Selector
predicateWithValue_forProperty_comparisonTypeSelector = mkSelector "predicateWithValue:forProperty:comparisonType:"

-- | @Selector@ for @property@
propertySelector :: Selector
propertySelector = mkSelector "property"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @comparisonType@
comparisonTypeSelector :: Selector
comparisonTypeSelector = mkSelector "comparisonType"

