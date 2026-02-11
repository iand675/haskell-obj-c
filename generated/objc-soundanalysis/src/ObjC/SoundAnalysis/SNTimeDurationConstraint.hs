{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constrains CMTime durations to a subset of legal values.
--
-- @SNTimeDurationConstraint@ is a union type, which, based on the value of its @type@ property, may assume one of several forms. Instance properties may be used to extract information from an object, but certain properties are only valid to exercise under certain circumstances. Before accessing a particular property, refer to its documentation to understand what @type@ value is required in order for that property to be valid.
--
-- Generated bindings for @SNTimeDurationConstraint@.
module ObjC.SoundAnalysis.SNTimeDurationConstraint
  ( SNTimeDurationConstraint
  , IsSNTimeDurationConstraint(..)
  , initWithEnumeratedDurations
  , init_
  , new
  , type_
  , enumeratedDurations
  , initWithEnumeratedDurationsSelector
  , initSelector
  , newSelector
  , typeSelector
  , enumeratedDurationsSelector

  -- * Enum types
  , SNTimeDurationConstraintType(SNTimeDurationConstraintType)
  , pattern SNTimeDurationConstraintTypeEnumerated
  , pattern SNTimeDurationConstraintTypeRange

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

import ObjC.SoundAnalysis.Internal.Classes
import ObjC.SoundAnalysis.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes an enumerated-type constraint.
--
-- - Parameter enumeratedDurations: A discrete set of duration values (represented as CMTime values boxed in NSValue instances) permitted by this constraint.
--
-- - Returns: An instance whose @type@ is @SNTimeDurationConstraintTypeEnumerated@, and which constrains duration values to the provided set of discrete values.
--
-- ObjC selector: @- initWithEnumeratedDurations:@
initWithEnumeratedDurations :: (IsSNTimeDurationConstraint snTimeDurationConstraint, IsNSArray enumeratedDurations) => snTimeDurationConstraint -> enumeratedDurations -> IO (Id SNTimeDurationConstraint)
initWithEnumeratedDurations snTimeDurationConstraint  enumeratedDurations =
withObjCPtr enumeratedDurations $ \raw_enumeratedDurations ->
    sendMsg snTimeDurationConstraint (mkSelector "initWithEnumeratedDurations:") (retPtr retVoid) [argPtr (castPtr raw_enumeratedDurations :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSNTimeDurationConstraint snTimeDurationConstraint => snTimeDurationConstraint -> IO (Id SNTimeDurationConstraint)
init_ snTimeDurationConstraint  =
  sendMsg snTimeDurationConstraint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SNTimeDurationConstraint)
new  =
  do
    cls' <- getRequiredClass "SNTimeDurationConstraint"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The time constraint type.
--
-- The value of this property dictates whether or not other properties associated with this class can be validly accessed. Please refer to the documentation of other individual properties to understand their relationship to this one. This property is always valid to access.
--
-- ObjC selector: @- type@
type_ :: IsSNTimeDurationConstraint snTimeDurationConstraint => snTimeDurationConstraint -> IO SNTimeDurationConstraintType
type_ snTimeDurationConstraint  =
  fmap (coerce :: CLong -> SNTimeDurationConstraintType) $ sendMsg snTimeDurationConstraint (mkSelector "type") retCLong []

-- | If the constraint type is enumerated, then the set of discrete allowable time durations.
--
-- - Returns: If the constraint type is enumerated, an array of CMTime structures (boxed in NSValue instances) representing the set of allowable time durations. The durations will always be provided sorted in order of ascending time. If the constraint type is not enumerated, an empty array will be returned.
--
-- The @type@ property should be queried before this property is accessed. This property will only yield meaningful values if the constraint type is considered to be 'enumerated'. The constraint type is considered to be 'enumerated' if the @type@ property is equal to @SNTimeDurationConstraintTypeEnumerated@.
--
-- ObjC selector: @- enumeratedDurations@
enumeratedDurations :: IsSNTimeDurationConstraint snTimeDurationConstraint => snTimeDurationConstraint -> IO (Id NSArray)
enumeratedDurations snTimeDurationConstraint  =
  sendMsg snTimeDurationConstraint (mkSelector "enumeratedDurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEnumeratedDurations:@
initWithEnumeratedDurationsSelector :: Selector
initWithEnumeratedDurationsSelector = mkSelector "initWithEnumeratedDurations:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @enumeratedDurations@
enumeratedDurationsSelector :: Selector
enumeratedDurationsSelector = mkSelector "enumeratedDurations"

