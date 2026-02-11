{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a model parameter along with a default value and any applicable constaint on the values.
--
-- Generated bindings for @MLParameterDescription@.
module ObjC.CoreML.MLParameterDescription
  ( MLParameterDescription
  , IsMLParameterDescription(..)
  , key
  , defaultValue
  , numericConstraint
  , keySelector
  , defaultValueSelector
  , numericConstraintSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- key@
key :: IsMLParameterDescription mlParameterDescription => mlParameterDescription -> IO (Id MLParameterKey)
key mlParameterDescription  =
  sendMsg mlParameterDescription (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defaultValue@
defaultValue :: IsMLParameterDescription mlParameterDescription => mlParameterDescription -> IO RawId
defaultValue mlParameterDescription  =
  fmap (RawId . castPtr) $ sendMsg mlParameterDescription (mkSelector "defaultValue") (retPtr retVoid) []

-- | @- numericConstraint@
numericConstraint :: IsMLParameterDescription mlParameterDescription => mlParameterDescription -> IO (Id MLNumericConstraint)
numericConstraint mlParameterDescription  =
  sendMsg mlParameterDescription (mkSelector "numericConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @numericConstraint@
numericConstraintSelector :: Selector
numericConstraintSelector = mkSelector "numericConstraint"

