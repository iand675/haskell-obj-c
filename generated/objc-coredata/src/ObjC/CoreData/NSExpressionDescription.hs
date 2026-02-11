{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExpressionDescription@.
module ObjC.CoreData.NSExpressionDescription
  ( NSExpressionDescription
  , IsNSExpressionDescription(..)
  , expression
  , setExpression
  , expressionResultType
  , setExpressionResultType
  , expressionSelector
  , setExpressionSelector
  , expressionResultTypeSelector
  , setExpressionResultTypeSelector

  -- * Enum types
  , NSAttributeType(NSAttributeType)
  , pattern NSUndefinedAttributeType
  , pattern NSInteger16AttributeType
  , pattern NSInteger32AttributeType
  , pattern NSInteger64AttributeType
  , pattern NSDecimalAttributeType
  , pattern NSDoubleAttributeType
  , pattern NSFloatAttributeType
  , pattern NSStringAttributeType
  , pattern NSBooleanAttributeType
  , pattern NSDateAttributeType
  , pattern NSBinaryDataAttributeType
  , pattern NSUUIDAttributeType
  , pattern NSURIAttributeType
  , pattern NSTransformableAttributeType
  , pattern NSObjectIDAttributeType
  , pattern NSCompositeAttributeType

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- expression@
expression :: IsNSExpressionDescription nsExpressionDescription => nsExpressionDescription -> IO (Id NSExpression)
expression nsExpressionDescription  =
  sendMsg nsExpressionDescription (mkSelector "expression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpression:@
setExpression :: (IsNSExpressionDescription nsExpressionDescription, IsNSExpression value) => nsExpressionDescription -> value -> IO ()
setExpression nsExpressionDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsExpressionDescription (mkSelector "setExpression:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expressionResultType@
expressionResultType :: IsNSExpressionDescription nsExpressionDescription => nsExpressionDescription -> IO NSAttributeType
expressionResultType nsExpressionDescription  =
  fmap (coerce :: CULong -> NSAttributeType) $ sendMsg nsExpressionDescription (mkSelector "expressionResultType") retCULong []

-- | @- setExpressionResultType:@
setExpressionResultType :: IsNSExpressionDescription nsExpressionDescription => nsExpressionDescription -> NSAttributeType -> IO ()
setExpressionResultType nsExpressionDescription  value =
  sendMsg nsExpressionDescription (mkSelector "setExpressionResultType:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expression@
expressionSelector :: Selector
expressionSelector = mkSelector "expression"

-- | @Selector@ for @setExpression:@
setExpressionSelector :: Selector
setExpressionSelector = mkSelector "setExpression:"

-- | @Selector@ for @expressionResultType@
expressionResultTypeSelector :: Selector
expressionResultTypeSelector = mkSelector "expressionResultType"

-- | @Selector@ for @setExpressionResultType:@
setExpressionResultTypeSelector :: Selector
setExpressionResultTypeSelector = mkSelector "setExpressionResultType:"

