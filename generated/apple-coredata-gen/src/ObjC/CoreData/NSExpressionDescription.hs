{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , expressionResultTypeSelector
  , expressionSelector
  , setExpressionResultTypeSelector
  , setExpressionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- expression@
expression :: IsNSExpressionDescription nsExpressionDescription => nsExpressionDescription -> IO (Id NSExpression)
expression nsExpressionDescription =
  sendMessage nsExpressionDescription expressionSelector

-- | @- setExpression:@
setExpression :: (IsNSExpressionDescription nsExpressionDescription, IsNSExpression value) => nsExpressionDescription -> value -> IO ()
setExpression nsExpressionDescription value =
  sendMessage nsExpressionDescription setExpressionSelector (toNSExpression value)

-- | @- expressionResultType@
expressionResultType :: IsNSExpressionDescription nsExpressionDescription => nsExpressionDescription -> IO NSAttributeType
expressionResultType nsExpressionDescription =
  sendMessage nsExpressionDescription expressionResultTypeSelector

-- | @- setExpressionResultType:@
setExpressionResultType :: IsNSExpressionDescription nsExpressionDescription => nsExpressionDescription -> NSAttributeType -> IO ()
setExpressionResultType nsExpressionDescription value =
  sendMessage nsExpressionDescription setExpressionResultTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expression@
expressionSelector :: Selector '[] (Id NSExpression)
expressionSelector = mkSelector "expression"

-- | @Selector@ for @setExpression:@
setExpressionSelector :: Selector '[Id NSExpression] ()
setExpressionSelector = mkSelector "setExpression:"

-- | @Selector@ for @expressionResultType@
expressionResultTypeSelector :: Selector '[] NSAttributeType
expressionResultTypeSelector = mkSelector "expressionResultType"

-- | @Selector@ for @setExpressionResultType:@
setExpressionResultTypeSelector :: Selector '[NSAttributeType] ()
setExpressionResultTypeSelector = mkSelector "setExpressionResultType:"

