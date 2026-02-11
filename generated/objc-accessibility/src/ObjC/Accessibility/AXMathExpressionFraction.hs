{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionFraction@.
module ObjC.Accessibility.AXMathExpressionFraction
  ( AXMathExpressionFraction
  , IsAXMathExpressionFraction(..)
  , initWithNumeratorExpression_denimonatorExpression
  , numeratorExpression
  , denimonatorExpression
  , initWithNumeratorExpression_denimonatorExpressionSelector
  , numeratorExpressionSelector
  , denimonatorExpressionSelector


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

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithNumeratorExpression:denimonatorExpression:@
initWithNumeratorExpression_denimonatorExpression :: (IsAXMathExpressionFraction axMathExpressionFraction, IsAXMathExpression numeratorExpression, IsAXMathExpression denimonatorExpression) => axMathExpressionFraction -> numeratorExpression -> denimonatorExpression -> IO (Id AXMathExpressionFraction)
initWithNumeratorExpression_denimonatorExpression axMathExpressionFraction  numeratorExpression denimonatorExpression =
withObjCPtr numeratorExpression $ \raw_numeratorExpression ->
  withObjCPtr denimonatorExpression $ \raw_denimonatorExpression ->
      sendMsg axMathExpressionFraction (mkSelector "initWithNumeratorExpression:denimonatorExpression:") (retPtr retVoid) [argPtr (castPtr raw_numeratorExpression :: Ptr ()), argPtr (castPtr raw_denimonatorExpression :: Ptr ())] >>= ownedObject . castPtr

-- | @- numeratorExpression@
numeratorExpression :: IsAXMathExpressionFraction axMathExpressionFraction => axMathExpressionFraction -> IO (Id AXMathExpression)
numeratorExpression axMathExpressionFraction  =
  sendMsg axMathExpressionFraction (mkSelector "numeratorExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- denimonatorExpression@
denimonatorExpression :: IsAXMathExpressionFraction axMathExpressionFraction => axMathExpressionFraction -> IO (Id AXMathExpression)
denimonatorExpression axMathExpressionFraction  =
  sendMsg axMathExpressionFraction (mkSelector "denimonatorExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNumeratorExpression:denimonatorExpression:@
initWithNumeratorExpression_denimonatorExpressionSelector :: Selector
initWithNumeratorExpression_denimonatorExpressionSelector = mkSelector "initWithNumeratorExpression:denimonatorExpression:"

-- | @Selector@ for @numeratorExpression@
numeratorExpressionSelector :: Selector
numeratorExpressionSelector = mkSelector "numeratorExpression"

-- | @Selector@ for @denimonatorExpression@
denimonatorExpressionSelector :: Selector
denimonatorExpressionSelector = mkSelector "denimonatorExpression"

