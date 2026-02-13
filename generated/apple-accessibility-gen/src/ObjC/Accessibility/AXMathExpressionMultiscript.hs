{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionMultiscript@.
module ObjC.Accessibility.AXMathExpressionMultiscript
  ( AXMathExpressionMultiscript
  , IsAXMathExpressionMultiscript(..)
  , initWithBaseExpression_prescriptExpressions_postscriptExpressions
  , baseExpression
  , prescriptExpressions
  , postscriptExpressions
  , baseExpressionSelector
  , initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector
  , postscriptExpressionsSelector
  , prescriptExpressionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBaseExpression:prescriptExpressions:postscriptExpressions:@
initWithBaseExpression_prescriptExpressions_postscriptExpressions :: (IsAXMathExpressionMultiscript axMathExpressionMultiscript, IsAXMathExpression baseExpression, IsNSArray prescriptExpressions, IsNSArray postscriptExpressions) => axMathExpressionMultiscript -> baseExpression -> prescriptExpressions -> postscriptExpressions -> IO (Id AXMathExpressionMultiscript)
initWithBaseExpression_prescriptExpressions_postscriptExpressions axMathExpressionMultiscript baseExpression prescriptExpressions postscriptExpressions =
  sendOwnedMessage axMathExpressionMultiscript initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector (toAXMathExpression baseExpression) (toNSArray prescriptExpressions) (toNSArray postscriptExpressions)

-- | @- baseExpression@
baseExpression :: IsAXMathExpressionMultiscript axMathExpressionMultiscript => axMathExpressionMultiscript -> IO (Id AXMathExpression)
baseExpression axMathExpressionMultiscript =
  sendMessage axMathExpressionMultiscript baseExpressionSelector

-- | @- prescriptExpressions@
prescriptExpressions :: IsAXMathExpressionMultiscript axMathExpressionMultiscript => axMathExpressionMultiscript -> IO (Id NSArray)
prescriptExpressions axMathExpressionMultiscript =
  sendMessage axMathExpressionMultiscript prescriptExpressionsSelector

-- | @- postscriptExpressions@
postscriptExpressions :: IsAXMathExpressionMultiscript axMathExpressionMultiscript => axMathExpressionMultiscript -> IO (Id NSArray)
postscriptExpressions axMathExpressionMultiscript =
  sendMessage axMathExpressionMultiscript postscriptExpressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBaseExpression:prescriptExpressions:postscriptExpressions:@
initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector :: Selector '[Id AXMathExpression, Id NSArray, Id NSArray] (Id AXMathExpressionMultiscript)
initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector = mkSelector "initWithBaseExpression:prescriptExpressions:postscriptExpressions:"

-- | @Selector@ for @baseExpression@
baseExpressionSelector :: Selector '[] (Id AXMathExpression)
baseExpressionSelector = mkSelector "baseExpression"

-- | @Selector@ for @prescriptExpressions@
prescriptExpressionsSelector :: Selector '[] (Id NSArray)
prescriptExpressionsSelector = mkSelector "prescriptExpressions"

-- | @Selector@ for @postscriptExpressions@
postscriptExpressionsSelector :: Selector '[] (Id NSArray)
postscriptExpressionsSelector = mkSelector "postscriptExpressions"

