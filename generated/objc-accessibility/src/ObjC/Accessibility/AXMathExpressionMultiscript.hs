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
  , initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector
  , baseExpressionSelector
  , prescriptExpressionsSelector
  , postscriptExpressionsSelector


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

-- | @- initWithBaseExpression:prescriptExpressions:postscriptExpressions:@
initWithBaseExpression_prescriptExpressions_postscriptExpressions :: (IsAXMathExpressionMultiscript axMathExpressionMultiscript, IsAXMathExpression baseExpression, IsNSArray prescriptExpressions, IsNSArray postscriptExpressions) => axMathExpressionMultiscript -> baseExpression -> prescriptExpressions -> postscriptExpressions -> IO (Id AXMathExpressionMultiscript)
initWithBaseExpression_prescriptExpressions_postscriptExpressions axMathExpressionMultiscript  baseExpression prescriptExpressions postscriptExpressions =
withObjCPtr baseExpression $ \raw_baseExpression ->
  withObjCPtr prescriptExpressions $ \raw_prescriptExpressions ->
    withObjCPtr postscriptExpressions $ \raw_postscriptExpressions ->
        sendMsg axMathExpressionMultiscript (mkSelector "initWithBaseExpression:prescriptExpressions:postscriptExpressions:") (retPtr retVoid) [argPtr (castPtr raw_baseExpression :: Ptr ()), argPtr (castPtr raw_prescriptExpressions :: Ptr ()), argPtr (castPtr raw_postscriptExpressions :: Ptr ())] >>= ownedObject . castPtr

-- | @- baseExpression@
baseExpression :: IsAXMathExpressionMultiscript axMathExpressionMultiscript => axMathExpressionMultiscript -> IO (Id AXMathExpression)
baseExpression axMathExpressionMultiscript  =
  sendMsg axMathExpressionMultiscript (mkSelector "baseExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- prescriptExpressions@
prescriptExpressions :: IsAXMathExpressionMultiscript axMathExpressionMultiscript => axMathExpressionMultiscript -> IO (Id NSArray)
prescriptExpressions axMathExpressionMultiscript  =
  sendMsg axMathExpressionMultiscript (mkSelector "prescriptExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- postscriptExpressions@
postscriptExpressions :: IsAXMathExpressionMultiscript axMathExpressionMultiscript => axMathExpressionMultiscript -> IO (Id NSArray)
postscriptExpressions axMathExpressionMultiscript  =
  sendMsg axMathExpressionMultiscript (mkSelector "postscriptExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBaseExpression:prescriptExpressions:postscriptExpressions:@
initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector :: Selector
initWithBaseExpression_prescriptExpressions_postscriptExpressionsSelector = mkSelector "initWithBaseExpression:prescriptExpressions:postscriptExpressions:"

-- | @Selector@ for @baseExpression@
baseExpressionSelector :: Selector
baseExpressionSelector = mkSelector "baseExpression"

-- | @Selector@ for @prescriptExpressions@
prescriptExpressionsSelector :: Selector
prescriptExpressionsSelector = mkSelector "prescriptExpressions"

-- | @Selector@ for @postscriptExpressions@
postscriptExpressionsSelector :: Selector
postscriptExpressionsSelector = mkSelector "postscriptExpressions"

