{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionSubSuperscript@.
module ObjC.Accessibility.AXMathExpressionSubSuperscript
  ( AXMathExpressionSubSuperscript
  , IsAXMathExpressionSubSuperscript(..)
  , initWithBaseExpression_subscriptExpressions_superscriptExpressions
  , baseExpression
  , subscriptExpressions
  , superscriptExpressions
  , initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector
  , baseExpressionSelector
  , subscriptExpressionsSelector
  , superscriptExpressionsSelector


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

-- | @- initWithBaseExpression:subscriptExpressions:superscriptExpressions:@
initWithBaseExpression_subscriptExpressions_superscriptExpressions :: (IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript, IsNSArray baseExpression, IsNSArray subscriptExpressions, IsNSArray superscriptExpressions) => axMathExpressionSubSuperscript -> baseExpression -> subscriptExpressions -> superscriptExpressions -> IO (Id AXMathExpressionSubSuperscript)
initWithBaseExpression_subscriptExpressions_superscriptExpressions axMathExpressionSubSuperscript  baseExpression subscriptExpressions superscriptExpressions =
withObjCPtr baseExpression $ \raw_baseExpression ->
  withObjCPtr subscriptExpressions $ \raw_subscriptExpressions ->
    withObjCPtr superscriptExpressions $ \raw_superscriptExpressions ->
        sendMsg axMathExpressionSubSuperscript (mkSelector "initWithBaseExpression:subscriptExpressions:superscriptExpressions:") (retPtr retVoid) [argPtr (castPtr raw_baseExpression :: Ptr ()), argPtr (castPtr raw_subscriptExpressions :: Ptr ()), argPtr (castPtr raw_superscriptExpressions :: Ptr ())] >>= ownedObject . castPtr

-- | @- baseExpression@
baseExpression :: IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript => axMathExpressionSubSuperscript -> IO (Id AXMathExpression)
baseExpression axMathExpressionSubSuperscript  =
  sendMsg axMathExpressionSubSuperscript (mkSelector "baseExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subscriptExpressions@
subscriptExpressions :: IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript => axMathExpressionSubSuperscript -> IO (Id NSArray)
subscriptExpressions axMathExpressionSubSuperscript  =
  sendMsg axMathExpressionSubSuperscript (mkSelector "subscriptExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- superscriptExpressions@
superscriptExpressions :: IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript => axMathExpressionSubSuperscript -> IO (Id NSArray)
superscriptExpressions axMathExpressionSubSuperscript  =
  sendMsg axMathExpressionSubSuperscript (mkSelector "superscriptExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBaseExpression:subscriptExpressions:superscriptExpressions:@
initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector :: Selector
initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector = mkSelector "initWithBaseExpression:subscriptExpressions:superscriptExpressions:"

-- | @Selector@ for @baseExpression@
baseExpressionSelector :: Selector
baseExpressionSelector = mkSelector "baseExpression"

-- | @Selector@ for @subscriptExpressions@
subscriptExpressionsSelector :: Selector
subscriptExpressionsSelector = mkSelector "subscriptExpressions"

-- | @Selector@ for @superscriptExpressions@
superscriptExpressionsSelector :: Selector
superscriptExpressionsSelector = mkSelector "superscriptExpressions"

