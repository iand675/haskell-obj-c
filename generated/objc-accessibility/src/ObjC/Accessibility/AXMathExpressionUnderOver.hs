{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionUnderOver@.
module ObjC.Accessibility.AXMathExpressionUnderOver
  ( AXMathExpressionUnderOver
  , IsAXMathExpressionUnderOver(..)
  , initWithBaseExpression_underExpression_overExpression
  , baseExpression
  , underExpression
  , overExpression
  , initWithBaseExpression_underExpression_overExpressionSelector
  , baseExpressionSelector
  , underExpressionSelector
  , overExpressionSelector


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

-- | @- initWithBaseExpression:underExpression:overExpression:@
initWithBaseExpression_underExpression_overExpression :: (IsAXMathExpressionUnderOver axMathExpressionUnderOver, IsAXMathExpression baseExpression, IsAXMathExpression underExpression, IsAXMathExpression overExpression) => axMathExpressionUnderOver -> baseExpression -> underExpression -> overExpression -> IO (Id AXMathExpressionUnderOver)
initWithBaseExpression_underExpression_overExpression axMathExpressionUnderOver  baseExpression underExpression overExpression =
withObjCPtr baseExpression $ \raw_baseExpression ->
  withObjCPtr underExpression $ \raw_underExpression ->
    withObjCPtr overExpression $ \raw_overExpression ->
        sendMsg axMathExpressionUnderOver (mkSelector "initWithBaseExpression:underExpression:overExpression:") (retPtr retVoid) [argPtr (castPtr raw_baseExpression :: Ptr ()), argPtr (castPtr raw_underExpression :: Ptr ()), argPtr (castPtr raw_overExpression :: Ptr ())] >>= ownedObject . castPtr

-- | @- baseExpression@
baseExpression :: IsAXMathExpressionUnderOver axMathExpressionUnderOver => axMathExpressionUnderOver -> IO (Id AXMathExpression)
baseExpression axMathExpressionUnderOver  =
  sendMsg axMathExpressionUnderOver (mkSelector "baseExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- underExpression@
underExpression :: IsAXMathExpressionUnderOver axMathExpressionUnderOver => axMathExpressionUnderOver -> IO (Id AXMathExpression)
underExpression axMathExpressionUnderOver  =
  sendMsg axMathExpressionUnderOver (mkSelector "underExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- overExpression@
overExpression :: IsAXMathExpressionUnderOver axMathExpressionUnderOver => axMathExpressionUnderOver -> IO (Id AXMathExpression)
overExpression axMathExpressionUnderOver  =
  sendMsg axMathExpressionUnderOver (mkSelector "overExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBaseExpression:underExpression:overExpression:@
initWithBaseExpression_underExpression_overExpressionSelector :: Selector
initWithBaseExpression_underExpression_overExpressionSelector = mkSelector "initWithBaseExpression:underExpression:overExpression:"

-- | @Selector@ for @baseExpression@
baseExpressionSelector :: Selector
baseExpressionSelector = mkSelector "baseExpression"

-- | @Selector@ for @underExpression@
underExpressionSelector :: Selector
underExpressionSelector = mkSelector "underExpression"

-- | @Selector@ for @overExpression@
overExpressionSelector :: Selector
overExpressionSelector = mkSelector "overExpression"

