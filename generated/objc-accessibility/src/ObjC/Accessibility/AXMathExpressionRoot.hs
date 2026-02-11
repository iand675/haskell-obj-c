{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionRoot@.
module ObjC.Accessibility.AXMathExpressionRoot
  ( AXMathExpressionRoot
  , IsAXMathExpressionRoot(..)
  , initWithRadicandExpressions_rootIndexExpression
  , radicandExpressions
  , rootIndexExpression
  , initWithRadicandExpressions_rootIndexExpressionSelector
  , radicandExpressionsSelector
  , rootIndexExpressionSelector


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

-- | @- initWithRadicandExpressions:rootIndexExpression:@
initWithRadicandExpressions_rootIndexExpression :: (IsAXMathExpressionRoot axMathExpressionRoot, IsNSArray radicandExpressions, IsAXMathExpression rootIndexExpression) => axMathExpressionRoot -> radicandExpressions -> rootIndexExpression -> IO (Id AXMathExpressionRoot)
initWithRadicandExpressions_rootIndexExpression axMathExpressionRoot  radicandExpressions rootIndexExpression =
withObjCPtr radicandExpressions $ \raw_radicandExpressions ->
  withObjCPtr rootIndexExpression $ \raw_rootIndexExpression ->
      sendMsg axMathExpressionRoot (mkSelector "initWithRadicandExpressions:rootIndexExpression:") (retPtr retVoid) [argPtr (castPtr raw_radicandExpressions :: Ptr ()), argPtr (castPtr raw_rootIndexExpression :: Ptr ())] >>= ownedObject . castPtr

-- | @- radicandExpressions@
radicandExpressions :: IsAXMathExpressionRoot axMathExpressionRoot => axMathExpressionRoot -> IO (Id NSArray)
radicandExpressions axMathExpressionRoot  =
  sendMsg axMathExpressionRoot (mkSelector "radicandExpressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rootIndexExpression@
rootIndexExpression :: IsAXMathExpressionRoot axMathExpressionRoot => axMathExpressionRoot -> IO (Id AXMathExpression)
rootIndexExpression axMathExpressionRoot  =
  sendMsg axMathExpressionRoot (mkSelector "rootIndexExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRadicandExpressions:rootIndexExpression:@
initWithRadicandExpressions_rootIndexExpressionSelector :: Selector
initWithRadicandExpressions_rootIndexExpressionSelector = mkSelector "initWithRadicandExpressions:rootIndexExpression:"

-- | @Selector@ for @radicandExpressions@
radicandExpressionsSelector :: Selector
radicandExpressionsSelector = mkSelector "radicandExpressions"

-- | @Selector@ for @rootIndexExpression@
rootIndexExpressionSelector :: Selector
rootIndexExpressionSelector = mkSelector "rootIndexExpression"

