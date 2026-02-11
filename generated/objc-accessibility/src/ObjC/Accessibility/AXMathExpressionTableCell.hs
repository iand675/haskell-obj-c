{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionTableCell@.
module ObjC.Accessibility.AXMathExpressionTableCell
  ( AXMathExpressionTableCell
  , IsAXMathExpressionTableCell(..)
  , initWithExpressions
  , expressions
  , initWithExpressionsSelector
  , expressionsSelector


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

-- | @- initWithExpressions:@
initWithExpressions :: (IsAXMathExpressionTableCell axMathExpressionTableCell, IsNSArray expressions) => axMathExpressionTableCell -> expressions -> IO (Id AXMathExpressionTableCell)
initWithExpressions axMathExpressionTableCell  expressions =
withObjCPtr expressions $ \raw_expressions ->
    sendMsg axMathExpressionTableCell (mkSelector "initWithExpressions:") (retPtr retVoid) [argPtr (castPtr raw_expressions :: Ptr ())] >>= ownedObject . castPtr

-- | @- expressions@
expressions :: IsAXMathExpressionTableCell axMathExpressionTableCell => axMathExpressionTableCell -> IO (Id NSArray)
expressions axMathExpressionTableCell  =
  sendMsg axMathExpressionTableCell (mkSelector "expressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector
expressionsSelector = mkSelector "expressions"

