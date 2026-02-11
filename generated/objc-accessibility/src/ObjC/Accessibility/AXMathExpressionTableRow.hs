{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionTableRow@.
module ObjC.Accessibility.AXMathExpressionTableRow
  ( AXMathExpressionTableRow
  , IsAXMathExpressionTableRow(..)
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
initWithExpressions :: (IsAXMathExpressionTableRow axMathExpressionTableRow, IsNSArray expressions) => axMathExpressionTableRow -> expressions -> IO (Id AXMathExpressionTableRow)
initWithExpressions axMathExpressionTableRow  expressions =
withObjCPtr expressions $ \raw_expressions ->
    sendMsg axMathExpressionTableRow (mkSelector "initWithExpressions:") (retPtr retVoid) [argPtr (castPtr raw_expressions :: Ptr ())] >>= ownedObject . castPtr

-- | @- expressions@
expressions :: IsAXMathExpressionTableRow axMathExpressionTableRow => axMathExpressionTableRow -> IO (Id NSArray)
expressions axMathExpressionTableRow  =
  sendMsg axMathExpressionTableRow (mkSelector "expressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector
expressionsSelector = mkSelector "expressions"

