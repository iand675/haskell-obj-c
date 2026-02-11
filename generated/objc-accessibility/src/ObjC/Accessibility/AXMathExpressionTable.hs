{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionTable@.
module ObjC.Accessibility.AXMathExpressionTable
  ( AXMathExpressionTable
  , IsAXMathExpressionTable(..)
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
initWithExpressions :: (IsAXMathExpressionTable axMathExpressionTable, IsNSArray expressions) => axMathExpressionTable -> expressions -> IO (Id AXMathExpressionTable)
initWithExpressions axMathExpressionTable  expressions =
withObjCPtr expressions $ \raw_expressions ->
    sendMsg axMathExpressionTable (mkSelector "initWithExpressions:") (retPtr retVoid) [argPtr (castPtr raw_expressions :: Ptr ())] >>= ownedObject . castPtr

-- | @- expressions@
expressions :: IsAXMathExpressionTable axMathExpressionTable => axMathExpressionTable -> IO (Id NSArray)
expressions axMathExpressionTable  =
  sendMsg axMathExpressionTable (mkSelector "expressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector
expressionsSelector = mkSelector "expressions"

