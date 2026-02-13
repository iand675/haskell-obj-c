{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionFenced@.
module ObjC.Accessibility.AXMathExpressionFenced
  ( AXMathExpressionFenced
  , IsAXMathExpressionFenced(..)
  , initWithExpressions_openString_closeString
  , expressions
  , openString
  , closeString
  , closeStringSelector
  , expressionsSelector
  , initWithExpressions_openString_closeStringSelector
  , openStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithExpressions:openString:closeString:@
initWithExpressions_openString_closeString :: (IsAXMathExpressionFenced axMathExpressionFenced, IsNSArray expressions, IsNSString openString, IsNSString closeString) => axMathExpressionFenced -> expressions -> openString -> closeString -> IO (Id AXMathExpressionFenced)
initWithExpressions_openString_closeString axMathExpressionFenced expressions openString closeString =
  sendOwnedMessage axMathExpressionFenced initWithExpressions_openString_closeStringSelector (toNSArray expressions) (toNSString openString) (toNSString closeString)

-- | @- expressions@
expressions :: IsAXMathExpressionFenced axMathExpressionFenced => axMathExpressionFenced -> IO (Id NSArray)
expressions axMathExpressionFenced =
  sendMessage axMathExpressionFenced expressionsSelector

-- | @- openString@
openString :: IsAXMathExpressionFenced axMathExpressionFenced => axMathExpressionFenced -> IO (Id NSString)
openString axMathExpressionFenced =
  sendMessage axMathExpressionFenced openStringSelector

-- | @- closeString@
closeString :: IsAXMathExpressionFenced axMathExpressionFenced => axMathExpressionFenced -> IO (Id NSString)
closeString axMathExpressionFenced =
  sendMessage axMathExpressionFenced closeStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:openString:closeString:@
initWithExpressions_openString_closeStringSelector :: Selector '[Id NSArray, Id NSString, Id NSString] (Id AXMathExpressionFenced)
initWithExpressions_openString_closeStringSelector = mkSelector "initWithExpressions:openString:closeString:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector '[] (Id NSArray)
expressionsSelector = mkSelector "expressions"

-- | @Selector@ for @openString@
openStringSelector :: Selector '[] (Id NSString)
openStringSelector = mkSelector "openString"

-- | @Selector@ for @closeString@
closeStringSelector :: Selector '[] (Id NSString)
closeStringSelector = mkSelector "closeString"

