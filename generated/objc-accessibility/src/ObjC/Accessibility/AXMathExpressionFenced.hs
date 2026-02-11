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
  , initWithExpressions_openString_closeStringSelector
  , expressionsSelector
  , openStringSelector
  , closeStringSelector


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

-- | @- initWithExpressions:openString:closeString:@
initWithExpressions_openString_closeString :: (IsAXMathExpressionFenced axMathExpressionFenced, IsNSArray expressions, IsNSString openString, IsNSString closeString) => axMathExpressionFenced -> expressions -> openString -> closeString -> IO (Id AXMathExpressionFenced)
initWithExpressions_openString_closeString axMathExpressionFenced  expressions openString closeString =
withObjCPtr expressions $ \raw_expressions ->
  withObjCPtr openString $ \raw_openString ->
    withObjCPtr closeString $ \raw_closeString ->
        sendMsg axMathExpressionFenced (mkSelector "initWithExpressions:openString:closeString:") (retPtr retVoid) [argPtr (castPtr raw_expressions :: Ptr ()), argPtr (castPtr raw_openString :: Ptr ()), argPtr (castPtr raw_closeString :: Ptr ())] >>= ownedObject . castPtr

-- | @- expressions@
expressions :: IsAXMathExpressionFenced axMathExpressionFenced => axMathExpressionFenced -> IO (Id NSArray)
expressions axMathExpressionFenced  =
  sendMsg axMathExpressionFenced (mkSelector "expressions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- openString@
openString :: IsAXMathExpressionFenced axMathExpressionFenced => axMathExpressionFenced -> IO (Id NSString)
openString axMathExpressionFenced  =
  sendMsg axMathExpressionFenced (mkSelector "openString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- closeString@
closeString :: IsAXMathExpressionFenced axMathExpressionFenced => axMathExpressionFenced -> IO (Id NSString)
closeString axMathExpressionFenced  =
  sendMsg axMathExpressionFenced (mkSelector "closeString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:openString:closeString:@
initWithExpressions_openString_closeStringSelector :: Selector
initWithExpressions_openString_closeStringSelector = mkSelector "initWithExpressions:openString:closeString:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector
expressionsSelector = mkSelector "expressions"

-- | @Selector@ for @openString@
openStringSelector :: Selector
openStringSelector = mkSelector "openString"

-- | @Selector@ for @closeString@
closeStringSelector :: Selector
closeStringSelector = mkSelector "closeString"

