{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMXPathExpression@.
module ObjC.WebKit.DOMXPathExpression
  ( DOMXPathExpression
  , IsDOMXPathExpression(..)
  , evaluate_type_inResult
  , evaluate
  , evaluate_type_inResultSelector
  , evaluateSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- evaluate:type:inResult:@
evaluate_type_inResult :: (IsDOMXPathExpression domxPathExpression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domxPathExpression -> contextNode -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate_type_inResult domxPathExpression  contextNode type_ inResult =
withObjCPtr contextNode $ \raw_contextNode ->
  withObjCPtr inResult $ \raw_inResult ->
      sendMsg domxPathExpression (mkSelector "evaluate:type:inResult:") (retPtr retVoid) [argPtr (castPtr raw_contextNode :: Ptr ()), argCUInt (fromIntegral type_), argPtr (castPtr raw_inResult :: Ptr ())] >>= retainedObject . castPtr

-- | @- evaluate:::@
evaluate :: (IsDOMXPathExpression domxPathExpression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domxPathExpression -> contextNode -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate domxPathExpression  contextNode type_ inResult =
withObjCPtr contextNode $ \raw_contextNode ->
  withObjCPtr inResult $ \raw_inResult ->
      sendMsg domxPathExpression (mkSelector "evaluate:::") (retPtr retVoid) [argPtr (castPtr raw_contextNode :: Ptr ()), argCUInt (fromIntegral type_), argPtr (castPtr raw_inResult :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @evaluate:type:inResult:@
evaluate_type_inResultSelector :: Selector
evaluate_type_inResultSelector = mkSelector "evaluate:type:inResult:"

-- | @Selector@ for @evaluate:::@
evaluateSelector :: Selector
evaluateSelector = mkSelector "evaluate:::"

