{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMXPathExpression@.
module ObjC.WebKit.DOMXPathExpression
  ( DOMXPathExpression
  , IsDOMXPathExpression(..)
  , evaluate_type_inResult
  , evaluate
  , evaluateSelector
  , evaluate_type_inResultSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- evaluate:type:inResult:@
evaluate_type_inResult :: (IsDOMXPathExpression domxPathExpression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domxPathExpression -> contextNode -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate_type_inResult domxPathExpression contextNode type_ inResult =
  sendMessage domxPathExpression evaluate_type_inResultSelector (toDOMNode contextNode) type_ (toDOMXPathResult inResult)

-- | @- evaluate:::@
evaluate :: (IsDOMXPathExpression domxPathExpression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domxPathExpression -> contextNode -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate domxPathExpression contextNode type_ inResult =
  sendMessage domxPathExpression evaluateSelector (toDOMNode contextNode) type_ (toDOMXPathResult inResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @evaluate:type:inResult:@
evaluate_type_inResultSelector :: Selector '[Id DOMNode, CUShort, Id DOMXPathResult] (Id DOMXPathResult)
evaluate_type_inResultSelector = mkSelector "evaluate:type:inResult:"

-- | @Selector@ for @evaluate:::@
evaluateSelector :: Selector '[Id DOMNode, CUShort, Id DOMXPathResult] (Id DOMXPathResult)
evaluateSelector = mkSelector "evaluate:::"

