{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFetchRequestExpression@.
module ObjC.CoreData.NSFetchRequestExpression
  ( NSFetchRequestExpression
  , IsNSFetchRequestExpression(..)
  , expressionForFetch_context_countOnly
  , requestExpression
  , contextExpression
  , countOnlyRequest
  , contextExpressionSelector
  , countOnlyRequestSelector
  , expressionForFetch_context_countOnlySelector
  , requestExpressionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ expressionForFetch:context:countOnly:@
expressionForFetch_context_countOnly :: (IsNSExpression fetch, IsNSExpression context) => fetch -> context -> Bool -> IO (Id NSExpression)
expressionForFetch_context_countOnly fetch context countFlag =
  do
    cls' <- getRequiredClass "NSFetchRequestExpression"
    sendClassMessage cls' expressionForFetch_context_countOnlySelector (toNSExpression fetch) (toNSExpression context) countFlag

-- | @- requestExpression@
requestExpression :: IsNSFetchRequestExpression nsFetchRequestExpression => nsFetchRequestExpression -> IO (Id NSExpression)
requestExpression nsFetchRequestExpression =
  sendMessage nsFetchRequestExpression requestExpressionSelector

-- | @- contextExpression@
contextExpression :: IsNSFetchRequestExpression nsFetchRequestExpression => nsFetchRequestExpression -> IO (Id NSExpression)
contextExpression nsFetchRequestExpression =
  sendMessage nsFetchRequestExpression contextExpressionSelector

-- | @- countOnlyRequest@
countOnlyRequest :: IsNSFetchRequestExpression nsFetchRequestExpression => nsFetchRequestExpression -> IO Bool
countOnlyRequest nsFetchRequestExpression =
  sendMessage nsFetchRequestExpression countOnlyRequestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expressionForFetch:context:countOnly:@
expressionForFetch_context_countOnlySelector :: Selector '[Id NSExpression, Id NSExpression, Bool] (Id NSExpression)
expressionForFetch_context_countOnlySelector = mkSelector "expressionForFetch:context:countOnly:"

-- | @Selector@ for @requestExpression@
requestExpressionSelector :: Selector '[] (Id NSExpression)
requestExpressionSelector = mkSelector "requestExpression"

-- | @Selector@ for @contextExpression@
contextExpressionSelector :: Selector '[] (Id NSExpression)
contextExpressionSelector = mkSelector "contextExpression"

-- | @Selector@ for @countOnlyRequest@
countOnlyRequestSelector :: Selector '[] Bool
countOnlyRequestSelector = mkSelector "countOnlyRequest"

