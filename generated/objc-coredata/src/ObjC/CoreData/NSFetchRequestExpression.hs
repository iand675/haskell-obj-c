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
  , expressionForFetch_context_countOnlySelector
  , requestExpressionSelector
  , contextExpressionSelector
  , countOnlyRequestSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ expressionForFetch:context:countOnly:@
expressionForFetch_context_countOnly :: (IsNSExpression fetch, IsNSExpression context) => fetch -> context -> Bool -> IO (Id NSExpression)
expressionForFetch_context_countOnly fetch context countFlag =
  do
    cls' <- getRequiredClass "NSFetchRequestExpression"
    withObjCPtr fetch $ \raw_fetch ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "expressionForFetch:context:countOnly:") (retPtr retVoid) [argPtr (castPtr raw_fetch :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argCULong (if countFlag then 1 else 0)] >>= retainedObject . castPtr

-- | @- requestExpression@
requestExpression :: IsNSFetchRequestExpression nsFetchRequestExpression => nsFetchRequestExpression -> IO (Id NSExpression)
requestExpression nsFetchRequestExpression  =
  sendMsg nsFetchRequestExpression (mkSelector "requestExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contextExpression@
contextExpression :: IsNSFetchRequestExpression nsFetchRequestExpression => nsFetchRequestExpression -> IO (Id NSExpression)
contextExpression nsFetchRequestExpression  =
  sendMsg nsFetchRequestExpression (mkSelector "contextExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- countOnlyRequest@
countOnlyRequest :: IsNSFetchRequestExpression nsFetchRequestExpression => nsFetchRequestExpression -> IO Bool
countOnlyRequest nsFetchRequestExpression  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequestExpression (mkSelector "countOnlyRequest") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expressionForFetch:context:countOnly:@
expressionForFetch_context_countOnlySelector :: Selector
expressionForFetch_context_countOnlySelector = mkSelector "expressionForFetch:context:countOnly:"

-- | @Selector@ for @requestExpression@
requestExpressionSelector :: Selector
requestExpressionSelector = mkSelector "requestExpression"

-- | @Selector@ for @contextExpression@
contextExpressionSelector :: Selector
contextExpressionSelector = mkSelector "contextExpression"

-- | @Selector@ for @countOnlyRequest@
countOnlyRequestSelector :: Selector
countOnlyRequestSelector = mkSelector "countOnlyRequest"

