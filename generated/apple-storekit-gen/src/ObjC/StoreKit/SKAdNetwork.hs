{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKAdNetwork@.
module ObjC.StoreKit.SKAdNetwork
  ( SKAdNetwork
  , IsSKAdNetwork(..)
  , startImpression_completionHandler
  , endImpression_completionHandler
  , registerAppForAdNetworkAttribution
  , updateConversionValue
  , updatePostbackConversionValue_completionHandler
  , updatePostbackConversionValue_coarseValue_completionHandler
  , updatePostbackConversionValue_coarseValue_lockWindow_completionHandler
  , endImpression_completionHandlerSelector
  , registerAppForAdNetworkAttributionSelector
  , startImpression_completionHandlerSelector
  , updateConversionValueSelector
  , updatePostbackConversionValue_coarseValue_completionHandlerSelector
  , updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector
  , updatePostbackConversionValue_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ startImpression:completionHandler:@
startImpression_completionHandler :: IsSKAdImpression impression => impression -> Ptr () -> IO ()
startImpression_completionHandler impression completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' startImpression_completionHandlerSelector (toSKAdImpression impression) completion

-- | @+ endImpression:completionHandler:@
endImpression_completionHandler :: IsSKAdImpression impression => impression -> Ptr () -> IO ()
endImpression_completionHandler impression completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' endImpression_completionHandlerSelector (toSKAdImpression impression) completion

-- | @+ registerAppForAdNetworkAttribution@
registerAppForAdNetworkAttribution :: IO ()
registerAppForAdNetworkAttribution  =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' registerAppForAdNetworkAttributionSelector

-- | @+ updateConversionValue:@
updateConversionValue :: CLong -> IO ()
updateConversionValue conversionValue =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' updateConversionValueSelector conversionValue

-- | @+ updatePostbackConversionValue:completionHandler:@
updatePostbackConversionValue_completionHandler :: CLong -> Ptr () -> IO ()
updatePostbackConversionValue_completionHandler conversionValue completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' updatePostbackConversionValue_completionHandlerSelector conversionValue completion

-- | @+ updatePostbackConversionValue:coarseValue:completionHandler:@
updatePostbackConversionValue_coarseValue_completionHandler :: IsNSString coarseValue => CLong -> coarseValue -> Ptr () -> IO ()
updatePostbackConversionValue_coarseValue_completionHandler fineValue coarseValue completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' updatePostbackConversionValue_coarseValue_completionHandlerSelector fineValue (toNSString coarseValue) completion

-- | @+ updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:@
updatePostbackConversionValue_coarseValue_lockWindow_completionHandler :: IsNSString coarseValue => CLong -> coarseValue -> Bool -> Ptr () -> IO ()
updatePostbackConversionValue_coarseValue_lockWindow_completionHandler fineValue coarseValue lockWindow completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMessage cls' updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector fineValue (toNSString coarseValue) lockWindow completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startImpression:completionHandler:@
startImpression_completionHandlerSelector :: Selector '[Id SKAdImpression, Ptr ()] ()
startImpression_completionHandlerSelector = mkSelector "startImpression:completionHandler:"

-- | @Selector@ for @endImpression:completionHandler:@
endImpression_completionHandlerSelector :: Selector '[Id SKAdImpression, Ptr ()] ()
endImpression_completionHandlerSelector = mkSelector "endImpression:completionHandler:"

-- | @Selector@ for @registerAppForAdNetworkAttribution@
registerAppForAdNetworkAttributionSelector :: Selector '[] ()
registerAppForAdNetworkAttributionSelector = mkSelector "registerAppForAdNetworkAttribution"

-- | @Selector@ for @updateConversionValue:@
updateConversionValueSelector :: Selector '[CLong] ()
updateConversionValueSelector = mkSelector "updateConversionValue:"

-- | @Selector@ for @updatePostbackConversionValue:completionHandler:@
updatePostbackConversionValue_completionHandlerSelector :: Selector '[CLong, Ptr ()] ()
updatePostbackConversionValue_completionHandlerSelector = mkSelector "updatePostbackConversionValue:completionHandler:"

-- | @Selector@ for @updatePostbackConversionValue:coarseValue:completionHandler:@
updatePostbackConversionValue_coarseValue_completionHandlerSelector :: Selector '[CLong, Id NSString, Ptr ()] ()
updatePostbackConversionValue_coarseValue_completionHandlerSelector = mkSelector "updatePostbackConversionValue:coarseValue:completionHandler:"

-- | @Selector@ for @updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:@
updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector :: Selector '[CLong, Id NSString, Bool, Ptr ()] ()
updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector = mkSelector "updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:"

