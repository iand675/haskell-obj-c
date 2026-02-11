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
  , startImpression_completionHandlerSelector
  , endImpression_completionHandlerSelector
  , registerAppForAdNetworkAttributionSelector
  , updateConversionValueSelector
  , updatePostbackConversionValue_completionHandlerSelector
  , updatePostbackConversionValue_coarseValue_completionHandlerSelector
  , updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ startImpression:completionHandler:@
startImpression_completionHandler :: IsSKAdImpression impression => impression -> Ptr () -> IO ()
startImpression_completionHandler impression completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    withObjCPtr impression $ \raw_impression ->
      sendClassMsg cls' (mkSelector "startImpression:completionHandler:") retVoid [argPtr (castPtr raw_impression :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @+ endImpression:completionHandler:@
endImpression_completionHandler :: IsSKAdImpression impression => impression -> Ptr () -> IO ()
endImpression_completionHandler impression completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    withObjCPtr impression $ \raw_impression ->
      sendClassMsg cls' (mkSelector "endImpression:completionHandler:") retVoid [argPtr (castPtr raw_impression :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @+ registerAppForAdNetworkAttribution@
registerAppForAdNetworkAttribution :: IO ()
registerAppForAdNetworkAttribution  =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMsg cls' (mkSelector "registerAppForAdNetworkAttribution") retVoid []

-- | @+ updateConversionValue:@
updateConversionValue :: CLong -> IO ()
updateConversionValue conversionValue =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMsg cls' (mkSelector "updateConversionValue:") retVoid [argCLong (fromIntegral conversionValue)]

-- | @+ updatePostbackConversionValue:completionHandler:@
updatePostbackConversionValue_completionHandler :: CLong -> Ptr () -> IO ()
updatePostbackConversionValue_completionHandler conversionValue completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    sendClassMsg cls' (mkSelector "updatePostbackConversionValue:completionHandler:") retVoid [argCLong (fromIntegral conversionValue), argPtr (castPtr completion :: Ptr ())]

-- | @+ updatePostbackConversionValue:coarseValue:completionHandler:@
updatePostbackConversionValue_coarseValue_completionHandler :: IsNSString coarseValue => CLong -> coarseValue -> Ptr () -> IO ()
updatePostbackConversionValue_coarseValue_completionHandler fineValue coarseValue completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    withObjCPtr coarseValue $ \raw_coarseValue ->
      sendClassMsg cls' (mkSelector "updatePostbackConversionValue:coarseValue:completionHandler:") retVoid [argCLong (fromIntegral fineValue), argPtr (castPtr raw_coarseValue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @+ updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:@
updatePostbackConversionValue_coarseValue_lockWindow_completionHandler :: IsNSString coarseValue => CLong -> coarseValue -> Bool -> Ptr () -> IO ()
updatePostbackConversionValue_coarseValue_lockWindow_completionHandler fineValue coarseValue lockWindow completion =
  do
    cls' <- getRequiredClass "SKAdNetwork"
    withObjCPtr coarseValue $ \raw_coarseValue ->
      sendClassMsg cls' (mkSelector "updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:") retVoid [argCLong (fromIntegral fineValue), argPtr (castPtr raw_coarseValue :: Ptr ()), argCULong (if lockWindow then 1 else 0), argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startImpression:completionHandler:@
startImpression_completionHandlerSelector :: Selector
startImpression_completionHandlerSelector = mkSelector "startImpression:completionHandler:"

-- | @Selector@ for @endImpression:completionHandler:@
endImpression_completionHandlerSelector :: Selector
endImpression_completionHandlerSelector = mkSelector "endImpression:completionHandler:"

-- | @Selector@ for @registerAppForAdNetworkAttribution@
registerAppForAdNetworkAttributionSelector :: Selector
registerAppForAdNetworkAttributionSelector = mkSelector "registerAppForAdNetworkAttribution"

-- | @Selector@ for @updateConversionValue:@
updateConversionValueSelector :: Selector
updateConversionValueSelector = mkSelector "updateConversionValue:"

-- | @Selector@ for @updatePostbackConversionValue:completionHandler:@
updatePostbackConversionValue_completionHandlerSelector :: Selector
updatePostbackConversionValue_completionHandlerSelector = mkSelector "updatePostbackConversionValue:completionHandler:"

-- | @Selector@ for @updatePostbackConversionValue:coarseValue:completionHandler:@
updatePostbackConversionValue_coarseValue_completionHandlerSelector :: Selector
updatePostbackConversionValue_coarseValue_completionHandlerSelector = mkSelector "updatePostbackConversionValue:coarseValue:completionHandler:"

-- | @Selector@ for @updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:@
updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector :: Selector
updatePostbackConversionValue_coarseValue_lockWindow_completionHandlerSelector = mkSelector "updatePostbackConversionValue:coarseValue:lockWindow:completionHandler:"

