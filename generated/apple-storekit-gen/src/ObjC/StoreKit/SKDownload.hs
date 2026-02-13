{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKDownload@.
module ObjC.StoreKit.SKDownload
  ( SKDownload
  , IsSKDownload(..)
  , contentURLForProductID
  , deleteContentForProductID
  , state
  , downloadState
  , contentLength
  , expectedContentLength
  , contentIdentifier
  , contentURL
  , contentVersion
  , error_
  , progress
  , timeRemaining
  , transaction
  , contentIdentifierSelector
  , contentLengthSelector
  , contentURLForProductIDSelector
  , contentURLSelector
  , contentVersionSelector
  , deleteContentForProductIDSelector
  , downloadStateSelector
  , errorSelector
  , expectedContentLengthSelector
  , progressSelector
  , stateSelector
  , timeRemainingSelector
  , transactionSelector

  -- * Enum types
  , SKDownloadState(SKDownloadState)
  , pattern SKDownloadStateWaiting
  , pattern SKDownloadStateActive
  , pattern SKDownloadStatePaused
  , pattern SKDownloadStateFinished
  , pattern SKDownloadStateFailed
  , pattern SKDownloadStateCancelled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ contentURLForProductID:@
contentURLForProductID :: IsNSString productID => productID -> IO (Id NSURL)
contentURLForProductID productID =
  do
    cls' <- getRequiredClass "SKDownload"
    sendClassMessage cls' contentURLForProductIDSelector (toNSString productID)

-- | @+ deleteContentForProductID:@
deleteContentForProductID :: IsNSString productID => productID -> IO ()
deleteContentForProductID productID =
  do
    cls' <- getRequiredClass "SKDownload"
    sendClassMessage cls' deleteContentForProductIDSelector (toNSString productID)

-- | @- state@
state :: IsSKDownload skDownload => skDownload -> IO SKDownloadState
state skDownload =
  sendMessage skDownload stateSelector

-- | @- downloadState@
downloadState :: IsSKDownload skDownload => skDownload -> IO SKDownloadState
downloadState skDownload =
  sendMessage skDownload downloadStateSelector

-- | @- contentLength@
contentLength :: IsSKDownload skDownload => skDownload -> IO (Id NSNumber)
contentLength skDownload =
  sendMessage skDownload contentLengthSelector

-- | @- expectedContentLength@
expectedContentLength :: IsSKDownload skDownload => skDownload -> IO CLong
expectedContentLength skDownload =
  sendMessage skDownload expectedContentLengthSelector

-- | @- contentIdentifier@
contentIdentifier :: IsSKDownload skDownload => skDownload -> IO (Id NSString)
contentIdentifier skDownload =
  sendMessage skDownload contentIdentifierSelector

-- | @- contentURL@
contentURL :: IsSKDownload skDownload => skDownload -> IO (Id NSURL)
contentURL skDownload =
  sendMessage skDownload contentURLSelector

-- | @- contentVersion@
contentVersion :: IsSKDownload skDownload => skDownload -> IO (Id NSString)
contentVersion skDownload =
  sendMessage skDownload contentVersionSelector

-- | @- error@
error_ :: IsSKDownload skDownload => skDownload -> IO (Id NSError)
error_ skDownload =
  sendMessage skDownload errorSelector

-- | @- progress@
progress :: IsSKDownload skDownload => skDownload -> IO CFloat
progress skDownload =
  sendMessage skDownload progressSelector

-- | @- timeRemaining@
timeRemaining :: IsSKDownload skDownload => skDownload -> IO CDouble
timeRemaining skDownload =
  sendMessage skDownload timeRemainingSelector

-- | @- transaction@
transaction :: IsSKDownload skDownload => skDownload -> IO (Id SKPaymentTransaction)
transaction skDownload =
  sendMessage skDownload transactionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentURLForProductID:@
contentURLForProductIDSelector :: Selector '[Id NSString] (Id NSURL)
contentURLForProductIDSelector = mkSelector "contentURLForProductID:"

-- | @Selector@ for @deleteContentForProductID:@
deleteContentForProductIDSelector :: Selector '[Id NSString] ()
deleteContentForProductIDSelector = mkSelector "deleteContentForProductID:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] SKDownloadState
stateSelector = mkSelector "state"

-- | @Selector@ for @downloadState@
downloadStateSelector :: Selector '[] SKDownloadState
downloadStateSelector = mkSelector "downloadState"

-- | @Selector@ for @contentLength@
contentLengthSelector :: Selector '[] (Id NSNumber)
contentLengthSelector = mkSelector "contentLength"

-- | @Selector@ for @expectedContentLength@
expectedContentLengthSelector :: Selector '[] CLong
expectedContentLengthSelector = mkSelector "expectedContentLength"

-- | @Selector@ for @contentIdentifier@
contentIdentifierSelector :: Selector '[] (Id NSString)
contentIdentifierSelector = mkSelector "contentIdentifier"

-- | @Selector@ for @contentURL@
contentURLSelector :: Selector '[] (Id NSURL)
contentURLSelector = mkSelector "contentURL"

-- | @Selector@ for @contentVersion@
contentVersionSelector :: Selector '[] (Id NSString)
contentVersionSelector = mkSelector "contentVersion"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] CFloat
progressSelector = mkSelector "progress"

-- | @Selector@ for @timeRemaining@
timeRemainingSelector :: Selector '[] CDouble
timeRemainingSelector = mkSelector "timeRemaining"

-- | @Selector@ for @transaction@
transactionSelector :: Selector '[] (Id SKPaymentTransaction)
transactionSelector = mkSelector "transaction"

