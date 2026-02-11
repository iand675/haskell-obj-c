{-# LANGUAGE PatternSynonyms #-}
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
  , contentURLForProductIDSelector
  , deleteContentForProductIDSelector
  , stateSelector
  , downloadStateSelector
  , contentLengthSelector
  , expectedContentLengthSelector
  , contentIdentifierSelector
  , contentURLSelector
  , contentVersionSelector
  , errorSelector
  , progressSelector
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
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ contentURLForProductID:@
contentURLForProductID :: IsNSString productID => productID -> IO (Id NSURL)
contentURLForProductID productID =
  do
    cls' <- getRequiredClass "SKDownload"
    withObjCPtr productID $ \raw_productID ->
      sendClassMsg cls' (mkSelector "contentURLForProductID:") (retPtr retVoid) [argPtr (castPtr raw_productID :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteContentForProductID:@
deleteContentForProductID :: IsNSString productID => productID -> IO ()
deleteContentForProductID productID =
  do
    cls' <- getRequiredClass "SKDownload"
    withObjCPtr productID $ \raw_productID ->
      sendClassMsg cls' (mkSelector "deleteContentForProductID:") retVoid [argPtr (castPtr raw_productID :: Ptr ())]

-- | @- state@
state :: IsSKDownload skDownload => skDownload -> IO SKDownloadState
state skDownload  =
    fmap (coerce :: CLong -> SKDownloadState) $ sendMsg skDownload (mkSelector "state") retCLong []

-- | @- downloadState@
downloadState :: IsSKDownload skDownload => skDownload -> IO SKDownloadState
downloadState skDownload  =
    fmap (coerce :: CLong -> SKDownloadState) $ sendMsg skDownload (mkSelector "downloadState") retCLong []

-- | @- contentLength@
contentLength :: IsSKDownload skDownload => skDownload -> IO (Id NSNumber)
contentLength skDownload  =
    sendMsg skDownload (mkSelector "contentLength") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- expectedContentLength@
expectedContentLength :: IsSKDownload skDownload => skDownload -> IO CLong
expectedContentLength skDownload  =
    sendMsg skDownload (mkSelector "expectedContentLength") retCLong []

-- | @- contentIdentifier@
contentIdentifier :: IsSKDownload skDownload => skDownload -> IO (Id NSString)
contentIdentifier skDownload  =
    sendMsg skDownload (mkSelector "contentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentURL@
contentURL :: IsSKDownload skDownload => skDownload -> IO (Id NSURL)
contentURL skDownload  =
    sendMsg skDownload (mkSelector "contentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentVersion@
contentVersion :: IsSKDownload skDownload => skDownload -> IO (Id NSString)
contentVersion skDownload  =
    sendMsg skDownload (mkSelector "contentVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- error@
error_ :: IsSKDownload skDownload => skDownload -> IO (Id NSError)
error_ skDownload  =
    sendMsg skDownload (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- progress@
progress :: IsSKDownload skDownload => skDownload -> IO CFloat
progress skDownload  =
    sendMsg skDownload (mkSelector "progress") retCFloat []

-- | @- timeRemaining@
timeRemaining :: IsSKDownload skDownload => skDownload -> IO CDouble
timeRemaining skDownload  =
    sendMsg skDownload (mkSelector "timeRemaining") retCDouble []

-- | @- transaction@
transaction :: IsSKDownload skDownload => skDownload -> IO (Id SKPaymentTransaction)
transaction skDownload  =
    sendMsg skDownload (mkSelector "transaction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentURLForProductID:@
contentURLForProductIDSelector :: Selector
contentURLForProductIDSelector = mkSelector "contentURLForProductID:"

-- | @Selector@ for @deleteContentForProductID:@
deleteContentForProductIDSelector :: Selector
deleteContentForProductIDSelector = mkSelector "deleteContentForProductID:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @downloadState@
downloadStateSelector :: Selector
downloadStateSelector = mkSelector "downloadState"

-- | @Selector@ for @contentLength@
contentLengthSelector :: Selector
contentLengthSelector = mkSelector "contentLength"

-- | @Selector@ for @expectedContentLength@
expectedContentLengthSelector :: Selector
expectedContentLengthSelector = mkSelector "expectedContentLength"

-- | @Selector@ for @contentIdentifier@
contentIdentifierSelector :: Selector
contentIdentifierSelector = mkSelector "contentIdentifier"

-- | @Selector@ for @contentURL@
contentURLSelector :: Selector
contentURLSelector = mkSelector "contentURL"

-- | @Selector@ for @contentVersion@
contentVersionSelector :: Selector
contentVersionSelector = mkSelector "contentVersion"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @progress@
progressSelector :: Selector
progressSelector = mkSelector "progress"

-- | @Selector@ for @timeRemaining@
timeRemainingSelector :: Selector
timeRemainingSelector = mkSelector "timeRemaining"

-- | @Selector@ for @transaction@
transactionSelector :: Selector
transactionSelector = mkSelector "transaction"

