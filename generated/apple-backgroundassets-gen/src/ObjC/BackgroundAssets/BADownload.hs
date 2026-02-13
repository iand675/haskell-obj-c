{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BADownload@.
module ObjC.BackgroundAssets.BADownload
  ( BADownload
  , IsBADownload(..)
  , copyAsNonEssential
  , init_
  , new
  , state
  , identifier
  , uniqueIdentifier
  , priority
  , isEssential
  , copyAsNonEssentialSelector
  , identifierSelector
  , initSelector
  , isEssentialSelector
  , newSelector
  , prioritySelector
  , stateSelector
  , uniqueIdentifierSelector

  -- * Enum types
  , BADownloadState(BADownloadState)
  , pattern BADownloadStateFailed
  , pattern BADownloadStateCreated
  , pattern BADownloadStateWaiting
  , pattern BADownloadStateDownloading
  , pattern BADownloadStateFinished

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.BackgroundAssets.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Copies an existing download ensuring that it has @isEssential == false@.
--
-- This serves as a convenience method for constructing a non-essential representation of an existing download. It is important to note that essential downloads can only be enqueued by the app extension during a content request. If an essential download fails, @copyAsNonEssential@ can be used to create a copy with @isEssential == false@ that can be re-queued with @BADownloadManager@.
--
-- ObjC selector: @- copyAsNonEssential@
copyAsNonEssential :: IsBADownload baDownload => baDownload -> IO (Id BADownload)
copyAsNonEssential baDownload =
  sendOwnedMessage baDownload copyAsNonEssentialSelector

-- | @- init@
init_ :: IsBADownload baDownload => baDownload -> IO (Id BADownload)
init_ baDownload =
  sendOwnedMessage baDownload initSelector

-- | @+ new@
new :: IO (Id BADownload)
new  =
  do
    cls' <- getRequiredClass "BADownload"
    sendOwnedClassMessage cls' newSelector

-- | The current state of the respresented download.
--
-- ObjC selector: @- state@
state :: IsBADownload baDownload => baDownload -> IO BADownloadState
state baDownload =
  sendMessage baDownload stateSelector

-- | A client defined identifier that uniquely identifies this asset.
--
-- ObjC selector: @- identifier@
identifier :: IsBADownload baDownload => baDownload -> IO (Id NSString)
identifier baDownload =
  sendMessage baDownload identifierSelector

-- | A UUID that uniquely identifies the download object.
--
-- ObjC selector: @- uniqueIdentifier@
uniqueIdentifier :: IsBADownload baDownload => baDownload -> IO (Id NSString)
uniqueIdentifier baDownload =
  sendMessage baDownload uniqueIdentifierSelector

-- | A client set priority to try to order downloads in order of importance
--
-- ObjC selector: @- priority@
priority :: IsBADownload baDownload => baDownload -> IO CLong
priority baDownload =
  sendMessage baDownload prioritySelector

-- | Whether this download is essential. Essential downloads will occur while the app is being installed. Users cannot launch the app while these downloads are occurring. Essential downloads cannot be scheduled with @BADownloadManager@, they may only be scheduled from the extension with a @BAContentRequest@ type of @Update@ or @Install@. Essential downloads must have an accurate @fileSize@ or they will fail.
--
-- ObjC selector: @- isEssential@
isEssential :: IsBADownload baDownload => baDownload -> IO Bool
isEssential baDownload =
  sendMessage baDownload isEssentialSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @copyAsNonEssential@
copyAsNonEssentialSelector :: Selector '[] (Id BADownload)
copyAsNonEssentialSelector = mkSelector "copyAsNonEssential"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BADownload)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BADownload)
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector '[] BADownloadState
stateSelector = mkSelector "state"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSString)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] CLong
prioritySelector = mkSelector "priority"

-- | @Selector@ for @isEssential@
isEssentialSelector :: Selector '[] Bool
isEssentialSelector = mkSelector "isEssential"

