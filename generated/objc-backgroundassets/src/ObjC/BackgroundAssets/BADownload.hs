{-# LANGUAGE PatternSynonyms #-}
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
  , priority
  , isEssential
  , copyAsNonEssentialSelector
  , initSelector
  , newSelector
  , stateSelector
  , prioritySelector
  , isEssentialSelector

  -- * Enum types
  , BADownloadState(BADownloadState)
  , pattern BADownloadStateFailed
  , pattern BADownloadStateCreated
  , pattern BADownloadStateWaiting
  , pattern BADownloadStateDownloading
  , pattern BADownloadStateFinished

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

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.BackgroundAssets.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Copies an existing download ensuring that it has @isEssential == false@.
--
-- This serves as a convenience method for constructing a non-essential representation of an existing download. It is important to note that essential downloads can only be enqueued by the app extension during a content request. If an essential download fails, @copyAsNonEssential@ can be used to create a copy with @isEssential == false@ that can be re-queued with @BADownloadManager@.
--
-- ObjC selector: @- copyAsNonEssential@
copyAsNonEssential :: IsBADownload baDownload => baDownload -> IO (Id BADownload)
copyAsNonEssential baDownload  =
  sendMsg baDownload (mkSelector "copyAsNonEssential") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsBADownload baDownload => baDownload -> IO (Id BADownload)
init_ baDownload  =
  sendMsg baDownload (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BADownload)
new  =
  do
    cls' <- getRequiredClass "BADownload"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The current state of the respresented download.
--
-- ObjC selector: @- state@
state :: IsBADownload baDownload => baDownload -> IO BADownloadState
state baDownload  =
  fmap (coerce :: CLong -> BADownloadState) $ sendMsg baDownload (mkSelector "state") retCLong []

-- | A client set priority to try to order downloads in order of importance
--
-- ObjC selector: @- priority@
priority :: IsBADownload baDownload => baDownload -> IO CLong
priority baDownload  =
  sendMsg baDownload (mkSelector "priority") retCLong []

-- | Whether this download is essential. Essential downloads will occur while the app is being installed. Users cannot launch the app while these downloads are occurring. Essential downloads cannot be scheduled with @BADownloadManager@, they may only be scheduled from the extension with a @BAContentRequest@ type of @Update@ or @Install@. Essential downloads must have an accurate @fileSize@ or they will fail.
--
-- ObjC selector: @- isEssential@
isEssential :: IsBADownload baDownload => baDownload -> IO Bool
isEssential baDownload  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg baDownload (mkSelector "isEssential") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @copyAsNonEssential@
copyAsNonEssentialSelector :: Selector
copyAsNonEssentialSelector = mkSelector "copyAsNonEssential"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @isEssential@
isEssentialSelector :: Selector
isEssentialSelector = mkSelector "isEssential"

