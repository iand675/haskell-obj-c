{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BEDownloadMonitorLocation@.
module ObjC.BrowserEngineKit.BEDownloadMonitorLocation
  ( BEDownloadMonitorLocation
  , IsBEDownloadMonitorLocation(..)
  , init_
  , new
  , url
  , bookmarkData
  , bookmarkDataSelector
  , initSelector
  , newSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBEDownloadMonitorLocation beDownloadMonitorLocation => beDownloadMonitorLocation -> IO (Id BEDownloadMonitorLocation)
init_ beDownloadMonitorLocation =
  sendOwnedMessage beDownloadMonitorLocation initSelector

-- | @+ new@
new :: IO (Id BEDownloadMonitorLocation)
new  =
  do
    cls' <- getRequiredClass "BEDownloadMonitorLocation"
    sendOwnedClassMessage cls' newSelector

-- | @- url@
url :: IsBEDownloadMonitorLocation beDownloadMonitorLocation => beDownloadMonitorLocation -> IO (Id NSURL)
url beDownloadMonitorLocation =
  sendMessage beDownloadMonitorLocation urlSelector

-- | @- bookmarkData@
bookmarkData :: IsBEDownloadMonitorLocation beDownloadMonitorLocation => beDownloadMonitorLocation -> IO (Id NSData)
bookmarkData beDownloadMonitorLocation =
  sendMessage beDownloadMonitorLocation bookmarkDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BEDownloadMonitorLocation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BEDownloadMonitorLocation)
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @bookmarkData@
bookmarkDataSelector :: Selector '[] (Id NSData)
bookmarkDataSelector = mkSelector "bookmarkData"

