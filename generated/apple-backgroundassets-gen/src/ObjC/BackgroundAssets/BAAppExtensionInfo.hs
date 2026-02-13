{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BAAppExtensionInfo@.
module ObjC.BackgroundAssets.BAAppExtensionInfo
  ( BAAppExtensionInfo
  , IsBAAppExtensionInfo(..)
  , init_
  , new
  , restrictedDownloadSizeRemaining
  , restrictedEssentialDownloadSizeRemaining
  , initSelector
  , newSelector
  , restrictedDownloadSizeRemainingSelector
  , restrictedEssentialDownloadSizeRemainingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundAssets.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAppExtensionInfo baAppExtensionInfo => baAppExtensionInfo -> IO (Id BAAppExtensionInfo)
init_ baAppExtensionInfo =
  sendOwnedMessage baAppExtensionInfo initSelector

-- | @+ new@
new :: IO (Id BAAppExtensionInfo)
new  =
  do
    cls' <- getRequiredClass "BAAppExtensionInfo"
    sendOwnedClassMessage cls' newSelector

-- | The number of bytes remaining that can be scheduled if the total download size is restricted.
--
-- When a download is restricted, your extension can only schedule up to its @BADownloadAllowance@ defined in your app's @Info.plist@. This result tells you the number of bytes remaining that can be scheduled before the application is launched. Once the application is launched, this restriction is removed.
--
-- Returns: The result is @nil@ if downloads are not restricted. It returns a valid number with the remaining available download size otherwise.
--
-- ObjC selector: @- restrictedDownloadSizeRemaining@
restrictedDownloadSizeRemaining :: IsBAAppExtensionInfo baAppExtensionInfo => baAppExtensionInfo -> IO (Id NSNumber)
restrictedDownloadSizeRemaining baAppExtensionInfo =
  sendMessage baAppExtensionInfo restrictedDownloadSizeRemainingSelector

-- | The number of bytes remaining that can be scheduled if the total download size of optional assets is restricted.
--
-- When a download is restricted, your extension can only schedule up to its @BAEssentialDownloadAllowance@ defined in your app's @Info.plist@. This result tells you the number of bytes remaining that can be scheduled before the application is launched. Once the application is launched, this restriction is removed.
--
-- Returns: The result is @nil@ if downloads are not restricted. It returns a valid number with the remaining available download size otherwise.
--
-- ObjC selector: @- restrictedEssentialDownloadSizeRemaining@
restrictedEssentialDownloadSizeRemaining :: IsBAAppExtensionInfo baAppExtensionInfo => baAppExtensionInfo -> IO (Id NSNumber)
restrictedEssentialDownloadSizeRemaining baAppExtensionInfo =
  sendMessage baAppExtensionInfo restrictedEssentialDownloadSizeRemainingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BAAppExtensionInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BAAppExtensionInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @restrictedDownloadSizeRemaining@
restrictedDownloadSizeRemainingSelector :: Selector '[] (Id NSNumber)
restrictedDownloadSizeRemainingSelector = mkSelector "restrictedDownloadSizeRemaining"

-- | @Selector@ for @restrictedEssentialDownloadSizeRemaining@
restrictedEssentialDownloadSizeRemainingSelector :: Selector '[] (Id NSNumber)
restrictedEssentialDownloadSizeRemainingSelector = mkSelector "restrictedEssentialDownloadSizeRemaining"

