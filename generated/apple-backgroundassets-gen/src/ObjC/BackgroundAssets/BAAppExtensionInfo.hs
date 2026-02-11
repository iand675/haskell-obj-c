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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBAAppExtensionInfo baAppExtensionInfo => baAppExtensionInfo -> IO (Id BAAppExtensionInfo)
init_ baAppExtensionInfo  =
    sendMsg baAppExtensionInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BAAppExtensionInfo)
new  =
  do
    cls' <- getRequiredClass "BAAppExtensionInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The number of bytes remaining that can be scheduled if the total download size is restricted.
--
-- When a download is restricted, your extension can only schedule up to its @BADownloadAllowance@ defined in your app's @Info.plist@. This result tells you the number of bytes remaining that can be scheduled before the application is launched. Once the application is launched, this restriction is removed.
--
-- Returns: The result is @nil@ if downloads are not restricted. It returns a valid number with the remaining available download size otherwise.
--
-- ObjC selector: @- restrictedDownloadSizeRemaining@
restrictedDownloadSizeRemaining :: IsBAAppExtensionInfo baAppExtensionInfo => baAppExtensionInfo -> IO (Id NSNumber)
restrictedDownloadSizeRemaining baAppExtensionInfo  =
    sendMsg baAppExtensionInfo (mkSelector "restrictedDownloadSizeRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The number of bytes remaining that can be scheduled if the total download size of optional assets is restricted.
--
-- When a download is restricted, your extension can only schedule up to its @BAEssentialDownloadAllowance@ defined in your app's @Info.plist@. This result tells you the number of bytes remaining that can be scheduled before the application is launched. Once the application is launched, this restriction is removed.
--
-- Returns: The result is @nil@ if downloads are not restricted. It returns a valid number with the remaining available download size otherwise.
--
-- ObjC selector: @- restrictedEssentialDownloadSizeRemaining@
restrictedEssentialDownloadSizeRemaining :: IsBAAppExtensionInfo baAppExtensionInfo => baAppExtensionInfo -> IO (Id NSNumber)
restrictedEssentialDownloadSizeRemaining baAppExtensionInfo  =
    sendMsg baAppExtensionInfo (mkSelector "restrictedEssentialDownloadSizeRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @restrictedDownloadSizeRemaining@
restrictedDownloadSizeRemainingSelector :: Selector
restrictedDownloadSizeRemainingSelector = mkSelector "restrictedDownloadSizeRemaining"

-- | @Selector@ for @restrictedEssentialDownloadSizeRemaining@
restrictedEssentialDownloadSizeRemainingSelector :: Selector
restrictedEssentialDownloadSizeRemainingSelector = mkSelector "restrictedEssentialDownloadSizeRemaining"

