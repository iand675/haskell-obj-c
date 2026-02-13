{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class represents a universal link available on the current system. Universal links can be opened in a browser or directly in an application.
--
-- Warning: The use of this class requires an entitlement.
--
-- Generated bindings for @SFUniversalLink@.
module ObjC.SafariServices.SFUniversalLink
  ( SFUniversalLink
  , IsSFUniversalLink(..)
  , new
  , init_
  , initWithWebpageURL
  , webpageURL
  , applicationURL
  , enabled
  , setEnabled
  , applicationURLSelector
  , enabledSelector
  , initSelector
  , initWithWebpageURLSelector
  , newSelector
  , setEnabledSelector
  , webpageURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFUniversalLink)
new  =
  do
    cls' <- getRequiredClass "SFUniversalLink"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO (Id SFUniversalLink)
init_ sfUniversalLink =
  sendOwnedMessage sfUniversalLink initSelector

-- | Initialize the receiver with a web URL that may or may not be a universal link.
--
-- ObjC selector: @- initWithWebpageURL:@
initWithWebpageURL :: (IsSFUniversalLink sfUniversalLink, IsNSURL url) => sfUniversalLink -> url -> IO (Id SFUniversalLink)
initWithWebpageURL sfUniversalLink url =
  sendOwnedMessage sfUniversalLink initWithWebpageURLSelector (toNSURL url)

-- | The URL passed when initializing the receiver.
--
-- ObjC selector: @- webpageURL@
webpageURL :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO (Id NSURL)
webpageURL sfUniversalLink =
  sendMessage sfUniversalLink webpageURLSelector

-- | The file URL to the application that can handle this universal link.
--
-- ObjC selector: @- applicationURL@
applicationURL :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO (Id NSURL)
applicationURL sfUniversalLink =
  sendMessage sfUniversalLink applicationURLSelector

-- | Whether or not this universal link is enabled. If it is enabled, the URL will open in the application instead of the browser. Set this property when the user indicates they wish to enable or disable this universal link.
--
-- ObjC selector: @- enabled@
enabled :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO Bool
enabled sfUniversalLink =
  sendMessage sfUniversalLink enabledSelector

-- | Whether or not this universal link is enabled. If it is enabled, the URL will open in the application instead of the browser. Set this property when the user indicates they wish to enable or disable this universal link.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> Bool -> IO ()
setEnabled sfUniversalLink value =
  sendMessage sfUniversalLink setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFUniversalLink)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFUniversalLink)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithWebpageURL:@
initWithWebpageURLSelector :: Selector '[Id NSURL] (Id SFUniversalLink)
initWithWebpageURLSelector = mkSelector "initWithWebpageURL:"

-- | @Selector@ for @webpageURL@
webpageURLSelector :: Selector '[] (Id NSURL)
webpageURLSelector = mkSelector "webpageURL"

-- | @Selector@ for @applicationURL@
applicationURLSelector :: Selector '[] (Id NSURL)
applicationURLSelector = mkSelector "applicationURL"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

