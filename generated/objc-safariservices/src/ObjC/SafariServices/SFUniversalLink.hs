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
  , newSelector
  , initSelector
  , initWithWebpageURLSelector
  , webpageURLSelector
  , applicationURLSelector
  , enabledSelector
  , setEnabledSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFUniversalLink)
new  =
  do
    cls' <- getRequiredClass "SFUniversalLink"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO (Id SFUniversalLink)
init_ sfUniversalLink  =
  sendMsg sfUniversalLink (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize the receiver with a web URL that may or may not be a universal link.
--
-- ObjC selector: @- initWithWebpageURL:@
initWithWebpageURL :: (IsSFUniversalLink sfUniversalLink, IsNSURL url) => sfUniversalLink -> url -> IO (Id SFUniversalLink)
initWithWebpageURL sfUniversalLink  url =
withObjCPtr url $ \raw_url ->
    sendMsg sfUniversalLink (mkSelector "initWithWebpageURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | The URL passed when initializing the receiver.
--
-- ObjC selector: @- webpageURL@
webpageURL :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO (Id NSURL)
webpageURL sfUniversalLink  =
  sendMsg sfUniversalLink (mkSelector "webpageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The file URL to the application that can handle this universal link.
--
-- ObjC selector: @- applicationURL@
applicationURL :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO (Id NSURL)
applicationURL sfUniversalLink  =
  sendMsg sfUniversalLink (mkSelector "applicationURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether or not this universal link is enabled. If it is enabled, the URL will open in the application instead of the browser. Set this property when the user indicates they wish to enable or disable this universal link.
--
-- ObjC selector: @- enabled@
enabled :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> IO Bool
enabled sfUniversalLink  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfUniversalLink (mkSelector "enabled") retCULong []

-- | Whether or not this universal link is enabled. If it is enabled, the URL will open in the application instead of the browser. Set this property when the user indicates they wish to enable or disable this universal link.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFUniversalLink sfUniversalLink => sfUniversalLink -> Bool -> IO ()
setEnabled sfUniversalLink  value =
  sendMsg sfUniversalLink (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithWebpageURL:@
initWithWebpageURLSelector :: Selector
initWithWebpageURLSelector = mkSelector "initWithWebpageURL:"

-- | @Selector@ for @webpageURL@
webpageURLSelector :: Selector
webpageURLSelector = mkSelector "webpageURL"

-- | @Selector@ for @applicationURL@
applicationURLSelector :: Selector
applicationURLSelector = mkSelector "applicationURL"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

