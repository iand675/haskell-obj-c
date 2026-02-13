{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariPageProperties@.
module ObjC.SafariServices.SFSafariPageProperties
  ( SFSafariPageProperties
  , IsSFSafariPageProperties(..)
  , url
  , title
  , usesPrivateBrowsing
  , active
  , activeSelector
  , titleSelector
  , urlSelector
  , usesPrivateBrowsingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- url@
url :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO (Id NSURL)
url sfSafariPageProperties =
  sendMessage sfSafariPageProperties urlSelector

-- | @- title@
title :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO (Id NSString)
title sfSafariPageProperties =
  sendMessage sfSafariPageProperties titleSelector

-- | @- usesPrivateBrowsing@
usesPrivateBrowsing :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO Bool
usesPrivateBrowsing sfSafariPageProperties =
  sendMessage sfSafariPageProperties usesPrivateBrowsingSelector

-- | @- active@
active :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO Bool
active sfSafariPageProperties =
  sendMessage sfSafariPageProperties activeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @usesPrivateBrowsing@
usesPrivateBrowsingSelector :: Selector '[] Bool
usesPrivateBrowsingSelector = mkSelector "usesPrivateBrowsing"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

